;;;
;;; Game-independent AI stuff
;;;

; todo
;	- beam search (n best)
;	- resource bounded search (n nodes)
;	- α-β optimizations (killer heuristic and others)
;	- a 'recursive wedge search', which tries to simulate 

(define-module lib-ai

	(export 
		make-random-player			; get-moves → player
		make-simple-player			; get-moves → do-move → eval-board → factor → player
		make-fixed-ply-player		; ply → get-moves → do-move → eval-board → eval-final-board → allow-skip? → player
		make-iterative-ply-player	; ply → get-moves → do-move → eval-board → eval-final-board → allow-skip? → player
		make-time-bound-player 		; ms get-moves do-move eval eval-final allow-skip?
	)

	;(import lib-random)
	(import lib-vt) ; debugging

	;;; all games are conceptually between black and white 

	(define (opponent-of x)
		(if (eq? x 'black) 'white 'black))

	;;;
	;;; Play random valid moves (usually called imbecile)
	;;;

	(define (make-random-player get-moves) 
		(λ (board in last color) ; → move|false x in'
			(lets
				((opts (get-moves board color))
				 (ss ms (clock))
				 (seed (expt ms 3)))
				(if (null? opts)
					(values False in)
					(lets 
						((rst n (rand seed (length opts)))
						 (move (lref opts n)))
						(values move in))))))

	;;;
	;;; An AI playing statistically to places which score higher using the eval function
	;;;

	(define (grab-move ps n)
		(if (null? ps)
			(error "could not grab move: " n)
			(let ((this (cdr (car ps))))
				(if (<= n this)
					(car ps)
					(grab-move (cdr ps) (- n this))))))

	(define (select-move proposals rst)
		(if (null? proposals)
			(cons False 0)
			(lets
				((min (+ 1 (abs (fold (λ (lead prop) (min lead (cdr prop))) 0 proposals))))
				 (proposals
					(map (λ (prop) (set prop 2 (+ (ref prop 2) min))) proposals))
				 (total (fold + 0 (map cdr proposals)))
				 (rst n (rand rst total)))
				(grab-move proposals n))))

	; look forward one move and see how good the situations are, and make 
	; a weighted move to the better half of the moves

	(define (make-simple-player get-moves do-move evaluate factor)
		(λ (board in last color)
			(lets
				((ss ms1 (clock))
				 (opts (get-moves board color))
				 (proposals  ; ((move . score) ...)
					(map (λ (move) (cons move (evaluate (do-move board move color) color))) opts))
				 (proposals (sort (λ (a b) (> (cdr a) (cdr b))) proposals))
				 (proposals (take proposals (max 1 (div (length proposals) factor))))
				 (ss ms2 (clock))
				 (move (select-move proposals (* ms1 ms2))))
				(values (car move) in))))

	;;;
	;;; a classic fixed-ply minimax with α-β 
	;;;

	(define no-move False)

	(define win   65535)
	(define lose -65535)

	(define (make-fixed-ply-player ply get-moves do-move eval eval-final allow-skip?)

		(define (plan-ahead board color α β ply)
			(if (= ply 0)
				(values (eval board color) no-move)
				(let ((opts (get-moves board color)))
					(if (null? opts)
						(let ((opp-moves (get-moves board (opponent-of color))))
							(cond
								((null? opp-moves) (values (eval-final board color) no-move))
								(allow-skip? 
									(lets ((oscore omove (plan-ahead board (opponent-of color) (- 0 β) (- 0 α) ply)))
										(values (- 0 oscore) no-move)))
								(else
									; for example chess goes like this
									(values lose no-move))))
						(let loop ((opts opts) (α α) (best (car opts)))
							(cond
								((null? opts) (values α best))
								((< α β)
									(lets
										((os om (plan-ahead (do-move board (car opts) color)
														(opponent-of color) (- 0 β) (- 0 α) (- ply 1)))
										 (score (- 0 os)))
										(if (> score α)
											(loop (cdr opts) score (car opts))
											(loop (cdr opts) α best))))
								(else (values α best))))))))

		(λ (board in last color)
			(lets ((score move (plan-ahead board color lose win ply)))
				(values move in))))

	;;;
	;;; Iterative deepening α-β with best trail 
	;;;

	(define (rest trail)
		(if (null? trail) trail (cdr trail)))

	(define (drop-move l x)
		(cond
			((null? l) False)
			((equal? (car l) x) (cdr l))
			(else 
				(let ((tl (drop-move (cdr l) x)))
					(if tl (cons (car l) tl) False)))))

	(define (lift moves trail)
		(cond
			((null? trail) moves)
			((drop-move moves (car trail)) => (λ (moves) (cons (car trail) moves)))
			(else moves)))

	(define (make-planner eval get-moves do-move eval-final allow-skip?)

		(define (plan-ahead board color α β ply moves) ; → score x (move ...), being the best move sequence for *both* players
			(if (eq? ply 0)
				(values (eval board color) null)
				(let ((opts (lift (get-moves board color) moves)))
					(if (null? opts)
						(let ((opp-moves (get-moves board (opponent-of color))))
							(cond
								((null? opp-moves) (values (eval-final board color) null))
								(allow-skip? 
									(lets ((oscore omoves (plan-ahead board (opponent-of color) (- 0 β) (- 0 α) ply (rest moves))))
										(values (- 0 oscore) (cons no-move omoves))))
								(else
									; for example chess goes like this
									(values lose null))))
						(let loop ((opts opts) (α α) (best (cons (car opts) (rest moves))))
							(cond
								((null? opts) 
									(values α best))
								((< α β)
									(lets
										((n oms 
											(plan-ahead (do-move board (car opts) color)
												(opponent-of color) (- 0 β) (- 0 α)
												(- ply 1) (rest moves)))
										 (score (- 0 n)))
										(if (> score α)
											(loop (cdr opts) score (cons (car opts) oms))
											(loop (cdr opts) α best))))
								(else (values α best))))))))
			plan-ahead)

	(define (make-iterative-ply-player ply get-moves do-move eval eval-final allow-skip?)

		(define plan-ahead (make-planner eval get-moves do-move eval-final allow-skip?))
		
		(λ (board in last color)
			(values 
				(car
					(fold
						(λ (trail ply)	
							(lets 
								((score trail 
									(plan-ahead board color lose win ply trail)))
								trail))
						null (iota 1 1 (+ ply 1))))
				in)))

	;;; time bound player, stop planning after the given number of ms have passed

	(define (now-ms)
		(lets ((secs ms (clock)))
			(+ (* secs 1000) ms)))

	(define (make-time-bound-player ms get-moves do-move eval eval-final allow-skip?)
	
		(λ (board in last color)
			(lets ((timeout (+ (now-ms) ms))) ; when to return the result
				(values
					(let loop ((trail null) (ply 1))
						(call/cc
							(lambda (ret)
								(lets 
								 ((time-bounded-get-moves
									(λ (board color)
										(if (< (now-ms) timeout) 
											(get-moves board color) 
											(ret (if (null? trail) False (car trail))))))
								  (plan-ahead 
									(make-planner eval time-bounded-get-moves do-move eval-final allow-skip?))
								  (score new-trail (plan-ahead board color lose win ply trail)))
								(loop new-trail (+ ply 1))))))
					in))))

)

