;;;
;;; Game-independent AI stuff
;;;

;; todo: probabilistic α-β (count scores for each move with normal search, and choose nondeterministically based on score)
;; todo: resource-bounded search (walk only n nodes in game tree -> bonus for deep forcing searches)
;; todo: a background searcher using the iterative or beam search versions, and use it to implement the hard AI opponents
;;		- start with a fairly narrow beam (say best 3 based on a local ply 2 search or direct eval)
;;		- have a maximum thinking time to avoid draining battery if left pondering for extended periods of time
;;			+ try not to start any more fans than necessary. think in bursts.
;; todo: add a unit test and benchmark the deterministic optimizations to plain minimax

(define-module lib-ai

	(export 
		make-random-player			; get-moves → player
		make-simple-player			; get-moves → do-move → eval-board → factor → player
		make-minimax-player			; ply → get-moves → do-move → eval-board → eval-final-board → allow-skip? → player 
		make-alphabeta-player		; ply → get-moves → do-move → eval-board → eval-final-board → allow-skip? → player
		make-ab-killer-player		; ply → get-moves → do-move → eval-board → eval-final-board → allow-skip? → player
		make-ab-fixnum-player		; ply → get-moves → do-move → eval-board → eval-final-board → allow-skip? → player
		make-iter-trail-player		; ply → get-moves → do-move → eval-board → eval-final-board → allow-skip? → player
		make-iter-killer-player		; ply → get-moves → do-move → eval-board → eval-final-board → allow-skip? → player
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


	;;; shared constants 

	(define no-move False)
	(define win   65535)		; <- stay within owl's fixnum range
	(define lose -65535)


	;;;
	;;; a classic run-of-the-mill minimax
	;;;

	; note, the below ones are non-heuristic optimizations of this one. only 
	; added to be able test speed and correctness. not for game use.

	(define (make-minimax-player ply get-moves do-move eval eval-final allow-skip?)

		(define (plan-ahead board color ply)
			(if (= ply 0)
				(values (eval board color) no-move)
				(let ((opts (get-moves board color)))
					(if (null? opts)
						(let ((opp-moves (get-moves board (opponent-of color))))
							(cond
								((null? opp-moves) (values (eval-final board color) no-move))
								(allow-skip? 
									(lets ((oscore omove (plan-ahead board (opponent-of color) ply)))
										(values (- 0 oscore) no-move)))
								(else
									; for example chess goes like this
									(values lose no-move))))
						(let loop ((opts opts) (score lose) (best (car opts)))
							(if (null? opts) 
								(values score best)
								(lets
									((os om (plan-ahead (do-move board (car opts) color) (opponent-of color) (- ply 1)))
									 (this-score (- 0 os)))
									(if (> this-score score)
										(loop (cdr opts) this-score (car opts))
										(loop (cdr opts) score best)))))))))

		(λ (board in last color)
			(lets ((score move (plan-ahead board color ply)))
				(values move (put in 'score score)))))

	; the below versions are mostly optimizations of the basic minimax algorithm.
	; they prune the search game tree while still finding an equally good move.
	; note that the move may be obviously be a different one, but it will always 
	; be as good one, according to the evaluation function.

	;;;
	;;; a classic fixed-ply minimax with α-β 
	;;;

	(define (make-alphabeta-player ply get-moves do-move eval eval-final allow-skip?)

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
				(values move (put in 'score score)))))

	;;;
	;;; a classic fixed-ply minimax with α-β and killer move heuristic 
	;;;

	;; both collecting the best opponent move and last best one (like now) seem to do 
	;; slightly worse than a vanilla alphabeta, at least in ataxx, so not in use for now.

	(define (lift-move lst move)
		(if move
			(let loop ((lst lst) (move move))
				(cond
					((null? lst) lst)
					((equal? (car lst) move) (cons move (cdr lst)))
					(else
						(let ((tl (loop (cdr lst) move)))
							(if (eq? tl lst) lst (cons (car lst) tl))))))
			lst))

	(define (make-ab-killer-player ply get-moves do-move eval eval-final allow-skip?)

		(define (plan-ahead board color α β ply great)
			(if (= ply 0)
				(values (eval board color) no-move)
				(let ((opts (lift-move (get-moves board color) great)))
					(if (null? opts)
						(let ((opp-moves (get-moves board (opponent-of color))))
							(cond
								((null? opp-moves) (values (eval-final board color) no-move))
								(allow-skip? 
									(lets ((oscore omove (plan-ahead board (opponent-of color) (- 0 β) (- 0 α) ply False)))
										(values (- 0 oscore) no-move)))
								(else
									(values lose no-move))))
						(let loop ((opts opts) (α α) (best (car opts)) (last False))
							(cond
								((null? opts) (values α best))
								((< α β)
									(lets
										((os om (plan-ahead (do-move board (car opts) color)
														(opponent-of color) (- 0 β) (- 0 α) (- ply 1) last))
										 (score (- 0 os)))
										(if (> score α)
											(loop (cdr opts) score (car opts) om)
											(loop (cdr opts) α best om))))
								(else (values α best))))))))

		(λ (board in last color)
			(lets ((score move (plan-ahead board color lose win ply False)))
				(values move (put in 'score score)))))

	;;;
	;;; Alphabeta with small constant primop/fixnum optimizations
	;;;

	; added just to see that they do not give any significant benefits (seems to be ~5%)

	(define flip negate)

	(define (less? a b)
		(if (teq? a fix+)
			(if (teq? b fix+) (lesser? a b) False)
			(if (teq? b fix+) True (lesser? b a))))

	(define (make-ab-fixnum-player ply get-moves do-move eval eval-final allow-skip?)

		(define (plan-ahead board color α β ply)
			(if (eq? ply 0)
				(values (eval board color) no-move)
				(let ((opts (get-moves board color)))
					(if (null? opts)
						(let ((opp-moves (get-moves board (opponent-of color))))
							(cond
								((null? opp-moves) (values (eval-final board color) no-move))
								(allow-skip? 
									(lets ((oscore omove (plan-ahead board (opponent-of color) (flip β) (flip α) ply)))
										(values (flip oscore) no-move)))
								(else
									; for example chess goes like this
									(values lose no-move))))
						(let ((ply (- ply 1)))
							(let loop ((opts opts) (α α) (best (car opts)))
								(cond
									((null? opts) (values α best))
									((less? α β)
										(lets
											((os om (plan-ahead (do-move board (car opts) color)
															(opponent-of color) (flip β) (flip α) ply))
											 (score (flip os)))
											(if (less? score α)
												(loop (cdr opts) α best)
												(loop (cdr opts) score (car opts)))))
									(else (values α best)))))))))

		(λ (board in last color)
			(lets ((score move (plan-ahead board color lose win ply)))
				(values move (put in 'score score)))))

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

	;;; iterative deepening while collecting trail of best moves

	(define (make-iter-trail-player ply get-moves do-move eval eval-final allow-skip?)

		(define plan-ahead (make-planner eval get-moves do-move eval-final allow-skip?))
		
		(λ (board in last color)
			(let loop ((p 1) (trail null) (score 0))
				(if (> p ply)
					(values 
						(if (null? trail) False (car trail))
						(put in 'score score))
					(lets
						((score trail 
							(plan-ahead board color lose win ply trail)))
						(loop (+ p 1) trail score))))))

	;;; iterative deepening without the trail, only for comparing against a run-of-the-mill alphabeta

	(define (make-iter-killer-player ply get-moves do-move eval eval-final allow-skip?)

		(define (plan-ahead board color α β ply great)
			(if (eq? ply 0)
				(values (eval board color) no-move)
				(let ((opts (lift-move (get-moves board color) great)))
					(if (null? opts)
						(let ((opp-moves (get-moves board (opponent-of color))))
							(cond
								((null? opp-moves) (values (eval-final board color) no-move))
								(allow-skip? 
									(lets ((oscore omove (plan-ahead board (opponent-of color) (- 0 β) (- 0 α) ply False)))
										(values (- 0 oscore) no-move)))
								(else
									(values lose no-move))))
						(let loop ((opts opts) (α α) (best (car opts)) (last False))
							(cond
								((null? opts) (values α best))
								((< α β)
									(lets
										((os om (plan-ahead (do-move board (car opts) color)
														(opponent-of color) (- 0 β) (- 0 α) (- ply 1) last))
										 (score (- 0 os)))
										(if (> score α)
											(loop (cdr opts) score (car opts) om)
											(loop (cdr opts) α best om))))
								(else (values α best))))))))

		(λ (board in last color)
			(let loop ((p 1) (score 0) (move False))
				(if (> p ply)
					(values move (put in 'score score))
						(lets ((score move (plan-ahead board color lose win ply move)))
							(loop (+ p 1) score move))))))


	;;; time bound player, stop planning after the given number of ms have passed

	(define (now-ms)
		(lets ((secs ms (clock)))
			(+ (* secs 1000) ms)))

	(define (make-time-bound-player ms get-moves do-move eval eval-final allow-skip?)
	
		(λ (board in last color)
			(lets 
				((timeout (+ (now-ms) ms)) ; when to return the result
				 (move
					(let loop ((trail null) (ply 1))
						;(print (list 'trail trail 'computing 'ply ply))
						(call/cc
							(λ (ret)
								(lets 
								 ((time-bounded-get-moves
									(λ (board color)
										(if (< (now-ms) timeout) 
											(get-moves board color)
											(begin
												;(print (list 'timeout 'returning 'from trail))
												(ret (if (null? trail) False (car trail)))))))
								  (plan-ahead 
									(make-planner eval time-bounded-get-moves do-move eval-final allow-skip?))
								  (score new-trail (plan-ahead board color lose win ply trail)))
								(print* (list "computed score " score " for ply " ply))
								(if (equal? trail new-trail) ; computed to game end
									(begin
										(show "computed rest of game: score minimum " score)
										(if (null? trail) False (car trail)))
									(loop new-trail (+ ply 1)))))))))
				(values move in))))

)

