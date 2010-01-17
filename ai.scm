;;;
;;; Game-independent AI stuff
;;;

(define-module lib-ai

	(export 
		make-random-player
		make-simple-player
		make-fixed-ply-player)

	(import lib-random)

	;;; a random AI

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
	;;; semirandom O(1) AIs 
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
	;;; a classical fixed-ply minimax α-β 
	;;;

	(define no-move False)

	(define win   65535)
	(define lose -65535)

	(define (make-fixed-ply-player ply get-moves do-move eval eval-final allow-skip?)

		(define (plan-ahead board color α β ply)
			(if (= ply 0)
				(values (eval-board board color) no-move) ; ← fixme, temp eval
				(let ((opts (valid-moves board color)))
					(if (null? opts)
						(let ((opp-moves (valid-moves board (opponent-of color))))
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
)
