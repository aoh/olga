;;;
;;; Sudoku 
;;; 

(define-module olgame-sudoku

	(export sudoku-node)

	(import lib-grale)

	;;;
	;;; Sudoku solving
	;;;

	; backtracking search on most restricted non-unique values propagating 
	; the basic constraints recursively.

	(define (remove lst val)
		(cond
			((null? lst) lst)
			((eq? (car lst) val) (cdr lst))
			(else (cons (car lst) (remove (cdr lst) val)))))

	(define (cells) (iota 0 1 81))

	(define rows
		(map (λ (s) (iota s 1 (+ s 9))) (iota 0 9 81)))

	(define add (λ a (λ b (+ a b))))

	(define cols 
		(map (λ (o) (map (add o) (iota 0 9 81))) (iota 0 1 9)))

	(define (make-region start)
		(let ((base (list start (+ start 1) (+ start 2))))
			(append base (append (map (add 9) base) (map (add 18) base)))))

	(define regions
		(map make-region
			(list 0 3 6 
				(+ 0 27) (+ 3 27) (+ 6 27)
				(+ 0 54) (+ 3 54) (+ 6 54))))

	(define all-options (iota 1 1 10))

	(define all-areas 
		(foldr append null (list rows cols regions)))

	(define (areas-of pos)
		(map (λ (x) (remove x pos))
			(keep (λ (x) (has? x pos)) all-areas)))

	(define *areas*
		(list->ff
			(map (λ (p) (cons p (areas-of p))) (cells))))
		
	(define (remove-dups lst)
		(if (null? lst)
			null
			(cons (car lst)
				(remove-dups (remove (cdr lst) (car lst))))))

	(define (peers-of pos)
		(remove (sort < (remove-dups (foldr append null (areas-of pos)))) pos))

	(define *peers*
		(list->ff (map (λ (p) (cons p (peers-of p))) (cells))))

	(define rasa all-options)

	(define (try-options fn opts)
		(for False opts
			(λ (soln move) (or soln (fn move)))))

	; check that each area has at least one cell which can have a
	; given value, and if there is only one such cell, set it.

	(define (check-availability update board area this)
		(if board
			(let 
				((options 
					(keep (λ (pos) (has? (get board pos rasa) this)) area)))
				(cond
					((eq? options null) False)
					((eq? (cdr options) null)
						(update board (car options) this))
					(else board)))
			False))

	(define (eliminate update board pos val)
		(if board
			(let ((this (get board pos rasa)))
				(if (has? this val)
					(fold
						(λ (board area)
							(check-availability update board area val))			; all areas must potentially have the value
						(lets 	
							((this (remove this val))
							 (board (put board pos this)))
							(cond
								((null? this) False) ; no options 
								((null? (cdr this)) ; only one option left
									(fold
										(λ (board pos) 
											(eliminate update board pos (car this)))
										board (get *peers* pos 'bug)))					; no peer can have the same value
								(else board)))
						(get *areas* pos 'bug))
					board))
			False))

	(define (update state pos val)
		(let ((all (get state pos rasa)))
			(cond
				((not state) False)
				((has? all val)
					(for state (remove all val)
						(λ (state ruled-out)
							(eliminate update state pos ruled-out))))
				(else False))))

	(define no-node (cons False all-options))

	(define (shorter? a b)
		(cond
			((null? b) False)
			((null? a) True)
			(else (shorter? (cdr a) (cdr b)))))

	(define empty-sudoku (put False 1 rasa))

	; state = ff of pos -> status
	(define (most-restricted state)
		(call/cc
			(λ (return)
				(let ((best
					(ff-fold
						(λ (best pos val) ; best = (move|False . opts)
							(cond
								((eq? pos 'bound) best) ; skip level info
								((and (pair? val) (pair? (cdr val)) (shorter? val (cdr best)))
									(cons pos val))
								(else best)))
						no-node state)))
					(if (eq? best no-node)
						(if (eq? state empty-sudoku) 1 False)
						(car best))))))

	(define (solve state)
		(if state
			(let ((next (most-restricted state)))
				(if next
					(try-options
						(λ (option) (solve (update state next option)))
						(get state next rasa))
					state)) ; solved
			False))

	(define (solved? state)
		(and state
			(eq? state (solve state))))

	(define (level->node level)
		(ff-fold
			(lambda (sudoku pos val)
				(if (number? pos)
					(and sudoku (update sudoku pos val))
					sudoku))
			empty-sudoku level))


	;;;
	;;; Sudoku generation
	;;;

	; a suitable sudoku for now is one which can be solved by logical
	; inference (as provided by the solver) without backtracking. this 
	; should later be the medium difficulty. hard ones have at least 
	; one move which has two options, only one of which will be correct 
	; and apparent after plotting it.

	(define (suitable? sudoku)
		(ff-fold (λ (is pos opts) (and is (= (length opts) 1))) True sudoku))

	(define (moves->sudoku moves) ; moves = ((pos . val) ...)
		(for (put False 'bound (list->ff moves)) moves
			(λ (sudoku move) (put sudoku (car move) (cdr move)))))

	(define (maybe-make-sudoku rst sudo n moves)
		(cond
			((suitable? sudo) (display "$") (values rst (moves->sudoku moves)))
			((> (length moves) 28) (values rst False))
			((> n 20) (values rst False))
			(else
				(lets
					((rst pos (rand rst 81))
					 (opts (get sudo pos rasa))
					 (rst try-opt (rand rst (length opts)))
					 (val (lref opts try-opt))
					 (new-sudoku (update sudo pos val)))
					(if new-sudoku
						(maybe-make-sudoku rst new-sudoku 0 (cons (cons pos val) moves))
						(maybe-make-sudoku rst sudo (+ n 1) moves))))))

	; limit x to [lo ... hi-1]
	(define (limit lo x hi)
		(cond
			((< x lo) lo)
			((< x hi) x)
			(else (- hi 1))))

	(define (make-sudoku rst)
		(grale-put-text font-8px 10 20 #b00011100  "Making a sudoku")
		(grale-update 0 0 320 200)
		(let loop ((rst rst) (x (div 320 2)) (y (div 200 2)) (col 255))
			(lets ((rst sudoku (maybe-make-sudoku rst empty-sudoku 0 null)))
				(if sudoku
					(begin
						(print " done")
						sudoku)
					(lets
						((rst dx (rand rst 3))
						 (rst dy (rand rst 3))
						 (rst dc (rand rst 3)))
						(grale-put x y col)
						(grale-update x y 1 1)
						(loop rst 
							(limit 0 (+ (- dx 1) x) 320)
							(limit 0 (+ (- dy 1) y) 200)
							(limit 0 (+ (- dc 1) col) 256)))))))

	;;;
	;;; Playing part
	;;;

	; the board are here ffs with cell -> number and 'bound -> ff of values given in the puzzle.

	(define (cell-of x y) (+ (* y 9) x))
	(define (coords-of cell) (values (rem cell 9) (div cell 9)))

	(define (collisions sudo poss val)
		(if (null? poss)
			null
			(let ((this (get sudo (car poss) False)))
				(if (equal? this val)
					(cons (car poss)
						(collisions sudo (cdr poss) val))
					(collisions sudo (cdr poss) val)))))

	(define (bound? sudoku pos)
		(get (get sudoku 'bound False) pos False))

	(define (plot sudoku x y n)
		(lets
			((cell (cell-of x y))
			 (bad (collisions sudoku (get *peers* cell 'bug) n)))
			(cond
				((bound? sudoku cell)
					False)
				((null? bad)
					(put sudoku cell n))
				(else 
					; could highlight the collisions 
					False))))

	(define x-start 30)
	(define y-start 30)

	(define (plot-cell x y val col)
		(grale-put-text font-8px (+ x-start (* x 10)) (+ y-start (* y 10)) col 
			(list->string (render (or val "x") null))))

	(define (print-sudoku state)
		(grale-fill-rect 0 0 320 200 0)
		(for False (iota 0 1 9)
			(λ (_ x)
				(for False (iota 0 1 9)
					(λ (_ y)
						(let ((pos (+ (* y 9) x)))
							(if (bound? state pos)
								(plot-cell x y (get state pos False) 255)
								(plot-cell x y (get state pos False) #b10010010)))))))
		(grale-update 0 0 320 200))

	(define (dir-check dir keycode)
		(λ (event)
			(tuple-case event
				((arrow d) (eq? d dir))
				((key k) (eq? k keycode))
				(else False))))

	(define up? (dir-check 'up 107))
	(define down? (dir-check 'down 106))
	(define left? (dir-check 'left 104))
	(define right? (dir-check 'right 108))

	(define (propose-move node level)
		(ff-fold
			(λ (best pos opts)
				(cond
					((get level pos False) best)
					((not best) pos)
					((< (length (get node pos rasa)) (length (get node best rasa))) pos)
					(else best)))
			False node))

	(define (solvable? sudo) (solve (level->node sudo)))

	(define (show-solvability sudo x y)
		(lets
			((sudoku (level->node sudo))
			 (res (solve sudoku)))
			(cond
				((not sudoku)
					(print "Arr, you are hosed.")
					(values x y))
				((not res)
					(print "Arr, you will be hosed.")
					(values x y))
				(else
					; note, most restricted does not give the definite ones
					(let ((best (propose-move sudoku sudo)))
						(if best
							(lets 
								((x y (coords-of best))
								 (opts (get sudoku best all-options)))
								(display "You are doing fine. ")
								(if (= (length opts) 1)
									(print* (list "This must logically be " (car opts) "."))
									(print* (list "This is one of " opts ".")))
								(values x y))
							(begin
								(print "You are done.")
								(values x y))))))))

	(define (apply-action sudo x y prev key) ; → sudo x y prev
		(case key
			((49 50 51 52 53 54 55 56 57)
				(let ((res (plot sudo x y (- key 48))))
					(if res
						(values res x y (cons (tuple x y sudo) prev))
						(begin
							(show "Cannot put here: " (- key 48))
							(values sudo x y prev)))))
			((32 127) ; [back]space
				(lets
					((cell (cell-of x y))
					 (val (get sudo cell False)))
					(if (and val (not (bound? sudo cell)))
						(values (del sudo (cell-of x y)) x y (cons (tuple x y sudo) prev))
						(values sudo x y prev))))
			((117)	; [u]ndo
				(if (null? prev) ; no can do
					(values sudo x y prev)
					(lets ((old (car prev)) (x y sudo old))
						(values sudo x y (cdr prev)))))
			((113) ; [q]uit
				(values False x y prev))	; caught by play-sudoku
			((116) ; [t]rouble
				(lets ((x y (show-solvability sudo x y)))
					(values sudo x y prev)))
			(else
				(show "No action for key " key)
				(values sudo x y prev))))

	(define (solved? sudo)
		(fold (λ (is pos) (and is (get sudo pos False))) True (cells)))

	(define (play-sudoku sudo in x y prev) 
		(clear-screen)
		(print-sudoku sudo)
		(if (solved? sudo)
			(begin
				(print "There, you fixed it!")
				0)
			(let loop ((in in) (x x) (y y))
				(lets ((ok val in (values 1 2 3)))
					(if ok
						(cond
							((up? val) (loop in x (max 0 (- y 1))))
							((down? val) (loop in x (min 8 (+ y 1))))
							((left? val) (loop in (max 0 (- x 1)) y))
							((right? val) (loop in (min 8 (+ x 1)) y))
							((eq? (ref val 1) 'key)
								(lets ((sudo x y prev (apply-action sudo x y prev (ref val 2))))
									(if sudo
										(play-sudoku sudo in x y prev)
										; should ask if want to save the better scores (if any)
										(begin
											(print "Bye.")
											'bye))))
							(else
								(show "that's funny: " val)
								(play-sudoku sudo in x y prev))))))))

	(define (string->sudoku str)
		(for False
			(zip cons (cells) (string->runes str))
			(λ (sudoku node)
				(lets ((cell char node))
					(cond
						((and (< 48 char) (< char 58))
							(let ((val (- char 48)))
								(put 
									(put sudoku cell val)
									'bound
									(put (get sudoku 'bound False) cell val))))
						((eq? char 46)
							sudoku)
						(else
							(show "funny char in sudoku: " char)
							sudoku))))))

	(define (seed)
		(lets ((ss ms (clock))) (expt (* ss (+ ms 1)) (+ 1 (band ms 3)))))

	(define (sudoku)
		(grale-fill-rect 0 0 320 200 0)
		(print-sudoku (make-sudoku (seed)))
		(lets ((x y (grale-wait-click))) 42))

	; (sudoku-entry '(sudoku))

	(define sudoku-node 
		(tuple 'proc False "sudoku" sudoku))

)

(import olgame-sudoku)

(define olgame-games 
	(cons sudoku-node olgame-games))


