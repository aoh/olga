;;;
;;; Sudoku 
;;; 

; - start indexing positios from 0
; - rename to moniku
;	+ add support to latin squares and other variants
;- level generation
;		+ at build time, separate files or on the fly?
;		+ where, if anywhere, to store the results?
;			- ah, $ sudoku --make-levels --difficulty easy --count 100 > my-own-levels.sdk
;			- and keep the format such that when levels are solved the time can be saved there
;			- when a level file is not given, just generate a fresh level 
; - [h]elp = show current status (unsolvable, solvable, hard step but solvable, many solutions)
; - [H]ALP = show next most restricted move and it's options

(import lib-lazy)

(define (remove lst val)
	(cond
		((null? lst) lst)
		((eq? (car lst) val) (cdr lst))
		(else (cons (car lst) (remove (cdr lst) val)))))

(define rows
	(map (λ (s) (iota s 1 (+ s 9))) (iota 1 9 82)))

(define add (λ a (λ b (+ a b))))

(define cols 
	(map (λ (o) (map (add o) (iota 1 9 82))) (iota 0 1 9)))

(define (make-region start)
	(let ((base (list start (+ start 1) (+ start 2))))
		(append base (append (map (add 9) base) (map (add 18) base)))))

(define regions
	(map make-region
		(list 1 4 7 
			(+ 1 27) (+ 4 27) (+ 7 27)
			(+ 1 54) (+ 4 54) (+ 7 54))))

(define all-options (iota 1 1 10))

(define all-areas 
	(foldr append null (list rows cols regions)))

(define (areas-of pos)
	(map (λ (x) (remove x pos))
		(keep (λ (x) (has? x pos)) all-areas)))

(define *areas*
	(list->tuple (map areas-of (iota 1 1 82))))
		
(define (remove-dups lst)
	(if (null? lst)
		null
		(cons (car lst)
			(remove-dups (remove (cdr lst) (car lst))))))

(define (peers-of pos)
	(remove (sort < (remove-dups (foldr append null (areas-of pos)))) pos))

(define *peers*
	(list->tuple (map peers-of (iota 1 1 82))))

(define rasa all-options)

(define (try-options fn opts)
	(for False opts
		(λ (soln move) (or soln (fn move)))))

(define (show-state state)
	(for-each
		(λ (pos)
			(let ((node (get state pos rasa)))
				(cond
					((number? node)
						(display node))
					((null? (cdr node))
						(display (car node)))
					(else 
						(display ".")))))
		(iota 1 1 82))
	(print ""))

(define (check-area update board area this)
	(if board
		(let 
			((options 
				(keep (λ (pos) (has? (get board pos rasa) this)) area)))
			(cond
				((eq? options null)
					False)
				((eq? (cdr options) null)
					(update board (car options) this))
				(else board)))
		False))

(define (eliminate update board pos val)
	(if board
		(let ((this (get board pos rasa)))
			(cond
				((eq? this val)
					False)
				((pair? this)
					(if (has? this val)
						(fold
							(λ (board area)
								(check-area update board area val))			; all areas must potentially have the value
							(lets 	
								((this (remove this val))
								 (board (put board pos this)))
								(cond
									((null? this) False) ; no options 
									((null? (cdr this)) ; only one
										(fold
											(λ (board pos) 
												(eliminate update board pos (car this)))
											board (ref *peers* pos)))					; no peer can have the same value
									(else 
										board)))
							(ref *areas* pos))
						board))
				(else board)))
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
;;; Playing part
;;;

; note, the above could be useful for level generation. playing needs only the 
; stuff below.


(import lib-vt)

(define (cell-of x y) (+ (* y 9) x))
; off by one occasionally -> change indexing, not this
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
		 (bad (collisions sudoku (ref *peers* cell) n)))
		(cond
			((bound? sudoku cell)
				False)
			((null? bad)
				(put sudoku cell n))
			(else False))))

(define mid-row    "+---+---+---+")
(define normal-row "|   |   |   |")

(define x-start 3)
(define y-start 2)

(define (plot-cell x y val)
	(lets
		((tx (+ (+ x x-start) (div (- x 1) 3)))
	 	 (ty (+ (+ y y-start) (+ 1 (div y 3)))))
		(cond
			((eq? val 'cursor)
				(set-cursor tx ty))
			(val
				(set-cursor tx ty)
				(display val))
			(else 42))))

(define (print-sudoku state)
	(for-each
		(λ (y) 
			(set-cursor x-start (+ y y-start))
			(display (if (= (rem y 4) 0) mid-row normal-row)))
		(iota 0 1 13))
	(for False (iota 1 1 10)
		(λ (_ x)
			(for False (iota 0 1 9)
				(λ (_ y)
					(let ((pos (+ (* y 9) x)))
						(if (bound? state pos)
							(begin
								(output-set-bold)
								(plot-cell x y (get state pos False))
								(output-mode-normal))
							(plot-cell x y (get state pos False))))))))
	(flush-port 1)
)

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

(define (show-solvability sudo x y)
	(set-cursor 1 16)
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
							; (set-cursor 1 17) (show-state res) ; cheat
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
						(set-cursor 1 16)
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
			(set-cursor 1 16)
			(show "No action for key " key)
			(values sudo x y prev))))

(define (play-sudoku sudo in x y prev) 
	(print-sudoku sudo)
	(plot-cell x y 'cursor)
	(flush-port 1)
	(lets ((ok val in (parse-vt in)))
		(clear-screen)
		(if ok
			(cond
				((up? val)
					(play-sudoku sudo in x (max 0 (- y 1)) prev))
				((down? val)
					(play-sudoku sudo in x (min 8 (+ y 1)) prev))
				((left? val)
					(play-sudoku sudo in (max 1 (- x 1)) y prev))
				((right? val)
					(play-sudoku sudo in (min 9 (+ x 1)) y prev))
				((eq? (ref val 1) 'key)
					(lets ((sudo x y prev (apply-action sudo x y prev (ref val 2))))
						(if sudo
							(play-sudoku sudo in x y prev)
							; should ask if want to save the better scores (if any)
							(begin
								(clear-screen)
								(set-cursor 1 1)
								(print "Bye.")
								'bye))))
				(else
					(set-cursor 1 16)
					(show "that's funny: " val)
					(play-sudoku sudo in x y prev))))))

(define (string->sudoku str)
	(for empty-sudoku 
		(zip cons (iota 1 1 82) (string->runes str))
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

(import lib-args)

(define usage-text 
"Usage: sudoku
Arrow- and vi-keys move the cursor, space and backspace erase the current
value and u undoes your moves. Numbers are used to write down numbers.

Arguments:")

(define about-text 
"This is a simple sudoku program. This version lacks
most features. A better version is hopefully available at
http://code.google.com/p/olgame/")

(define command-line-rules
   (cl-rules
      `((about "-A" "--about")
        (help "-h" "--help")
        (version "-V" "--version")
		  (level "-s" "--sudoku" has-arg default "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
        )))

(define (lets-sudoku level)
	(raw-console)
	(clear-screen)
	(play-sudoku 
		level
		(port->byte-stream 0) 1 0 null)
	(normal-console))

(define (sudoku-entry vm-args)
	(set-signal-action 'halt)	; no thread controller, just exit when breaked
	(or
		(process-arguments (cdr vm-args) command-line-rules usage-text
			(λ (dict others)
				(cond 
					((get dict 'help False)
						(print usage-text)
						(print-rules command-line-rules)
						0)
					((get dict 'about False)
						(print about-text)
						0)
					((get dict 'version False)
						(print "Yes")
						0)
					((get dict 'level False) =>
						(λ (proposal)
							; fixme, catch errors and check solvability
							(lets-sudoku (string->sudoku proposal))
							0))
					(else 
						(print "sudoku: no level given.")
						0))))
		(begin
			(print "que?")
			1)))


(sudoku-entry '(sudoku "."))

(dump sudoku-entry "sudoku.c")

