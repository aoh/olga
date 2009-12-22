;;;
;;; Sudoku 
;;; 

; empty sudoku can now be played, but i forgot the solver will automatically 
; fill in also all logical consequences thus helping a bit ;)

; what to do instead
;	- make a simplified version of the current solver
;		+ use for hints and level generation
;		+ bit twiddlin not necessary, just about anything is solvable in a few ms anyway
;	- level generation
;		+ at build time, separate files or on the fly?
;		+ where, if anywhere, to store the results?
;			- ah, $ sudoku --make-levels --difficulty easy --count 100 > my-own-levels.sdk
;			- and keep the format such that when levels are solved the time can be saved there
;			- when a level file is not given, just generate a fresh level 
;	- different geometries and variants later
;	- [h]elp = show current status (unsolvable, solvable, hard step but solvable, many solutions)
;	- [H]ALP = show next most restricted move and it's options

(import lib-lazy)

(define (remove lst a)
	(keep (λ (x) (not (eq? x a))) lst))

(define rows
	(map (λ (start) (iota start 1 (+ start 9))) 
		(iota 1 9 82)))

(define add (λ a (λ b (+ a b))))

(define cols 
	(map
		(λ (offset) (map (add offset) (iota 1 9 82)))
		(iota 0 1 9)))

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

(define rasa #b1111111111000)

(define-syntax available?
	(syntax-rules ()
		((available? a bit)
			(eq? bit (fxband a bit)))))

(define-syntax freeness
	(syntax-rules ()
		((freeness x)
			(fxband x #b1111))))

(define (show-digit n d)
	(cond
		((eq? n 1)
			(display d))
		((eq? n 0)
			(error "digit went to 0: " d))
		(else
			(show-digit (>> n 1) (+ d 1)))))

(define all-options (map (λ i (<< 1 i)) (iota 4 1 13)))

(define (try-options fn opts this)
	(cond
		((eq? this #b10000000000000)
			False)
		((eq? this (fxband opts this))
			(let ((soln (fn this)))
				(if soln soln
					(try-options fn opts (<< this 1)))))
		(else
			(try-options fn opts (<< this 1)))))

(define (options bits)
	(keep (λ node (eq? node (fxband bits node))) all-options))

(define (show-bits n)
	(for-each
		(λ (option)
			(if (available? n option)
				(show-digit (>> option 4) 1)
				(display "x")))
		all-options)
	(mail 1 32))

(define (show-state state)
	(for-each
		(λ (pos)
			(let ((node (get state pos rasa)))
				(if (= (freeness node) 0)
					(show-digit (>> node 4) 1)
					(display "."))))
		(iota 1 1 82))
	(print ""))

(define (check-area update board area this)
	(if board
		(let 
			((options 
				(keep (λ (pos) (available? (get board pos rasa) this)) area)))
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
			(if (available? this val)
				(if (eq? this val)
					False							; error, removing the only possible (maybe assigned) value
					(let*
						((this (- this (+ val 1)))
						 (board (put board pos this))
						 (board
							(if (eq? (freeness this) 0)
								(begin
									(fold
										(λ (board pos) 
											(eliminate update board pos this))
										board (ref *peers* pos)))					; no peer can have the same value
								board)))
						(fold
							(λ (board area)
								(check-area update board area val))			; all areas must potentially have the value
							board
							(ref *areas* pos))))
				board))
		False))

; assign value to node
; check that all areas have the removed options

(define (update state pos val)
	(let ((all (get state pos rasa)))
		(cond
			((not state) False)
			((available? all val)
				(fold
					(λ (state ruled-out)
						(eliminate update state pos ruled-out))
					state
					(options (- all val))))
			(else False))))

(define no-node (cons 100 False))

; state = ff of pos -> status
(define (most-restricted state)
	(call/cc
		(λ (return)
			(let ((best
				(ff-fold
					(λ (best pos val)
						(let ((this (freeness val)))
							(cond
								((eq? this 0) best)
								((eq? this 1) (return pos))		
								((< this (car best)) (cons this pos))
								(else best))))
					no-node
					state)))
				(if (eq? best no-node)
					(if state False 1)
					(cdr best))))))


(define (solve state)
	(if state
		(let ((next (most-restricted state)))
			(if next
				(try-options
					(λ (option)
						(solve (update state next option)))
					(get state next rasa)
					#b10000)
				state))
		False))

;;; and a quick test

; eww, the original used byte-based io. a quick change to strings here.

(define (read-solve-board state pos in)
	(if (null? in)
		False
		(let ((byte (car in)) (in (cdr in)))
			(cond
				((not state) False)
				((eof? byte) False)
				((eq? byte 46)
					(read-solve-board state (+ pos 1) in))
				((and (< 47 byte) (< byte 58))
					(read-solve-board
						(update state pos (<< 1 (+ (- byte 48) 3)))
						(+ pos 1) in))
				((and (eq? byte 10) state)
					(display " <- ")
					(show-state state)
					(let ((res (solve state)))
						(if res
							(begin
								(display " -> ")
								(show-state res)
								(print "")
								in)
							(begin
								(print " *** NOT SOLVED ***")
								False))))
				(else
					(read-solve-board state pos in))))))
				
(define empty-sudoku (put False 1 rasa))

(define (play-boards str)
	(let loop ((input (string->bytes str)))
		(let ((input (read-solve-board empty-sudoku 1 input)))
			(if input (loop input) 'done))))
				
(show-state
	(solve (update empty-sudoku 1 #b10000)))

;; old test levels, probably from some collection of hard sudokus. NOT MINE!
(play-boards 
";4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......
;52...6.........7.13...........4..8..6......5...........418.........3..2...87.....
;6.....8.3.4.7.................5.4.7.3..2.....1.6.......2.....5.....8.6......1....
;48.3............71.2.......7.5....6....2..8.............1.76...3.....4......5....
;....14....3....2...7..........9...3.6.1.............8.2.....1.4....5.6.....7.8...
;......52..8.4......3...9...5.1...6..2..7........3.....6...1..........7.4.......3.
;6.2.5.........3.4..........43...8....1....2........7..5..27...........81...6.....
;.524.........7.1..............8.2...3.....6...9.5.....1.6.3...........897........
;6.2.5.........4.3..........43...8....1....2........7..5..27...........81...6.....
;.923.........8.1...........1.7.4...........658.........6.5.2...4.....7.....9.....
;6..3.2....5.....1..........7.26............543.........8.15........4.2........7..
;.6.5.1.9.1...9..539....7....4.8...7.......5.8.817.5.3.....5.2............76..8...
;..5...987.4..5...1..7......2...48....9.1.....6..2.....3..6..2.......9.7.......5..
;3.6.7...........518.........1.4.5...7.....6.....2......2.....4.....8.3.....5.....
;1.....3.8.7.4..............2.3.1...........958.........5.6...7.....8.2...4.......
;6..3.2....4.....1..........7.26............543.........8.15........4.2........7..
;....3..9....2....1.5.9..............1.2.8.4.6.8.5...2..75......4.1..6..3.....4.6.
;45.....3....8.1....9...........5..9.2..7.....8.........1..4..........7.2...6..8..
;.237....68...6.59.9.....7......4.97.3.7.96..2.........5..47.........2....8.......
;..84...3....3.....9....157479...8........7..514.....2...9.6...2.5....4......9..56
;.98.1....2......6.............3.2.5..84.........6.........4.8.93..5...........1..
;..247..58..............1.4.....2...9528.9.4....9...1.........3.3....75..685..2...
;4.....8.5.3..........7......2.....6.....5.4......1.......6.3.7.5..2.....1.9......
;.2.3......63.....58.......15....9.3....7........1....8.879..26......6.7...6..7..4
;1.....7.9.4...72..8.........7..1..6.3.......5.6..4..2.........8..53...7.7.2....46
;4.....3.....8.2......7........1...8734.......6........5...6........1.4...82......
;.......71.2.8........4.3...7...6..5....2..3..9........6...7.....8....4......5....
;6..3.2....4.....8..........7.26............543.........8.15........8.2........7..
;.47.8...1............6..7..6....357......5....1..6....28..4.....9.1...4.....2.69.
;......8.17..2........5.6......7...5..1....3...8.......5......2..4..8....6...3....
;38.6.......9.......2..3.51......5....3..1..6....4......17.5..8.......9.......7.32
;...5...........5.697.....2...48.2...25.1...3..8..3.........4.7..13.5..9..2...31..
;.2.......3.5.62..9.68...3...5..........64.8.2..47..9....3.....1.....6...17.43....
;.8..4....3......1........2...5...4.69..1..8..2...........3.9....6....5.....2.....
;..8.9.1...6.5...2......6....3.1.7.5.........9..4...3...5....2...7...3.8.2..7....4
;4.....5.8.3..........7......2.....6.....5.8......1.......6.3.7.5..2.....1.8......
;1.....3.8.6.4..............2.3.1...........958.........5.6...7.....8.2...4.......
;1....6.8..64..........4...7....9.6...7.4..5..5...7.1...5....32.3....8...4........
;249.6...3.3....2..8.......5.....6......2......1..4.82..9.5..7....4.....1.7...3...
;...8....9.873...4.6..7.......85..97...........43..75.......3....3...145.4....2..1
;...5.1....9....8...6.......4.1..........7..9........3.8.....1.5...2..4.....36....
;......8.16..2........7.5......6...2..1....3...8.......2......7..3..8....5...4....
;.476...5.8.3.....2.....9......8.5..6...1.....6.24......78...51...6....4..9...4..7
;.....7.95.....1...86..2.....2..73..85......6...3..49..3.5...41724................
;.4.5.....8...9..3..76.2.....146..........9..7.....36....1..4.5..6......3..71..2..
;.834.........7..5...........4.1.8..........27...3.....2.6.5....5.....8........1..
;..9.....3.....9...7.....5.6..65..4.....3......28......3..75.6..6...........12.3.8
;.26.39......6....19.....7.......4..9.5....2....85.....3..2..9..4....762.........4
;2.3.8....8..7...........1...6.5.7...4......3....1............82.5....6...1.......
;6..3.2....1.....5..........7.26............843.........8.15........8.2........7..
;1.....9...64..1.7..7..4.......3.....3.89..5....7....2.....6.7.9.....4.1....129.3.
;.........9......84.623...5....6...453...1...6...9...7....1.....4.5..2....3.8....9
;.2....5938..5..46.94..6...8..2.3.....6..8.73.7..2.........4.38..7....6..........5
;9.4..5...25.6..1..31......8.7...9...4..26......147....7.......2...3..8.6.4.....9.
;...52.....9...3..4......7...1.....4..8..453..6...1...87.2........8....32.4..8..1.
;53..2.9...24.3..5...9..........1.827...7.........981.............64....91.2.5.43.
;1....786...7..8.1.8..2....9........24...1......9..5...6.8..........5.9.......93.4
;....5...11......7..6.....8......4.....9.1.3.....596.2..8..62..7..7......3.5.7.2..
;.47.2....8....1....3....9.2.....5...6..81..5.....4.....7....3.4...9...1.4..27.8..
;......94.....9...53....5.7..8.4..1..463...........7.8.8..7.....7......28.5.26....
;.2......6....41.....78....1......7....37.....6..412....1..74..5..8.5..7......39..
;1.....3.8.6.4..............2.3.1...........758.........7.5...6.....8.2...4.......
;2....1.9..1..3.7..9..8...2.......85..6.4.........7...3.2.3...6....5.....1.9...2.5
;..7..8.....6.2.3...3......9.1..5..6.....1.....7.9....2........4.83..4...26....51.
;...36....85.......9.4..8........68.........17..9..45...1.5...6.4....9..2.....3...
;34.6.......7.......2..8.57......5....7..1..2....4......36.2..1.......9.......7.82
;......4.18..2........6.7......8...6..4....3...1.......6......2..5..1....7...3....
;.4..5..67...1...4....2.....1..8..3........2...6...........4..5.3.....8..2........
;.......4...2..4..1.7..5..9...3..7....4..6....6..1..8...2....1..85.9...6.....8...3
;8..7....4.5....6............3.97...8....43..5....2.9....6......2...6...7.71..83.2
;.8...4.5....7..3............1..85...6.....2......4....3.26............417........
;....7..8...6...5...2...3.61.1...7..2..8..534.2..9.......2......58...6.3.4...1....
;......8.16..2........7.5......6...2..1....3...8.......2......7..4..8....5...3....
;.2..........6....3.74.8.........3..2.8..4..1.6..5.........1.78.5....9..........4.
;.52..68.......7.2.......6....48..9..2..41......1.....8..61..38.....9...63..6..1.9
;....1.78.5....9..........4..2..........6....3.74.8.........3..2.8..4..1.6..5.....
;1.......3.6.3..7...7...5..121.7...9...7........8.1..2....8.64....9.2..6....4.....
;4...7.1....19.46.5.....1......7....2..2.3....847..6....14...8.6.2....3..6...9....
;......8.17..2........5.6......7...5..1....3...8.......5......2..3..8....6...4....
;963......1....8......2.5....4.8......1....7......3..257......3...9.2.4.7......9..
;15.3......7..4.2....4.72.....8.........9..1.8.1..8.79......38...........6....7423
;..........5724...98....947...9..3...5..9..12...3.1.9...6....25....56.....7......6
;....75....1..2.....4...3...5.....3.2...8...1.......6.....1..48.2........7........
;6.....7.3.4.8.................5.4.8.7..2.....1.3.......2.....5.....7.9......1....
;....6...4..6.3....1..4..5.77.....8.5...8.....6.8....9...2.9....4....32....97..1..
;.32.....58..3.....9.428...1...4...39...6...5.....1.....2...67.8.....4....95....6.
;...5.3.......6.7..5.8....1636..2.......4.1.......3...567....2.8..4.7.......2..5..
;.5.3.7.4.1.........3.......5.8.3.61....8..5.9.6..1........4...6...6927....2...9..
;..5..8..18......9.......78....4.....64....9......53..2.6.........138..5....9.714.
;..........72.6.1....51...82.8...13..4.........37.9..1.....238..5.4..9.........79.
;...658.....4......12............96.7...3..5....2.8...3..19..8..3.6.....4....473..
;.2.3.......6..8.9.83.5........2...8.7.9..5........6..4.......1...1...4.22..7..8.9
;.5..9....1.....6.....3.8.....8.4...9514.......3....2..........4.8...6..77..15..6.
;.....2.......7...17..3...9.8..7......2.89.6...13..6....9..5.824.....891..........
;3...8.......7....51..............36...2..4....7...........6.13..452...........8..
")

; ----

;;;
;;; Playing part
;;;

; note, the above could be useful for level generation. playing needs only the 
; stuff below.

(define empty-sudoku (put False 1 False))

(import lib-vt)

(define (cell-of x y) (+ (* y 9) x))

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


(sudoku-entry '(sudoku))

(dump sudoku-entry "sudoku.c")

