;;;
;;; A quick sokoban hack
;;;

; todo, 
;	- stream events
;	- mouse support
;	- center puzzle
;  - info display (current lead, move)
;	- dead cell highlight
;	- command line arguments
;	- should propose fetching levels by (http-get X) when not given
;		+ there is no lib-http yet though
;	- skins

(import lib-lazy)
(import lib-vt)

(define empty   #b00)
(define box     #b01)
(define goal    #b10)
(define boxgoal #b11)
(define wall   #b100)

(define wall-byte 35)
(define player-byte 64)
(define box-byte 36)
(define empty-byte 32)
(define goal-byte 46)
(define boxgoal-byte 42)
(define playergoal-byte 43)

(define (put2 lev x y val)
	(put lev x
		(put (get lev x False) y val)))

; -> value, default empty
(define (get2 lev x y)
	(get (get lev x False) y empty))


;;;
;;; Reading levels
;;;

(define (skip-comment str)
	(cond
		((null? str) str)
		((pair? str)
			(if (eq? (car str) 10)
				(cdr str)
				(skip-comment (cdr str))))
		(else
			(skip-comment (str)))))

; udlr100

(define (format-walls cells)
	(ff-fold 
		(lambda (new x ys)
			(ff-fold
				(lambda (new y val)
					(if (eq? val wall)
						(let*
							((val (if (eq? (get2 cells x (- y 1)) wall) 
										(+ val #b1000000) val))	;up
							 (val (if (eq? (get2 cells x (+ y 1)) wall) 
										(+ val #b0100000) val))	;down
							 (val (if (eq? (get2 cells (- x 1) y) wall) 
										(+ val #b0010000) val))	;left
							 (val (if (eq? (get2 cells (+ x 1) y) wall) 
										(+ val #b0001000) val))	;right
							 )
							(put2 new x y val))
						(put2 new x y val)))
				new ys))
		False cells))


; -> ok? #(player lev) str
(define (parse-level str x y lev player walled)
	(flush-port 1)
	(cond
		((pair? str)
			(let ((hd (car str)) (str (cdr str)))
				(cond
					((eq? hd wall-byte)
						(parse-level str (+ x 1) y (put2 lev x y wall) 
							player True))
					((eq? hd player-byte)
						(if player
							(values False "Two players in a level." False)
							(parse-level str (+ x 1) y lev (tuple x y) True)))
					((eq? hd box-byte)
						(parse-level str (+ x 1) y (put2 lev x y box) player True))
					((eq? hd empty-byte)
						(parse-level str (+ x 1) y lev player walled))
					((eq? hd goal-byte)
						(parse-level str (+ x 1) y (put2 lev x y goal) 
							player True))
					((eq? hd boxgoal-byte)
						(parse-level str (+ x 1) y (put2 lev x y boxgoal) 
							player True))
					((eq? hd playergoal-byte)
						(if player
							(values False "Two players in a level." False)
							(parse-level str (+ x 1) y (put2 lev x y goal) 
								(tuple x y) walled)))
					((or (eq? hd 10) (not hd))
						(cond
							(walled
								(parse-level str 1 (+ y 1) lev player False))
							(player
								(values True (tuple player (format-walls lev)) str))
							(else
								(values False "No player in level" False))))
					(else
						(values False "Bad content" False)))))
		((null? str)
			(parse-level (list 10) x y lev player walled))
		(else
			(parse-level (str) x y lev player walled))))



(define (parse-levels str pos)
	(cond
		((null? str)
			null)
		((pair? str)
			(let ((hd (car str)) (tl (cdr str)))
				(cond
					((eq? hd wall-byte)
						(receive 
							(parse-level tl (+ pos 1) 1 (put2 False pos 1 wall) 
								False True)
							(lambda (ok level str)
								(display "*")
								(if ok
									(let ((tail (parse-levels str 1)))
										(if tail
											(cons level tail)
											False))
									(begin
										(show "Unable to read levels: " level)
										False)))))
					((eq? hd 10)
						(parse-levels tl 1))
					((eq? hd empty-byte)
						(parse-levels tl (+ pos 1)))
					((or (eq? hd 59) (eq? hd 39))
						(parse-levels (skip-comment tl) 1))
					((not hd)
						null)
					(else
						(show "Strange byte between levels: " hd)))))
		(else
			(parse-levels (str) pos))))
			



; levels = ((false|n-moves . level) ...)

(define (read-level-file path)
	(flush-port 1)
	(let ((port (open-input-file path)))
		(if port
			(let ((levels (parse-levels (port->byte-stream port) 1)))
				(cond
					((not levels)
						(show "failed to load levels from " path)
						null)
					((open-input-file (string-append path ".soko")) =>
						(lambda (s)
							(cond
								((read-file s 
									(lambda (err)
										(show "error in score file: " err)
										False)) =>
									(lambda (scores)
										(if (= (length scores) (length levels))
											(zip cons scores levels)
											(begin
												(show "bad number of scores" scores)
												(map
													(lambda (x) (cons False x))
													levels)))))
								(else
									(map (lambda (x) (cons False x)) levels)))))
					(else
						(map (lambda (x) (cons False x)) levels))))
			(begin
				(show "Failed to open levels file " path)
				null))))

(define level-offset 2)

;wall udlr100

; could use buttons to play on n810 without opening the keyboard
;       ┌───┐
;       │ ↑ │
;       └───┘
; ┌───┐ ┌───┐ ┌───┐
; │ ← │ │ ↓ │ │ → │
; └───┘ └───┘ └───┘
; or just click where to move

(define nodes
	(list->ff
		'(( #b00 . " ")
		  ( #b01 . "☐") ; o
		  ( #b10 . "·") ; ✮
		  ( #b11 . "☒") ; 0
		  ;  udlr
		  (#b0000100 . "❍") ; +
		  (#b0001100 . "─")
		  (#b0010100 . "─")
		  (#b0011100 . "─")
		  (#b0100100 . "│")
		  (#b0101100 . "┌") ; 
		  (#b0110100 . "┐") ; 
		  (#b0111100 . "┬")	; 3
		  (#b1000100 . "│")
		  (#b1001100 . "└") ; └
		  (#b1010100 . "┘")  
		  (#b1011100 . "┴") ; 3
		  (#b1100100 . "│")
		  (#b1101100 . "├") ; 3
		  (#b1110100 . "┤") ; 3
		  (#b1111100 . "┼") ; 4
		  (player . "@")
		  (playergoal . "a"))))

(define (plot-cell x y val)
	(set-cursor (+ x level-offset) (+ y level-offset))
	(display (get nodes val val)))

(define (print-level lev)
	(bind (cdr lev)
		(lambda (player cells)
			(clear-screen)
			; draw the cells
			(set-cursor 1 1)
			(ff-fold 
				(lambda (s x ys)
					(ff-fold
						(lambda (s y val)
							(plot-cell x y val))
						s ys))
				42 cells)

			; and the player
			(bind player
				(lambda (x y)
					(if (eq? (get2 cells x y) empty)
						(plot-cell x y 'player)
						(plot-cell x y 'playergoal))))

			; and update screen
			(flush-port 1)
			'done)))

(define (step x y dir)
	(cond
		((eq? dir 'up) (values x (- y 1)))
  		((eq? dir 'down) (values x (+ y 1)))
		((eq? dir 'left) (values (- x 1) y))
		((eq? dir 'right) (values (+ x 1) y))
		(else (error "Bad move: " dir))))

(define (movable value)
	(eq? 0 (fxband value #b1111111101)))

(define (pushable value)
	(eq? 1 (fxband value #b0000000001)))

(define (level-solved? cells)
	(call/cc
		(lambda (ret)
			(ff-fold
				(lambda (s x ys)
					(ff-fold
						(lambda (s y val)
							(if (eq? val box)
								(ret False)
								s))
						s ys))
				True cells))))

(define (try-move cells x y dir undo)
	; (x,y) is either goal or empty, and need not be updated
	(receive (step x y dir)
		(lambda (xp yp)
			(let ((val (get2 cells xp yp)))
				(cond
					((movable val)
						; move player to an empty cell
						(plot-cell x y (get2 cells x y))
						(values False cells xp yp 
							(cons (tuple 'step dir) undo)))

					((pushable val)
						; try to push a box
						(receive (step xp yp dir)
							(lambda (xpp ypp)
								(let ((to (get2 cells xpp ypp)))
									(cond
										((movable to)
											(let*
												((old (- val box))
												 (new (+ to box))
												 (cells (put2 cells xp yp old))
												 (cells (put2 cells xpp ypp new))
												 (undo (cons (tuple 'push dir) undo)))
												(plot-cell x y (get2 cells x y))
												(plot-cell xpp ypp new)
												(if (eq? new boxgoal)
													(values (level-solved? cells) cells xp yp undo)
													(values False cells xp yp undo))))
										(else
											(values False cells x y undo)))))))
					(else
						; cannot move
						(values False cells x y undo)))))))

; undo = step in direction or step in direction and pull object
(define (reverse dir)
	(cond
		((eq? dir 'up) 'down)
		((eq? dir 'down) 'up)
		((eq? dir 'left) 'right)
		((eq? dir 'right) 'left)
		(else (error "bad direction: " dir))))

(define (undo-move cells x y op)
	(tuple-case op
		((step dir)
			(receive (step x y (reverse dir))
				(lambda (xp yp)
					(plot-cell x y (get2 cells x y))
					(values cells xp yp))))
		((push dir)
			(receive (step x y (reverse dir))
				(lambda (nx ny)
					(receive (step x y dir)
						; pull object from (xp.yp) to (x.y) and move player to (nx.ny)
						(lambda (xp yp)				; <- the object that was pushed
							(let*
								((old (- (get2 cells xp yp) box))
								 (cells (put2 cells xp yp old))
								 (new (+ (get2 cells x y) box))
								 (cells (put2 cells x y new)))
								(plot-cell xp yp old)
								(plot-cell x y new)
								(values cells nx ny)))))))
		(else
			(error "Bad undo move: " op))))
	
(define (handle-move in cells x y dir undo cont)
	(receive (try-move cells x y dir undo)
		(lambda (solved cells x y undo)
			(if solved 
				(length undo)
				(cont in cells x y undo)))))

; just stream vt events, so that a mouse click can extend the stream

(define (play-level in cells x y undo)
	(let ((val (get2 cells x y)))
		(if (eq? val empty)
			(plot-cell x y 'player)
			(plot-cell x y 'playergoal)))
	(set-cursor 1 1)
	(display "> ") (display (length undo)) (clear-right)
	(flush-port 1)
	(receive (parse-vt in)
		(lambda (ok val in)
			(if ok
				(tuple-case val
					((arrow dir)	
						(handle-move in cells x y dir undo play-level))
					((key k)
						(cond
							((eq? k 113) ; [q]uit
								'quit)
							((eq? k 114) ; [r]estart
								'restart)
							((eq? k 110) ; [n]ext
								'next)
							((eq? k 78)  ; [N]ext unsolved
								'next-unsolved)
							((eq? k 104) ; [h] left
								(handle-move in cells x y 'left undo play-level))
							((eq? k 106) ; [j] down
								(handle-move in cells x y 'down undo play-level))
							((eq? k 107) ; [k] up
								(handle-move in cells x y 'up undo play-level))
							((eq? k 108) ; [l] right
								(handle-move in cells x y 'right undo play-level))
							((eq? k 117) ; [u]ndo
								(if (null? undo)
									(play-level in cells x y undo)
									(receive (undo-move cells x y (car undo))
										(lambda (cells x y)
											(play-level in cells x y (cdr undo))))))
							(else
								(play-level in cells x y undo))))
					((mouse mx my btn)
						(set-cursor mx my)
						(mail 1 42)
						(flush-port 1)
						(play-level in cells x y undo))
					(else
						(play-level in cells x y undo)))
				(play-level in cells x y undo)))))

(define (start-level lev)
	(print-level lev)
	(bind (cdr lev)
		(lambda (player cells)
			(bind player
				(lambda (x y)
					(play-level (port->byte-stream 0) cells x y null))))))

(define (update-score-file levels)
	(let ((port (open-output-file (string-append (get levels 'path False) ".soko"))))
		(if port
			(let loop ((id 1))
				(let ((this (get levels id False)))
					(if this
						(begin
							(mail port (render (car this) (list 32)))
							(loop (+ id 1)))
						(close-port port)))))))
				
; levels = ff of id -> (score . level)

(define (play-levels levels id)
	(let ((lev (get levels id False)))
		(if lev
			(let ((res (start-level lev)))
				(cond
					((number? res)
						(if (or (not (car lev)) (< res (car lev)))
							(let ((levels (put levels id (cons res (cdr lev)))))
								(update-score-file levels)
								(play-levels levels (+ id 1)))
							(play-levels levels (+ id 1))))
					((eq? res 'quit) 'bye)
					((eq? res 'restart) (play-levels levels id))
					((eq? res 'next) (play-levels levels (+ id 1)))
					((eq? res 'next-unsolved) 
						(let loop ((pos id))
							(let ((this (get levels pos False)))
								(cond
									((not this) 'done)
									((not (car this)) (play-levels levels pos))
									(else (loop (+ pos 1)))))))
					(else
						(error "strange outcome: " res)))))
			'done))

(define (print-sokoban-info)
	(set-cursor 1 1) (display "Sokoban v0.001")
	(set-cursor 1 2) (display "Controls")
	(set-cursor 1 3) (display " movement: left | down | up | right")
	(set-cursor 1 4) (display "           h | j | k | l")
	(set-cursor 1 6) (display " commands: [n]ext level")
	(set-cursor 1 7) (display "           [r]estart current level")
	(set-cursor 1 8) (display "           [u]ndo last move(s)")
	(set-cursor 1 9) (display "           [q]uit sokoban")
	(set-cursor 1 10) (display "           [N]ext unsolved level")
	(set-cursor 1 11) (display "")
	(set-cursor 1 12) (display "Loading levels:")
	)

(define (index lst pos) (if (null? lst) null (cons (cons pos (car lst)) (index (cdr lst) (+ pos 1)))))

(define no-levels-lol
"I need a file with some levels.
You can do worse than to start with http://users.bentonrea.com/~sasquatch/sokoban/m1.
")
(define (start-sokoban files)
	(if (= (length files) 1)
		(begin
			(raw-console)
			(clear-screen)
			(mouse-click-tracking True)
			(print-sokoban-info)
			(let ((levels (list->ff (index (foldr append null (map read-level-file files)) 1))))
				(if (not levels)
					(begin
						(print "No levels, no game.")
						0)
					(begin
						(set-cursor 1 15)
						(display "Press any key to begin.")
						(flush-port 1)
						(interact 0 'input)
						(play-levels (put levels 'path (car files)) 1)
						0))
				(mouse-click-tracking False)
				(normal-console)
				))
		(begin
			(print no-levels-lol)
			0)))

; to dump a sokoban heap
(dump 
	(lambda (args) 
		(set-signal-action 'halt)
		(start-sokoban (cdr args))) 
	"soko.c")

;(start-sokoban (list "levels-microban"))

