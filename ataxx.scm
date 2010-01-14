;;;
;;; ataxx
;;;

; started with basic gameplay. a saner ai next.

(define usage-text "Usage: ataxx [args]")

(define about-ataxx
"ataxx - a clone of the game ataxx
This game is from http://code.google.com/p/olgame.

fixme: rules and history here.
")

(import lib-args)
(import lib-vt)

(define xo 3)
(define yo 2)

(define black 'black)
(define white 'white)

(define (render-cell val)
	(cond
		((eq? val black) "● ")
		((eq? val white) "○ ")
		(else "· ")))

(define s 7) 

(define cells (let ((max (* s s))) (λ () (iota 0 1 max))))

(define (position-cursor x y)
	(set-cursor (+ xo (* x 2)) (+ yo y)))

(define (print-board board x y)
	(clear-screen)
	(for 42 (iota 0 1 s)
		(λ (_ y)
			(set-cursor xo (+ y yo))
			(for 42 (iota 0 1 s)
				(λ (_ x)
					(display (render-cell (get board (+ x (* y s)) 'blank)))))))
	(position-cursor x y)
	(flush-port 1))

(define (move-focus board x y dir)
	(cond
		((eq? dir 'up) (values x (max 0 (- y 1))))
		((eq? dir 'down) (values x (min (- s 1) (+ y 1))))
		((eq? dir 'left) (values (max 0 (- x 1)) y))
		((eq? dir 'right) (values (min (- s 1) (+ x 1)) y))
		(else (error "now that's a thought! let's all move to " dir))))

(define (xy->pos x y) (+ (* y s) x))

(define (over? p) (or (< p 0) (>= p s)))

(define (grab pos offs) 
	(lets ((x (rem pos s)) (y (div pos s)))
		(for null offs (λ (peers off)
			(lets ((dx dy off) (x (+ x dx)) (y (+ y dy)))
				(cond
					((over? x) peers)
					((over? y) peers)
					(else (cons (+ x (* y s)) peers))))))))

(define (uniq l)
	(if (null? l)
		null
		(cons (car l) 
			(remove (λ x (equal? x (car l))) (cdr l)))))

; ((dx . dy) ...)
(define (neighbours-at distance)
	(let ((offs (iota (- 0 distance) 1 (+ distance 1))))
		(uniq 
			(for null offs (λ (x a) 
				(for x offs (λ (x b) 
					(if (or (= (abs a) distance) (= (abs b) distance))
						(cons (cons a b) x)
						x))))))))

(define neighbour-offsets (neighbours-at 1))
(define jump-offsets (neighbours-at 2))

(define neighbours
	(list->ff (map (λ pos (cons pos (grab pos neighbour-offsets))) (cells))))

(define jumps
	(list->ff (map (λ pos (cons pos (grab pos jump-offsets))) (cells))))

(define (pick-winner board)
	(define status
		(for (tuple 0 0 0) (cells)
			(λ (status pos)
				(let ((val (get board pos False)))
					(cond
						((eq? val 'white) (set status 1 (+ (ref status 1) 1)))
						((eq? val 'black) (set status 2 (+ (ref status 2) 1)))
						(else             (set status 3 (+ (ref status 3) 1))))))))
	(cond
		((= 0 (ref status 3)) ; 0 free cells
			(let ((w (ref status 1)) (b (ref status 2)))
				(cond
					((> w b) white)
					((> b w) black)
					(else 'draw))))
		((= 0 (ref status 1)) 'black) ; white wiped out
		((= 0 (ref status 2)) 'white) ; black wiped out
		(else False)))
			
(define (blanks-in board poss out)
	(for null poss
		(λ out pos
			(if (get board pos False) 
				out 
				(cons pos out)))))

(define (valid-moves-of board pos)
	(values
		(blanks-in board (get neighbours pos 'bug) null)
		(blanks-in board (get jumps pos 'bug) null)))

(define (valid-moves board player)
	(ff-fold
		(λ moves pos val
			(if (eq? val player)
				(lets ((clones jumps (valid-moves-of board pos)))
					(if (and (null? clones) (null? jumps))
						moves
						(cons (tuple pos clones jumps) moves)))
				moves))
		null board))

(define (opponent-of x) (if (eq? x black) white black))

(define (blank? board pos)
	(eq? False (get board pos False)))

(define (blank? board pos)
	(eq? 'blank (get board pos 'blank)))

(define (valid-move? board source pos)
	(cond
		((has? (get neighbours source null) pos) True)
		((has? (get jumps source null) pos) True)
		(else False)))

(define (human-player board in pos color) ; → move|false|quit target in
	(let ((moves (valid-moves board color)))
		(if (null? moves)
			(values False pos in)
			(let loop ((in in) (x (rem pos s)) (y (div pos s)) (source False))
				(print-board board x y)
				(position-cursor x y)
				(flush-port 1)
				(cond
					((null? in) (values 'quit False in))
					((pair? in)
						(tuple-case (car in)
							((arrow dir)
								(lets ((x y (move-focus board x y dir)))
									(loop (cdr in) x y source)))
							((key k)
								; faactoor
								(case k
									((32) 
										(let ((pos (+ x (* y s))))
											(cond
												((blank? board pos)
													(if (and source (valid-move? board source pos))
														(values source pos (cdr in))
														(loop (cdr in) x y False)))
												((eq? color (get board pos False))
													(loop (cdr in) x y pos))
												(else
													(loop (cdr in) x y False)))))
									((113) ; [q]uit
										(values 'quit 'quit (cdr in)))
									(else
										(loop (cdr in) x y source))))
							(else
								(loop (cdr in) x y source))))
					(else (loop (in) x y source)))))))


;;; artificial stupidity begins

; imbecile - play randomly 
(define-module lib-ai-imbecile
	(export ai-imbecile)
	(import lib-random)

	(define (board-seed board)
		(ff-fold
			(λ (seed pos val)
				(if (eq? pos black) (* seed 2) (+ seed 1)))
			(time 1) board))

	(define (ai-imbecile board in last color)
		(lets
			((opts (valid-moves board color))
			 (seed (board-seed board)))
			(if (null? opts)
				(values False False in)
				(lets 
					((rst n (rand seed (length opts)))
					 (move (lref opts n))
					 (from moves jumps move)
					 (targets (append moves (append moves jumps)))
					 (rst n (rand seed (length targets)))
					 (target (lref targets n)))
					(values from target in))))))

(import lib-ai-imbecile ai-imbecile)

(define empty-board 
	(list->ff
		(list
			(cons 0 black)
			(cons (- s 1) white)
			(cons (* s (- s 1)) white)
			(cons (- (* s s) 1) black))))

(define players
	(list->ff
		(list 
			(cons ai-imbecile "imbecile") 
			(cons human-player "human")
			)))

(define (choose-player str)
	(let ((choice (ff-fold (λ (taken op name) (if (equal? name str) op taken)) False players)))
		(if (function? choice)
			choice
			(begin
				(show "Unknown player. I know " (map cdr (ff->list players)))
				False))))

(define (choose-side str)
	(cond
		((equal? str "black") black)
		((equal? str "white") white)
		(else False)))

(define command-line-rules
	(cl-rules 
		`((about "-A" "--about")
		  (help  "-h" "--help")
		  (black "-b" "--black" cook ,choose-player default "human" comment "choose black player")
		  (white "-w" "--white" cook ,choose-player default "imbecile" comment "choose white player")
		  )))

(define (board-full? board)
	(call/cc 
		(λ (ret)
			(for-each (λ pos (if (blank? board pos) (ret False))) (cells))
			True)))

(define (report-winner winner)
	(set-cursor 1 10)
	(cond
		((eq? winner black)
			(print "The black knight always triumphs."))
		((eq? winner white)
			(print "The white wizard is victorius."))
		(else
			(print "All right. We'll call it a draw."))))

(define (disqualify player reason)
	(show "Player disqualified due to " reason)
	(sleep 1000)
	(opponent-of player))

(define (make-move board pos player)
	(for (put board pos player) (get neighbours pos null)
		(λ (board pos) 
			(if (get board pos False)
				(put board pos player)
				board))))

(define (make-jump board from to player)
	(make-move (del board from) to player))

; -> black | white | draw | quit
(define (match board in pos next player opponent)
	(print-board board (rem pos s) (div pos s))
	(cond
		((pick-winner board) =>
			(λ (winner)
				(report-winner winner)
				; wait for a key press unless war of the ais
				(if (or (eq? player human-player) (eq? opponent human-player))
					(interact 0 'input))
				winner))
		(else
			(lets ((move to in (player board in pos next)))
				(if move
					(cond
						; is the player in the from position
						((eq? next (get board move False))
							; is the move target ok
							(lets ((moves jumps (valid-moves-of board move)))
								(cond
									((has? moves to)
										(match (make-move board to next) in to 
											(opponent-of next) opponent player))
									((has? jumps to)
										(match (make-jump board move to next) in to
											(opponent-of next) opponent player))
									(else
										(disqualify next "an erronous move target.")))))
						((eq? move 'quit)
							'quit)
						(else
							(disqualify next "an erronous move.")))
					(match board in pos (opponent-of next) opponent player))))))

; names have to be printed differently, because rendering asks function
; names are from the 'meta thread, which is (stupid and) kind of useless 
; to have running around in dumped code.

(define (name-of player)
	(if (eq? player 'draw)
		"draw"
		(get players player "mysterious")))

(define (start-match black-player white-player)
	(let loop ((status False) (bp black-player) (wp white-player))
		(lets
			((res 
				(match empty-board (vt-events 0) 0 black bp wp))
			 (status
				(cond
					((eq? res black) (put status bp (+ 1 (get status bp 0))))
					((eq? res white) (put status wp (+ 1 (get status wp 0))))
					((eq? res 'draw) (put status 'draw (+ 1 (get status 'draw 0))))
					(else status))))
			(show "res is " res)
			(if (eq? res 'quit)
				(begin
					(print (del status res))
					0)
				(begin
					(clear-screen)
					(set-cursor 1 1)
					(show "outcomes: "
						(ff-fold (lambda (out player score) (cons (cons (name-of player) score) out)) null status))
					(sleep 500)
					(loop status wp bp))))))

(define (play-ataxx args)
	(raw-console)
	(lets
		((board empty-board) 
		 (white (get args 'white 'bug))
		 (black (get args 'black 'bug))
		 (result (start-match black white)))
		(normal-console)
		(clear-screen)
		(set-cursor 1 1)
		(print "Bye bye.")
		0))

(define (ataxx args)
	(or 
		(process-arguments (cdr args) command-line-rules usage-text
			(λ (dict others)
				(cond
					((get dict 'about False) 
						(print about-ataxx))
					((get dict 'help False) 
						(print usage-text)
						(print-rules command-line-rules))
					(else
						(play-ataxx dict)))))
		1))


; (ataxx '("ataxx"))

(dump ataxx "ataxx.c")

