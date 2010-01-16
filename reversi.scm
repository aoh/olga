;;;
;;; reversi - it's like othello
;;;

(define usage-text "Usage: reversi [args]")

(define about-reversi
"reversi - a game of reversi
this game is from http://code.google.com/p/olgame

Use the arrow keys move and space to enter your move.
By default you will be playing against the easy AI. 
You can choose the players using the -b and -w flags.
You are human. Pres s to skip a move.
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

(define s 8)

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

(define (print-moves poss color)
	(let ((marker (if (eq? color 'black) "•" "◦")))
		(for-each
			(λ (pos)
				(set-cursor (+ xo (* (rem pos s) 2)) (+ yo (div pos s)))
				(display marker))
			poss)
		(flush-port 1)))
	
(define (move-focus board x y dir)
	(cond
		((eq? dir 'up) (values x (max 0 (- y 1))))
		((eq? dir 'down) (values x (min (- s 1) (+ y 1))))
		((eq? dir 'left) (values (max 0 (- x 1)) y))
		((eq? dir 'right) (values (min (- s 1) (+ x 1)) y))
		(else (error "now that's a thought! let's move to " dir))))

(define (xy->pos x y) (+ (* y s) x))

; (x . y)
(define dirs '((-1 . -1) (0 . -1) (+1 . -1) (-1 . 0) (+1 . 0) (-1 . +1) (0 . +1) (+1 . +1)))

(define (over? p) (or (< p 0) (>= p s)))

(define (read-neighbours x y dx dy)
	(cond
		((over? x) null)
		((over? y) null)
		(else (cons (xy->pos x y) (read-neighbours (+ x dx) (+ y dy) dx dy)))))

(define (neighbours x y)
	(map (λ (dir) (cdr (read-neighbours x y (car dir) (cdr dir)))) dirs))
(define (board-full? board)
	(call/cc 
		(λ (ret) 
			(for-each 
				(λ (pos) (if (eq? (get board pos 'blank) 'blank) (ret False)))
				(cells))
			True)))

(define (pick-winner board)
	(define score 
		(ff-fold (λ (n pos val) (+ n (if (eq? val black) 1 -1))) 0 board))
	(cond
		((< score 0) white)
		((= score 0) 'draw)
		(else black)))

(define (opponent-of x) (if (eq? x black) white black))

(define (add-flips board ns wanted taken)
	(if (null? ns) 
		null
		(let ((val (get board (car ns) 'blank)))
			(cond
				((eq? val wanted)
					(add-flips board (cdr ns) wanted (cons (car ns) taken)))
				((eq? val 'blank) null)
				(else taken)))))

(define (flips-of board x y color)
	(let ((wanted (opponent-of color)))
		(for null (neighbours x y)
			(λ (flips ns)
				(append (add-flips board ns wanted null) flips)))))

(define (blank? board pos)
	(eq? 'blank (get board pos 'blank)))

(define (valid-moves board color)
	(fold
		(lambda (ok pos)
			(if (blank? board pos)
				(let ((flips (flips-of board (rem pos s) (div pos s) color)))
					(if (null? flips)
						ok
						(cons (cons pos flips) ok)))
				ok))
		null (cells)))

(define (valid-move? board x y color)
	(and (blank? board (xy->pos x y))
		(not (null? (flips-of board x y color)))))

(define (human-player board in x y color)
	(let ((moves (map car (valid-moves board color))))
		(if (null? moves)
			(values False x y)
			(let loop ((in in) (x x) (y y))
				(print-board board x y)
				(print-moves moves color)
				(position-cursor x y)
				(flush-port 1)
				(cond
					((null? in)
						(print "bye"))
					((pair? in)
						(tuple-case (car in)
							((arrow dir)
								(lets ((x y (move-focus board x y dir)))
									(loop (cdr in) x y)))
							((key k)
								(case k
									((32) ; [space] move here (if applicable)
										(if (valid-move? board x y color)
											(values True (cdr in) x y)
											(loop (cdr in) x y)))
									((113) ; [q]uit
										(values False (cdr in) 'quit 'quit))
									((115) ; [s]kip
										(values False x y))
									(else
										(loop (cdr in) x y))))
							(else
								(loop (cdr in) x y))))
					(else (loop (in) x y)))))))


;;; artificial stupidity begins


; imbecile - play randomly 
(define-module lib-ai-imbecile

	(export imbecile)

	(import lib-random)

	(define max (* s s))

	(define poss (list->ff (zip cons (iota 0 1 max) (iota 0 1 max))))

	(define (imbecile board in x y color)
		;(print-board board x y)
		(let loop ((free poss) (rst (time 1)))
			(if (not free) 
				(values False in x y)
				(lets ((rst pos (rand rst max)))
					(if (get free pos False)
						(lets
							((x (rem pos s))
							 (y (div pos s)))
							(if (valid-move? board x y color)
								(values True in x y)
								(loop (del free pos) rst)))
						(loop free rst)))))))

(import lib-ai-imbecile imbecile)


; stupid - play with weights without lookahead

(define-module lib-ai-stupid

	(export stupid)

	; off the top of my head. 
	(define scores
		(list->ff
			(zip cons (cells)
				(list  9  -6  3  2  2  3  -6  9
						-6  -6  2  1  1  2  -6 -6
						 3   2  3  2  2  3   2  3
						 2   1  2  1  1  2   1  2
						 2   1  2  1  1  2   1  2
						 3   2  3  2  2  3   2  3
						-6  -6  2  1  1  2  -6 -6
				       9  -6  3  2  2  3  -6  9))))

	(define (stupid board in x y color)
		(lets
			((moves (map car (valid-moves board color)))
			 (moves (map (lambda (x) (cons (get scores x 'bug) x)) moves))
			 (moves (sort (lambda (a b) (> (car a) (car b))) moves)))
			(if (null? moves)	
				(values False in x y)
				(lets
					((best (cdar moves))
					 (x (rem best s))
					 (y (div best s)))
					(values True in x y))))))

(import lib-ai-stupid stupid)

; normal - the run-of-the-mill alpha-beta optimized minimax 

(define empty-board 
	(list->ff
		(list
			(cons (+ (* 3 s) 3) white)
			(cons (+ (* 3 s) 4) black)
			(cons (+ (* 4 s) 3) black)
			(cons (+ (* 4 s) 4) white))))

(define-module lib-ai-normal

	(export fixed-ply-player)

	; again off the top of my head. compete ais later to see which are good scores
	; and, it's symmetric so you only need half of one quadrant, or compute the 
	; scores by a function and learn good parameters from a tournament (prettier solution)
	; this would also work for a more progress-aware evaluation function which would score 
	; different aspects at different parts of the game. then it would also be able to 
	; make coffee and read email.

	(define scores
		(list->ff
			(zip cons (cells)
				(list 20  -3  4  2  2  4  -3 20
						-3  -3  2  1  1  2  -3 -3
						 4   2  4  2  2  4   2  4
						 2   1  2  1  1  2   1  2
						 2   1  2  1  1  2   1  2
						 4   2  4  2  2  4   2  4
						-3  -3  2  1  1  2  -3 -3
				      20  -3  4  2  2  4  -3 20))))

	; normally you would value mobility. this is the first test.
	(define (evaluate board color)
		(ff-fold
			(λ (score pos val)
				(if (eq? val color)
					(+ score (get scores pos 0))
					(- score (get scores pos 0))))
			0 board))

	(define win-score 65535)
	(define lose-score -65535)

	(define (evaluate-game-over board me)
		(let ((winner (pick-winner board)))
			(cond
				((eq? winner me) win-score)
				((eq? winner 'draw) 0)
				(else lose-score))))

	(define (make-move board cells color)
		(for board cells
			(λ (board pos) (put board pos color))))

	(define (plan-ahead board color α β ply)	
		(if (= ply 0)
			(values (evaluate board color) False)
			(let ((moves (valid-moves board color)))
				; sorting by scores would probably lead to more cutoffs
				(if (null? moves)
					(let ((opp-moves (valid-moves board (opponent-of color))))
						(if (null? opp-moves)
							(values (evaluate-game-over board color) False)
							(lets ((oscore omove (plan-ahead board (opponent-of color) (- 0 β) (- 0 α) ply)))
								(values (- 0 oscore) False))))
					(let loop ((moves moves) (α α) (best (caar moves)))
						(cond
							((null? moves)
								(values α best))
							((< α β)
								(lets
									((oscore omove
										(plan-ahead
											(make-move board (car moves) color)
											(opponent-of color)
											(- 0 β) (- 0 α) (- ply 1)))
									 (score (- 0 oscore)))
									(if (< α score)
										(loop (cdr moves) score (caar moves))
										(loop (cdr moves) α best))))
							(else
								; cutoff <3
								(values α best))))))))

	
	(define (fixed-ply-player ply)
		(λ (board in x y color)
			(lets ((score pos (plan-ahead board color lose-score win-score ply)))
				(if pos
					(values True in (rem pos s) (div pos s))
					(values False in x y)))))

	;((fixed-ply-player 2) empty-board null 0 0 'black)
)

(import lib-ai-normal)

(define ai-easy (fixed-ply-player 2))
(define ai-normal (fixed-ply-player 4))
(define ai-hard (fixed-ply-player 6))

(define players
	(list->ff
		(list 
			(cons imbecile "imbecile") 
			(cons stupid "stupid") 
			(cons human-player "human")
			(cons ai-easy "easy") 
			(cons ai-normal "normal")
			(cons ai-hard "hard")
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
		  ;(tournament  "-t" "--tournament" comment "alternate players and count wins") ; now default
		  (black "-b" "--black" cook ,choose-player default "human" comment "choose black player")
		  (white "-w" "--white" cook ,choose-player default "easy" comment "choose white player")
		  ;(first "-s" "--start" cook ,choose-side default "black" comment "which player starts")
		  ;(board "-b" "--board" cook ,string->board default ,default-board comment "board layout")
		  )))


(define (get2 board x y def) (get board (xy->pos x y) def))

; -> black | white | draw | quit
(define (match board in x y next player opponent skipped?)
	(print-board board x y)
	(if (board-full? board)
		(pick-winner board)
		(lets ((move? in x y (player board in x y next)))
			(cond
				(move?
					(let ((flips (flips-of board x y next)))
						(cond
							((null? flips)
								(show "player made an illegal move: " next)
								(opponent-of next))
							((get2 board x y False)
								(show "player cannot move there: " next)
								(opponent-of next))
							(else
								(match
									(fold
										(λ (board pos) 
											(put board pos next))
										board
										(cons (xy->pos x y) flips))
									in x y (opponent-of next) opponent player False)))))
				(skipped? 
					; both player skipped or were forced to skip -> game over
					(pick-winner board))
				((eq? x 'quit)
					'quit)
				(else
					(match board in x y (opponent-of next) opponent player True))))))

(define (name-of player)
	(if (eq? player 'draw)
		"draw"
		(get players player "mysterious")))

(define (start-match black-player white-player)
	(let loop ((status False) (bp black-player) (wp white-player))
		(lets
			((res 
				(match empty-board (vt-events 0) 3 3 black bp wp False))
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
					;(interact 0 'input)
					(sleep 500)
					(loop status wp bp))))))

(define (play-reversi args)
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

(define (reversi args)
	(or 
		(process-arguments (cdr args) command-line-rules usage-text
			(λ (dict others)
				(cond
					((get dict 'about False) 
						(print about-reversi))
					((get dict 'help False) 
						(print usage-text)
						(print-rules command-line-rules))
					(else
						(play-reversi dict)))))
		1))


; (reversi '("reversi" "-w" "normal" "-b" "human"))

(dump reversi "reversi.c")

