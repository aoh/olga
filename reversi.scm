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

,r "ai.scm"

(import lib-args) ; command line argument handling
(import lib-vt)	  ; basic terminal control
(import lib-ai)	  ; olgame's ai stuff

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
(define (pos->xy pos) (values (rem pos s) (div pos s)))
(define (xy->pos x y) (+ (* y s) x))

(define cells (let ((max (* s s))) (λ () (iota 0 1 max))))

(define (position-cursor x y)
	(set-cursor (+ xo (* x 2)) (+ yo y)))

(define (print-board-xy board x y)
	(clear-screen)
	(for 42 (iota 0 1 s)
		(λ (_ y)
			(set-cursor xo (+ y yo))
			(for 42 (iota 0 1 s)
				(λ (_ x)
					(display (render-cell (get board (+ x (* y s)) 'blank)))))))
	(position-cursor x y)
	(flush-port 1))

(define (move->xy move)
	(if move (pos->xy (car move)) (values 1 1)))

(define (print-board board move)
	(lets ((x y (move->xy move)))
		(print-board-xy board x y)))

(define (print-moves moves color)
	(let ((marker (if (eq? color 'black) "•" "◦")))
		(for-each
			(λ (pos)
				(set-cursor (+ xo (* (rem pos s) 2)) (+ yo (div pos s)))
				(display marker))
			(map car moves))
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
	(if (board-full? board)
		(let ((score
			(ff-fold (λ (n pos val) (+ n (if (eq? val black) 1 -1))) 0 board)))
			(cond
				((< score 0) white)
				((= score 0) 'draw)
				(else black)))
		False))

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

(define (valid-move? board color move)
	(mem equal? (valid-moves board color) move))

(define empty-board 
	(list->ff
		(list
			(cons (+ (* 3 s) 3) white)
			(cons (+ (* 3 s) 4) black)
			(cons (+ (* 4 s) 3) black)
			(cons (+ (* 4 s) 4) white))))

(define (human-player board in last-move color)
	(lets 
		((moves (valid-moves board color))
		 (pos (if last-move (car last-move) (xy->pos 1 1)))
		 (ff (list->ff moves)) ; ff of pos → flips
		 (x y (pos->xy pos))
		 (last 
			(cond
				((eq? board empty-board)
					(list "Open the game."))
				(last-move
					(list (opponent-of color) " moved to (" x "," y ")."))
				(else (list (opponent-of color) " skipped the last move.")))))
		(print-board-xy board x y)
		(print-moves moves color)
		(set-cursor 1 12)
		(print* last)
		(if (null? moves)
			(values False in)
			(let loop ((in in) (x x) (y y))
				(position-cursor x y)
				(flush-port 1)
				(cond
					((null? in)
						(values 'quit in))
					((pair? in)
						(tuple-case (car in)
							((arrow dir)
								(lets ((x y (move-focus board x y dir)))
									(loop (cdr in) x y)))
							((key k)
								(case k
									((13 32) ; [enter space] move here (if applicable)
										(lets ((pos (xy->pos x y)) (flips (get ff pos False)))
											(if flips
												(values (cons pos flips) (cdr in))
												(loop (cdr in) x y))))
									((113) ; [q]uit
										(values 'quit (cdr in)))
									((115) ; [s]kip
										(values False (cdr in)))
									(else
										(loop (cdr in) x y))))
							(else
								(loop (cdr in) x y))))
					(else (loop (in) x y)))))))


;;; artificial stupidity begins

(define scores
	(list->ff
		(zip cons (cells)
			(list 50  -9  4  2  2  4  -9 50
					-9  -9  2  1  1  2  -9 -9
					 4   2  4  2  2  4   2  4
					 2   1  2  1  1  2   1  2
					 2   1  2  1  1  2   1  2
					 4   2  4  2  2  4   2  4
					-9  -9  2  1  1  2  -9 -9
					50  -9  4  2  2  4  -9 50))))

(define (eval-board board color)
	(ff-fold
		(λ (score pos val)
			(if (eq? val color)
				(+ score (get scores pos 0))
				(- score (get scores pos 0))))
		0 board))

(define (eval-board-with-mobility board color)
	(+ (eval-board board color)
		(* 4
			(- (length (valid-moves board color))
				(length (valid-moves board (opponent-of color)))))))

(define (do-move board cells color)
	(for board cells (λ (board pos) (put board pos color))))

(define win-score 65535)
(define lose-score -65535)

(define (eval-final board me)
	(let ((winner (pick-winner board)))
		(cond
			((eq? winner me) win-score)
			((eq? winner 'draw) 0)
			(else lose-score))))


(define ai-imbecile (make-random-player valid-moves))
(define ai-stupid   (make-simple-player valid-moves do-move eval-board 2))	
(define ai-easy     (make-simple-player valid-moves do-move eval-board-with-mobility 3))	; marginally less stupid
; demote these to easy and normal later
(define ai-normal (make-fixed-ply-player 2 valid-moves do-move eval-board eval-final True))
(define ai-hard (make-iterative-ply-player 5 valid-moves do-move eval-board eval-final True))

(define players
	(list->ff
		(list 
			; roughly in order 
			(cons ai-imbecile "imbecile") 
			(cons ai-stupid "stupid") 
			(cons ai-easy "easy") 
			(cons human-player "human")
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
		  (white "-w" "--white" cook ,choose-player default "normal" comment "choose white player")
		  ;(first "-s" "--start" cook ,choose-side default "black" comment "which player starts")
		  ;(board "-b" "--board" cook ,string->board default ,default-board comment "board layout")
		  (matches "-n" "--matches" default "1" cook ,string->number check ,(λ x (and (number? x) (> x 0))))
		  )))

(define (get2 board x y def) (get board (xy->pos x y) def))

(define (disqualify player reason)
	(print* (list "Player " player " disqualified due to " reason "."))
	(opponent-of player))

(define (move->xy maybe-move)
	(if maybe-move
		(pos->xy (car maybe-move))
		(values 1 1)))

,r "match.scm"
(import lib-match)

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
						(play-match dict empty-board print-board
							pick-winner valid-moves do-move players '(0))))))
		1))


; (reversi '("reversi" "-w" "human" "-b" "stupid"))

(dump reversi "reversi.c")
