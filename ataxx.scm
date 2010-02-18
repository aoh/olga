;;;
;;; ataxx
;;;

,r "match.scm"
(import lib-match)

; todo
;	- show selected piece differently in human-player
;	- benchmark how much faster bit boards would be
;	- saner evaluation function
;		+ possible improvements
;			o bonus to surrounded cells
;			o attack coverage? probably less useful than an extra ply..
; 	- alternative starting configurations and board layouts

(define usage-text "Usage: ataxx [args]")

(define about-ataxx
"ataxx - a clone of the game ataxx
This game is from http://code.google.com/p/olgame.

Each piece can either grow a new one to a free neighbouring 8 
cells or jump to a distance of 2, in which case the original 
position becomes free. In both cases all the occupied neghbours 
of the new position, if any, are convered to the players pieces.

Use the arrow keys to move, and space or enter to first pick a 
cell, then select the new position and press again.

Pressing q ends the game.
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

(define (xy->pos x y) (+ (* y s) x))
(define (pos->xy pos) (values (div pos s) (rem pos s)))

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

(define (print-board board move)
	(lets ((x y (pos->xy move)))
		(print-board-xy board x y)))
	
(define (move-focus board x y dir)
	(cond
		((eq? dir 'up) (values x (max 0 (- y 1))))
		((eq? dir 'down) (values x (min (- s 1) (+ y 1))))
		((eq? dir 'left) (values (max 0 (- x 1)) y))
		((eq? dir 'right) (values (min (- s 1) (+ x 1)) y))
		(else (error "now that's a thought! let's all move to " dir))))

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

; a -> b and c -> b result in equal boards (for moves)

(define (remove-duplicates moves)
	(let loop ((moves moves) (targets False) (out null))
		(if (null? moves)
			out
			(let ((target (ref (car moves) 3)))
				(if (get targets target False)
					(loop (cdr moves) targets out)
					(loop (cdr moves) (put targets target True) (cons (car moves) out)))))))

(define (valid-moves board player) ; → (#(jump|clone from to) ...)
	(ff-fold
		(λ (moves pos val)
			(if (eq? val player)
				(lets
					((moves 
						(for moves (blanks-in board (get neighbours pos null) null)
							(λ (tail move) (cons (tuple 'clone pos move) tail))))
					 (moves 
						(for moves (blanks-in board (get jumps pos null) null)
							(λ (tail move) (cons (tuple 'jump pos move) tail)))))
					moves)
				moves))
		null board))

(define (valid-unique-moves board player) ; → (#(jump|clone from to) ...)
	(ff-fold
		(λ (moves pos val)
			(if (eq? val player)
				(lets
					((moves 
						(for moves (blanks-in board (get neighbours pos null) null)
							(λ (tail move) (cons (tuple 'clone pos move) tail))))
					 (moves (remove-duplicates moves))
					 (moves 
						(for moves (blanks-in board (get jumps pos null) null)
							(λ (tail move) (cons (tuple 'jump pos move) tail)))))
					moves)
				moves))
		null board))

(define (opponent-of x) (if (eq? x black) white black))

(define (blank? board pos)
	(eq? False (get board pos False)))

(define (find-move moves source pos)
	(call/cc (λ (ret)
		(for-each	
			(λ (move)
				(lets ((kind from to move))
					(if (and (eq? from source) (eq? to pos))
						(ret move))))
			moves)
		False)))

; board → int (to be used as random seed)
(define (board-seed board)
	(ff-fold
		(λ (seed pos val)
			(if (eq? pos black) (* seed 2) (+ seed 1)))
		(time 1) board))

(define (make-move board pos player)
	(for (put board pos player) (get neighbours pos null)
		(λ (board pos) 
			(if (get board pos False)
				(fupd board pos player)
				board))))

(define (make-jump board from to player)
	(make-move (del board from) to player))

(define (eval-board board color)
	(ff-fold
		(λ (score pos val)
			(if (eq? val color) (+ score 1) (- score 1)))
		0 board))

(define win   65535)
(define lose -65535)

(define (eval-board-final board color)
	(let ((score (eval-board board color)))
		(cond
			((> score 0) win)
			((eq? score 0) 0)
			(else lose))))

(define (do-move board move color)
	(lets ((type from to move))
		(if (eq? type 'jump)
			(make-jump board from to color)
			(make-move board to color))))

;;; Make AI players

,r "ai.scm"

(import lib-ai)

; note, could set allow-skip to false, but then if the loser makes the last valid move, the 
; possibly winning player may end up in a situation where there are no valid moves. the allow-skip 
; should be allowed to be a function which describes the behavior in those cases, namely make 
; any valid move for the opponent while applicable.

(define ai-imbecile (make-random-player valid-unique-moves))
(define ai-easy (make-simple-player valid-unique-moves do-move eval-board 2))
(define ai-normal (make-fixed-ply-player 2 valid-unique-moves do-move eval-board eval-board-final True))
(define ai-hard (make-iterative-ply-player 4 valid-unique-moves do-move eval-board eval-board-final True))
(define ai-experimental
	(make-time-bound-player 1000 valid-unique-moves do-move eval-board eval-board-final True))

;;; Make a human player

(define (human-player board in pos color) ; → move|false|quit target in
	(let ((moves (valid-moves board color)))
		(print-board-xy board 1 1)
		(if (null? moves)
			(values False in)
			(let loop ((in in) (x (rem pos s)) (y (div pos s)) (source False))
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
													(cond
														((find-move moves source pos) =>	
															(λ (move) (values move (cdr in))))
														(else
															(loop (cdr in) x y False))))
												((eq? color (get board pos False))
													(loop (cdr in) x y pos))
												(else
													(loop (cdr in) x y False)))))
									((113) ; [q]uit
										(values 'quit (cdr in)))
									(else
										(loop (cdr in) x y source))))
							(else
								(loop (cdr in) x y source))))
					(else (loop (in) x y source)))))))

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
			(cons ai-easy "easy")
			(cons human-player "human")
			(cons ai-normal "normal")
			(cons ai-hard "hard")
			(cons ai-experimental "experimental")
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
		  (white "-w" "--white" cook ,choose-player default "normal" comment "choose white player")
		  (matches "-n" "--matches" default "1" cook ,string->number check ,(λ x (and (number? x) (> x 0))))
		  )))

(define (board-full? board)
	(call/cc 
		(λ (ret)
			(for-each (λ pos (if (blank? board pos) (ret False))) (cells))
			True)))

(define (do-move board move color)
	(tuple-case move
		((clone pos to) (make-move board to color))
		((jump from to) (make-jump board from to color))
		(else (error "bad move: " move))))

(define (move-target move) (ref move (size move)))

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
						(play-match dict empty-board print-board
							pick-winner valid-moves do-move players move-target
							)))))
		1))

; (ataxx '("ataxx" "-b" "imbecile" "-w" "easy"))
; (ataxx '("ataxx" "-b" "easy" "-w" "easy" "-n" "10"))

(dump ataxx "ataxx.c")

