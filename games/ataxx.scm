;;;
;;; ataxx
;;;

;; todo: ok to click neighbour cells without choosing source

,r "match.scm"
,r "ai.scm"

(define-module olgame-ataxx

	(export ataxx-node)

	(import lib-grale)
	(import lib-match)
	(import lib-ai)

	(define black 'black)
	(define white 'white)

	(define s 7) 

	(define cell (div (min w h) s))

	(define cells (let ((max (* s s))) (λ () (iota 0 1 max))))

	(define (xy->pos x y) (+ (* y s) x))
	(define (pos->xy pos) (values (rem pos s) (div pos s)))
	(define (move-target move) (ref move (size move)))
	(define (move->xy move) (if move (pos->xy (move-target move)) (values 1 1)))

	(define (cell-color val)
		(cond
			((eq? val 'black) #b11100000)
			((eq? val 'white) #b00011100)
			(else #b00000001)))

	(define (print-board-xy board x y)
		(grale-fill-rect 10 10 100 100 #b11111100)
		(for 42 (iota 0 1 s)
			(λ (_ y)
				(for 42 (iota 0 1 s)
					(λ (_ x)
						(grale-fill-rect (* x cell) (* y cell) cell cell 
							(cell-color (get board (+ x (* y s)) 'blank)))))))
		(grale-update 0 0 w h)
		)

	(define (print-board board move)
		(lets ((x y (move->xy move)))
			(print-board-xy board x y)))
		
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

	(define (pick-winner board game-over?)
		(define status
			(for (tuple 0 0 0) (cells)
				(λ (status pos)
					(let ((val (get board pos False)))
						(cond
							((eq? val 'white) (set status 1 (+ (ref status 1) 1)))
							((eq? val 'black) (set status 2 (+ (ref status 2) 1)))
							(else             (set status 3 (+ (ref status 3) 1))))))))
		(cond
			((or game-over? (= 0 (ref status 3))) ; 0 free cells
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
		(lets
			((moves (valid-moves board color))
			 (x y (move->xy pos)))
			(if (null? moves)
				(values False in)
				(let loop ((in in) (x x) (y y) (source False))
					(tuple-case (grale-wait-event)
						((click btn xp yp)
							(lets
								((x (div xp cell))
								 (y (div yp cell)))
								(if (and (< x s) (< y s))
									(let ((pos (+ x (* y s))))
										(cond
											((blank? board pos)
												(cond
													((find-move moves source pos) =>
														(λ (move) (values move in)))
													(else
														;; fixme: no visual selection effect
														(loop in x y pos))))
												; select an own piece
												((eq? color (get board pos False))
													(loop in x y pos))
												(else
													(loop in x y False))))
									; click outside of board to quit for now
									(values 'quit in))))
						(else
							(loop in x y source)))))))

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

	(define start-position (tuple 0))

   ; no menus or settings yet, just play a default game and exit
   (define (ataxx)
      (define winner
         (match empty-board
            (put False 'print-board print-board)
            False black human-player ai-normal print-board pick-winner valid-moves do-move))
      (cond
         ((eq? winner black)
            (print "Black player wins"))
         ((eq? winner white)
            (print "White player wins"))
         ((eq? winner 'draw)
            (print "A draw"))
         ((eq? winner 'quit)
            (print "Quitter"))
         (else
            (print "Something completely different"))))

	(define ataxx-node
		(tuple 'proc False "ataxx" ataxx))

)

;; add to olgame indx
(import olgame-ataxx)
(define olgame-games (cons ataxx-node olgame-games))

