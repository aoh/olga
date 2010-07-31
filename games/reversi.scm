;;;
;;; reversi - it's like othello
;;;

;; todo: center and box game end message
;; todo: count pieces for game end message 
;; todo: change game end wording based on difference in pieces
;; todo: set fgcolor and bold fgcolor in style (use for showing player names, the menu button and other buttons in the future, if necessary)
;; todo: implement tournaments to test AIs 
;; todo: figure out how and where to handle cell hightlighting so that it would work for all games
;; todo: lots of opportunities for constant optimizations and precomputation to be made to improve speed later

(define-module olgame-reversi

	(import lib-grale)
	(import lib-ai)
	(import lib-menu show-menu)
	(import lib-match)

	(export reversi-node)

	;;;
	;;; Drawing-related things
	;;;

	(define box-outline
		(build-sprite
			'(25
				o x x x x x x x x x x x x x x x x x x x x x x x x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x - - - - - - - - - - - - - - - - - - - - - - - x 
				x x x x x x x x x x x x x x x x x x x x x x x x x 
				)))

	(define box-cross
		(build-sprite
			'(25
				- - - - - - - - - - - - - - - - - - - - - - - - - 
				- - - - - - - - - - - - x - - - - - - - - - - - -  
				- - - - - - - - - - - - x - - - - - - - - - - - -  
				- - - - - - - - - - - - x - - - - - - - - - - - - 
				- - - - - - - - - - - - - - - - - - - - - - - - -  
				- - - - - - - - - - - - - - - - - - - - - - - - -  
				- - - - - - - - - - - - x - - - - - - - - - - - - 
				- - - - - - - - - - - - x - - - - - - - - - - - -  
				- - - - - - - - - - - - x - - - - - - - - - - - -  
				- - - - - - - - - - - - - - - - - - - - - - - - - 
				- - - - - - - - - - - - - - - - - - - - - - - - -  
				- - - - - - - - - - - - x - - - - - - - - - - - -  
				- x x x - - x x x - - x o x - x x x - - x x x - - 
				- - - - - - - - - - - - x - - - - - - - - - - - -  
				- - - - - - - - - - - - - - - - - - - - - - - - -  
				- - - - - - - - - - - - - - - - - - - - - - - - - 
				- - - - - - - - - - - - x - - - - - - - - - - - -  
				- - - - - - - - - - - - x - - - - - - - - - - - -  
				- - - - - - - - - - - - x - - - - - - - - - - - - 
				- - - - - - - - - - - - - - - - - - - - - - - - -  
				- - - - - - - - - - - - - - - - - - - - - - - - -  
				- - - - - - - - - - - - x - - - - - - - - - - - - 
				- - - - - - - - - - - - x - - - - - - - - - - - -  
				- - - - - - - - - - - - x - - - - - - - - - - - - 
				- - - - - - - - - - - - - - - - - - - - - - - - - 
				)))

	(define game-piece
		(build-sprite 
			'(21 
			- - - - - - - - - - - - - - - - - - - - -
			- - - - - - - - - - - - - - - - - - - - -
			- - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - x x x x x - - - - - - - -
			- - - - - - x x x x x x x x x - - - - - -
			- - - - - x x x x x x x x x x x - - - - -
			- - - - x x x x x x x x x x x x x - - - -
			- - - - x x x x x x x x x x x x x - - - -
			- - - x x x x x x x x x x x x x x x - - -
			- - - x x x x x x x x x x x x x x x - - -
			- - - x x x x x x x o x x x x x x x - - -
			- - - x x x x x x x x x x x x x x x - - -
			- - - x x x x x x x x x x x x x x x - - -
			- - - - x x x x x x x x x x x x x - - - -
			- - - - x x x x x x x x x x x x x - - - -
			- - - - - x x x x x x x x x x x - - - - -
			- - - - - - x x x x x x x x x - - - - - -
			- - - - - - - - x x x x x - - - - - - - -
			- - - - - - - - - - - - - - - - - - - - -
			- - - - - - - - - - - - - - - - - - - - -
			- - - - - - - - - - - - - - - - - - - - -
			)))

	(define game-piece-border
		(build-sprite 
			'(21 
			- - - - - - - - - - - - - - - - - - - - -
			- - - - - - - - - - - - - - - - - - - - -
			- - - - - - - - - - - - - - - - - - - - -
			- - - - - - - - x x x x x - - - - - - - -
			- - - - - - x x - - - - - x x - - - - - -
			- - - - - x - - - - - - - - - x - - - - -
			- - - - x - - - - - - - - - - - x - - - -
			- - - - x - - - - - - - - - - - x - - - -
			- - - x - - - - - - - - - - - - - x - - -
			- - - x - - - - - - - - - - - - - x - - -
			- - - x - - - - - - + - - - - - - x - - -
			- - - x - - - - - - - - - - - - - x - - -
			- - - x - - - - - - - - - - - - - x - - -
			- - - - x - - - - - - - - - - - x - - - -
			- - - - x - - - - - - - - - - - x - - - -
			- - - - - x - - - - - - - - - x - - - - -
			- - - - - - x x - - - - - x x - - - - - -
			- - - - - - - - x x x x x - - - - - - - -
			- - - - - - - - - - - - - - - - - - - - -
			- - - - - - - - - - - - - - - - - - - - -
			- - - - - - - - - - - - - - - - - - - - -
			)))

	(define game-piece-x
		(build-sprite 
			'(13
			x - - - - - - - - - - - x 
			- x - - - - - - - - - x - 
			- - x - - - - - - - x - - 
			- - - x - - - - - x - - - 
			- - - - x - - - x - - - - 
			- - - - - x - x - - - - - 
			- - - - - - o - - - - - - 
			- - - - - x - x - - - - - 
			- - - - x - - - x - - - - 
			- - - x - - - - - x - - - 
			- - x - - - - - - - x - -
			- x - - - - - - - - - x -
			x - - - - - - - - - - - x
			)))

	(define free-piece
		(build-sprite 
			'(21
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - o - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
			)))

	(define highlight-piece
		(build-sprite 
			'(21
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - x x x - - - - - - - - -
	      - - - - - - - - x x x x x - - - - - - - -
	      - - - - - - - x x x x x x x - - - - - - -
	      - - - - - - - x x x o x x x - - - - - - -
	      - - - - - - - x x x x x x x - - - - - - -
	      - - - - - - - - x x x x x - - - - - - - -
	      - - - - - - - - - x x x - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
	      - - - - - - - - - - - - - - - - - - - - -
			)))

	(if (not (all (λ (x) x) (list game-piece-border game-piece highlight-piece box-cross)))
		(error "some game sprites failed" "lol"))

	(define black 'black)
	(define white 'white)

	(define s 8) ; n rows 
	(define (pos->xy pos) (values (rem pos s) (div pos s)))
	(define (xy->pos x y) (+ (* y s) x))

	; will be static later 
	(define w 320)
	(define h 200)

	; size of cells to draw
	(define cell 25)

	(define cell-mid (>> cell 1))

	(define cells (let ((max (* s s))) (iota 0 1 max)))

	;;; different style of cel highlight

	(define (update-cell x y val)
		(if (and (< x s) (< y s))
			(lets
				((xp (+ (* x cell) cell-mid))
				 (yp (+ (* y cell) cell-mid)))
				(cond
					((eq? val black) 
					 	(grale-puts xp yp #b00000010 game-piece)
					 	(grale-puts xp yp #b00000001 game-piece-border))
					((eq? val white)
					 	(grale-puts xp yp #b11011011 game-piece)
					 	(grale-puts xp yp #b01001001 game-piece-border))
					((eq? val 'highlight)
						(grale-puts xp yp #b00011100 highlight-piece))
					(else
						(grale-fill-rect (- xp cell-mid) (- yp cell-mid) cell cell 0) ;; fixme, style-dependent
					 	(grale-puts xp yp #b00000001 free-piece))))))

	(define (xo-green-update-cell x y val)
		(if (and (< x s) (< y s))
			(lets
				((x (* x cell)) (xm (+ x cell-mid))
				 (y (* y cell)) (ym (+ y cell-mid)))
				(cond
					((eq? val black) 
						(grale-puts x y #b00000100 box-outline)
					 	(grale-puts xm ym #b00011100 game-piece-border))
					((eq? val white)
						(grale-puts x y #b00000100 box-outline)
					 	(grale-puts xm ym #b00011100 game-piece-x))
					((eq? val 'highlight)
						(grale-puts xm ym #b00011100 highlight-piece))
					(else
						(grale-fill-rect x y cell cell 0)
						(grale-puts x y #b00000100 box-outline))))))

	(define (board-update-cell x y val)
		(if (and (< x s) (< y s))
			(lets
				((xp (+ (* x cell) cell-mid))
				 (yp (+ (* y cell) cell-mid)))
				(cond
					((eq? val black) 
					 	(grale-puts xp yp #x88 box-cross)
					 	(grale-puts xp yp #x00 game-piece)
					 	;(grale-puts xp yp #x44 game-piece-border)
					 	(grale-puts xp yp #x68 game-piece-border)
						)
					((eq? val white)
					 	(grale-puts xp yp #x88 box-cross)
					 	(grale-puts xp yp #b11111111 game-piece)
					 	(grale-puts xp yp #b11011010 game-piece-border))
					((eq? val 'highlight)
						(grale-puts xp yp #b00011100 highlight-piece))
					(else
						(grale-fill-rect (- xp cell-mid) (- yp cell-mid) cell cell #b10010001)
					 	;(grale-puts xp yp #b01101100 box-cross)
					 	(grale-puts xp yp #x88 box-cross)
						)))))
		
	(define (block-update-cell x y val)
		(if (and (< x s) (< y s))
			(lets
				((xp (* x cell))
				 (yp (* y cell)))
				(cond
					((eq? val black) 
						(grale-fill-rect xp yp cell cell 0))
					((eq? val white)
						(grale-fill-rect xp yp cell cell 255))
					((eq? val 'highlight)
						(grale-puts (+ xp cell-mid) (+ yp cell-mid) #b00011100 highlight-piece))
					(else
						(grale-fill-rect xp yp cell cell 1))))))

	(define (highlight-cell x y col opts)
		(if (and (< x s) (< y s))
			(lets
				((xp (* x cell))
				 (yp (* y cell)))
				;(grale-fill-rect (+ xp 1) (+ yp 1) (- cell 1) (- cell 1) #b10011011)
				(grale-puts (+ xp cell-mid) (+ yp cell-mid) #b00011000 highlight-piece)
				)))

	(define style-white-blue
		(list->ff
			(list
				(cons 'color #b11111111)
				(cons 'color-light #b10010010)
				(cons 'bgcolor 0)
				(cons 'update-cell update-cell))))

	(define style-xo-green
		(list->ff
			(list
				(cons 'color #b00011100)
				(cons 'color-light #b00001100)
				(cons 'bgcolor 0)
				(cons 'update-cell xo-green-update-cell))))

	(define style-blocks
		(list->ff
			(list
				(cons 'color #b11111111)
				(cons 'color-light #b10010010)
				(cons 'bgcolor 0)
				(cons 'update-cell block-update-cell))))

	(define style-board
		(list->ff
			(list
				(cons 'color #x44)
				(cons 'color-light #x88)
				(cons 'bgcolor #b10010001)
				(cons 'update-cell board-update-cell))))

	(define (print-board-default board x y opts)
		(lets
			((style (get opts 'style False))
			 (update (get style 'update-cell update-cell)))
			(for 42 (iota 0 1 s)
				(λ (_ y)
					(for 42 (iota 0 1 s)
						(λ (_ x)
							(update x y (get board (+ x (* y s)) 'blank))))))
			(grale-update 0 0 w h)))

	(define (print-board board last-move opts color)
		(print-board-default board 1 1 opts))


	;;;
	;;; Game-related code
	;;;

	(define (move->xy maybe-move)
		(if maybe-move (pos->xy (car maybe-move)) (values 1 1)))

	; (dx . dy)
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
					cells)
				True)))

	(define (pick-winner board game-over?)
		(if (or game-over? (board-full? board))
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
			(λ (ok pos)
				(if (blank? board pos)
					(let ((flips (flips-of board (rem pos s) (div pos s) color)))
						(if (null? flips)
							ok
							(cons (cons pos flips) ok)))
					ok))
			null cells))

	(define (valid-move? board color move)
		(mem equal? (valid-moves board color) move))

	(define empty-board 
		(list->ff
			(list
				(cons (+ (* 3 s) 3) white)
				(cons (+ (* 3 s) 4) black)
				(cons (+ (* 4 s) 3) black)
				(cons (+ (* 4 s) 4) white))))

	(define (styled-update opts x y val)
		((get (get opts 'style False) 'update-cell update-cell) x y val))

	;;; artificial stupidity begins

	(define co 50) ; corner score
	(define scores
		(list->tuple
			(list     -9  4  2  2  4  -9 co
					-9  -9 -2 -1 -1  2  -9 -9
					 4  -2  4  2  2  4  -2  4
					 2  -1  2  3  3  2  -1  2
					 2  -1  2  3  3  2  -1  2
					 4  -2  4  2  2  4  -2  4
					-9  -9 -2 -1 -1 -2  -9 -9
					co  -9  4  2  2  4  -9 co)))

	(define (eval-board board color)
		(ff-fold
			(λ (score pos val)
				(if (eq? val color)
					(if (eq? pos 0)
						(+ score co)
						(+ score (ref scores pos)))
					(if (eq? pos 0)
						(- score co)
						(- score (ref scores pos)))))
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
		(let ((winner (pick-winner board True)))
			(cond
				((eq? winner me) win-score)
				((eq? winner 'draw) 0)
				(else lose-score))))

	;; own name useful for adding to menu defaults
	(define ai-normal
		(make-alphabeta-player 2 valid-moves do-move eval-board eval-final True))

	(define player-options
		(list
			(tuple 'option "human" "" 'human) ; note, a label, not the player code. see add-selected-players.
			(tuple 'option "ai imbecile" "" (make-random-player valid-moves))
			(tuple 'option "ai stupid"   "" (make-simple-player valid-moves do-move eval-board 2))
			(tuple 'option "ai easy"     "" (make-simple-player valid-moves do-move eval-board-with-mobility 3))
			;; fixme: default AI should be nondeterministic
			(tuple 'option "ai normal"   "" ai-normal) ; default ;; fixme: easy < normal < medium?
			(tuple 'option "ai medium"   ""
				(make-alphabeta-player 3 valid-moves do-move eval-board eval-final True))
			(tuple 'option "ai hard"     "" 
				(make-time-bound-player 3000 valid-moves do-move eval-board eval-final True))))

	(define reversi-menu
		(tuple 'menu "trolololo" "reversi menu"
			(list
				(tuple 'choose "black player" "choose black player" 'black-player player-options)
				(tuple 'choose "white player" "choose white player" 'white-player player-options)
				(tuple 'choose "board style" "choose board style" 'style
					(list
						(tuple 'option "white and blue" "" style-white-blue)
						(tuple 'option "xo green" "" style-xo-green)
						(tuple 'option "board" "" style-board)
						(tuple 'option "blocks" "" style-blocks)))
				(tuple 'choose "highlight" "what to highlight when moving mouse" 'show-moves
					(list
						(tuple 'option "all changes" "" 'flips)
						(tuple 'option "just move" "" 'move)
						(tuple 'option "nothing" "" 'nothing)))
				(tuple 'back "play")
				(tuple 'spacer)
				(tuple 'quit "exit reversi")
				)))

	(define default-options
		(list->ff
			(list
				(cons 'print-board print-board) ; always passen in opts to players
				(cons 'black-player 'human)
				(cons 'white-player ai-normal)
				(cons 'style style-xo-green)
				(cons 'show-moves 'move))))


	(define (find-move moves pos)
		(for False moves
			(λ (found this)
				(or found 
					(if (eq? (car this) pos) this False)))))

	(define human-state False) ; False | pos

	;; restore graphics to cell
	(define (remove-effect opts board pos color)
		(if pos
			;(lets 
			;	((x y (pos->xy pos))
			;	 (update (get (get opts 'style False) 'update-cell False)))
			;	(update x y (get board pos 'blank))
			;	(grale-update (* x cell) (* y cell) cell cell))
			(begin
				((get opts 'print-board 'bug-no-printer) board pos opts color)
				(paint-screen))))


	;; highlight the cell if requested and movable
	(define (add-effect opts board pos moves)
		(cond
			((find-move moves pos) =>
				(λ (move)
					(case (get opts 'show-moves False)
						((flips) ; highlight all cells about to change
							(for-each
								(λ (pos)
									(lets 
										((x y (pos->xy pos))
										 (xp (* x cell))
										 (yp (* y cell)))
										((get (get opts 'style False) 'update-cell False) x y 'highlight)
										(grale-update xp yp cell cell)))
								move)
							pos)
						((move) ; highlight just the target cell
							(lets 
								((x y (pos->xy pos))
								 (xp (* x cell))
								 (yp (* y cell)))
								((get (get opts 'style False) 'update-cell False) x y 'highlight)
								(grale-update xp yp cell cell))
							pos)
						(else False)))) ; highlight nothing
			(else False)))

	(define (coords->pos x y)
		(lets
			((x (div x cell))
			 (y (div y cell)))
			(if (and (< x s) (< y s))
				(xy->pos x y)
				False)))

	; (grale-update 0 0 w h)

	(define (act-human board opts state x y moves color btn)
		(cond
			(btn ; mouse cliced -> try to move
				(lets
					((pos (xy->pos (div x cell) (div y cell))))
					(cond
						((find-move moves pos) =>
							(λ (move) 
								(values opts state move)))
						(else 
							(values opts state False)))))
			((coords->pos x y) =>
				(λ (pos)
					(if (equal? pos state)
						(values opts state False) ; hover on the same cell as on last time
						(begin
							(remove-effect opts board state color)
							(values opts (add-effect opts board pos moves) False)))))
			(else
				; mouse wandering elsewhere
				(values opts state False))))

			 
	(define reversi
		(make-board-game
			default-options
			empty-board
			reversi-menu
			black
			pick-winner
			valid-moves
			do-move
			player-options
			act-human
			human-state
			print-board))

	(define reversi-node
		(tuple 'proc False "reversi" reversi))

)

;; add reversi to olgame indx
(import olgame-reversi)
(define olgame-games (cons reversi-node olgame-games))

