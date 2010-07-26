;;;
;;; reversi - it's like othello
;;;

,r "ai.scm"
,r "match.scm"
,r "menu.scm"

; switch the input parameter from match to opts (ff of property -> value)
; store game parameters there
;	- initially board printing function
; an exit button and menu button
; use something like olgame's main menu for menus

(define-module olgame-reversi

	(import lib-grale)
	(import lib-ai)
	(import lib-match)
	(import lib-menu show-menu)

	(export reversi-node)

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

	(if (not free-piece) (error "free piece bad " free-piece))
	(if (not game-piece) (error "game piece bad " game-piece))
	(if (not highlight-piece) (error "highlight piece bad " highlight-piece))
	(if (not box-cross) (error "bad box cross " box-cross))

	;(define bgcolor #b10010001)
	(define bgcolor #b00000000)

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
					(else
						(grale-fill-rect (- xp cell-mid) (- yp cell-mid) cell cell bgcolor)
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
					 	(grale-puts xp yp #b01101101 box-cross)
					 	(grale-puts xp yp #b00000000 game-piece)
					 	(grale-puts xp yp #b01001000 game-piece-border))
					((eq? val white)
					 	(grale-puts xp yp #b01101100 box-cross)
					 	(grale-puts xp yp #b11111111 game-piece)
					 	(grale-puts xp yp #b11011010 game-piece-border))
					(else
						(grale-fill-rect (- xp cell-mid) (- yp cell-mid) cell cell #b10010001)
					 	(grale-puts xp yp #b01101100 box-cross))))))
		
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
				(cons 'bgcolor 0)
				(cons 'update-cell update-cell))))

	(define style-xo-green
		(list->ff
			(list
				(cons 'bgcolor 0)
				(cons 'update-cell xo-green-update-cell))))

	(define style-blocks
		(list->ff
			(list
				(cons 'bgcolor 0)
				(cons 'update-cell block-update-cell))))

	(define style-board
		(list->ff
			(list
				(cons 'bgcolor #b10010001)
				(cons 'update-cell board-update-cell))))

	(define (print-board-default board x y opts)
		(lets
			((style (get opts 'style False))
			 (update (get style 'update-cell update-cell)))
			(grale-fill-rect 0 0 w h 
				(get style 'bgcolor bgcolor))
			(for 42 (iota 0 1 s)
				(λ (_ y)
					(for 42 (iota 0 1 s)
						(λ (_ x)
							(update x y (get board (+ x (* y s)) 'blank))))))
			(grale-update 0 0 w h)))

	(define (move->xy move)
		(if move (pos->xy (car move)) (values 1 1)))

	(define (print-board board move opts)
		(lets ((x y (move->xy move)))
			(print-board-default board x y opts)))

	;(define (print-moves moves color)
	;	(let ((marker (if (eq? color 'black) "•" "◦")))
	;		(for-each
	;			(λ (pos)
	;				(set-cursor (+ xo (* (rem pos s) 2)) (+ yo (div pos s)))
	;				(display marker))
	;			(map car moves))
	;		(flush-port 1)))
		
	(define (xy->pos x y) (+ (* y s) x))

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
			(lambda (ok pos)
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

	;;  - 

	(define (styled-update opts x y val)
		((get (get opts 'style False) 'update-cell update-cell)
			x y val))



	;;; artificial stupidity begins

	(define scores
		(list->ff
			(zip cons cells
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
		(let ((winner (pick-winner board True)))
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

	(define ai-exp-1 (make-time-bound-player 100 valid-moves do-move eval-board eval-final True))
	(define ai-exp-2 (make-time-bound-player 200 valid-moves do-move eval-board eval-final True))
	(define ai-exp-3 (make-time-bound-player 400 valid-moves do-move eval-board eval-final True))
	(define ai-exp-4 (make-time-bound-player 800 valid-moves do-move eval-board eval-final True))

	;; menu will have links to the players, including human. sigh. must add that separately or 
	;; use AI names in place of the actual AI functions, which would have been more convenient..

	(define player-options
		(list
			(tuple 'option "human" "" 'human) ; note, not the player code
			(tuple 'option "easy ai" "" ai-normal)))

	(define default-options
		(list->ff
			(list
				(cons black 'human)
				(cons white ai-normal)
				(cons 'style style-board))))

	(define reversi-menu
		(tuple 'menu
			"trolololo"
			"reversi preferences"
			(list
				(tuple 'choose "black player" "choose black player" black player-options)
				(tuple 'choose "white player" "choose white player" white player-options)
				(tuple 'choose "board style" "choose board style" 'style
					(list
						(tuple 'option "white and blue" "" style-white-blue)
						(tuple 'option "xo green" "" style-xo-green)
						(tuple 'option "board" "" style-board)
						(tuple 'option "blocks" "" style-blocks)))
				(tuple 'choose "show moves" "show available moves" 'show-moves
					(list
						(tuple 'option "no" "" False)
						(tuple 'option "later yes" "" True)))
				(tuple 'back)
				(tuple 'spacer)
				(tuple 'quit)
				)))

	(define (human-player board opts last-move color)
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
			(if (null? moves)
				(values False opts)
				(let loop ((x x) (y y) (opts opts))
					(tuple-case (grale-wait-event)
						((click btn xp yp)
							(lets
								((x (div xp cell))  ;; fixme: round to nearest instead
								 (y (div yp cell))
								 (pos (xy->pos x y))
								 (flips (get ff pos False)))
								(cond
									((>= x s)
										(tuple-case (show-menu reversi-menu opts)
											((save opts)
												(print-board board last-move opts)
												(loop x y opts))
											((quit)
												(values 'quit False))
											(else is bad
												(show "Bad menu output: " bad)
												(values 'quit False))))
									(flips
										(values (cons pos flips) opts))
									(else
										(loop x y opts)))))
						((mouse-move xp yp)
							(lets
								((nx (div xp cell))
								 (ny (div yp cell))
								 (pos (xy->pos nx ny)))
								(cond
									((and (eq? nx x) (eq? ny y))
										; hovering on same cell
										(loop x y opts))
									((get ff pos False) 
										(styled-update opts x y (get board (xy->pos x y) 'blank))
										(highlight-cell nx ny color opts)
										(grale-update 0 0 w h)
										(loop nx ny opts))
									(else
										(styled-update opts x y (get board (xy->pos x y) 'blank))
										(grale-update 0 0 w h)
										(loop nx ny opts)))))
						(else is ev
							(loop x y opts)))))))

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
				(cons ai-exp-1 "100ms")
				(cons ai-exp-2 "200ms")
				(cons ai-exp-3 "400ms")
				(cons ai-exp-4 "800ms")
				)))

	(define (get2 board x y def) (get board (xy->pos x y) def))

	(define (disqualify player reason)
		(print* (list "Player " player " disqualified due to " reason "."))
		(opponent-of player))

	(define (move->xy maybe-move)
		(if maybe-move
			(pos->xy (car maybe-move))
			(values 1 1)))

	; (match empty-board (vt-events 0) start black bp wp printer pick-winner valid-moves do-move)

	(define (show-result text)
		(grale-fill-rect 20 20 100 20 0)
		(grale-put-text font-8px (+ 20 2) (+ 20 14) #b11111111 text)
		(paint-screen)
		(lets ((x y (grale-wait-click))) 42))

	; no menus or settings yet, just play a default game and exit
	(define (reversi)
		(define winner 
			(match empty-board 
				default-options ; opts
				'(0) black human-player ai-normal print-board pick-winner valid-moves do-move))
		(cond
			((eq? winner black)
				(show-result "Black player wins"))
			((eq? winner white)
				(show-result "White player wins"))
			((eq? winner 'draw)
				(show-result "A draw"))
			((eq? winner 'quit)
				42)
			(else
				(show-result "Something completely different"))))


	(define reversi-node
		(tuple 'proc False "reversi" reversi))



)

;; add reversi to olgame indx
(import olgame-reversi)
(define olgame-games (cons reversi-node olgame-games))

