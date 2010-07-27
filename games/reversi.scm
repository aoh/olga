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

	(export reversi-node)

	(define menu-button
		(build-sprite
			'(20
				- - - - - - - - - - - - - - - - - - - -
				- + - - - x x x x x x x x x x x x x x -
				- - - - x - - x - - - - - - - - - - - x
				- - - - x - - x - - - - - - - - - - - x
				- - - - x - x x x x x x x x x x x x x -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - - - x - - - - - - - - - - - - x - -
				- - x x x - - - - - - - - - - - - x - -
				- x - - x - - - - - - - - - - - - x - -
				- x - x - - - - - - - - - - - - - x - -
				- - x x x x x x x x x x x x x x x - - -
				- - - - - - - - - - - - - - - - - - - -)))

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
			(for 42 (iota 0 1 s)
				(λ (_ y)
					(for 42 (iota 0 1 s)
						(λ (_ x)
							(update x y (get board (+ x (* y s)) 'blank))))))
			(grale-update 0 0 w h)))

	(define (move->xy maybe-move)
		(if maybe-move (pos->xy (car maybe-move)) (values 1 1)))

	(define (show-players pb pw opts color)
		(lets
			((pb (string-append pb " (black)"))
			 (pw (string-append pw " (white)"))
			 (pb-w (grale-text-width font-8px pb))
			 (pw-w (grale-text-width font-8px pw)))
			(grale-put-text font-8px 
				(- 317 pb-w) 10 
				(if (eq? color black) #b00011100 #b00001100)
				pb)
			(grale-put-text font-8px 
				(- 317 pw-w) 20 
				(if (eq? color white) #b00011100 #b00001100)
				pw)))

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
		(make-fixed-ply-player 2 valid-moves do-move eval-board eval-final True))

	(define player-options
		(list
			(tuple 'option "human" "" 'human) ; note, a label, not the player code. see add-selected-players.
			(tuple 'option "ai imbecile" "" (make-random-player valid-moves))
			(tuple 'option "ai stupid"   "" (make-simple-player valid-moves do-move eval-board 2))
			(tuple 'option "ai easy"     "" (make-simple-player valid-moves do-move eval-board-with-mobility 3))
			;; fixme: default AI should be nondeterministic
			(tuple 'option "ai normal"   "" ai-normal) ; default ;; fixme: easy < normal < medium?
			(tuple 'option "ai medium"   ""
				(make-iterative-ply-player 3 valid-moves do-move eval-board eval-final True))
			(tuple 'option "ai hard"     "" 
				(make-time-bound-player 3000 valid-moves do-move eval-board eval-final True))))

	(define (player-name opts color)
		(lets
			((id (if (eq? color black) 'black-player 'white-player))
			 (selected (get opts id 'bug))
			 (name
				(for False player-options
					(λ (found this)
						(if (eq? (ref this 4) selected) (ref this 2) found)))))
			(if name name "anonimasu")))

	; click close enough to menu? (later have gui and choose clicks based on their position and extent)
	(define (menu-click? x y)
		(and (>= x 297) (>= y 174)))
		
	(define (print-board board last-move opts color)
		(lets
			((p-black (player-name opts black))
			 (p-white (player-name opts white)))
			(grale-fill-rect 0 0 w h 
				(get (get opts 'style False) 'bgcolor 0))
			(grale-puts 298 175 #b00000100 menu-button)
			(show-players p-black p-white opts color)
			(lets ((x y (move->xy last-move)))
				(print-board-default board x y opts))))

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
				;; not especially useful atm
				;(tuple 'choose "show moves" "show available moves" 'show-moves
				;	(list
				;		(tuple 'option "hover" "" 'hover)))
				(tuple 'back "play")
				(tuple 'spacer)
				(tuple 'quit "exit reversi")
				)))

	;; take the selected players from 'black and 'white (selected via the menu) and 
	;; add them to keys black and white, while converting 'human to given human. 
	;; (owl has only trees. self-reference is not prohibited, merely impossible.)

	(define (inhuman value human)
		(if (eq? value 'human) human value))

	(define (add-selected-players opts human)
		(lets
			((opts (put opts white (inhuman (get opts 'white-player 'bug) human)))
			 (opts (put opts black (inhuman (get opts 'black-player 'bug) human))))
			opts))

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
										(if (menu-click? xp yp)
											(tuple-case (show-menu reversi-menu opts)
												((save opts)
													((get opts 'print-board 'bug-no-printer)
														board last-move opts color)
													;; bounce off the trampoline because the player code may have changed
													(values 'reload
														(add-selected-players opts human-player)))
												((quit text)
													(values 'quit False))
												(else is bad
													(show "Bad menu output: " bad)
													(values 'quit False)))
											(loop x y opts)))
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

	(define default-options
		(add-selected-players
			(list->ff
				(list
					(cons 'print-board print-board) ; always passen in opts to players
					(cons 'black-player 'human)
					(cons 'white-player ai-normal)
					(cons 'style style-xo-green)
					(cons 'show-moves 'hover)))
			human-player))

	(define (show-result text)
		(grale-fill-rect 20 20 (+ (grale-text-width font-8px text) 4) 20 0)
		(grale-put-text font-8px (+ 20 2) (+ 20 14) #b11111111 text)
		(paint-screen)
		(lets ((x y (grale-wait-click))) 42))

	(define (show-match-result opts winner)
		(cond
			((eq? winner black)
				(show-result 
					(string-append (player-name opts black)
						" triumphs with black pieces")))
			((eq? winner white)
				(show-result 
					(string-append (player-name opts white)
						" triumphs with white pieces")))
			((eq? winner 'draw)
				(show-result "We will call it a draw"))
			(else
				(show-result "Something completely different"))))

	; -> opts' | quit
	(define (match board opts pos next pick-winner valid-moves do-move)
		(let loop ((board board) (opts opts) (pos pos) (next next) (skipped? False))
			((get opts 'print-board 'bug) board pos opts next)
			(cond
				((pick-winner board False) =>
					(λ (winner) 
						(show-match-result opts winner) 
						opts))
				(else
					(lets ((move opts ((get opts next 'bug-no-player) board opts pos next)))
						(cond
							;; player makes a no-move or cannot move
							((not move)
								(if skipped?
									; neither player can or is willing to move
									(begin
										(show-match-result opts (pick-winner board True))
										opts)
									(loop board opts pos (opponent-of next) True)))
							;; special requests
							((eq? move 'reload) ; try move again (probably human selected new player from menu)
								(loop board opts pos next skipped?))
							((eq? move 'quit)
								'quit)
							;; check if the response is a valid move
							((mem equal? (valid-moves board next) move)
								(loop (do-move board move next)
									opts move (opponent-of next) False))
							(else
								(show-result "Game terminated because of an invalid move")
								opts)))))))

	(define (reversi)
		(let loop ((opts default-options))
			(let ((res (match empty-board opts '(0) black pick-winner valid-moves do-move)))
				(cond
					((eq? res 'quit)
						'quit)
					((or 
						(eq? 'human (get res 'black-player False))
						(eq? 'human (get res 'white-player False)))
						;; continue if a human player is present
						(loop res))
					(else
						;; otherwise show a menu
						(tuple-case (show-menu reversi-menu res)
							((save opts)
								; continue playing
								(loop (add-selected-players opts human-player)))
							(else 'quit)))))))

	(define reversi-node
		(tuple 'proc False "reversi" reversi))

)

;; add reversi to olgame indx
(import olgame-reversi)
(define olgame-games (cons reversi-node olgame-games))

