;;;
;;; ataxx
;;;

;; todo: ok to click neighbour cells without choosing source
;; todo: set flippage in menu (8/4)?

;; todo: when hovering on a movable cell (unique or cloning move) highlight the sources automatically
;; todo: when selecting an own cell, highlight the target cells

(define-module olgame-ataxx

	(export ataxx-node)

	(import lib-grale)
	(import lib-menu)
	(import lib-ai)

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


	(define black 'black)
	(define white 'white)

	(define s 7) ; board size

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


	(define empty-board 
		(list->ff
			(list
				(cons 0 black)
				(cons (- s 1) white)
				(cons (* s (- s 1)) white)
				(cons (- (* s s) 1) black))))

	; click close enough to menu? (later have gui and choose clicks based on their position and extent)
	(define (menu-click? x y)
		(and (>= x 297) (>= y 174)))

	(define player-options
		(list
			(tuple 'option "human" "" 'human) ; special case
			(tuple 'option "ai imbecile" "" ai-imbecile)
			(tuple 'option "ai easy"   "" ai-easy)
			(tuple 'option "ai normal"     "" ai-normal)
			(tuple 'option "ai hard"   "" ai-hard)))

	(define ataxx-menu
		(tuple 'menu "trolololo" "ataxx menu"
			(list
				(tuple 'choose "black player" "choose black player" 'black-player player-options)
				(tuple 'choose "white player" "choose white player" 'white-player player-options)
				;(tuple 'choose "board style" "choose board style" 'style
				;	(list
				;		(tuple 'option "white and blue" "" style-white-blue)
				;		(tuple 'option "xo green" "" style-xo-green)
				;		(tuple 'option "board" "" style-board)
				;		(tuple 'option "blocks" "" style-blocks)))
				(tuple 'back "play")
				(tuple 'spacer)
				(tuple 'quit "exit ataxx")
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

	(define (human-player board opts pos color)
		(lets
			((moves (valid-moves board color))
			 (x y (move->xy pos)))
			(if (null? moves)
				(values False opts)
				(let loop ((opts opts) (x x) (y y) (source False))
					(tuple-case (grale-wait-event)
						((click btn xp yp)
							(lets
								((x (div xp cell))
								 (y (div yp cell)))
								(cond
									((and (< x s) (< y s))
										(let ((pos (+ x (* y s))))
											(cond
												((blank? board pos)
													(cond
														((find-move moves source pos) =>
															(λ (move) (values move opts)))
														(else
															;; fixme: no visual selection effect
															(loop opts x y pos))))
													; select an own piece
													((eq? color (get board pos False))
														(loop opts x y pos))
													(else
														(loop opts x y False)))))
									((menu-click? xp yp)
										(tuple-case (show-menu ataxx-menu opts)
											((save opts)
												((get opts 'print-board 'bug-no-printer)
													board pos opts color)
												;; bounce off the trampoline because the player code may have changed
												(values 'reload
													(add-selected-players opts human-player)))
											((quit text)
												(values 'quit False))
											(else is bad
												(show "Bad menu output: " bad)
												(values 'quit False))))
									(else
										(loop opts x y source)))))
						(else is ev
							(loop opts x y source)))))))

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
	(define (player-name opts color)
		(lets
			((id (if (eq? color black) 'black-player 'white-player))
			 (selected (get opts id 'bug))
			 (name
				(for False player-options
					(λ (found this)
						(if (eq? (ref this 4) selected) (ref this 2) found)))))
			(if name name "anonimasu")))

	
	(define (print-board board move opts color)
		(lets
			((p-black (player-name opts black))
			 (p-white (player-name opts white))
			 (x y (move->xy move)))
			(grale-fill-rect 0 0 w h 
				(get (get opts 'style False) 'bgcolor 0))
			(grale-puts 298 175 #b00000100 menu-button)
			(show-players p-black p-white opts color)
			(print-board-xy board x y)))


	(define default-options
		(add-selected-players
			(list->ff
				(list
					(cons 'print-board print-board) ; always passen in opts to players
					(cons 'black-player 'human)
					(cons 'white-player ai-normal)))
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

   (define (ataxx)
		(let loop ((opts default-options))
			(let ((res (match empty-board opts False black pick-winner valid-moves do-move)))
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
						(tuple-case (show-menu ataxx-menu res)
							((save opts)
								; continue playing
								(loop (add-selected-players opts human-player)))
							(else 'quit)))))))

	(define ataxx-node
		(tuple 'proc False "ataxx" ataxx))

)


;; add to olgame indx
(import olgame-ataxx)
(define olgame-games (cons ataxx-node olgame-games))

; ----------------------------------------------------------------------------

