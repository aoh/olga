;;;
;;; ataxx
;;;

;; todo: allow setting flippage to 4
;; todo: when hovering on a movable cell (unique or cloning move) highlight the sources
;; todo: when selecting on (or overing on) own cell, highlight the target cells
;; todo: when hovering on potential move, could also highlight the cells to be convered, onless it looks like a mess

(define-module olgame-ataxx

	(export ataxx-node)

	(import lib-menu)
	(import lib-ai)
	(import lib-match)


	(define black 'black)
	(define white 'white)

	(define s 7) ; board size

	(define cell (div (min w h) s))

	(define cells (iota 0 1 (expt s 2)))

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
		(for 42 (iota 0 1 s)
			(λ (_ y)
				(for 42 (iota 0 1 s)
					(λ (_ x)
						(grale-fill-rect (+ 1 (* x cell)) (+ 1 (* y cell)) (- cell 1) (- cell 1) 
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
		;(list->ff (map (λ pos (cons pos (grab pos neighbour-offsets))) cells))
		(list->vector (map (λ pos (grab pos neighbour-offsets)) cells))
		)

	(define jumps
		;(list->ff (map (λ pos (cons pos (grab pos jump-offsets))) cells))
		(list->vector (map (λ pos (grab pos jump-offsets)) cells))
		)

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
			(for (tuple 0 0 0) cells
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
							(for moves (blanks-in board (vec-ref neighbours pos) null)
								(λ (tail move) (cons (tuple 'clone pos move) tail))))
						 (moves 
							(for moves (blanks-in board (vec-ref jumps pos) null)
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
							(for moves (blanks-in board (vec-ref neighbours pos) null)
								(λ (tail move) (cons (tuple 'clone pos move) tail))))
						 (moves (remove-duplicates moves))
						 (moves 
							(for moves (blanks-in board (vec-ref jumps pos) null)
								(λ (tail move) (cons (tuple 'jump pos move) tail)))))
						moves)
					moves))
			null board))

	(define (opponent-of x) (if (eq? x black) white black))

	(define (blank? board pos)
		(eq? False (get board pos False)))

	(define (find-cloning-move moves pos)
		(for False moves
			(λ (found this)
				(or found 
					(lets ((kind from to this))
						(if (and (eq? kind 'clone) (eq? pos to)) this False))))))

	(define (find-unique-jump moves pos)
		(call/cc
			(λ (ret)
				(for False moves
					(λ (found this)
						(lets ((kind from to this))
							(if (and (eq? kind 'jump) (eq? pos to))
								(if found
									(ret False) ; found many options
									this)
								found)))))))

	(define (find-move moves source pos)
		(cond
			(source ; source explicitly selected?
				(call/cc (λ (ret)
					(for-each	
						(λ (move)
							(lets ((kind from to move))
								(if (and (eq? from source) (eq? to pos))
									(ret move))))
						moves)
					False)))
			((find-cloning-move moves pos) =>
				(λ (move) move))
			((find-unique-jump moves pos) =>
				(λ (move) move))
			(else False)))

	(define (make-move board pos player)
		(for (put board pos player) (vec-ref neighbours pos)
			(λ (board pos) 
				(if (get board pos False)
					(fupd board pos player)
					board))))

	(define (make-jump board from to player)
		(make-move (del board from) to player))

	;;; just count the pieces
	(define (eval-board-naive board color)
		(ff-fold
			(λ (score pos val)
				(if (eq? val color) 
					(+ score 1)
					(- score 1)))
			0 board))

	; should probably take into account cell-mobility, safety (all neighbours occupied), etc

	(define eval-board 
		eval-board-naive)

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
	(define ai-hard 
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



	(define (board-full? board)
		(call/cc 
			(λ (ret)
				(for-each (λ pos (if (blank? board pos) (ret False))) cells)
				True)))

	(define (do-move board move color)
		(tuple-case move
			((clone pos to) (make-move board to color))
			((jump from to) (make-jump board from to color))
			(else (error "bad move: " move))))

	(define start-position (tuple 0))

	;; fixme, match should handle most of this
	(define (print-board board move opts color)
		(print-board-xy board 1 1))

	(define default-options
		(list->ff
			(list
				(cons 'style
					(list->ff
						(list
							(cons 'color #b11111111)
							(cons 'color-light #b01101101))))
				(cons 'print-board print-board) ; always passen in opts to players
				(cons 'black-player 'human)
				(cons 'white-player ai-normal))))

	(define human-state False) ; selected node

	(define (act-like-human board opts state x y moves color btn)
		(if btn
			(lets
				((x (div x cell))
				 (y (div y cell)))
				(if (and (< x s) (< y s))
					(let ((pos (+ x (* y s))))
						(cond
							((find-move moves state pos) =>
								(λ (move) (values opts False move)))
							((eq? color (get board pos False))
								(values opts pos False))
							(else
								(values opts False False))))
					(values opts False False)))
			(begin
				(show "i see mouse at " (cons x y))
				(values opts state False))))

	(define ataxx
		(make-board-game
			default-options
			empty-board
			ataxx-menu
			black
			pick-winner
			valid-moves
			do-move
			player-options
			act-like-human
			human-state
			print-board
			))

	(define ataxx-node
		(tuple 'proc False "ataxx" ataxx))

;	;; AI unit test
;	(import lib-test)
;	; these should always find moves with equal score 
;	(define alphabeta (make-fixed-ply-player 4 valid-unique-moves do-move eval-board eval-board-final True))
;	(define minimax   (make-minimax-player   4 valid-unique-moves do-move eval-board eval-board-final True))
;	(define (random-ataxx-configuration rst)
;		(lets ((rst n (rand rst 10)))
;			(let loop ((rst rst) (board empty-board) (n n) (player black))
;				(if (= n 0)
;					board
;					(lets
;						((moves (valid-moves board player))
;						 (rst n (rand rst (length moves))))
;						(if (null? moves)
;							(loop rst empty-board 3 black) ; stuck
;							(loop rst (do-move board (lref moves n) player) (- n 1) (opponent-of player))))))))
;	(test
;		(lmap random-ataxx-configuration
;			(liter rand-succ 
;				(lets ((ss ms (clock))) (* ss (expt (+ ms 1) 3)))))
;		(λ (board) (lets ((move opts (alphabeta board False 0 black))) (get opts 'score 'bug)))
;		(λ (board) (lets ((move opts (minimax board False 0 black))) (get opts 'score 'bag))))
	
)

;; add to olgame indx
(import olgame-ataxx)
(define olgame-games (cons ataxx-node olgame-games))



