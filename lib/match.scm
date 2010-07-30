
;; todo: check for menu clicks also when two computer-players are playing (not sure how to do this without adding a message queue yet..)
;; todo: add tournaments to be able to test AI searches and evaluation functions
;;		+ note: the probabilistic simple selector AI can be used directly to test the quality of eval functions \o/
;;	todo: pass also mouse movements to human player so that optional hovering effects can be generated

(define-module lib-match
	
	(export 
		make-board-game		;; 
	)		

	(define black 'black)
	(define white 'white)
	(define (opponent-of x) (if (eq? x black) white black))

	(define win   65535)
	(define lose -65535)

	(import lib-grale)
	(import lib-menu)
	(import lib-ai)

	(define (show-players pb pw opts color)
		(lets
			((pb (string-append pb " (black)"))
			 (pw (string-append pw " (white)"))
			 (pb-w (grale-text-width font-8px pb))
			 (pw-w (grale-text-width font-8px pw))
			 (style (get opts 'style False))
			 (fg (get style 'color 255))
			 (bg (get style 'color-light 255)))
			(grale-put-text font-8px 
				(- 317 pb-w) 10 
				(if (eq? color black) fg bg)
				pb)
			(grale-put-text font-8px 
				(- 317 pw-w) 20 
				(if (eq? color white) fg bg)
				pw)))

	(define (menu-click? x y)
		(and (>= x 297) (>= y 174)))

	(define (inhuman choise human)
		(if (eq? choise 'human) human choise))

	(define (add-selected-players opts human)
		(lets
			((opts (put opts white (inhuman (get opts 'white-player 'bug) human)))
			 (opts (put opts black (inhuman (get opts 'black-player 'bug) human))))
			opts))

	(define (make-human-player menu valid-moves act initial-state)
	
		(define (human-player board opts pos color)
			(let ((moves (valid-moves board color)))
				(if (null? moves) 
					(values False opts) ; no way to move, so do not bother the human by asking
					(let loop ((opts opts) (state initial-state))
						(tuple-case (grale-wait-event)
							((click btn xp yp)
								(if (menu-click? xp yp)
									(tuple-case (show-menu menu opts)
										((save opts)
											((get opts 'print-board 'bug-no-printer)
												board pos opts color)
											;; bounce off the match trampoline because the player code may have changed
											(values 'reload opts))
										((quit text)
											(values 'quit False))
										(else is bad
											(show "Bad menu output: " bad)
											(values 'quit False)))
									(lets ((opts state move (act board opts state xp yp moves color btn)))
										(cond
											((eq? move 'skip)
												(values False opts))
											((not move)
												(loop opts state))
											((mem equal? moves move)
												(values move opts))
											(else
												(show "Invalid move: " move)
												(values False opts))))))
							((mouse-move xp yp)
								(lets ((opts state move (act board opts state xp yp moves color False)))
									(if move
										(print "note: discarding human move request following a mouse move event"))
									(loop opts state)))
							(else 
								(loop opts state)))))))
		human-player)

	(define (player-name opts color player-options)
		(lets
			((id (if (eq? color black) 'black-player 'white-player))
			 (selected (get opts id 'bug))
			 (name
				(for False player-options
					(λ (found this)
						(if (eq? (ref this 4) selected) (ref this 2) found)))))
			(if name name "anonimasu")))

	;; todo: a proper result window
	;; make a centered box to the game-area, which is assumed to be 200x200 (full left side)
	(define (show-result text)
		(lets
			((text-width (grale-text-width font-8px text))
			 (box-width (+ text-width 4))
			 (box-height 20)
			 (corner-x (div (- 200 box-width) 2))
			 (corner-y (div (- 200 box-height) 2)))
			(grale-fill-rect (- corner-x 1) (- corner-y 1) (+ box-width 2) (+ box-height 2) 255)
			(grale-fill-rect corner-x corner-y box-width box-height 0)
			(grale-put-text font-8px (+ corner-x 2) (+ corner-y 14) 255 text)
			(paint-screen)
			(lets ((x y (grale-wait-click))) 42)))

	(define (show-match-result opts winner players)
		(cond
			((eq? winner black)
				(show-result 
					(string-append (player-name opts black players)
						" triumphs with black pieces")))
			((eq? winner white)
				(show-result 
					(string-append (player-name opts white players)
						" triumphs with white pieces")))
			((eq? winner 'draw)
				(show-result "We will call it a draw"))
			(else
				(show-result "Something completely different"))))

	; assume board area is 200x200 on the left (can be changed easily later by adding a (board-click? x y) -> pos|False)

	; -> opts' | quit
	(define (match board opts pos next pick-winner valid-moves do-move human)
		(let loop ((board board) (opts opts) (pos pos) (next next) (skipped? False))
			((get opts 'print-board 'bug) board pos opts next)
			(cond
				((pick-winner board False) =>
					(λ (winner) 
						(values opts winner)))
				(else
					(lets ((move opts ((get opts next 'bug-no-player) board opts pos next)))
						(cond
							;; player makes a no-move or cannot move
							((not move)
								(if skipped?
									; neither player can or is willing to move
									(values opts (pick-winner board True))
									(loop board opts pos (opponent-of next) True)))
							;; special requests
							((eq? move 'reload) ; try move again (probably human selected new player from menu)
								(loop board 
									(add-selected-players opts human)
									pos next skipped?))
							((eq? move 'quit)
								(values opts 'quit))
							;; check if the response is a valid move
							((mem equal? (valid-moves board next) move)
								(loop (do-move board move next)
									opts move (opponent-of next) False))
							(else
								(show " match got move proposal " move)
								(show " valids are " (valid-moves board next))
								(show-result "Game terminated because of an invalid move")
								(values opts 'quit))))))))


	;; a printer which also adds the menu button(s) and player information

	(define (extended-print-board print-board players)
		(λ (board move opts color)
			(lets 
				((p-black (player-name opts black players))
				 (p-white (player-name opts white players)))
				(grale-fill-rect 0 0 w h 
					(get (get opts 'style False) 'bgcolor 0)) 
				(grale-puts 298 175 
					(get (get opts 'style False) 'color-light 255)
					menu-button)
				(show-players p-black p-white opts color)
				(print-board board move opts color))))

	;; note, players is usually already in menu, but added anyway

   (define (make-board-game default-options empty-board menu starter pick-winner valid-moves do-move players 
					act initial-state print-board) 
		(define human
			(make-human-player menu valid-moves act initial-state))

		(λ ()
			(lets
				((print-board (extended-print-board print-board players))
				 (opts (put default-options 'print-board print-board)))
				(let loop ((opts (add-selected-players opts human)))
					(lets ((opts res (match empty-board opts False starter pick-winner valid-moves do-move human)))
						(if (not (eq? res 'quit))
							(show-match-result opts res players))
						(cond
							((eq? res 'quit)
								'quit)
							((or 
								(eq? 'human (get opts 'black-player False))
								(eq? 'human (get opts 'white-player False)))
								;; continue if a human player is present
								(loop opts))
							(else
								;; otherwise show a menu
								(tuple-case (show-menu menu opts)
									((save opts)
										; continue playing
										(loop (add-selected-players opts human)))
									(else 'quit)))))))))

)


