
;; fixme: move player printing and button generation here and wrap them around each game-specific print-board

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
			 (pw-w (grale-text-width font-8px pw)))
			(grale-put-text font-8px 
				(- 317 pb-w) 10 
				(if (eq? color black) #b00011100 #b00001100)
				pb)
			(grale-put-text font-8px 
				(- 317 pw-w) 20 
				(if (eq? color white) #b00011100 #b00001100)
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
									(lets ((opts state move (act board opts state xp yp moves color)))
										(cond
											((eq? move 'skip)
												(values opts False))
											((not move)
												(loop opts state))
											((mem equal? moves move)
												(values move opts))
											(else
												(print*
													(list 
														"human-player: requested to make move " 
														move 
														", but the valid moves are " 
														moves 
														", so dropping request and asking againg."))
												(loop opts state))))))
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
	
	(define (show-result text)
		(grale-fill-rect 20 20 (+ (grale-text-width font-8px text) 4) 20 0)
		(grale-put-text font-8px (+ 20 2) (+ 20 14) #b11111111 text)
		(paint-screen)
		(lets ((x y (grale-wait-click))) 42))

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
					(λ (winner) (values winner opts)))
				(else
					(lets ((move opts ((get opts next 'bug-no-player) board opts pos next)))
						(cond
							;; player makes a no-move or cannot move
							((not move)
								(if skipped?
									; neither player can or is willing to move
									(values (pick-winner board True) opts)
									(loop board opts pos (opponent-of next) True)))
							;; special requests
							((eq? move 'reload) ; try move again (probably human selected new player from menu)
								(loop board 
									(add-selected-players opts human)
									pos next skipped?))
							((eq? move 'quit)
								(values 'quit opts))
							;; check if the response is a valid move
							((mem equal? (valid-moves board next) move)
								(loop (do-move board move next)
									opts move (opponent-of next) False))
							(else
								(show " match got move proposal " move)
								(show " valids are " (valid-moves board next))
								(show-result "Game terminated because of an invalid move")
								(values 'quit opts))))))))


	;; a printer which also adds the menu button(s) and player information

	(define (extended-print-board print-board players)
		(λ (board move opts color)
			(lets 
				((p-black (player-name opts black players))
				 (p-white (player-name opts white players)))
				(grale-fill-rect 0 0 w h 
					(get (get opts 'style False) 'bgcolor 0)) 
				(grale-puts 298 175 #b00000100 menu-button) 
				(show-players p-black p-white opts color)
				(print-board board move opts color))))

	;; note, players is usually already in menu, but added anyway

   (define (make-board-game default-options empty-board menu starter pick-winner valid-moves do-move players 
					act initial-state print-board) 
		(define human
			(make-human-player menu valid-moves act initial-state))

		(λ ()
			(print " ***************** ATAXXXXXXXXXXXXXXXXXXX *************************")
			(lets
				((print-board (extended-print-board print-board players))
				 (opts (put default-options 'print-board print-board)))
				(let loop ((opts (add-selected-players opts human)))
					(lets ((opts res (match empty-board opts False starter pick-winner valid-moves do-move human)))
						(show-match-result opts res players)
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
								(tuple-case (show-menu menu res)
									((save opts)
										; continue playing
										(loop (add-selected-players opts human)))
									(else 'quit)))))))))

)


