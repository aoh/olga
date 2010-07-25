
; a generic match controller for two-player games
; fixme, just quickly carried out all dependencies in variables. running out of battery for laptop and can't get up to 
; get a charger or the kids might wake up...

(define-module lib-match
	
	(export play-match match)

	(import lib-vt)

	(define black 'black)
	(define white 'white)
	(define (opponent-of x) (if (eq? x black) white black))

	(define (report-winner winner)
		(set-cursor 1 10)
		(cond
			((eq? winner black)
				(print "The black knight always triumphs."))
			((eq? winner white)
				(print "The white wizard is victorius."))
			(else
				(print "All right. We'll call it a draw."))))

	(define (disqualify player reason)
		(show "Player disqualified due to " reason)
		(sleep 3000)
		(opponent-of player))

	; -> black | white | draw | quit
	(define (match board in pos next player opponent printer pick-winner valid-moves do-move)
		(let loop ((board board) (in in) (pos pos) (next next) (player player) (opponent opponent) (skipped? False))
			(printer board pos in)
			(cond
				((pick-winner board False) =>
					(λ (winner) winner))
				(else
					(lets ((move in (player board in pos next)))
						(cond
							((not move)
								(if skipped?
									; neither player can or is willing to move
									(begin
										(print "deadlock")
										(pick-winner board True))
									(loop board in pos (opponent-of next) opponent player True)))
							((eq? move 'quit)
								'quit)
							((mem equal? (valid-moves board next) move)
								(loop (do-move board move next) 
									in move (opponent-of next) 
									opponent player False))
							(else
								(disqualify next "invalid move."))))))))


	; names have to be printed differently, because rendering asks function
	; names are from the 'meta thread, which is (stupid and) kind of useless 
	; to have running around in dumped code.

	(define (name-of player players)
		(if (eq? player 'draw)
			"draw"
			(get players player "mysterious")))

	(define (show-match-results res players)
		(normal-console)
		(if res
			(lets
				((res (ff->list res))
				 (res (sort (λ (a b) (> (cdr a) (cdr b))) res)))
				(clear-screen)
				(set-cursor 1 1)
				(print "Results: ")
				(for-each
					(λ (node)
						(lets ((winner count node))
							(print* (list " - " (name-of winner players) ": " count))))
					res)
				0)
			(print "Quitter.")))

	(define (start-match black-player white-player empty-board games printer pick-winner valid-moves do-move players start)
		(let loop ((status False) (bp black-player) (wp white-player) (games games))
			(if (> games 0)
				(lets
					((res 
						(match empty-board (vt-events 0) start black bp wp printer pick-winner valid-moves do-move))
					 (status
						(cond
							((eq? res black) (put status bp (+ 1 (get status bp 0))))
							((eq? res white) (put status wp (+ 1 (get status wp 0))))
							((eq? res 'draw) (put status 'draw (+ 1 (get status 'draw 0))))
							(else status))))
					(if (eq? res 'quit)
						(show-match-results (del status res) players)
						(begin
							(clear-screen)
							(set-cursor 1 1)
							(show "Status: "
								(ff-fold (lambda (out player score) (cons (cons (name-of player players) score) out)) null status))
							(flush-port 1)
							; (sleep 500) ; enough to see the progress in ai matches
							(loop status wp bp (- games 1)))))
				(show-match-results status players))))

	(define (play-match args empty-board print-board pick-winner valid-moves do-move players start)
		(raw-console)
		(lets
			((white (get args 'white 'bug))
			 (black (get args 'black 'bug))
			 (result (start-match black white empty-board (get args 'matches 1) print-board pick-winner valid-moves do-move players start)))
			(normal-console)
			0))

)
