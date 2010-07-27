;;;
;;; One game to rule them all!!1
;;;

;; todo: catch owl errors in programs and handle without closing olgame itself
;; todo: allow storing some program state (like preferences) when closing apps
;; todo: store test programs below a separate menu
;; todo: convert old games (sokoban sudoku five(rewrite!) flip)
;; todo: write new games (old and new puzzles, chess, arcade games, olganoid)

(import lib-grale)
(import lib-args)

;; global window size 

(define w 320)
(define h 200)

; a list of
;   #(proc <icon>|False <label> start-thunk)
;	 #(dir <icon>|False (list-of-nodes))

;; list to which loaded games add themselves
(define olgame-games null)

(define (paint-screen)
	(grale-update 0 0 w h))

(define (clear-screen)
	(grale-fill-rect 0 0 w h 0)
	(paint-screen))

(define (grale-wait-click)
	(paint-screen)
	(let loop ((ev (grale-wait-event)))
		(if ev
			(tuple-case ev
				((click btn x y)
					(values x y))
				(else
					(loop (grale-wait-event))))
			(begin
				(print "grale got disconnected")
				(values False False)))))

;; shared code for games 

,r "lib/menu.scm"
,r "lib/ai.scm"

;; test programs

(define (princess-rescue)
	(grale-put-text font-8px 100 100 #b11101100 "You rescued the princess")
	(lets ((x y (grale-wait-click))) 42))

(define (prince-rescue)
	(grale-put-text font-8px 100 100 #b00001101 "You rescued the prince")
	(lets ((x y (grale-wait-click))) 42))

,r "games/reversi.scm"
;,r "games/ataxx.scm" ;; not in use until the new match code is done (testing in reversi)
,r "apps/colortest.scm"
,r "apps/keytest.scm"

(define olgame-games
	(ilist
		(tuple 'dir  False "rescue games"
			(list
				(tuple 'proc False "rescue the princess" princess-rescue)
				(tuple 'proc False "rescue the prince" prince-rescue)
				(tuple 'proc False "rescue the princess II" princess-rescue)
				(tuple 'proc False "rescue the princess III" princess-rescue)
				(tuple 'proc False "rescue the princess IV (EU)" princess-rescue)
				(tuple 'proc False "rescue the prince V" prince-rescue)))
		olgame-games))

;; todo: generate the main menu using lib-menu

(define (choose-nearest-row opts y row-height)
	(fold
		(λ (best yp node)
			(or best
				(if (and (>= y yp) (< y (+ yp row-height)))
					node
					False)))
		False opts))

(define (wait-row-click opts)
	(let ((ev (grale-wait-event)))
		(if ev
			(tuple-case ev
				((click btn x y)
					(or (choose-nearest-row opts y 15)
						(wait-row-click opts)))
				(else
					(wait-row-click opts)))
			(tuple 'quit))))
	
(define (main-menu node)
	(clear-screen)
	(grale-puts (- w 10) 10 #b00011100 owl-logo)
	(let 
		;; add back-option to list
		((node
			(append node
				(list 
					(tuple 'back False 
						(if (eq? node olgame-games) "leave olgame" "go back"))))))
		(grale-put-text font-8px 10 20 #b00011100 
			(foldr string-append "" (list "You now have " (runes->string (render (length node) null)) " choices")))
		;; print the options and save y coord of each (keeping next one at 'y)
		(define opts
			(fold
				(λ (opts thing)
					(lets
						((y (get opts 'y 40))
						 (label (ref thing 3)))
						(grale-put-text font-8px 20 y 
							(cond
								((eq? (ref thing 1) 'proc) #b00011100)
								((eq? (ref thing 1) 'dir) #b00000011)
								((eq? (ref thing 1) 'back) #b11111100)
								(else #b11111111))
							label)
						(put (put opts 'y (+ y 15)) (- y 8) thing))) ; put row top
				False node))
		(paint-screen)
		;; pick first click within a row
		(wait-row-click (del opts 'y))))

(define (olgame-root node)
	(paint-screen)
	(let ((choice (main-menu node)))
		(tuple-case choice
			((quit)
				(print "bye bye"))
			((proc ico label code)
				(clear-screen)
				(code)
				(clear-screen)
				(olgame-root node))
			((back ico label)
				(clear-screen)
				'done)
			((dir ico label stuff)
				(clear-screen)
				(olgame-root stuff)
				(clear-screen)
				(olgame-root node))
			(else is thing
				(error "confused by " thing)))))

(define (start-olgame dict others)
	(olgame-root olgame-games))

(define usage-text "Usage: olgame")

(define about-olgame 
"This is a collection of small games.
Written by Aki Helin.")

(define command-line-rules
	(cl-rules 
		`((about "-A" "--about")
		  (help  "-h" "--help")
		  )))

(define (olgame args)
	(or 
		(process-arguments (cdr args) command-line-rules usage-text
			(λ (dict others)
				(cond
					((get dict 'about False) 
						(print about-olgame))
					((get dict 'help False) 
						(print usage-text)
						(print-rules command-line-rules))
					(else
						(start-grale)
						(tuple-case (grale-init w h)
							((connected)
								(start-olgame dict others))
							(else
								(print "Cannot start grale (the graphics server)")
								(print "Are you sure it is available?")
								(print "To install it, it should suffice to say $ ...")
								1))))))
		1))


; (olgame '(olgame))

olgame


