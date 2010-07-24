;;;
;;; One game to rule them all!!1
;;;

; olgame has a simple program-selection menu
; programs are in separate libraries under games/
; they add themselves to the directory tree exposing only icon, name and program entry point

(import lib-grale)
(import lib-args)

(define command-line-rules
	(cl-rules 
		`((about "-A" "--about")
		  (help  "-h" "--help")
		  )))

(define w 320)
(define h 200)

; a list of
;   #(proc <icon>|False <label> start-thunk)
;	 #(dir <icon>|False (list-of-nodes))

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

(define (princess-rescue)
	(grale-put-text font-8px 100 100 #b11101100 "You rescued the princess")
	(lets ((x y (grale-wait-click))) 42))

(define (prince-rescue)
	(grale-put-text font-8px 100 100 #b00001101 "You rescued the prince")
	(lets ((x y (grale-wait-click))) 42))

(define olgame-games
	(ilist
		(tuple 'dir  False "princess series"
			(list
				(tuple 'proc False "rescue the princess" princess-rescue)
				(tuple 'proc False "rescue the prince" prince-rescue)
				(tuple 'proc False "rescue the princess II" princess-rescue)
				(tuple 'proc False "rescue the princess III" princess-rescue)
				(tuple 'proc False "rescue the princess IV (EU)" princess-rescue)
				(tuple 'proc False "rescue the prince V" prince-rescue)))
		olgame-games))

,r "games/reversi.scm"

; ,r "games/ataxx.scm"

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
	(grale-put-text font-8px 10 20 #b00011100 
		(foldr string-append "" (list "You now have " (runes->string (render (length node) null)) " choices")))
	(grale-puts (- w 10) 10 #b00011100 owl-logo)
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
			False 
			(append  node (list(tuple 'back False "exit")))))
	(paint-screen)
	(wait-row-click (del opts 'y)))

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

(define usage-text "It's no use.")

(define (olgame args)
	(or 
		(process-arguments (cdr args) command-line-rules usage-text
			(λ (dict others)
				(cond
					((get dict 'about False) 
						(print "about what"))
					((get dict 'help False) 
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

