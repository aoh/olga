;;;
;;; One game to rule them all!!1
;;;

;; todo: make a simple windowed menu-generation system
; rough idea: pass information in a ff with state-transformers mapped to bounding boxes of elements. selections, checkboxes, sliders etc initially

(import lib-grale)
(import lib-args)

(define command-line-rules
	(cl-rules 
		`((about "-A" "--about")
		  (help  "-h" "--help")
		  )))

(define w 640)
(define h 480)

; a list of
;   #(proc <icon>|False <label> start-thunk)
;	 #(dir <icon>|False (list-of-nodes))

(define olgame-games null)

(define (grale-wait-click)
	(let ((ev (grale-wait-event)))
		(if (not ev) 
			(begin
				(print "wait-click: disconnected") 
				(values False False))
			(tuple-case ev
				((click btn x y)
					(values x y))
				(else
					(grale-wait-click))))))

(define (paint-screen)
	(grale-update 0 0 w h))

(define (clear-screen)
	(grale-fill-rect 0 0 w h 0)
	(paint-screen))

(define (princess-rescue)
	(print "pricess rescued \o/"))

(define (prince-rescue)
	(print "price rescued \o/"))

(define olgame-games
	(ilist
		(tuple 'proc False "rescue the princess" princess-rescue)
		(tuple 'proc False "rescue the pricne" prince-rescue)
		olgame-games))

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
	(grale-put-text font-8px 10 20 #b00011100 "You see a menu.")
	(define opts
		(fold
			(λ (opts thing)
				(lets
					((y (get opts 'y 40))
					 (label (ref thing 3)))
					(grale-put-text font-8px 20 y #b11111100 label)
					(put (put opts 'y (+ y 15)) (- y 8) thing))) ; put row top
			False node))
	(paint-screen)
	(wait-row-click (del opts 'y)))

(define (olgame-root node)
	(grale-put-text font-8px 10 10 #b00011100 "Welcome to Olgame, green wizard.")
	(grale-update 0 0 w h)
	(let ((choice (main-menu node)))
		(tuple-case choice
			((quit)
				(print "bye bye"))
			((proc ico label code)
				(clear-screen)
				(code)
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


(olgame '(olgame))
