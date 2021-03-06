
; to be represented with something like
; 
;      x x x x x x x x x
;    xx                 x
;    x xx x x x x x x x
;    x               x
;    x               x
;    x               x
;    x               x
;    x               x
;  xxx               x
; x  x               x
;  xx x x x x x x x x
; 
; in the upper right corner of all games, along with other game-specific buttons

(define-module lib-menu

	(export 
		show-menu		;; menu choices -> action
		menu-button		;; a shared menu-icon to lower right corner
	)


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

	(define row-height 15)

	(define (label-of things val)
		(cond
			((null? things) "nothing")
			((and (eq? (ref (car things) 1) 'option) (eq? val (ref (car things) 4)))
				(ref (car things) 2))
			(else
				(label-of (cdr things) val))))

	(define (draw-rows things opts)
		(del 
			(fold
				(λ (rows thing)
					(lets 
						((y (get rows 'y False))
						 (rows 	
						 	(if (eq? (ref thing 1) 'spacer)
								rows
								(put rows y thing))))
						(tuple-case thing
							((option name desc value)
								(grale-put-text font-8px 20 y #b00011100 name))
							((choose name desc id options)
								(grale-put-text font-8px 20 y #b11111100 
									(foldr string-append "" 
										(list name " (" (label-of options (get opts id False)) ")"))))
							((menu name desc stuff)
								(grale-put-text font-8px 20 y #b00000011 
									(string-append name " (menu)")))
							((spacer)
								42)
							((back text)
								(grale-put-text font-8px 20 y #b00011111 text))
							((quit text)
								(grale-put-text font-8px 20 y #b11100000 text))
							(else
								(error "draw-rows: what is a " thing)))
						(put rows 'y (+ y row-height))))
				(put False 'y 35)
				things
				)
			'y))

	(define (choose-nearest-row opts y)
		(fold
			(λ (best yp node)
				(or best
					(if (and (>= yp y) (> y (- yp row-height)))
						node
						False)))
			False opts))

	(define (wait-row-click rows)
		(let ((ev (grale-wait-event)))
			(if ev
				(tuple-case ev
					((click btn x y)
						(or (choose-nearest-row rows y)
							(wait-row-click rows)))
					(else
						(wait-row-click rows)))
				(tuple 'quit "eof"))))

	; also handles scrolling later (add up- and down-buttons to side)
	(define (pick-thing things opts)
		(lets ((rows (draw-rows things opts))) ; y -> value
			(paint-screen)
			(wait-row-click rows)))

	(define (show-label name desc)
		(grale-put-text font-8px 10 18 #b00011100 desc))
		
	(define (choose-menu name desc id options opts)
		(clear-screen)
		(show-label name desc)
		(tuple-case (pick-thing options opts)
			((option name desc value) value)
			((back text) (get opts id False)) ; keep old value
			(else is bad
				(error "choose menu had a non-option: " bad))))

	(define (show-menu menu opts)
		(tuple-case menu
			((menu name desc stuff)
				(clear-screen)
				(show-label name desc)
				(tuple-case (pick-thing stuff opts)
					((quit text) (tuple 'quit text)) ; quit from this menu
					((back text) (tuple 'save opts)) ; return with changes
					((option name desc value) value) ; return a custom value as-is
					((choose name desc id options)
						(show-menu menu
							(put opts id (choose-menu name desc id options opts))))
					(else
						(error "show-menu: wtf is a " menu))))
			(else
				(error "show-menu: this is no menu: " menu))))
)

;(import lib-menu show-menu)
;(start-grale)
;(grale-init 320 200)
;(define test-menu
;	(tuple 'menu
;		"main menu"
;		"this is the main menu"
;		(list
;			(tuple 'choose "picken" "pick something" 'picked
;				(list
;					(tuple 'option "first" "aa" 1)
;					(tuple 'option "second" "bb" 2)
;					(tuple 'option "third" "cc" 3)))
;			(tuple 'choose "loltron" "pick something" 'mecha
;				(list
;					(tuple 'option "lalaa" "aa" 1)
;					(tuple 'option "loloo" "bb" 2)
;					(tuple 'option "lilith" "cc" 3)))
;			(tuple 'quit))))
;
;(show "menu generated " (show-menu test-menu False))

