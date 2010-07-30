
(define-module olgame-colortest

	(import lib-grale)

	(export color-test-node)

	(define w 320)
	(define h 200)

	(define s 12)

	(define hexes (string->list "0123456789abcdef"))

	(define (hex n)
		(lets
			((hi (>> n 4))
			 (lo (band n 15)))
			(runes->string
				(list
					(lref hexes hi)
					(lref hexes lo)))))

	(define cell-w 20)
	(define cell-h 25)

	(define row-width (div w cell-w))

	(define (show-color x y n)
		(lets
			((str (hex n))
			 (wid (grale-text-width font-8px str))
			 (pad-x (div (- cell-w wid) 2))
			 (pad-y (div (- cell-h 8) 2)))
			(grale-put-text font-8px (+ x pad-x) (+ y (+ pad-y 8)) (bxor n 255) str)))

	(define (show-colors root)
		(for-each
			(λ (n)
				(lets 
					((row col (quotrem (- n root) row-width))
					 (x (* col cell-w))
					 (y (* row cell-h)))
					(grale-fill-rect x y cell-w cell-h n)
					(show-color x y n)))
			(iota root 1 (+ root 128)))
		(grale-update 0 0 w h)
		(lets ((x y (grale-wait-click))) 42))

			
	(define (color-test)
		(show-colors 0)
		(show-colors 128))

	(define color-test-node
		(tuple 'proc False "show palette" color-test))

)

(import olgame-colortest)

(define olgame-games 
	(cons color-test-node olgame-games))

