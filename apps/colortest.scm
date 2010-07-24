
(define-module olgame-colortest

	(import lib-grale)

	(export color-test-node)

	(define w 320)
	(define h 200)

	(define cols 20)
	(define rows (div 256 (+ cols 1)))
	
	(define col-width (div w cols))
	(define col-height (div h rows))

	(define (color-test)
		(for-each
			(λ (n)
				(lets ((row col (quotrem n cols)))
					(grale-fill-rect
						(* col col-width)
						(* row col-height)
						col-width
						col-height
						n)))
			(iota 0 1 256))
		(grale-update 0 0 w h)
		(lets ((x y (grale-wait-click)))
			42))
			
	(define color-test-node
		(tuple 'proc False "show palette" color-test))

)

(import olgame-colortest)

(define olgame-games 
	(cons color-test-node olgame-games))

