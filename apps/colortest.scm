
(define-module olgame-colortest

	(import lib-grale)

	(export color-test-node)

	(define w 320)
	(define h 200)

	(define s 12)
	
	(define (color-test)
		(for-each
			(λ (n)
				(lets ((col row (quotrem n s)))
					(grale-fill-rect (* col s) (* row s) s s n)))
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

