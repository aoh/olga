
(define-module olgame-keytest

	(import lib-grale)

	(export key-test-node)

	(define w 320)
	(define h 200)

	(define s 12)
	(define row-height 10)

	(define (clear-cursor x y)	
		(grale-fill-rect x (- y 8) 3 9 0)
		(grale-update x (- y 8) 3 9))

   (define cursor
      (build-sprite
       '(3 x - x
           - x -
           - x -
           - x -
           - x -
           - x -
           - x -
           o - x)))

	(define (put-cursor x y)
		(grale-puts x y #b11111100 cursor)
		(grale-update x (- y 8) 3 9))

	(define no-char (get font-8px -1 False))

	(define col #b00011100)

	(define (handle-key x y key)
		(if (eq? key 13) ; carriage, return!
			(let ((y (+ y row-height)))
				(if (<= y h)
					(values 1 y)
					(begin
						(clear-screen)
						(values 1 row-height))))
			(lets 
				((char (get font-8px key no-char)) ; (width . data)
				 (xp (+ x (car char))))
				(if (< xp w)
					(begin
						(grale-puts x y col (cdr char))
						(grale-update x (- y 8) (car char) 9)
						(values xp y))
					(lets
						((x y (handle-key x y 13)))
						(handle-key x y key))))))
		
	(define (tester x y)
		(put-cursor x y)
		(tuple-case (grale-wait-event)
			((click btn x y)
				'clicked)
			((key cp)
				(clear-cursor x y)
				(if (= cp 0)
					(tester x y) ; shift etc special key
					(lets ((x y (handle-key x y cp)))
						(tester x y))))
			(else
				(tester x y))))
			
	(define (key-test)
		(tester 1 row-height))

	(define key-test-node
		(tuple 'proc False "test keyboard" key-test))

)

(import olgame-keytest)

(define olgame-games 
	(cons key-test-node olgame-games))

