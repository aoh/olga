;;;
;;; a flipper game (could also be called mod, don't know the real name)
;;;

; todo (very very low priority)
;	- could be easily generalized to n states
;	- undo and move counts
;	- alternative rules and board layouts?

(define usage-text "Usage: flip [args]")

(define about-flip 
"flip - a cleanup game

You are living on a toroid which has nice small dots and big nasty 
dots. Your task is to eliminate all the nasty dots by hitting on the
dots causing them to flip to the other state. The trouble is that the
neighbouring 8 dots also flip from the sheer force of your choice.

I have probably played a game like this a long time ago, but 
fail to remember where, and what it was called.
")

(import lib-args)

(define command-line-rules
	(cl-rules 
		`((about "-A" "--about")
		  (help  "-h" "--help")
		  (clean  "-c" "--clean" comment "experiment with a clean start")
		  (size  "-s" "--size" comment "board size" default "5" cook ,string->natural))))

(define (flip-cell size)
	(define max (expt size 2))
	(define (flipper state pos)
		(cond
			((< pos 0) (flipper state (+ pos max)))
			((>= pos max) (flipper state (- pos max)))
			(else
				(put state pos 
					(not (get state pos False))))))
	flipper)

(define (flip-at state pos s)
	(lets
		((r (if (ediv (+ pos 1) s) (- 1 s) 1))
		 (l (if (ediv pos s) (- s 1) -1))
		 (up (- pos s))
		 (do (+ pos s)))
		(fold (get state 'flipper 'bug) state
			(sort < 
				(list (+ up r) up (+ up l)
					  (+ pos r) pos (+ pos l)
					  (+  do r) do (+ do l))))))

(define (flip state x y)
	(lets 
		((s (get state 'size False))
		 (pos (+ x (* y s))))
		(flip-at state pos s)))

(define (random-seed) 
	(* (time 1) (begin (sleep 100) (time !))))

(import lib-random rand)

(define (randomize board)
	(let loop ((rst (time 1)) (pos 0) (max (get board 'size False)) (board board))
		(if (= pos (* max max)) board
			(lets ((rst n (rand rst 2)))
				(loop rst (+ pos 1) max
					(if (= n 0)
						board
						(flip-at board pos max)))))))

(define (empty-board size)
	(lets
		((board (put False 'size size))
		 (board (put board 'flipper (flip-cell size))))
		(for board (iota 0 1 (expt size 2))
			(λ (board pos) (put board pos False)))))

(define new-board (o randomize empty-board))

(import lib-vt)

(define yo 3)
(define xo 4)

(define (output state x y)
	(let ((s (get state 'size False)))
		(clear-screen)
		(for 42 (iota 0 1 s)
			(λ (_ y)
				(set-cursor xo (+ y yo))
				(for 42 (iota 0 1 s)
					(λ (_ x)
						(display (if (get state (+ x (* y s)) False) "● " "· "))))))
		"○"
		(for 42 (iota 0 1 s)
			(λ (_ p)
				(set-cursor (+ (- xo 1) (* p 2)) (- yo 1)) (display "--")
				(set-cursor (+ (- xo 1) (* p 2)) (+ yo s)) (display "--")
				(set-cursor (- xo 1) (+ yo p)) (display "|")
				(set-cursor (- (+ xo (* s 2)) 1) (+ yo p)) (display "|")
				))
		(set-cursor (- xo 1) (- yo 1)) (display "+")
		(set-cursor (- xo 1) (+ yo s)) (display "+")
		(set-cursor (+ (- xo 1) (* s 2)) (- yo 1)) (display "+")
		(set-cursor (+ (- xo 1) (* s 2)) (+ yo s)) (display "+")
		(set-cursor
			(+ xo (* x 2))
			(+ yo y))
		(flush-port 1)))

(define (move-focus board x y dir)
	(let ((s (get board 'size False)))
		(cond
			((eq? dir 'up) (values x (max 0 (- y 1))))
			((eq? dir 'down) (values x (min (- s 1) (+ y 1))))
			((eq? dir 'left) (values (max 0 (- x 1)) y))
			((eq? dir 'right) (values (min (- s 1) (+ x 1)) y))
			(else (error "funny direction: " dir)))))

(define (solved? board)
	(call/cc (λ (ret)
		(ff-fold
			(λ (maybe pos val)
				(if (symbol? pos) 
					maybe
					(if val (ret False) maybe)))
			True board))))

(define (play board in x y)
	(output board x y)
	(cond
		((null? in)
			(print "bye"))
		((pair? in)
			(tuple-case (car in)
				((arrow dir)
					(lets ((x y (move-focus board x y dir)))
						(play board (cdr in) x y)))
				((key k)
					(case k
						((32 13)
							(let ((board (flip board x y)))
								(if (solved? board)
									"There, you fixed it!"
									(play board (cdr in) x y))))
						((113)
							"Quitter. It's still all messed up.")
						; hjkl
						((106) (play board (cons (tuple 'arrow 'down) (cdr in)) x y))
						((107) (play board (cons (tuple 'arrow 'up) (cdr in)) x y))
						((104) (play board (cons (tuple 'arrow 'left) (cdr in)) x y))
						((108) (play board (cons (tuple 'arrow 'right) (cdr in)) x y))
						(else
							(play board (cdr in) x y))))
				(else
					(play board (cdr in) x y))))
		(else (play board (in) x y))))

(define (lets-flip size args)
	(raw-console)
	(clear-screen)
	(lets
		((size (max size 3))
		 (size (min size 30))
		 (board (if (get args 'clean False) (empty-board size) (new-board size)))
		 (result (play board (vt-events 0) 0 0)))
		(normal-console)
		(clear-screen)
		(set-cursor 1 1)
		(print result)
		0))

(define (flip args)
	(or 
		(process-arguments (cdr args) command-line-rules usage-text
			(λ (dict others)
				(cond
					((get dict 'about False) 
						(print about-flip))
					((get dict 'help False) 
						(print usage-text)
						(print-rules command-line-rules))
					((get dict 'size False) =>
						(λ (n) (lets-flip n dict)))
					(else
						(print "bug")))))
		1))

;(flip '(flipper "-s" "10"))

flip
