;;;
;;; Freestyle gomoku (aka connect five)
;;;

; todo:
;	- convert to generic ai code

(define board-size 15)
(define beam-width 4)

;;; 1D part

(define initial-threat
	'(-1 x x x x x))

(define (lset l n v)
	(if (= n 0) (cons v (cdr l))
		(cons (car l)
			(lset (cdr l) (- n 1) v))))

(define (index l i)
	(if (null? l)
		null
		(cons (cons i (car l))
			(index (cdr l) (+ i 1)))))

(define (reduce-threat th offset)
	(let*
		((base (map (λ x (if (eq? x 'x) 'x '_)) (cdr th)))
		 (severity (+ offset (+ (car th) 2))))
		(if (all (λ x (eq? x '_)) base)
			null
			(for null (keep (λ x (eq? (cdr x) 'x)) (index base 0))
				(λ (out node)
					(cons
						(cons severity (lset base (car node) 'm))
						out))))))

(define (grow-threat th out)
	(if (has? th 'x)
		(foldr grow-threat (cons th out)
			(reduce-threat th 0))
		(cons th out)))

(define (eql? a b)
	(cond
		((null? a) (null? b))
		((null? b) False)
		((eq? (car a) (car b))
			(eql? (cdr a) (cdr b)))
		(else False)))

(define (contains? l n)
	(cond
		((null? l) False)
		((eql? (car l) n) True)
		(else (contains? (cdr l) n))))

(define (uniques l)
	(cond
		((null? l) l)
		((contains? (cdr l) (car l))
			(uniques (cdr l)))
		(else
			(cons (car l) (uniques (cdr l))))))

(define all-threats
	(uniques
		(foldr grow-threat null 
			(ilist
				'(3 x _ x x m _ x)
				'(3 x _ x m x _ x)
				'(3 x _ m x x _ x)
				;; fail, does not hold for otherwise blocked fives
				'(0 m x x x x x)
				'(0 x x x x x m)
				(append
					(reduce-threat '(1 _ x x x x _) -1)
					(reduce-threat initial-threat 0))))))

; (for-each print all-threats)

(define (match node lst taken matched player)
	(cond
		((not node) matched)
		((pair? node)
			(if (null? lst)
				matched
				(let ((hd (car lst)))
					(cond
						((teq? hd fix+)	 ; free
							(match (cdr node) (cdr lst) (cons hd taken) matched player))
						((eq? hd player)
							(match (car node) (cdr lst) taken matched player))
						(else matched)))))
		(else
			(bind node
				(λ (nth score node)
					(if nth
						(match node lst taken
							(cons (cons score (lref taken nth)) matched)
							player)
						(match node lst taken matched player)))))))

(define (insert-pattern node pat n score)
	(cond
		((null? pat)
			(if n
				(tuple n score node)
				(error "no move in pattern: " pat)))
		((tuple? node)
			(set node 3
				(insert-pattern (ref node 3) pat n score)))
		((not node)
			(insert-pattern (cons False False) pat n score))
		((eq? (car pat) 'm)
			(insert-pattern node (cons '_ (cdr pat)) -1 score))
		((eq? (car pat) '_)
			(cons (car node)
				(insert-pattern (cdr node) (cdr pat)
					(if n (+ n 1) n) score)))
		(else
			(cons
				(insert-pattern (car node) (cdr pat) n score)
				(cdr node)))))

(define all-patterns
	(fold 
		(λ (pats pat) 
			(show "inserting " pat)
			(insert-pattern pats (cdr pat) 0 (car pat)))
		False all-threats))

(define (match-for player lst)
	(let loop ((lst lst) (matched null))
		(if (null? lst)
			matched
			(loop (cdr lst) (match all-patterns lst '() matched player)))))

(define game-over 100000000)
(define winning-move 31337)

(define win-score game-over)
(define lose-score (- 0 game-over))

(define scores
	(fold
		(λ (scores n)
			(put scores n
				(cond
					((eq? n 0) win-score)
					((eq? n 1) winning-move)
					((eq? n 2) 700)
					((eq? n 3) 80)
					(else (div 100 (max 1 (* n (>> n 1))))))))
		False (iota 0 1 20))) ; moves go up to around 11 atm 

(define (proposals player lst) ; -> ff of move -> score
	(fold
		(λ (sums node) ; (score . move)
			(put sums (cdr node)
				(+ (get sums (cdr node) 0)
					(get scores (car node) sums))))
		False (match-for player lst)))


;;; 2D part


(define bs board-size)

(define rows
	(map
		(λ (start) (iota start 1 (+ bs start)))
		(iota 0 bs (* bs bs))))

(define cols 
	(map
		(λ (start) (iota start bs (+ start (* bs bs))))
		(iota 0 1 bs)))

(define (slope pos dir n)
	(if (= n 0)
		null
		(cons pos
			(slope (+ pos dir) dir (- n 1)))))

; a bit naive these

(define downs
	(map 
		(λ (node) (slope (car node) (+ bs 1) (cdr node)))
		(append
			(zip cons (iota (* bs (- bs 1)) (- 0 bs) 0) (iota 1 1 bs))
			(zip cons (iota 0 1 bs) (iota bs -1 0)))))

(define ups
	(map 
		(λ (node) (slope (car node) (- 0 (- bs 1)) (cdr node)))
		(append
			(zip cons (iota 0 bs (* bs bs)) (iota 1 1 bs))
			(zip cons (iota (* bs (- bs 1)) 1 (* bs bs)) (iota bs -1 0)))))

(define all-slices 
	(keep 
		(λ (x) (>= (length x) 5))
		(foldr append null (list rows cols downs ups))))


(define slices
	(list->tuple
		(map
			(λ pos 
				(keep (λ x (has? x pos)) all-slices))
			(iota 0 1 (* bs bs)))))

(define (slices-of move)
	(lets ((move o (fx+ move 1)))
		(ref slices move)))

; #(cells threats next-player last-move)
; cells = ff of move -> value | False for blank
; threats = (xs . os)
;	 xs = ff of slice -> x's threats in the slice
;

(define (opponent-of player)
	(if (eq? player 'x) 'o 'x))

(define (update-threats threats cells pos player)
	(for threats (slices-of pos)
		(λ (threats slice)
			(lets
				((data (map (λ (x) (get cells x x)) slice))
				 (new (proposals player data)))
				(cond
					(new
						(put threats slice new))
					((get threats slice False)
						(del threats slice))
					(else threats))))))
				
(define (plot board pos)
	(bind board
		(λ (cells threats next-player last-move)
			(if (eq? 'blank (get cells pos 'blank))
				(let ((cells (put cells pos next-player)))
					(tuple cells
						(cons
							(update-threats (car threats) cells pos 'x)
							(update-threats (cdr threats) cells pos 'o))
						(opponent-of next-player)
						pos))
				False))))


(define (five-of piece n pcs)
	(cond
		((= n 5) piece)
		((null? pcs) False)
		((car pcs)
			(if (eq? (car pcs) piece)
				(five-of piece (+ n 1) (cdr pcs))
				(five-of (car pcs) 1 (cdr pcs))))
		(else
			(five-of False 0 (cdr pcs)))))

(define (pick-winner board)
	(let ((pieces (ref board 1)))
		(for False all-slices
			(lambda (winner slice)
				(or winner
					(five-of 'nan 0 
						(map (λ (x) (get pieces x False)) slice)))))))


;;;
;;; Output
;;;

(import lib-vt)

(define x-start 3)
(define y-start 2)

(define (xy->move x y) (+ x (* y bs)))

(define (x->terminal-x x) (+ (* x 2) x-start))
(define (y->terminal-y y) (+ y y-start))

(define (terminal-x->x x) 
	(if (= 0 (band (- x x-start) 1))
		(let ((val (div (- x x-start) 2)))
			(if (and (<= 0 val) (< val bs))
				val False))
		False))

(define (terminal-y->y y)
	(let ((val (- y y-start)))
		(if (and (<= 0 val) (< val bs))
			val
			False)))

(define (get-piece val)
	(cond
		((eq? val '_) "·")
		((eq? val 'x) "●")
		((eq? val 'o) "○")
		(else "?")))

; temp, eww..

(define (print-board-borders)
	(set-cursor (- x-start 2) (- y-start 1))
	(display "┌")
	(for-each (λ (x) (display "──")) (iota 1 1 bs))
	(display "───┐")
	(set-cursor (- x-start 2) (+ y-start bs))
	(display "└")
	(for-each (λ (x) (display "──")) (iota 1 1 bs))
	(display "───┘")
	(for-each (λ (i) (set-cursor (- x-start 2) (+ y-start i)) (display "│")) (iota 0 1 bs))
	(for-each (λ (i) (set-cursor (+ x-start (* bs 2)) (+ y-start i)) (display "│")) (iota 0 1 bs))
	)

(define tabula-rasa 
	(tuple False (cons False False) 'x (div (* bs bs) 2)))

(define (insert lst score move)
	(cond
		((null? lst) (list (cons score move)))
		((< (caar lst) score)
			(cons (car lst) (insert (cdr lst) score move)))
		(else
			(cons (cons score move) lst))))

(define (sum-scores threats out offset ret)
	(ff-fold
		(λ (out slice proposals)
			(ff-fold
				(λ (out move score)
					(cond
						((>= score winning-move)
							(ret (list (cons score move))))
						(offset
							(put out move
								(+ (get out move 0) (+ score (>> score offset)))))
						(else
							(put out move
								(+ (get out move 0) score)))))
				out proposals))
		out threats))

; (cells threats next-player last-move)

(define my-bonus 4) ; add floor(score*1/2^n) to own moves

(define (threat-info board)
	(lets 
		((me (ref board 3))
		 (ths (ref board 2)))
		(if (eq? me 'x)
			(values me (car ths) (cdr ths))
			(values me (cdr ths) (car ths)))))

(define search-init
	(let ((lol '(0 . -1)))
		(map (λ (x) lol) (iota 0 1 beam-width))))

(define (first-blanks board)
	(let ((cells (ref board 1)))
		(map (λ (x) (cons 1 x))
			(take 
				(keep (λ (x) (not (get cells x False))) 
					(iota 0 1 (* bs bs)))
				3))))

(define (search-good-moves board search-init)
	(call/cc
		(λ (ret)
			(lets
				((me my opp (threat-info board))
				 (sums (sum-scores my False my-bonus ret))
				 (sums (sum-scores opp sums False ret))
				 (goodies
					(reverse
						(keep (λ (x) (not (eq? (car x) 0)))
							(ff-fold
								(λ (lead move score)
									(if (> score (car (car lead)))
										(insert (cdr lead) score move)
										lead))
								search-init sums)))))
				(if (null? goodies)
					(first-blanks board)
					goodies)))))

(define (good-moves board)
	(search-good-moves board search-init))

(define (guess-response board)
	(search-good-moves board
		'((0 . 0) (0 . 0) (0 . 0) (0 . 0) (0 . 0) (0 . 0) (0 . 0) (0 . 0))))

; evaluate while detecting wins and walking down forcing chains

(define (evaluate board)
	(lets ((me my opp (threat-info board)))
		(call/cc
			(λ (eject)
				(lets
					((my-goodness
						(ff-fold (λ (sum slices data)
							(ff-fold (λ (sum move score)
								(if (>= score winning-move)
									(eject win-score)
									(+ sum score)))
								sum data))
							0 my))
					 (opp-goodness
					 	(ff-fold (λ (sum slices data)
							(ff-fold (λ (sum move score)
								(if (>= score winning-move)
									(eject (- 0 (evaluate (plot board move))))
									(+ sum score))
								)
								sum data))
							0 opp)))
					(- (+ my-goodness (>> my-goodness my-bonus))
						opp-goodness))))))

(define (plot-focus x y l r)
	(let 
		((tx (x->terminal-x x))
		 (ty (y->terminal-y y)))
		(set-cursor (- tx 1) ty)
		(display l)
		(set-cursor (+ tx 1) ty)
		(display r)))


(define (print-board board) ; -naive
	(clear-screen)
	(print-board-borders)
	(let ((cells (ref board 1)))
		(for False (iota 0 1 bs)
			(λ (foo x)
				(for False (iota 0 1 bs)
					(λ (foo y)
						(lets
							((move (xy->move x y))
							 (val (get cells move '_)))
							(set-cursor (x->terminal-x x) (y->terminal-y y))
							(display (get-piece val))
							(if (eq? move (ref board 4))
								(plot-focus x y "(" ")"))))))))
	(set-cursor 1 (+ bs 3))
	(flush-port 1))


(import lib-lazy)

(define (fix)
	(mouse-click-tracking False)
	(normal-console))

(define (search board alpha beta ply) ; -> score + move|False 
	(if (eq? ply 0)
		(values (evaluate board) False)
		(lets
			((moves (good-moves board))
			 (ply sub (fx- ply 1)))
			(let loop 
				((moves (map cdr moves))
					(alpha alpha) (best False))
				(cond
					((null? moves)
						(values alpha best))
					((> alpha beta)
						(values alpha best))
					(else
						(lets 
							((board (plot board (car moves)))
							 (opp-score opp-move 
								(search board (- 0 beta) (- 0 alpha) ply))
							 (score (- 0 opp-score)))
							(if (> score alpha)
								(loop (cdr moves) score (car moves))
								(loop (cdr moves) alpha best)))))))))

(define (forward in)
	(if (pair? in)
		(forward (cdr in))
		in))

(define (step x y dir)
	(cond
		((and (eq? dir 'up) (> y 0)) (values x (- y 1)))
		((and (eq? dir 'down) (< y (- bs 1))) (values x (+ y 1)))
		((and (eq? dir 'left) (> x 0)) (values (- x 1) y))
		((and (eq? dir 'right) (< x (- bs 1))) (values (+ x 1) y))
		(else (values x y))))

(define (move-focus board dir)
	(lets
		((pos (ref board 4))
		 (x (rem pos bs))
		 (y (div pos bs))
		 (xp yp (step x y dir)))
		(plot-focus x y " " " ")
		(plot-focus xp yp "(" ")")
		(set-cursor 1 (+ bs 3))
		(flush-port 1)
		(set board 4 (xy->move xp yp))))

(define (lpos l v)
	(let loop ((l l) (p 1))
		(cond
			((null? l) False)
			((eq? (car l) v) p)
			(else (loop (cdr l) (+ p 1))))))

(define (human-player board in)
	(let loop ((in (forward in)) (board board))
		(cond
			((null? in) (values False in))
			((pair? in) 
				(tuple-case (car in)
					((mouse x y btn)
						(let*
							((x (terminal-x->x x))
							 (y (terminal-y->y y)))
							(if (and x y)
								(let* 
									((move (xy->move x y))
									 (new (plot board move)))
									(if new
										(values new in)
										(loop (tail in) board)))
								(loop (tail in) board))))
					((arrow dir)
						(loop (tail in)
							(move-focus board dir)))
					((key k)
						(case k
							((113) ; q(uit)
								(values False in))
							((13 32) ; enter/space (move)
								(let ((new (plot board (ref board 4))))
									'(cond
										((lpos (map cdr (guess-response board)) (ref board 4))
											=> (λ (n)
												(show " * my quess # " n)))
										(else
											(print " !! didn't see that coming")))
									'(sleep 1000)
									(if new
										(values new in)
										(loop (tail in) board))))
							(else
								(loop (tail in) board))))
					(else 
						(loop (tail in) board))))
			(else
				(loop (in) board)))))

(define (show-computer-opinion board)
	;; show ply 0 beam 
	(fold
		(λ (pref node)
			(if (> (car node) 0)
				(begin
					(set-cursor (x->terminal-x (rem (cdr node) bs)) 
						(y->terminal-y (div (cdr node) bs)))
					;(display pref)
					(display "•")
					(+ pref 1))
				pref))
		1 (good-moves board))
	;; show score of current node
	;(set-cursor 1 (+ bs 3))
	;(show " * my score " (evaluate board))
	;(set-cursor 1 (+ bs 4))
	(flush-port 1))

(define mid (div bs 2))

;; go for center if free 
(define (try-first-move board answer)
	(if (eq? '_ (get (ref board 1) (xy->move mid mid) '_))
		(answer (plot board (xy->move (div bs 2) (div bs 2))))
		False))

;; if all neighbours of center are free, pick one at random
(define try-second-move 
	(lets
		((u (- 0 bs)) (d bs) (l -1) (r +1)
		 (center (div (* bs bs) 2))
		 (neighbours
			(map (λ (off) (+ center off))
				(list (+ u l) u (+ u r)
							  l     r
						(+ d l) d (+ d r)))))
		(λ (board answer)
			(let ((cells (ref  board 1)))
				(if (all (λ (x) (not x))
						(map (λ (p) (get cells p False))
							neighbours))
					(answer
						(plot board
							(lref neighbours
								(band (time 1) 7)))))))))

(define (try-winning-move board my answer)
	;; make a winning move if applicable
	(ff-fold (λ (the slices data)
		(ff-fold (λ (the move score)
			(if (>= score winning-move)	
				(answer (plot board move))
				the))
			the data))
		False my))

(define try-defend-move try-winning-move)

(define (make-computer-player ply-limit)
	(define (computer-player board in)
		;; debug/devel
		;(show-computer-opinion board)
		;; ai
		(lets ((me my opp (threat-info board)))
			(values
				(call/cc
					(λ (answer)
						(try-first-move board answer)
						(try-second-move board answer)
						(try-winning-move board my answer)
						(try-defend-move board opp answer)
						(for board (iota 1 1 ply-limit)
							(λ (prop ply)
								(if (pick-winner prop)
									prop
									(lets 
										((score move 
											(search board lose-score win-score ply)))
										(plot board move)))))))
				in)))
	computer-player)


(define (match board player opponent in)
	(print-board board)
	(cond
		((pick-winner board) =>
			(λ (winner) (values winner in)))
		((null? (first-blanks board))
			(values 'draw in))
		(else 
			(lets ((board in (player board in)))
				(if board
					(match board opponent player in)
					(values 'resigned in))))))

(define (eternal-battle black white)
	(raw-console)
	(mouse-click-tracking True)
	(flush-port 1)
	(let loop ((black black) (white white)
					(in (vt-events 0)) (wins 0) (matches 0))
		(lets ((winner in (match tabula-rasa black white in)))
			(cond
				((eq? winner 'resigned)
					(print "ack, bailing out.")
					(normal-console)
					(mouse-click-tracking False)
					(flush-port 1)
					0)
				((eq? winner 'draw)
					(print "all right, we'll call it a draw")
					(sleep 3000)
					(loop white black in wins (+ matches 1)))
				(else
					(show (get-piece winner) " triumphs!")
					(lets
						((wins 
							(+ wins 
								(if (or (and (eq? winner 'x) (eq? black human-player))
										(and (eq? winner 'o) (eq? white human-player)))
									1 0)))
						 (matches (+ matches 1)))
						(print "")
						(set-cursor 1 (+ bs 5))
						(print* (list "You are " (div (* wins 100) matches) 
							"% victorious"))
						(sleep 4000)
						(loop white black in wins matches)))))))

; build-time play
; (eternal-battle human-player (make-computer-player 3))

(import lib-args)

(define command-line-rules
	(cl-rules
		`((about "-A" "--about")
		  (help "-h" "--help")
		  (version "-V" "--version")
		  ;(strict "-s" "--strict"
		  ; comment "only win by exactly five stones (no more)")
		  (difficulty "-d" "--difficulty" cook ,string->number
			comment "difficulty level (1-7)"
			default "3")
		  )))

(define usage-text "Usage: five [args]")

(define about-five
"Five -- a simple connect-five game
Written by Aki Helin

Click the moves with a mouse or use the arrow keys to
move and enter to plot your piece. You must have a 
UTF-8 capable vt100-like terminal.
")


;;;
;;; Startup
;;;

(define build-time (time 1))

(define (bound min val max)
	(cond
		((< val min) min)
		((> val max) max)
		(else        val)))

(define (five args)
	(set-signal-action 'halt)
	(or
		(process-arguments (cdr args) command-line-rules usage-text
			(λ (dict others)
				(cond
					((get dict 'help False)
						(print usage-text)
						(print-rules command-line-rules)
						0)
					((get dict 'about False)
						(print about-five)
						0)
					((get dict 'version False)
						(show "five v0." build-time)
						0)
					((not (null? others))
						(show "funny arguments: " others)
						1)
					(else
						(eternal-battle 
							(make-computer-player 
								(bound 1 (get dict 'difficulty "bug") 7))
							human-player)))))
		1))


(dump five "five.c")


