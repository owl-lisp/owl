;;;
;;; (stupidly) compute all winning configurations of size up to n in 1d gomoku
;;;

; tags: 

(define e #f)
(define x "x")
(define o "o")
(define player-x "+")

(define (see lst) 
	(map 
		(lambda (x) (if x x "_"))
		lst))

(define null '())

(define (some pred lst)
	(cond
		((null? lst) #f)
		((pred (car lst)) =>
			(lambda (x) x))
		(else
			(some pred (cdr lst)))))

(define (take-winner lst)
	(let loop ((lst lst) (which #f) (n 0))
		(cond
			((eq? n 5)
				(cond
					; 5 at end
					((null? lst) which)
					; over 5, no win
					((eq? (car lst) which)
						(loop (cdr lst) which (+ n 1)))
					; exactly 5
					(else which)))
			((null? lst)
				#f)
			((eq? (car lst) which)
				(loop (cdr lst) which (+ n 1)))
			((car lst)
				(loop (cdr lst) (car lst) 1))
			(else
				(loop (cdr lst) #f 1)))))

(define (plot lst pos val)
	(cond
		((eq? pos 0)
			(cons val (cdr lst)))
		(else 
			(cons (car lst)
				(plot (cdr lst) (- pos 1) val)))))

(define (empty-places lst)
	(let loop ((lst lst) (n 0))
		(cond
			((null? lst)
				null)
			((car lst)
				(loop (cdr lst) (+ n 1)))
			(else
				(cons n
					(loop (cdr lst) (+ n 1)))))))


(define (report-winnable lst player move)
	(show "Winnable: " 
		(list 'board (see lst) 'player player 'to move)))
		

; play all possible games from the state onwards 

(define (opponent player)
	(if (eq? player x) o x))

(define (check-options lst player)
	(cond
		((take-winner lst) =>
			(lambda (who) (cons who 'no-board)))
		(else
			(let loop ((options (empty-places lst)) (tied? #f) (lost? False))
				(if (null? options)
					(cond
						(tied? #f)
						(lost? (cons (opponent player) 'no-board-2))
						(else #f))
					(let ((winner (check-options (plot lst (car options) player) (opponent player))))
						(if winner
							(if (eq? (car winner) player)
								(cons (car winner)
									(plot lst (car options) player-x))
								(loop (cdr options) tied? #t))
							(loop (cdr options) #t lost?))))))))

(define (match-prefix pat lst)
	(cond
		((null? pat) #t)
		((null? lst) #f)
		((eq? (car pat) (car lst))
			(match-prefix (cdr pat) (cdr lst)))
		(else #f)))

(define (match-any-position pat lst)
	(cond
		((null? lst) #f)
		((match-prefix pat lst) #t)
		(else
			(match-any-position pat (cdr lst)))))

(define (known-winnable? wins lst)
	(some
		(lambda (old)
			(match-any-position old lst))
		wins))

(define (inc board)
	(cond
		((null? board) 
			(list e))
		((eq? (car board) x)
			(cons e (inc (cdr board))))
		(else
			(cons x (cdr board)))))

(define (try board wins)
	(if (known-winnable? wins board)
		(try (inc board) wins)
		(let ((winner (check-options board x)))
			(if (and winner (eq? (car winner) x))
				(begin
					(display (see (cdr winner)))
					(display "
")
					(if (= (length board) 8)
						'done
						(try (inc board)
							(cons board wins))))
				(try (inc board) wins)))))

(define (test args)
	(try null 
		(list 
			(list x x x x x)))
   (list (+ 40 2)))

test



