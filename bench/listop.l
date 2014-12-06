
; tags: 

(define null '())
(define (add a b) (+ a b))
(define (sub a b) (- a b))
(define (kull? a) (eq? a null))

(define (kons a b) (cons a b))
(define (kar a) (car a))
(define (kdr a) (cdr a))

(define (kappend a b)
	(if (kull? a)
		b
		(kons (kar a)
			(kappend (kdr a) b))))

(define (kold op init lst)
	(if (kull? lst)
		init
		(op (kar lst)
			(kold op init (kdr lst)))))

(define (interval first last)
	(if (= first last)
		null
		(cons first
			(interval (add first 1) last))))

(define (kap fn lst)
	(if (null? lst)
		null
		(kons
			(fn (kar lst))
			(kap fn (kdr lst)))))

(define (kip op a b)
	(if (or (kull? a) (kull? b))
		null
		(kons (op (kar a) (kar b))
			(kip op (kdr a) (kdr b)))))

(define (kompose f g)
	(lambda (x)
		(f (g x))))

(define (double x)
	(add x x))

(define quad
	(kompose double double))

(define (test-run)
	(= 0 
		(kold add 0 
			(kappend
				(kap double
					(kip sub
						(interval 1 10000)
						(interval 3 10002)))
				(kap double
					(kip sub
						(interval 3 10002)
						(interval 1 10000)))))))

(define (iterate thunk times)
	(cond
      ((= times 0)
         (list (+ 40 2)))
      ((thunk)
			(iterate thunk (sub times 1)))
      (else
         #f)))

(define (test args)
	(iterate test-run 20))

test

