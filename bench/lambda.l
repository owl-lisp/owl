
; tags: macro

(define-syntax lif
	(syntax-rules ()
		((lif test a b)
			(test (lambda () a) (lambda () b)))))

(define-syntax lcond
	(syntax-rules (else)
		((lcond (else exp))
			exp)
		((lcond (test then) clause ...)
			(lif test then (lcond clause ...)))))

(define (true a b) (a))	
(define (false a b) (b))
(define (lcons a b) (lambda (c) (c false a b)))
(define (lnull? a) (a (lambda (a b c) a)))
(define (lnull a) (a true true true))
(define (lcar a) (a (lambda (a b c) b)))
(define (lcdr a) (a (lambda (a b c) c)))

(define-syntax llist
	(syntax-rules ()
		((llist)
			lnull)
		((llist a b ...)
			(lcons a (llist b ...)))))

(define zero lnull)
(define one (lcons true lnull))

(define (same? a b)
	(lif a 
		(lif b true false)
		(lif b false true)))

(define (y f)
	((lambda (x) (x x))
		(lambda (x)
			(f (lambda (y) ((x x) y))))))	;;; <- the wrapper prevents premature infinite evaluation of (x x) 

(define (equals? a b)
	(lcond
		((lnull? a) (lnull? b))
		((same? (lcar a) (lcar b))
			(equals? (lcdr a) (lcdr b)))
		(else false)))

(define-syntax define-rec
	(syntax-rules ()
		((define-rec (name formal) body)
			(define name
				(y (lambda (name) (lambda (formal) body)))))
		((define-rec (name formal ...) body)
			(define name
				(let ((myy (lambda (f) ((lambda (x) (x x)) (lambda (x) (f (lambda (formal ...) ((x x) formal ...))))))))
					(myy (lambda (name) (lambda (formal ...) body))))))))
	
(define-rec (succ a)
	(lcond
		((lnull? a) one)
		((lcar a)
			(lcons false (succ (lcdr a))))
		(else
			(lcons true (lcdr a)))))

(define-rec (pred a)
	(lif (lcar a)
		(lif (lnull? (lcdr a))
			lnull
			(lcons false (lcdr a)))
		(lcons true (pred (lcdr a)))))

(define-rec (add a b)
	(lif (lnull? b)
		a
		(add (succ a) (pred b))))

(define (translate x)
	(x (lambda () 'true) (lambda () 'false)))

; (see (equals? (add one big) zero))

(define x true)
(define o false)

(define a (llist x x x x x x x x x x x x x))
(define b (llist o o o o o o o o o o o o o x))

(define (work)
	(equals? (add a b) (add b a)))

(define (run n)
	(cond	
		((= n 0)
         (list (+ n 42)))
		((eq? (translate (work)) 'true)
			(run (- n 1)))
		(else
			'fail)))

(define (test args)
   (run 20))

test

