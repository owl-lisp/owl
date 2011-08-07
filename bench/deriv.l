;; This is deriv.scm from Larceny benchmarks, with minor changes to make it work in this benchmark

(define (deriv a)
  (cond ((not (pair? a))
         (if (eq? a 'x) 1 0))
        ((eq? (car a) '+)
         (cons '+
               (map deriv (cdr a))))
        ((eq? (car a) '-)
         (cons '-
               (map deriv (cdr a))))
        ((eq? (car a) '*)
         (list '*
                a
                (cons '+
                      (map (lambda (a) (list '/ (deriv a) a)) (cdr a)))))
        ((eq? (car a) '/)
         (list '-
               (list '/
                     (deriv (cadr a))
                     (caddr a))
               (list '/
                     (cadr a)
                     (list '*
                           (caddr a)
                           (caddr a)
                           (deriv (caddr a))))))
        (else
         ;(fatal-error "No derivation method available") no fatal-error in all systems here
         0)))

(define output 
	'(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
       (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
       (* (* b x) (+ (/ 0 b) (/ 1 x)))
       0))

(define input '(+ (* 3 x x) (* a x x) (* b x) 5))

(define (test args)
	(let loop ((n 100000) (last null))
		(if (eq? n 0)
			(if (equal? last output)
				(print (cons 42 null))
				(print last))
			(loop (- n 1) (deriv input)))))

test
