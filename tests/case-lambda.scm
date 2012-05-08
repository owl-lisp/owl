
;; case-lambda is being added
;; here are things that happen to work at the moment

(define foo (case-lambda (x x)))

(print (foo 1 2 3))

(define foo (case-lambda ((a) a)))

(print (foo "trololo"))

(define foo (case-lambda ((a b c . d) (list a b c d))))

(print (foo 11 22 33 ))
(print (foo 11 22 33 44))
(print (foo 11 22 33 44 55))

