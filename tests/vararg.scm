
(define foo (lambda x x))

(print (foo))
(print (foo 1))
(print (foo 1 2))

(define foo (lambda (a . b) (list a b)))

(print (foo 1))
(print (foo 1 2))
(print (foo 1 2 3))
