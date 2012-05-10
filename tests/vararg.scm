
(define foo (lambda x x))

(print (foo))
(print (foo 1))
(print (foo 1 2))

(define foo (lambda (a . b) (list a b)))

(print (foo 1))
(print (foo 1 2))
(print (foo 1 2 3))

;; operator position, compile time transformation
(print ((lambda (a . b) (list a b)) 11))
(print ((lambda (a . b) (list a b)) 11 22 33))

;; variable arity continuations
(define (foo) (values 11 22))
(define (bar) (values 11 22 33 44))

(print (receive (foo) (lambda x x)))
(print (receive (bar) (lambda (a b . c) (list a b c))))

;; \o\ (--------------- TODO ---------------------) /o/
;; direct variable arity calls




