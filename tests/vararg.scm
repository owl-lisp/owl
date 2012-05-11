
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

;; direct variable arity calls coming from receive
; minimal test for issue FIXME'd in owl/cps.scm 
; (receive (values 42) (lambda x x))   ; (1)
; (print (receive (values 1) (lambda (x) x))) ; 1
; (print (receive (values 1) (lambda x x)))   ; (1)
; (print (receive (values 1 2) (lambda (x y) (list x y))))       ; (1 2)
; (print (receive (values 1 2) (lambda (x . y) (list x y))))     ; (1 (2))
; (print (receive (values 1 2) (lambda (x y . z) (list x y z)))) ; (1 2 ())
; (print (receive (values 1 2) (lambda x x))) ; (1 2)

;; variable arity vs recursion

; (define (foo a . b) (if (eq? a 0) b ...))



