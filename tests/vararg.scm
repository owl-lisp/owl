
(define foo (lambda x x))

(print (foo))
(print (foo 1))
(print (foo 1 2))

(define foo (lambda (a . b) (list a b)))

(print (foo 1))
(print (foo 1 2))
(print (foo 1 2 3))

;; TODO --------------------------------------------------------- 

;; operator position 

;; todo: convert at compile time to a regular operator lambda?
; ((lambda (a . b) (list a b)) 11 22 33)

;; continuations

; this will work when the above case is handled
; (receive (values 1 2) (lambda (a . b) (list a b)))
