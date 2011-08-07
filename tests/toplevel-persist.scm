
(define foo "correct")

(define (bar x) foo)

(define foo "wrong")

(print (bar 42))

(define baz "correct")

(define quux baz)

(define baz "wrong")

(print quux)
