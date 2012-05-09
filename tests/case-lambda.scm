;; case-lambda is being added
;; here are things that happen to work at the moment

;; downgrades to regular lambdas 

(define foo (case-lambda (x x)))
(print (foo 1 2 3))

(define foo (case-lambda ((a) a)))
(print (foo "trololo"))

(define foo (case-lambda ((a b c . d) (list a b c d))))
(print (foo 11 22 33 ))
(print (foo 11 22 33 44))
(print (foo 11 22 33 44 55))

;; dispatch, fixed, simple

(define foo
   (case-lambda
      (() 0)
      ((a) 1)
      ((a b) 2)
      ((a b c) 3)))

(print (foo))
(print (foo 11))
(print (foo 11 22))
(print (foo 11 22 33))

(define foo
   (case-lambda
      ((a) 1)
      ((a b) 2)
      (() 0)
      ((a b c) 3)))

(print (foo))
(print (foo 11))
(print (foo 11 22))
(print (foo 11 22 33))

;; dispatch w/ variable arity

(define foo
   (case-lambda
      ((a) (list a))
      ((a) 111)    ;; not reachable
      ((a b . c) (list a b c))
      ((a b) 222)  ;; not reachable
      (x x)))      ;; not reachable

(print (foo 1))
(print (foo 1 2))
(print (foo 1 2 3))
(print (foo 1 2 3 4))
(print (foo))

;; dispatch, variable arity, literal values, check that their indeces are ok

(define foo
   (case-lambda
      (() 'zero)
      ((a) (list 'o 'n 'e))
      ((a b) 'two)    ;; not reachable
      ((a b c) (cons 'th 'ree))
      (x 'any)))

(print (foo)) ;; any
(print (foo 1))
(print (foo 1 2))
(print (foo 1 2 3))
(print (foo 1 2 3 4))

;; TODO ---------------------------------------------------------------------

;; Operator position, should do compile time dispatch
;
; (print ((case-lambda ((a) a) (xs xs)) 1 2 3))

;; Recursion
;
; (define slartibartfast
;    (case-lambda
;      ((a) a)
;      ((a b) (slartibartfast b))
;      ((a b . c) (slartibartfast b c))))
;
; (print (slartibartfast 'x 'y 11 22 33 44))
