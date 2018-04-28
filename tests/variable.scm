(import 
  (owl variable))

(define foo (make-variable))

(foo 42)
(print (foo))
(foo "new value")
(print (foo))

;; many async writes
(for-each foo (iota 0 1 100))

;; sync read -> last
(print (foo))

(foo 0)
(print (fold + 0 (iota 0 1 1000)))
(for-each 
  (lambda (x) (foo 'call (lambda (y) (+ x y))))
  (iota 0 1 1000))
(print (foo))
