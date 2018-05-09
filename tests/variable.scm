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
  (lambda (x) (foo 'call (C + x)))
  (iota 0 1 1000))
(print (foo))

(define foo  (make-variable 'foo))
(define foo2 (link-variable 'foo))

(foo 11)
(print (foo))
(print (foo2))
(foo2 22)
(print (foo))
(print (foo2))
