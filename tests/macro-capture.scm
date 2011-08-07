;; in Scheme this gives 43, but this may change for owl later

(define foo 42)

(define-syntax bar
   (syntax-rules ()
      ((bar anything) foo)))

(define foo 43)

(print (bar 1))
