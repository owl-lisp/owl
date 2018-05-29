;; macros must preserve bindings

(define foo 42)

(define-syntax bar
   (syntax-rules ()
      ((bar anything) foo)))

(define foo 43)

(print (bar 1))
