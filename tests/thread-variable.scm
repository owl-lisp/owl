(import
  (owl variable))

(define foo (make-variable))

(foo 11)
(print (foo) " = " 11)

(begin
   (foo 'set-local 22)
   (print (foo 'get-local 'missing) " = " 22) ;; this runs in the same thread
   (print (foo) " = " 11)) ;; global is the same

(print (foo) " = 11") ;; global is the same here also

(print (foo 'get-local 404) " = " 404) ;; this runs in another thread, previous bindings are no longer there
