(print "ohai, no apply yet.")

;
;(print (apply (lambda (a b c) (list a b c)) '(1 2 3)))
;(print (apply (lambda (a b c) (list a b c)) 1 '(2 3)))
;(print (apply (lambda (a b c) (list a b c)) 1 2 '(3)))
;(print (apply (lambda (a b c) (list a b c)) 1 2 3 '()))
;
;(print (apply (lambda (a b . c) (list a b c)) '(1 2 3 4)))
;(print (apply (lambda (a b . c) (list a b c)) 1 '(2 3 4)))
;(print (apply (lambda (a b . c) (list a b c)) 1 2 '(3 4)))
;(print (apply (lambda (a b . c) (list a b c)) 1 2 3 '(4)))
;(print (apply (lambda (a b . c) (list a b c)) 1 2 3 4 '()))
;
;; (print (apply (lambda x x) 42)) ;; <- don't know yet if this is allowed
