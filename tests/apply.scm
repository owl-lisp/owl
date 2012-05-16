(print (apply (lambda (a b c) (list a b c)) '(1 2 3)))
(print (apply (lambda (a b c) (list a b c)) 1 '(2 3)))
(print (apply (lambda (a b c) (list a b c)) 1 2 '(3)))
(print (apply (lambda (a b c) (list a b c)) 1 2 3 '()))

(print (apply (lambda (a b . c) (list a b c)) '(1 2 3 4)))
(print (apply (lambda (a b . c) (list a b c)) 1 '(2 3 4)))
(print (apply (lambda (a b . c) (list a b c)) 1 2 '(3 4)))
(print (apply (lambda (a b . c) (list a b c)) 1 2 3 '(4)))
(print (apply (lambda (a b . c) (list a b c)) 1 2 3 4 '()))

(print (receive (call/cc (λ (k) (apply k 1 null)))     (lambda x x)))
(print (receive (call/cc (λ (k) (apply k 1 2 null)))   (lambda x x)))
(print (receive (call/cc (λ (k) (apply k 1 2 '(3))))   (lambda x x)))
(print (receive (call/cc (λ (k) (apply k 1 2 '(3 4)))) (lambda x x)))

; (print (apply (lambda x x) 42)) ;; <- don't know yet if this is allowed
