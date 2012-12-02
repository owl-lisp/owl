
(define range (iota 0 1 10))

(define square (list->ff (map (λ (x) (cons x (* x x))) range)))

(print 
   (square -1 
      (map square range))) ;; <- default

