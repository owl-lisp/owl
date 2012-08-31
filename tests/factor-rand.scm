; 1 factors to (), and is thus ok in the test
(define n (max 2 (band (time-ms) #xfffffff)))

(define fs (factor n))

(print
   (if 
      (= n 
         (fold * 1 
            (map 
               (λ (p) (expt (car p) (cdr p)))
               fs)))
      #true
      (cons n fs)))
   
