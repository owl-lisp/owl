
(define (library-test state)
   (λ (bvec)
      (values 
         state
         (library-test 
            (vec-fold + state bvec)))))

(library-test 0) 

