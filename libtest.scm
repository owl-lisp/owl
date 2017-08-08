
(define (library-test state)
   (λ (bvec)
      (values 
         state
         (library-test (+ state 1)))))

(library-test 0) 

