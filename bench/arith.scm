
(define end 300000)

(define (work p)
   (if (= p end)
      (print "OK")
      (work 
         (+ 
            (- (/ (* p 3) (* p 2)) 1/2)
            (/ (* p p) p)))))

(λ (args)
   (work 1))

