
(define end 100000)

(define (work p)
   (if (= p end)
      (print "OK")
      (work 
         (+ 
            (- (/ (* p 3) (* p 2)) 1/2)
            (/ (* p p) p)))))

(lambda (args)
   (work 1))

