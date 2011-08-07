
(define (test args)
   (let loop ((n 0) (last null))
      (if (= n 10)
         last
         (loop (+ n 1)
            (list (+ (if (eq? 2 (* 3 (/ (expt 2/3 3000) (expt 2/3 2999)))) 40 0) 2))))))

