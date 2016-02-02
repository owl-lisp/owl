
(define steps 30)
(define rounds 30)

(define (try rs)
  (lets 
    ((rs a (rand rs 20))
     (ff (put #empty a a)))
    (let loop ((rs rs) (n steps) (ff ff) (lo a) (hi a))
      (cond
        ((eq? n 0)
          rs)
        ((not (eq? lo (ff-min ff 0)))
          (print "fail 1: " ff ", got min " (ff-min ff 0) " instead of " lo)
          #false)
        ((not (eq? hi (ff-max ff 0)))
          (print "fail 2: " ff ", got hi " (ff-max ff 0) " instead of " hi)
          #false)
        (else
          (lets ((rs a (rand rs 20)))
            (loop rs (- n 1)
              (put ff a a)
              (min a lo)
              (max a hi))))))))

(call/cc
  (lambda (ret)
    (fold 
      (λ (rs x) 
        (let ((rs (try rs)))
          (or rs
            (ret #false))))
      (seed->rands (time-ms))
      (iota 0 1 rounds))
    (print "All OK")))
