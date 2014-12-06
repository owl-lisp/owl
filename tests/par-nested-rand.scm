
(define (small-range? lo hi)
   (< (- hi lo) 100))

(define (seek rs lo hi n)
   (if (small-range? lo hi)
      (if (has? (iota lo 1 hi) n)
         (begin
            (print "found it!")
            #true)
         #false)
      (lets 
         ((rs mid (rand-range rs lo hi))
          (rs a (rand rs 3)) ;; delays for thread stepping randomization
          (rs b (rand rs 3))
          (rs seed (rand rs #xfffffffffff)) ;; new seed for new random state in the other branch
          (rsp (seed->rands seed)))
         (por
            (begin
               (set-ticker a)
               (seek rs lo mid n))
            (begin
               (set-ticker b)
               (seek rsp mid hi n))))))

(define seed (time-ms))

(define rs (seed->rands seed))

(define lo 0)

(define hi 10000)

(define-values (rs needle) (rand-range rs lo hi))

(print "looking...")

(seek rs lo hi needle)

