;; a small ff benchmark

(define *bits* #xffff)

(define (step n p)
   (band *bits* (+ n p)))

; n = 0 < n <= 0xfffff
(define (work ff n p x)
   ;(print "size " (ff-fold (λ (n a b) (+ n 1)) 0 ff))
   (cond
      ((eq? p 0)
         ;(print (ff-fold (λ (n a b) (+ n 1)) 0 ff))
         (print "OK"))
      ((eq? (band p 1) 1) ;; read 50%
         ;(print "read " (if (eq? 'foo (get ff n 'foo)) "miss" "hit"))
         (work ff 
            (step n p)
            (- p 1)
            (get ff n n)))
      ((eq? (band n #b1) #b0)
         ;(print "write " (if (eq? 'foo (get ff n 'foo)) "insert" "rewrite"))
         (work 
            (put ff n n)
            (step n p)
            (- p 1)
            x)
         )
      (else
         (let ((k (band *bits* p)))
            ;(print "del " (if (eq? 'foo (get ff k 'foo)) "miss" "hit"))
            (work 
               (del ff k)
               (step n p)
               (- p 1)
               x))
            )))

; (work #false 11111 2000000 42) & 0xffff → 
;   245882 del hit
;   254118 del miss
;   303443 read hit
;   696557 read miss
;   266384 write insert
;   233616 write rewrite
;   final ff size 20502 of 65535

(λ (args)
   (work #false 11111 2000000 42))
