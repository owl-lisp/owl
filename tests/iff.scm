;; integer finite functions test

(import lib-iff)

(define max-bits 1024)
(define max-nums 100)

(define rst 
   (seed->rands (time-ms)))

(define (test rst)
   (lets
      ((rst shift (rnd rst max-bits))
       (rst nnums (rnd rst max-nums))
       (nnums (max 1 nnums))
       (rst nums (random-numbers rst (<< 1 shift) (max 1 nnums)))
       ;(_ (print nums))
       (pairs (zip cons nums (iota 0 1 nnums)))
       (iff (fold (λ (iff pair) (iput iff (car pair) (cons (cdr pair) (iget iff (car pair) null)))) False pairs))
       (ok
         (fold
            (λ (iff pair)
               (lets 
                  ((key val pair)
                   (stored (iget iff key null)))
                  (if (has? stored val) 
                     iff
                     (error "Not there: " val))))
            iff pairs)))
      (if ok rst 'bug)))

(let loop ((rst rst) (n 0))
   (print n)
   (if (< n 20)
      (loop (test rst) (+ n 1))))

