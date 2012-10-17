;;; medium mail test:
; - have threads with ids 0..n
; - each either reads message or sends at random a mail to 0..n with content 0..n
; - when some thread has gotten all numbers 0..n, it sends message to a 'fini thread
; - fini prints the content of the first message and mails 'halt to all 0..n
; - each thread starts dropping messages when 'fini has sent 'halt to them

(define seed (* (time-ms) (time-ms)))

(define n 30) ;; make 100 threads, ids 0-99

(define (wait-for msg)
   (let ((mail (wait-mail)))
      (if (eq? msg (ref mail 2))
         'done
         (wait-for msg))))

;; ff of all numbers
(define wanted 
   (fold (λ (ff n) (put ff n n)) empty (iota 0 1 n)))

(define (drop-mails)
   (wait-mail)
   (drop-mails))

(define (spammer rst)
   (wait-for 'start)
   (let loop ((wanted wanted) (rst rst))
      (lets ((rst bit (rand rst 2)))
         (cond
            ((and (eq? bit 0) (check-mail)) =>
               (λ (envelope)
                  (lets ((from msg envelope))
                     (if (eq? msg 'halt)
                        (drop-mails)
                        (loop (del wanted msg) rst)))))
            ((eq? wanted #empty) ;; received all already
               (mail 'fini "i has all")
               (drop-mails))
            (else
               (lets 
                  ((rst to  (rand rst n))
                   (rst num (rand rst n))
                   (rst rounds (rand rst 3)))
                  (mail to num)
                  (wait rounds)
                  (loop wanted rst)))))))

(fork-server 'fini
   (λ ()
      (print (ref (wait-mail) 2)) ; omit the id to make output equal in all cases
      (for-each (λ (id) (mail id 'halt)) 
      (drop-mails))))

(fold
   (λ (rst id)
      (lets ((rst n (rand rst #xffffffff)))
         (fork-server id
            (λ () (spammer (seed->rands n))))
         rst))
   (seed->rands seed)
   (iota 0 1 n))

;; start all threads
(for-each
   (λ (id) (mail id 'start))
   (iota 0 1 n))
