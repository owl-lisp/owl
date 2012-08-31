;; send and receive mails asynchronously between threads and check that the order is correct

; n mails to send between each thread pair (to both directions)
(define mails 300)

; how many threads to start (even)
(define n-threads 30)

(define (mailer rst peer)
   (ref (wait-mail) 2) ;; startup message
   (let loop ((rst rst) (out 0) (in 0)) ;; outgoing mail and incoming expected
      (if (and (= out mails) (= in mails))
         (print "ok " mails)
         (lets ((rst n (rand rst 3)))
            (cond
               ((eq? n 0) ;; maybe send out
                  (if (= out mails)
                     (loop rst out in)
                     (begin
                        (mail peer out)
                        (loop rst (+ out 1) in))))
               ((eq? n 1) ;; maybe read in
                  (let ((env (check-mail)))
                     (if env
                        (if (= in (ref env 2))
                           (loop rst out (+ in 1))
                           (error "funny order: " (list 'got env 'expecting in)))
                        (loop rst out in))))
               ((eq? n 2) ;; maybe skip rounds
                  (lets ((rst n (rand rst 3)))
                     (loop rst out in)))
               (else
                  (error "funny case " n)))))))

(fold
   (λ (rst id)
      (lets
         ((rst seed-1 (rand rst #xfffffffffffffff))
          (rst seed-2 (rand rst #xfffffffffffffff)))
         (fork-server id (λ () (mailer (seed->rands seed-1) (+ id 1))))
         (fork-server (+ id 1) (λ () (mailer (seed->rands seed-2) id)))
         rst))
   (seed->rands (* (time-ms) (<< (time-ms) 9)))
   (iota 0 2 n-threads))

(for-each (λ (id) (mail id 'start)) (iota 0 1 n-threads))

