
;;; a simple mail test: send 100 messages and have them echoed back from another thread

(define echo "echo server")

;; funktion to be forked to run as the echo thread
(define (echoer)
   (lets
      ((envelope (wait-mail))
       (from msg envelope)) ; 
      (mail from msg)
      (echoer)))

(fork-server echo echoer)

(begin
   ;; send 0-99 mails and leave the responses to inbox
   (let loop ((n 0))
      (if (= n 100)
         'done
         (begin
            (mail echo n)
            (loop (+ n 1)))))
   ;; read mails and expect to get them in the same order
   (let loop ((n 0))
      (if (= n 100)
         (if (check-mail) ;; if mail still in inbox, bug
            (print "too much")
            (print "ok"))
         (lets 
            ((mail (wait-mail)) ; <- wait blockingly
             (from message mail))
            (if (eq? message n)
               (loop (+ n 1))
               (print (list 'got mail 'expecting n)))))))
      

