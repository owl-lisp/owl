(import
   (owl sys)
   (owl date))

;; create a communication channel

(define p1 (pipe)) ;; parent read, child write
(define p2 (pipe)) ;; child read, parent write

(if (and p1 p2)
   (print "Pipes check"))

(define sub (fork))

(define (child infd outfd)
   (write-bytes outfd
      (cons 42
         (vector->list (get-block infd #xffff)))))

(define (main sub in out)
   (let ((data (string->bytes (date-str (time)))))
      (write-bytes out data)
      (if (equal? (list->vector (cons 42 data))
                  (get-block in #xffff))
         (begin
            (print "Subprocess echo with star ok")
            (print "Closing parent end of port " (close-port out))
            (print "Waiting child: " (waitpid sub)))

         (print "Echo failed"))))

(cond
   ((eq? sub #true)
      (child (car p2) (cdr p1)))
   ((integer? sub)
      (print "Subprocess forked")
      (main sub (car p1) (cdr p2)))
   (else
      (print "Fork failed")))
