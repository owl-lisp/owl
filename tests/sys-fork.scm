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
   (let loop ()
      (let ((data (vector->list (get-block infd #xffff))))
         (write-bytes outfd (cons 42 data))
         (loop))))

(define (main in out)
   (let ((data (string->bytes (date-str (time)))))
      (write-bytes out data)
      (if (equal? (list->vector (cons 42 data))
                  (get-block in #xffff))
         (print "Subprocess echo with star ok")
         (print "Echo failed"))))
   
(cond
   ((eq? sub #true)
      (child (car p2) (cdr p1)))
   ((number? sub)
      (print "Subprocess forked")
      (main  (car p1) (cdr p2)))
   (else 
      (print "Fork failed")))
   

