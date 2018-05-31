;; read first line, write it back, close port
(define (tcp-echo clis)
   (call/cc (lambda (exit)
     (lfold
        (λ (nth client)
           (print "server serving client " nth)
           (lets
              ((lines (lines (cdr client)))
               (first lines (uncons lines #false)))
              (print-to (cdr client) first)
              (close-port (cdr client))
              (exit (+ nth 1))))
        1 clis))))

(define (start-tcp-echo-server name)
   (let loop ((port 1025))
      (if (< port #x10000)
         (let ((clis (tcp-clients port)))
            (if clis
               (begin
                  (thread name (tcp-echo clis))
                  port)
               (loop (+ port 1))))
         #false)))

(define port (start-tcp-echo-server 'echo))

(if port
  (print "server running"))

(define cli (open-connection (vector 127 0 0 1) port))

(if (not cli)
  (print "client not connected"))

(print-to cli "SLARTIBARTFAST")

(display (list->string (force-ll (port->byte-stream cli))))

