;;; tcp - read a short request from the client

; (socket . port)
(define sp
   (let loop ((port 1025))
      (if (< port #x10000)
         (let ((sock (open-socket port)))
            (if sock 
               (cons sock port)
               (loop (+ port 1))))
         (error "Couldn't open any ports" port))))

(define sock (car sp))
(define port (cdr sp))

(print "Opened socket")

(define message (render "hello there" null))

(fork-server 'socket-thread
   (lambda ()
      (let ((cli (interact sock 'accept))) ; <- blocks the thread
         (print "Client connected")
         (show "Client says: " (list->string (vector->list (interact cli 'input))))
         (close-port cli))))

(let ((conn (open-connection (vector 127 0 0 1) port)))
   (if conn
      (begin
         (mail conn message)
         (close-port conn))
      (print "Could not connect to socket")))
