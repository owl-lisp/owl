;;; tcp - read a short request

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

(define message (render render "hello there" null))

(fork-server 'socket-thread
   (lambda ()
      (print "Server thread waiting for connection")
      (let ((cli (interact sock 'accept)))
         (print "Server thread got connection")
         (mail cli message)
         (close-port cli))))

(print "Connecting to local server")
(let ((conn (open-connection (vector 127 0 0 1) port)))
   (if conn
      (begin
         (print "Made connection")
         (show "Server says: " (list->string (vector->list (interact conn 'input))))
         (close-port conn))
      (print "Unable to connect to the local server")))
