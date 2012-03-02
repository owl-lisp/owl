;;; tcp - read a short request
;; note: output order may differ, so this sould really print less, sort or something

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

(define message (render "hello there" null))

(fork-server 'socket-thread
   (lambda ()
      (let ((cli (interact sock 'accept)))
         (mail cli message)
         (close-port cli))))

(let ((conn (open-connection (vector 127 0 0 1) port)))
   (if conn
      (begin
         (show "Server says: " (list->string (vector->list (interact conn 'input))))
         (close-port conn))
      (print "Unable to connect to the local server")))
