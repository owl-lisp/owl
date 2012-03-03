;;; tcp - read a short request

; find a happy port
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

(define a (list 1 2 3 4 5))
(define b (vector 11 22 10 33 44))
(define c (list 100 101 10 102 103))
(define d (vector 20 30 40 50))

(define (push x tl)
   (append (if (vector? x) (vec->list x) x) tl))

;; separate writes
(define elems (list a b c d))

;; all that should be received
(define all-data (foldr push null elems))

;; read until all-data comes out 
(define (reader fd)
   (let loop ((read null))
      (if (equal? read all-data)
         (print "That's all folks.")
         (loop (append read (vec->list (interact fd 'input)))))))

;; send chunks and drop read requests
(define (sender fd)
   (for-each (λ (x) (mail fd x)) elems))
   
(fork-server 'socket-thread
   (λ ()
      (let ((cli (interact sock 'accept)))
         (sender cli)
         (mail cli 'flush)
         (reader cli)
         (close-port cli))))

(let ((conn (open-connection (vector 127 0 0 1) port)))
   (if conn
      (begin
         (sender conn)
         (mail conn 'flush)
         (reader conn)
         (close-port conn))
      (print "Unable to connect to the local server")))
