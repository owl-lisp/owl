;;;
;;; IO testing and server prototyping - not part of owl build yet
;;;

(define-library (owl server)

   (export 
      make-http-handler
      start-server)

   (import
      (owl io)
      (owl ff)
      (owl syscall)
      (owl defmac)
      (owl base)
      (owl parse)
      (owl env))

   (begin
      
      (define max-request-time (* 10 1000))
      (define max-request-size (* 1024 1024))
      (define max-request-block-size 32768)

      (define (block->list block tail)
         (let ((end (sizeb block)))
            (let loop ((pos (- end 1)) (out tail))
               (if (eq? pos -1)
                  out
                  (loop (- pos 1)
                     (cons (refb block pos) out))))))

      (define (get-block-timeout fd maxlen end)
         (let ((block (sys-prim 5 fd maxlen 0)))
            (if (eq? block #true) ;; would block
               (begin
                  (print "Waiting for input from " fd)
                  (if (eq? 'timeout (interact 'iomux (tuple 'read-timeout fd (- end (time-ms)))))
                     (begin
                        (print-to stderr "Timeout on fd " fd)
                        #false)
                     (get-block-timeout fd maxlen end)))
               block)))

      (define (stream-fd-timeout fd timeout)
         (let ((end (+ (time-ms) timeout)))
            (let loop ()
               (lambda ()
                  (let ((block (get-block-timeout fd max-request-block-size end)))
                     (cond
                        ((eof? block)
                           null)
                        ((not block)
                           (print-to stderr "Read error on fd " fd)
                           null)
                        (else
                           (block->list block loop))))))))

      (define (grab-line ll)
         (let loop ((ll ll) (out null))
            (cond
               ((null? ll)
                  (values #false (reverse out)))
               ((pair? ll)
                  (lets ((a ll ll))
                     (cond
                        ((eq? a #\newline)
                           (values (reverse out) ll))
                        ((eq? a #\return)
                           (lets ((b ll (uncons ll #false)))
                              (if (eq? b #\newline)
                                 (values (reverse out) ll)
                                 ;; stray carriage returns are forbidden
                                 #false)))
                        (else
                           (loop ll (cons a out))))))
               (else
                  (loop (ll) out)))))


      ;;;
      ;;; HTTP parsing
      ;;;

      (define get-nonspace
         (get-byte-if (λ (x) (not (eq? x #\space)))))

      (define parse-get
         (let-parses
            ((method (get-word "GET " 'get))
             (query (get-greedy+ get-nonspace))
             (skip (get-word " HTTP/1." 'foo))
             (ver-char (get-either (get-imm #\0) (get-imm #\1))))
            (tuple 'get query 
               (- ver-char #\0))))

      (define parse-query parse-get)
         
      (define (try-parse-query bs)
         (try-parse parse-query bs #f #f 
            (tuple 'error bs)))
    
      (define (cut-at x lst)
         (let loop ((lst lst) (out null))
            (if (null? lst)
               (values (reverse out) null)
               (lets ((hd lst lst))
                  (if (eq? hd x)
                     (values (reverse out) lst)
                     (loop lst (cons hd out)))))))
                  
      (define (split-params query)
         (lets ((q ps (cut-at #\? query)))
            (values q ps)))

      (define (http-respond env)
         (lets 
            ((fd (getf env 'fd))
             (status (getf env 'status))
             (version (getf env 'http-version))
             (content-type (get env 'content-type "text/html")))
            (if (eq? 200 (getf 'status env))
               (begin
                  (print-to fd "HTTP/1." version " 200 OK")
                  (print-to fd "Content-type:" content-type)
                  (print-to fd "")
                  (print-to fd (get env 'content "(no content specified)"))
                  (close-port fd))
               (begin
                  (print-to fd "HTTP/1." version " " status " OK")
                  (print-to fd "Content-type:" content-type)
                  (print-to fd "")
                  (print-to fd (get env 'content "(no content specified)"))
                  (close-port fd)))))

      (define (make-http-handler router)
         (λ (ll env)
            ;(print "-----------------------------------------------")
            ;(print "http-handler: ll " ll ", env " env)
            (lets ((line ll (grab-line ll)))
               (if line
                  (begin
                     (tuple-case (try-parse-query line)
                        ((get query version)
                           (lets ((query params (split-params query)))
                              (if query
                                 (http-respond
                                    (router
                                       (-> env
                                          (put 'query (list->string query))
                                          (put 'get-params params)
                                          (put 'http-version version)
                                          (put 'http-method 'get))))
                                 (print-to (getf env 'fd)
                                    "HTTP/1.0 500 Bad query\r\n\r\n"))))
                        (else
                           (print-to (getf env 'fd)
                              "HTTP/1.0 200 OK\r\n\r\nHelo wat " (list->string line)))))
                  (print-to (getf env 'fd) "HTTP/1.0 200 OK\r\nContent-type: text/html\r\n\r\nWAT " line))
               (close-port (getf env 'fd)))))

      ;; a thread which reads connections from socket and sends them to recipient
      (define (socket-accepter sock recipient)
         (let ((cli (sys-prim 4 sock #f #f)))
            (if cli
               (begin 
                  ;(print "Socket acceptor got " cli)
                  (mail recipient cli))
               (interact 'iomux (tuple 'read sock)))
            (socket-accepter sock recipient)))
      
      (define (server-loop handler clis)
         ;(print "Server loop waiting for mail and processing " (length (ff->list clis)) " connections")
         (lets ((envelope (wait-mail))
                (from msg envelope))
            (cond
               ((pair? msg) 
                  (lets 
                     ((ip rfd msg)
                      (fd (fd->port rfd))
                      (id ip))
                     (fork-linked-server id
                        (λ () 
                           (handler 
                              (stream-fd-timeout fd max-request-time)
                              (-> #empty
                                 (put 'ip ip)
                                 (put 'fd fd)))))
                     (server-loop handler (put clis id (time-ms)))))
               ((eq? msg 'stop)
                  (print-to stderr "stopping server"))
               ((tuple? msg) 
                  (tuple-case msg
                     ((finished a b c)
                        (let ((elapsed (- (time-ms) (get clis from 0))))
                           (server-loop handler (del clis from))))
                     ((crashed op a b)
                        (let ((cli (getf clis from)))
                           (print-to stderr "Thread crash while processing connection " from)
                           (print-to stderr (verbose-vm-error *toplevel* op a b))
                           (if from
                              (begin
                                 (print-to (ref from 3) "HTTP/1.0 500 ERROR CODE " op "\r\n\r\nERROR " op)
                                 (close-port (ref from 3))))
                           (server-loop handler (del clis from))))
                     (else
                        (print-to stderr "weird message: " envelope)
                        (server-loop handler clis))))
               (else
                  (print-to stderr "unknown message: " msg)
                  (server-loop handler clis)))))

      ;; a threat which receives clients and server commands and acts accordingly
      (define (start-server server-id handler port)
         (let ((sock (open-socket port)))
            (print "Server socket " sock)
            (if sock
               (fork-linked-server server-id
                  (λ ()
                     ;; start reading connections
                     (print "Starting reader")
                     (let ((reader (fork (λ () (socket-accepter sock server-id)))))
                        (print "Reader is " reader)
                        (server-loop handler #empty)
                        (kill reader)
                        (close-port sock))))
               (begin
                  (print-to stderr server-id "failed to get socket")
                  #false))))
))

 (import (owl server))

 (print "server: " 
   (start-server 'serveri 
      (make-http-handler 
         (λ (env) 
            ;(print "router got " env) 
            (-> env
               (put 'status 200)
               (put 'content (list "hello " env)))))
      31337))


