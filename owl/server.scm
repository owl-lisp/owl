;;;
;;; IO testing and server prototyping - not part of owl build yet
;;;

(define-library (owl server)

   (export 
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

      ;; a thread which reads connections from socket and sends them to recipient
      (define (socket-accepter sock recipient)
         (let ((cli (sys-prim 4 sock #f #f)))
            (if cli
               (begin 
                  ;(print "Socket acceptor got " cli)
                  (mail recipient cli))
               (interact 'iomux (tuple 'read sock)))
            (socket-accepter sock recipient)))

      ;;;
      ;;; Request processing passes
      ;;;
      
      (define (fail env code reason)
         (-> env 
            (put 'error code)
            (put 'status code)
            (put 'status-text reason)
            (put 'content reason)))

      (define (get-request env)
         (lets ((line ll (grab-line (getf env 'bs))))
            (if line
               (tuple-case (try-parse-query line)
                  ((get query version)
                     (lets ((query params (split-params query)))
                        (if query
                           (-> env
                              (fupd 'bs ll) ;; rest of data
                              (put 'query (list->string query))
                              (put 'get-params params)  ;; byte list
                              (put 'http-version version)
                              (put 'http-method 'get))
                           (fail env 500 "Bad query"))))
                  (else
                     (fail env 200 (string-append "Query wat? " (list->string line)))))
               (fail env 500 "No query received"))))

      ;; doing string->symbol on all would cause memory leak
      (define (known-header->symbol str)
         (cond
            ((string-eq? str "User-Agent") 'user-agent)
            ((string-eq? str "Accept-Language") 'accept-language)
            ((string-eq? str "Accept") 'accept) ;; text/html, text/plain, ...
            ((string-eq? str "Accept-Encoding") 'accept-encoding)
            (else #false)))

      (define (drop-space lst)
         (cond
            ((null? lst) lst)
            ((eq? (car lst) #\space)
               (drop-space (cdr lst)))
            (else
               lst)))

      (define (header-value x)
         (list->string (drop-space x)))

      (define (split-at x lst)
         (if (null? lst)
            lst
            (lets ((these lst (cut-at x lst)))
               (if (null? these)
                  (split-at x lst)
                  (cons these (split-at x lst))))))

      (define (hex-val a)
         (cond
            ((not a) #false)
            ((< 47 a 58) (- a 48))
            ((< 96 a 103) (- a 87))
            ((< 64 a 71) (- a 55))
            (else #false)))

      (define (url-decode lst)
         (let loop ((lst lst) (out null))
            (if (null? lst)
               (reverse out)
               (lets ((a lst lst))
                  (cond
                     ((eq? a #\+)
                        (loop lst (cons #\space out)))
                     ((eq? a #\%)
                        (lets
                           ((a lst (uncons lst #false))
                            (b lst (uncons lst #false))
                            (a (hex-val a))
                            (b (hex-val b)))
                        (if (and a b)
                           (loop lst (cons (bor (<< a 4) b) out))
                           #false)))
                     (else
                        ;; overly permissive for now, A-Za-z0-9*-._ are ok.
                        (loop lst (cons a out))))))))

      (define (split-get-params bs)
         (lets/cc ret
            ((bss (split-at #\& bs)))
            (map
               (λ (pair)
                  (lets ((parts (split-at #\= pair)))
                     (if (= (length parts) 2)
                        (lets 
                           ((name (url-decode (car parts)))
                            (value (url-decode (cadr parts))))
                           (if (and name value)
                              (cons (list->string name)
                                    (list->string value))
                              (ret #false)))
                        (ret #false))))
               bss)))

      (define (parse-get-params env)
         (let ((val (getf env 'get-params)))
            (if val
               (let ((val (split-get-params val)))
                  (if val
                     (fupd env 'get-params val)
                     (fail env 400 "Bad GET parameters")))
               env)))

      (define (get-headers env)
         (lets 
            ((line ll (grab-line (getf env 'bs)))
             (env (fupd env 'bs ll)))
            (if line
               (if (null? line)
                  env
                  (lets ((pre post (cut-at #\: line))
                         (pre (list->string pre))
                         (tagp (known-header->symbol pre)))
                     (if tagp
                        (if (getf env tagp)
                           (begin
                              (print-to stderr "tried to redefine header " tagp)
                              (get-headers env))
                           (get-headers (put env tagp (header-value post))))
                        (get-headers
                           (-> env
                              (fupd 'bs ll)
                              (put 'headers 
                                 (cons 
                                    (cons pre (header-value post))
                                    (get env 'headers null))))))))
               (fail env 500 "No content"))))
      
      ;;;
      ;;; Responding
      ;;;

      (define (http-respond req)
         (let ((fd (getf req 'fd)))
            (begin
               (print-to fd "HTTP/1." (get req 'http-version 0) " " (get req 'status 200) " " (get req 'status-text "OK") "\r")
               (print-to fd "Content-Type: " (get req 'response-type "text/html") "\r")
               (print-to fd "\r")
               (let ((data (getf req 'content)))
                  (if data
                     (print-to fd data)))
               (close-port fd))))

      (define (request-pipe . handlers)
         (λ (req)
            (fold
               (λ (req handler)
                  (if (getf req 'error)
                     req
                     (handler req)))
               req handlers)))

      (define pre-handler
         (request-pipe
            get-request
            parse-get-params
            get-headers
            ))

      (define post-handler
         (request-pipe
            http-respond))

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
                           (-> empty
                              (put 'ip ip)
                              (put 'fd fd)
                              (put 'bs (stream-fd-timeout fd max-request-time))
                              pre-handler
                              handler
                              post-handler)))
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

      ;; a thread which receives clients and server commands and acts accordingly
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
      (λ (env) 
         (print "router got " env) 
         (-> env
            (put 'status 200)
            (put 'content (list "hello " env))))
      80))


