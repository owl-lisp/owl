;;;
;;; HTTP server
;;;

(define-library (owl http)

   (export 
      server
      query-case
      add-response-header
      fail)

   (import
      (owl base)
      (owl parse)
      (owl env))

   (begin
      
      (define max-request-time (* 15 1000))
      (define max-request-size (* 1024 1024))
      (define max-request-block-size 32768)
      (define max-post-length (* 1024 1024))

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

      (define parse-get-or-head
         (let-parses
            ((method 
               (get-either 
                  (get-word "GET " 'get)
                  (get-word "HEAD " 'head)))
             (query (get-greedy+ get-nonspace))
             (skip (get-word " HTTP/1." 'foo))
             (ver-char (get-either (get-imm #\0) (get-imm #\1))))
            (tuple method query (- ver-char #\0))))

      (define parse-post
         (let-parses
            ((method (get-word "POST " 'get))
             (query (get-greedy+ get-nonspace))
             (skip (get-word " HTTP/1." 'foo))
             (ver-char (get-either (get-imm #\0) (get-imm #\1))))
            (tuple 'post query 
               (- ver-char #\0))))

      (define parse-query 
         (get-either 
            parse-get-or-head
            parse-post))
         
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
                  (print "Socket acceptor got " cli)
                  (mail recipient cli))
               (interact 'iomux (tuple 'read sock)))
            (socket-accepter sock recipient)))

      ;;;
      ;;; Request processing passes
      ;;;
      
      (define (fail env code reason)
         (-> env 
            (put 'error code)
            (put 'bs null)            ;; disconnect on error
            (put 'status code)
            (put 'status-text reason)
            (put 'content reason)))

      (define (nat str)
         (if str
           (string->number str 10)
           #false))

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
                  ((head query version) ;; clone of get for now
                     (lets ((query params (split-params query)))
                        (if query
                           (-> env
                              (fupd 'bs ll) ;; rest of data
                              (put 'query (list->string query))
                              (put 'get-params params)  ;; byte list
                              (put 'http-version version)
                              (put 'http-method 'head))
                           (fail env 500 "Bad query"))))
                  ((post query version)
                     (if query
                       (-> env
                          (fupd 'bs ll)
                          (put 'query (list->string query))
                          (put 'http-version version)
                          (put 'http-method 'post))
                       (fail env 500 "Bad query")))
                  (else
                     (fail env 500 "Query wat")))
               (fail env 500 "No query received"))))

      ;; doing string->symbol on all would cause memory leak
      (define (known-header->symbol str)
         (cond
            ((string-eq? str "Host") 'host)
            ((string-eq? str "User-Agent") 'user-agent)
            ((string-eq? str "Referer") 'referer) ;; originally Referrer
            ((string-eq? str "Content-type") 'content-type)
            ((string-eq? str "Content-length") 'content-length)
            ((string-eq? str "Content-Length") 'content-length)
            ((string-eq? str "content-length") 'content-length)
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

      (define (url-decode lst plus?)
         (let loop ((lst lst) (out null))
            (if (null? lst)
               (reverse out)
               (lets ((a lst lst))
                  (cond
                     ((eq? a #\+)
                        (if plus?
                           (loop lst (cons #\space out))
                           (loop lst (cons #\+ out))))
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

      (define (split-url-params bs plus?)
         (lets/cc ret
            ((bss (split-at #\& bs)))
            (map
               (λ (pair)
                  (lets ((parts (split-at #\= pair)))
                     (if (= (length parts) 2)
                        (lets 
                           ((name (url-decode (car parts) plus?))
                            (value (url-decode (cadr parts) plus?)))
                           (if (and name value)
                              (cons (list->string name)
                                    (list->string value))
                              (ret #false)))
                        (ret #false))))
               bss)))

      (define (parse-get-params env)
         (let ((val (getf env 'get-params)))
            (if val
               (let ((val (split-url-params val #true)))
                  (if val
                     (fupd env 'get-params val)
                     (fail env 400 "Bad GET parameters")))
               env)))

      (define default-post-type "application/x-www-form-urlencoded")

      (define (lazy-read lst n)
         (let loop ((lst lst) (out null) (n n))
            (cond
               ((eq? n 0)
                  (values (reverse out) lst))
               ((pair? lst)
                  (loop (cdr lst) (cons (car lst) out) (- n 1)))
               ((null? lst)
                  (values #false lst))
               (else
                  (loop (lst) out n)))))

      (define (read-post-data env)
         (if (eq? (getf env 'http-method) 'post)
            (lets
               ((type (get env 'content-type default-post-type))
                (len (nat (getf env 'content-length))))
               (cond
                  ((not len)
                     (fail env 400 "No POST data length"))
                  ((> len max-post-length)
                     (fail env 400 "Too much POST data"))
                  ((equal? type default-post-type)
                     (lets ((hd tl (lazy-read (getf env 'bs) len)))
                        (if hd
                           (-> env 
                              (put 'bs tail)
                              (put 'post-data hd))
                           (fail env 500 "Insufficient POST data"))))
                  (else
                     (fail env 400 "Unknown post content type"))))
            env))

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

      ;; clamp content heavily to avoid header content escapes even with 
      ;; encoding confusion. might make sense to respond with an error 
      ;; instead.
      (define (render-header val tail)
         (foldr
            (λ (char tl)
               (cond
                  ((< char #\space) tl)
                  ((> char 126) tl)
                  (else (cons char tl))))
            tail
            (render val null)))

      (define (emit-header fd name val)
         (write-really
            (list->byte-vector
               (render-header name 
                  (ilist #\: #\space
                     (render-header val '(#\return #\newline)))))
            fd))

      (define (maybe-emit-header env fd sym name)
         (let ((val (getf env sym)))
            (if val
               (emit-header fd name val))))

      (define (emit-response-headers env fd)
         (ff-fold
            (λ (_ name value) 
               (emit-header fd name value))
            null (get env 'response-headers empty)))

      (define (byte-list? data)
         (and (pair? data) 
            (number? (car data))))

      (define (block-list? data)
         (and (pair? data)
            (byte-vector? (car data))))

      (define (block-list-length lst)
         (fold (λ (s b) (+ s (sizeb b))) 0 lst))

      (define (http-respond req)
         (let ((fd (getf req 'fd)))
            (print-to fd "HTTP/1." (get req 'http-version 0) " " (get req 'status 200) " " (get req 'status-text "OK") "\r")
            (emit-header fd "Content-Type" (get req 'response-type "text/html"))
            (maybe-emit-header req fd 'server "Server") ;; move to response-headers
            (emit-response-headers req fd)
            (let ((data (get req 'content "No data")))
               (cond
                  ((byte-vector? data)
                     (emit-header fd "Content-length" (sizeb data))
                     (print-to fd "\r")
                     (if (not (eq? 'head (getf req 'http-method)))
                        (write-really data fd)))
                  ((vector? data)
                     (emit-header fd "Content-length" (vector-length data))
                     (print-to fd "\r")
                     (if (not (eq? 'head (getf req 'http-method)))
                        (write-vector data fd)))
                  ((string? data)
                     (lets
                        ((data (string->list data))
                         (len (length data)))
                        (emit-header fd "Content-length" len)
                        (print-to fd "\r")
                        (if (not (eq? 'head (getf req 'http-method)))
                           (byte-stream->port data fd))))
                  ((byte-list? data)
                     (emit-header fd "Content-length" (length data))
                     (print-to fd "\r")
                     (if (not (eq? 'head (getf req 'http-method)))
                        (byte-stream->port data fd)))
                  ((block-list? data)
                     (emit-header fd "Content-length" (block-list-length data))
                     (print-to fd "\r")
                     (if (not (eq? 'head (getf req 'http-method)))
                        (block-stream->port data fd)))
                  ((eq? (getf req 'status) 404)
                     (print-to fd "\r")
                     (if (not (eq? 'head (getf req 'http-method)))
                        (print-to fd "404 No such anything")))
                  (else
                     (print-to fd "\r")
                     (if (not (eq? 'head (getf req 'http-method)))
                        (print-to fd data)))))
            req))

      (define (request-pipe . handlers)
         (λ (req)
            (fold
               (λ (req handler)
                  (if (getf req 'error)
                     req
                     (handler req)))
               req handlers)))

      (define (parse-post-data env)
         (let ((data (getf env 'post-data)))
            (if data
               (lets ((params (split-url-params data #true)))
                  (if params
                     (put (del env 'post-data) 'post-params params)
                     (fail env 500 "Bad POST data")))
               env)))

      (define (debug-handler msg)
         (λ (env)
            (print-to stderr "*** DEBUG HANDLER (" msg "): " env)
            env))

      (define (add-server-info env)
         (put env 'server "ohttpd/0.1"))

      (define pre-handler
         (request-pipe
            get-request
            parse-get-params
            get-headers
            read-post-data
            parse-post-data
            ;(debug-handler "AA 3")
            add-server-info
            ))

      (define (add-response-header env key value)
         (put env 'response-headers
            (put (get env 'response-headers empty) key value)))

      (define maybe-add-close-header
         (λ (env)
            (if (not (pair? (getf env 'bs)))
               (add-response-header env 'Connection "close")
               env)))

      (define post-handler
         (request-pipe
            ;(debug-handler "POST FIRST")
            ;http-respond
            maybe-add-close-header)) ;; Connection: close, if bs is non-pair

      (define (unless-error handler)
         (λ (env)
            (if (getf env 'error)
               env
               (handler env))))

      (define (clear-env env)
         ; (print "clearing env, bs is " (getf env 'bs))
         (-> empty
            (put 'ip (getf env 'ip))
            (put 'fd (getf env 'fd))
            (put 'start (time-ms))
            (put 'bs (getf env 'bs))))

      (define (ip->str ip)
         (list->string
            (foldr
               (λ (thing tl) (render thing (if (null? tl) tl (cons #\. tl))))
               null 
               (vector->list ip))))
            
      (define (print-request-info env n str)
         (let ((elapsed (- (time-ms) (get env 'start 0))))
            (print 
               (time) " " (ip->str (getf env 'ip)) " [" (getf env 'fd ) "," n "]: "
               (getf env 'http-method) " " (getf env 'query) " -> " (get env 'status 200) " " str " (" elapsed "ms)")))

      (define (handle-connection handler env)
         (let loop ((n 0) (env env))
            (let ((env (http-respond (post-handler (handler (pre-handler env))))))
               (if (eq? 200 (get env 'status 200))
                  (lets ((bs (get env 'bs null)))
                     (if (pair? bs)
                        (begin
                            (print-request-info env n "")
                            (loop (+ n 1) (clear-env env)))
                        (begin
                           (print-request-info env n "closing on non-pair data")
                           (close-port (getf env 'fd)))))
                  (begin
                     (print-request-info env n " -> closing on non-200")
                     (close-port (getf env 'fd)))))))

      (define (server-loop handler clis)
         ;(print "Server loop waiting for mail and processing " (length (ff->list clis)) " connections")
         (lets ((envelope (wait-mail))
                (from msg envelope))
            (cond
               ((pair? msg) 
                  (lets 
                     ((ip rfd msg)
                      (fd (fd->port rfd))
                      (id (tuple 'http-client ip fd)))
                     (print-to stderr "server starting client fd " fd " with ip " ip)
                     (fork-linked-server id
                        (λ () 
                           (handle-connection 
                              (unless-error handler)
                              (-> empty
                                 (put 'ip ip)
                                 (put 'fd fd)
                                 (put 'start (time-ms))
                                 (put 'bs (stream-fd-timeout fd max-request-time))))))
                     (server-loop handler (put clis id (time-ms)))))
               ((eq? msg 'stop)
                  (print-to stderr "server stopping")
                  (mail 'from 'stopped))
               ((tuple? msg) 
                  (tuple-case msg
                     ((finished a b c)
                        (let ((elapsed (- (time-ms) (get clis from 0))))
                           (server-loop handler (del clis from))))
                     ((crashed op a b)
                        (let ((cli (getf clis from)))
                           ;; todo: add log & client handler
                           (print-to stderr "Thread crash while processing connection " from)
                           (print-to stderr (verbose-vm-error *toplevel* op a b))
                           (if from
                              (begin
                                 (print-to (ref from 3) "HTTP/1.0 500 ERROR CODE " op "\r\n\r\nERROR " op)
                                 (close-port (ref from 3))))
                           (server-loop handler (del clis from))))
                     ((update handler state-transform)
                        (server-loop handler (state-transform clis)))
                     (else
                        (print-to stderr "weird message: " envelope)
                        (server-loop handler clis))))
               (else
                  (print-to stderr "unknown message: " msg)
                  (server-loop handler clis)))))

      ;; a thread which receives clients and server commands and acts accordingly
      (define (server server-id handler port)
         (let ((sock (open-socket port)))
            (print "Server socket " sock)
            (if sock
               (fork-linked-server server-id
                  (λ ()
                     ;; start reading connections
                     (print "Starting reader")
                     (let ((reader (fork (λ () (socket-accepter sock server-id)))))
                        ;(print "Reader is " reader)
                        (server-loop handler #empty)
                        (close-port sock)
                        (kill reader)
                        )))
               (begin
                  (print-to stderr server-id "failed to get socket")
                  #false))))

   (define-syntax query-case
      (syntax-rules (_dispatch)
         ((query-case (op . args) a ...)
            (let ((env (op . args)))
               (query-case env a ...)))
         ((query-case _dispatch q (pat ms . body) next ...)
            (let ((out (pat q)))
               (if out
                  (begin
                     (apply (lambda ms . body) out))
                  (query-case _dispatch q next ...))))
         ((query-case _dispatch q)
            (error "unmatched query: " q))
         ((query-case q . opts)
            (if q
               (query-case _dispatch q . opts)
               (error "query-case: no query" q)))))

))


