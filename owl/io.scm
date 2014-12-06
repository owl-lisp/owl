;;;
;;; Simple direct blocking IO (replaces the old thread-based one)
;;;

(define-library (owl io)

  (export 
      ;; thread-oriented non-blocking io
      open-output-file        ;; path → fd | #false
      open-input-file         ;; path → fd | #false
      open-socket             ;; port → thread-id | #false
      open-connection         ;; ip port → thread-id | #false
      port->fd                ;; port → fixnum
      fd->port                ;; fixnum → port
      port?                   ;; _ → bool
      flush-port              ;; fd → _
      close-port              ;; fd → _
      start-sleeper           ;; start a (global) sleeper thread
      sleeper-id              ;; id of sleeper thread
      start-base-threads      ;; start stdio and sleeper threads
      wait-write              ;; fd → ? (no failure handling yet)

      ;; stream-oriented blocking (for the writing thread) io
      blocks->port            ;; ll fd → ll' n-bytes-written, don't close fd
      closing-blocks->port    ;; ll fd → ll' n-bytes-written, close fd
      tcp-socket              ;; port-num → socket | #false
      tcp-client              ;; port → ip tcp-fd | #f #f
      tcp-clients             ;; port → ((ip . fd) ... . X), X = null → ok, #false → error
      tcp-send                ;; ip port (bvec ...) → (ok|write-error|connect-error) n-bytes-written
   
      file->vector            ;; vector io, may be moved elsewhere later
      file->list              ;; list io, may be moved elsewhere later
      vector->file
      write-vector            ;; vec port
      port->byte-stream       ;; fd → (byte ...) | thunk 

      ;; temporary exports
      fclose                 ;; fd → _

      stdin stdout stderr
      display-to        ;; port val → bool
      print-to          ;; port val → bool
      display
      print
      print*
      print*-to         ;; port val → bool
      write 
      writer-to         ;; names → (port val → bool + io)
      write-to          ;; port val → bool
      write-bytes       ;; port byte-list   → bool
      write-byte-vector ;; port byte-vector → bool
      get-block         ;; fd n → bvec | eof | #false
      try-get-block     ;; fd n block? → bvec | eof | #false=error | #true=block
      lines             ;; fd → null | ll of string, read error is just null, each [\r]\n removed

      system-print system-println system-stderr
      take-nap
      fasl-save         ;; obj path → done?
      fasl-load         ;; path default → done?
   )

   (import
      (owl defmac)
      (owl syscall)
      (owl queue)
      (owl string)
      (owl list-extra)
      (owl ff)
      (owl equal)
      (owl vector)
      (owl render)
      (owl list)
      (owl math)
      (owl fasl)
      (owl tuple)
      (owl primop)
      (owl port)
      (owl eof)
      (owl lazy)
      (only (owl vector) merge-chunks vec-leaves))

   (begin

      ;; standard io ports
      (define stdin  (fd->port 0))
      (define stdout (fd->port 1))
      (define stderr (fd->port 2))

      ;; use type 12 for fds 

      (define (fclose fd)
         (sys-prim 2 fd #false #false))

      (define (fopen path mode)
         (cond
            ((c-string path) => 
               (λ (raw) (sys-prim 1 raw mode #false)))
            (else #false)))

      ;; use fd 65535 as the unique sleeper thread name.
      (define sid (fd->port 65535))

      (define sleeper-id sid)

      ;;; Writing

      ;; #[0 1 .. n .. m] n → #[n .. m]
      (define (bvec-tail bvec n)
         (raw (map (lambda (p) (refb bvec p)) (iota n 1 (sizeb bvec))) type-vector-raw #false))

      (define (try-write-block fd bvec len)
         (cond
            ;; one does not simply write() on all platforms
            ((tcp? fd) (sys-prim 15 fd bvec len))
            ((port? fd) (sys-prim 0 fd bvec len))
            (else 
               ;(sys-prim 0 fd bvec len)
               #false)))

      ;; bvec port → bool
      (define (write-really bvec fd)
         (let ((end (sizeb bvec)))
            (if (eq? end 0)
               #true
               (let loop ()
                  (let ((wrote (try-write-block fd bvec end)))
                     (cond
                        ((eq? wrote end) #true) ;; ok, wrote the whole chunk
                        ((eq? wrote 0) ;; 0 = EWOULDBLOCK
                           (interact sid 2) ;; fixme: adjustable delay rounds 
                           (loop))
                        (wrote ;; partial write
                           (write-really (bvec-tail bvec wrote) fd))
                        (else #false))))))) ;; write error or other failure

      ;; how many bytes (max) to add to output buffer before flushing it to the fd
      (define output-buffer-size 4096)

      (define (open-input-file path) 
         (let ((fd (fopen path 0)))
            (if fd (fd->port fd) fd)))

      (define (open-output-file path)
         (let ((fd (fopen path 1)))
            (if fd (fd->port fd) fd)))

      ;;; Reading

      (define input-block-size 
         *vec-leaf-size*) ;; changing from 256 breaks vector leaf things

      (define (try-get-block fd block-size block?)
         (let ((res (sys-prim 5 fd block-size 0)))
            (if (eq? res #true) ;; would block
               (if block?
                  (begin
                     (interact sid 5)
                     (try-get-block fd block-size #true))
                  res)
               res))) ;; is #false, eof or bvec

      ;; get a block of size up to block size
      (define (get-block fd block-size)
         (try-get-block fd block-size #true))

      (define (bvec-append a b)
         (list->byte-vector
            (append
               (vector->list a)
               (vector->list b))))

      ;; get a block of size block-size, wait more if less is available and not eof
      ;; fd n → eof-seen? eof|#false|bvec
      (define (get-whole-block fd block-size)
         (let ((this (get-block fd block-size)))
            (cond
               ((eof? this) (values #true this))
               ((not this) (values #false this))
               (else
                  (let ((n (sizeb this)))
                     (if (eq? n block-size)
                        (values #false this)
                        (lets ((eof-seen? tail (get-whole-block fd (- block-size n))))
                           (cond
                              ((eof? tail) (values #true this))
                              ((not tail) (values #false this)) ;; next read will also fail, return last ok data
                              (else 
                                 ;; unnecessarily many conversions if there are many partial
                                 ;; reads, but block size is tiny in file->vector making this
                                 ;; irrelevant
                                 (values eof-seen?
                                    (bvec-append this tail)))))))))))

      ;;; TCP sockets

      ;; needed a bit later for stream interface
      (define (send-next-connection thread fd)
         (let loop ((rounds 0)) ;; count for temporary sleep workaround
            (let ((res (sys-prim 4 fd #false #false)))
               (if res ; did get connection
                  (lets ((ip fd res))
                     (mail thread fd)
                     #true)
                  (begin
                     (interact sid 5) ;; delay rounds
                     (loop rounds))))))
                     
      (define (open-socket port)
         (let ((sock (sys-prim 3 port #false #false)))
            (if sock 
               (list 'sock (fd->port sock))
               #false)))

      ;;; TCP connections

      (define (open-connection ip port)
         (cond
            ((not (eq? (type port) type-fix+))
               #false)
            ((and (eq? (type ip) type-vector-raw) (eq? 4 (sizeb ip))) ;; silly old formats
               (let ((fd (_connect ip port)))
                  (if fd
                     (fd->tcp fd)
                     #false)))
            (else 
               ;; note: could try to autoconvert formats to be a bit more user friendly
               #false)))

      ;;; Sleeper thread

      ;; todo: later probably sleeper thread and convert it to a syscall

      ;; run thread scheduler for n rounds between possibly calling vm sleep()
      (define sleep-check-rounds 10)

      ;; number of milliseconds to sleep for real at a time when no threads are running but
      ;; they want to sleep, typically waiting for input or output
      (define ms-per-round 10)

      ;; IO is closely tied to sleeping in owl now, because instead of the poll there are 
      ;; several threads doing their own IO with their own fds. the ability to sleep well 
      ;; is critical, so the global sleeping thread is also in lib-io.

      (define (find-bed ls id n)
         (if (null? ls) 
            (list (cons n id)) ;; last bed, select alarm
            (let ((this (caar ls)))
               (if (< n this) ;; add before someone to be waked later
                  (ilist 
                     (cons n id)
                     (cons (- this n) (cdr (car ls)))
                     (cdr ls))
                  (cons (car ls)
                     (find-bed ls id (- n this))))))) ;; wake some time after this one

      (define (add-sleeper ls env)
         (lets ((from n env))
            (if (eq? (type n) type-fix+)
               (find-bed ls from n)
               (find-bed ls from 10))))   ;; silent fix

      ;; note: might make sense to _sleep a round even when rounds=0 if single-thread? and did not _sleep any of the prior rounds, because otherwise we might end up having cases where many file descriptors keep ol running because at least one fd thread is always up and running. another solution would be to always wake up just one thread, which would as usual suspend during the round when inactive. needs testing.

      ;; suspend execution for <rounds> thread scheduler rounds (for current thread) and also suspend the vm if no other threads are running
      (define (sleep-for rounds)
         (cond
            ((eq? rounds 0)
               rounds)
            ((single-thread?)
               ;; note: could make this check every n rounds or ms
               (if (_sleep (* ms-per-round rounds)) ;; sleep really for a while
                  ;; stop execution if breaked to enter mcp
                  (set-ticker 0)))
            (else
               (lets
                  ((a (wait 1))
                   (rounds _ (fx- rounds 1)))
                  (sleep-for rounds)))))

      (define (wake-neighbours l)
         (cond
            ((null? l) l)
            ((eq? 0 (caar l))
               (mail (cdar l) 'rise-n-shine)
               (wake-neighbours (cdr l)))
            (else l)))
         
      ;; ls = queue of ((rounds . id) ...), sorted and only storing deltas
      (define (sleeper ls)
         (cond
            ((null? ls)
               (sleeper (add-sleeper ls (wait-mail))))
            ((check-mail) =>
               (λ (env) 
                  (sleeper (add-sleeper ls env))))
            (else
               (sleep-for (caar ls))
               (mail (cdar ls) 'awake) ;; wake up the thread ((n . id) ...)
               (sleeper (wake-neighbours (cdr ls)))))) ;; wake up all the ((0 . id) ...) after it, if any

      (define (start-sleeper)
         (fork-server sid
            (λ () (sleeper null))))

      ;; start normally mandatory threads (apart form meta which will be removed later)
      (define (start-base-threads)
         (start-sleeper) ;; <- could also be removed later
         (wait 1)
         )

      ;; deprecated
      (define (flush-port fd)
         ;(mail fd 'flush)
         42
         )

      (define (close-port fd)
         (fclose fd)
         )



      ;;;
      ;;; STREAM BASED IO
      ;;;

      (define socket-read-delay 2)

      ;; In case one doesn't need asynchronous atomic io operations, one can use 
      ;; threadless stream-based blocking (for the one thred) IO.

      ;; write a stream of byte vectors to a fd and 
      ;; (bvec ...) fd → ll' n-written, doesn't close port
      ;;                  '-> null if all written without errors
      (define (blocks->port ll fd)
         (let loop ((ll ll) (n 0))
            (cond
               ((pair? ll)
                  (if (byte-vector? (car ll))
                     (if (write-really (car ll) fd)
                        (loop (cdr ll) (+ n (sizeb (car ll))))
                        (values ll n))
                     (values ll n)))
               ((null? ll)
                  (values ll n))
               (else
                  (loop (ll) n)))))

      (define (closing-blocks->port ll fd)
         (lets ((r n (blocks->port ll fd)))
            (fclose fd)
            (values r n)))

      ;; sock → #f #f | ip client
      (define (tcp-client sock)
         (let ((res (sys-prim 4 sock #false #false)))
            (if res 
               (lets ((ip fd res))
                  (values ip (fd->tcp fd)))
               (begin
                  (interact sid socket-read-delay)
                  (tcp-client sock)))))

      ;; port → ((ip . fd) ... . null|#false), CLOSES SOCKET
      (define (socket-clients sock)
         (lets ((ip cli (tcp-client sock)))
            (if ip
               (pair (cons ip cli) (socket-clients sock))
               null)))

      (define (tcp-socket port)
         (let ((fd (sys-prim 3 port #false #false)))
            (if fd (fd->socket fd) fd)))

      ;; port → ((ip . fd) ... . null|#false), CLOSES SOCKET
      (define (tcp-clients port)
         (let ((sock (tcp-socket port)))
            (if sock
               (λ () (socket-clients sock))
               #false)))

      ;; ip port (bvec ...) → #true n-written | #false error-sym
      (define (tcp-send ip port ll)
         (let ((fd (_connect ip port)))
            (if fd
               (lets ((ll n (closing-blocks->port ll fd)))
                  (if (null? ll)
                     (values 'ok n)
                     (values 'error n)))
               (values 'connect-error 0))))


      ;;;
      ;;; Rendering and sending
      ;;;

      ;; splice lst to bvecs and call write on fd
      (define (printer lst len out fd)
         (cond
            ((eq? len output-buffer-size)
               (and 
                  (write-really (raw (reverse out) type-vector-raw #false) fd)
                  (printer lst 0 null fd)))
            ((null? lst)
               (write-really (raw (reverse out) type-vector-raw #false) fd))
            (else
               ;; avoid dependency on generic math in IO
               (lets ((len _ (fx+ len 1)))
                  (printer (cdr lst) len (cons (car lst) out) fd)))))

      (define (write-byte-vector port bvec)
         (write-really bvec port))

      (define (write-bytes port byte-list)
         (printer byte-list 0 null port))

      (define (print-to to . stuff)
         (printer (foldr render '(10) stuff) 0 null to))

      (define (writer-to names)
         (let ((serialize (make-serializer names)))
            (λ (to obj)
               (printer (serialize obj '()) 0 null to))))

      (define write-to 
         (writer-to 
            (put #empty map "map")))

      (define (display-to to obj)
         (printer (render obj '()) 0 null to))

      (define (display x)
         (display-to stdout x))

      (define print 
         (case-lambda
            ((obj) (print-to stdout obj))
            (xs (printer (foldr render '(#\newline) xs) 0 null stdout))))

      (define (write obj) (write-to stdout obj))

      (define (print*-to to lst)
         (printer (foldr render '(10) lst) 0 null to))

      (define (print* lst)
         (printer (foldr render '(10) lst) 0 null stdout))

      (define-syntax output
         (syntax-rules () 
            ((output . stuff)
               (print* (list stuff)))))

      ;; fixme: system-X do not belong here
      (define (system-print str)
         (sys-prim 0 1 str (sizeb str)))

      (define (system-println str)
         (system-print str)
         (system-print "
      "))

      (define (system-stderr str) ; <- str is a raw or pre-rendered string
         (sys-prim 0 2 str (sizeb str)))

      ;;; 
      ;;; Files <-> vectors
      ;;;

      ;; read all blocks for a port, all but possibly last one having input-block-size bytes
      (define (read-blocks port buff)
         (lets ((eof-seen? val (get-whole-block port input-block-size)))
            (cond
               (eof-seen?
                  (let ((buff (if (eof? val) buff (cons val buff))))
                     (merge-chunks
                        (reverse buff)
                        (fold + 0 (map sizeb buff)))))
               ((not val)
                  #false)
               (else
                  (read-blocks port 
                     (cons val buff))))))

      (define (explode-block block tail)
         (let ((end (sizeb block)))
            (if (eq? end 0)
               tail
               (let loop ((pos (- end 1)) (tail tail))
                  (if (eq? pos -1)
                     tail
                     (loop (- pos 1) (cons (refb block pos) tail)))))))

      (define (read-blocks->list port buff)
         (let ((block (get-block port 4096)))
            (cond
               ((eof? block)
                  (foldr explode-block null (reverse buff)))
               ((not block)
                  ;; read error
                  (foldr explode-block null (reverse buff)))
               (else
                  (read-blocks->list port (cons block buff))))))

      (define (maybe-open-file path)
         (if (equal? path "-")
            stdin
            (open-input-file path)))

      (define (maybe-close-port port)
         (if (eq? port stdin)
            #true
            (close-port port)))

      (define (file->vector path) ; path -> vec | #false
         (let ((port (maybe-open-file path)))
            (if port
               (let ((data (read-blocks port null)))
                  (maybe-close-port port)
                  data)
               (begin
                  ;(print "file->vector: cannot open " path)
                  #false))))
      
      (define (file->list path) ; path -> vec | #false
         (let ((port (maybe-open-file path)))
            (if port
               (let ((data (read-blocks->list port null)))
                  (maybe-close-port port)
                  data)
               (begin
                  ;(print "file->vector: cannot open " path)
                  #false))))

      (define (file->list path) ; path -> vec | #false
         (let ((port (maybe-open-file path)))
            (if port
               (let ((data (read-blocks->list port null)))
                  (maybe-close-port port)
                  data)
               (begin
                  ;(print "file->vector: cannot open " path)
                  #false))))

      ;; write each leaf chunk separately (note, no raw type testing here -> can fail)
      (define (write-vector vec port)
         (let loop ((ll (vec-leaves vec)))
            (cond
               ((pair? ll)
                  (write-byte-vector port (car ll))
                  (loop (cdr ll)))
               ((null? ll) #true)
               (else (loop (ll))))))

      ;; fixme: no way to poll success yet. last message should be ok-request, which are not there yet.
      ;; fixme: detect case of non-bytevectors, which simply means there is a leaf which is not of type (raw 11)
      (define (vector->file vec path)
         (let ((port (open-output-file path)))
            (if port
               (let ((outcome (write-vector vec port)))
                  (close-port port)
                  outcome)
               #false)))

      (define (wait-write fd)
         (interact fd 'wait))

      (define (stream-chunk buff pos tail)
         (if (eq? pos 0)
            (cons (refb buff pos) tail)
            (lets ((next x (fx- pos 1)))
               (stream-chunk buff next
                  (cons (refb buff pos) tail)))))

      (define (port->byte-stream fd)
         (λ ()
            (let ((buff (get-block fd input-block-size)))
               (cond  
                  ((eof? buff)
                     (close-port fd)
                     null)
                  ((not buff)
                     ;(print "bytes-stream-port: no buffer received?")
                     null)
                  (else
                     (stream-chunk buff (- (sizeb buff) 1)
                        (port->byte-stream fd)))))))

      (define (lines fd)
         (let loop ((ll (port->byte-stream fd)) (out null))
            (cond
               ((pair? ll)
                  (lets ((byte ll ll))
                     (if (eq? byte #\newline)
                        (pair
                           (list->string
                              (reverse
                                 (if (and (pair? out) (eq? #\return (car out)))
                                    (cdr out)
                                    out)))
                           (loop ll null))
                        (loop ll (cons byte out)))))
               ((null? ll)
                  (if (null? out)
                     null
                     (list
                        (list->string (reverse out)))))
               (else
                  (loop (ll) out)))))

      (define (file->byte-stream path)
         (let ((fd (open-input-file path)))
            (if fd
               (port->byte-stream fd)
               #false)))

      (define (take-nap)
         (interact sid 5))

      (define (fasl-save obj path) 
         (vector->file 
            (list->vector (fasl-encode obj))
            path))

      (define (fasl-load path fail-val)
         (let ((bs (file->byte-stream path)))
            (if bs 
               (fasl-decode bs fail-val)
               fail-val)))
))
