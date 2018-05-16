;;;
;;; Simple direct blocking IO (replaces the old thread-based one)
;;;

(define-library (owl io)

  (export
      ;; thread-oriented non-blocking io
      open-output-file        ;; path → fd | #false
      open-input-file         ;; path → fd | #false
      open-append-file        ;; path → fd | #false
      open-socket             ;; port → fd | #false
      open-tcp-socket         ;; port → fd | #false
      open-udp-socket         ;; port → fd | #false
      open-connection         ;; ip port → thread-id | #false
      file-size
      port->fd                ;; port → fixnum
      fd->port                ;; fixnum → port
      port?                   ;; _ → bool
      close-port              ;; fd → _
      start-base-threads      ;; start stdio and sleeper threads
      wait-write              ;; fd → ? (no failure handling yet)

      ;; stream-oriented blocking (for the writing thread) io
      blocks->port            ;; ll fd → ll' n-bytes-written, don't close fd
      closing-blocks->port    ;; ll fd → ll' n-bytes-written, close fd
      tcp-client              ;; port → ip tcp-fd | #f #f
      tcp-clients             ;; port → ((ip . fd) ... . X), X = null → ok, #false → error
      tcp-send                ;; ip port (bvec ...) → (ok|write-error|connect-error) n-bytes-written

      ;; datagram-oriented IO
      udp-packets             ;; port → null | ((ip . bvec) ...)
      udp-client-socket       ;; temp
      wait-udp-packet         ;; port → (ip . bvec), blocks
      check-udp-packet        ;; port → #false | (ip . bvec), does not block
      send-udp-packet         ;; sock ip port bvec → bool 

      file->vector            ;; vector io, may be moved elsewhere later
      file->list              ;; list io, may be moved elsewhere later
      file->byte-stream       ;; path → #false | (byte ...)
      vector->file
      write-vector            ;; vec port
      port->meta-byte-stream  ;; fd → (byte|'io-error|'block ...) | thunk 
      port->byte-stream       ;; fd → (byte ...) | thunk
      port->tail-byte-stream  ;; fd → (byte ...) | thunk 
      byte-stream->port       ;; bs fd → bool
      port->block-stream      ;; fd → (bvec ...)
      block-stream->port      ;; (bvec ...) fd → bool

      display-to        ;; port val → bool
      print-to          ;; port val → bool
      print
      print*
      print*-to         ;; port val → bool
      write
      writer-to         ;; names → (port val → bool + io)
      write-to          ;; port val → bool
      write-bytes       ;; port byte-list   → bool
      write-byte-vector ;; port byte-vector → bool
      get-block         ;; fd n → bvec | eof | #false
      write-really
      try-get-block     ;; fd n block? → bvec | eof | #false=error | #true=block
      byte-stream->lines ;; (byte ...) → null | ll of string, read error is just null, each [\r]\n removed
      lines              ;; fd → null | ll of string, read error is just null, each [\r]\n removed

      system-print system-println system-stderr
      fasl-save         ;; obj path → done?
      fasl-load         ;; path default → done?

      start-muxer       ;; new io muxer
      sleep             ;; sleep ms -> sleep at least for ms
   )

   (import
      (owl defmac)
      (owl eof)
      (owl syscall)
      (owl queue)
      (owl string)
      (owl list-extra)
      (prefix (owl sys) sys-)
      (owl ff)
      (owl equal)
      (owl vector)
      (owl render)
      (owl list)
      (owl symbol)
      (owl math)
      (owl unicode)
      (owl fasl)
      (owl tuple)
      (owl primop)
      (owl port)
      (owl lazy)
      (only (owl vector) merge-chunks vec-leaves))

   (begin

      ;;; Writing

      ;; #[0 1 .. n .. m] n → #[n .. m]
      (define (bvec-tail bvec n)
         (raw (map (H refb bvec) (iota n 1 (sizeb bvec))) type-vector-raw))

      (define (try-write-block fd bvec len)
         ;; stdio ports are in blocking mode, so poll always
         (if (stdio-port? fd)
            (interact 'iomux (tuple 'write fd)))
         (sys-write fd bvec len))

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
                           (interact 'iomux (tuple 'write fd))
                           (loop))
                        (wrote ;; partial write
                           (write-really (bvec-tail bvec wrote) fd))
                        (else #false))))))) ;; write error or other failure

      ;; how many bytes (max) to add to output buffer before flushing it to the fd
      (define output-buffer-size 4096)

      (define read-mode
         sys-O_RDONLY)
      (define (output-mode)
         (bor (bor (sys-O_WRONLY) (sys-O_CREAT)) (sys-O_TRUNC)))
      (define (append-mode)
         (bor (bor (sys-O_WRONLY) (sys-O_CREAT)) (sys-O_APPEND)))

      (define (open-input-file path)
         (sys-open path (read-mode) 0))

      (define (open-output-file path)
         (sys-open path (output-mode) #o600)) ;; temporarily also applies others

      (define (open-append-file path)
         (let ((port (sys-open path (append-mode) #o600)))
            (and port (begin (sys-seek-end port) port))))

      ;;; Reading

      (define input-block-size
         *vec-leaf-size*) ;; changing from 256 breaks vector leaf things

      (define stream-block-size
         #x8000)

      (define (try-get-block fd block-size block?)
         ;; stdio ports are in blocking mode, so poll always
         (if (stdio-port? fd)
            (interact 'iomux (tuple 'read fd)))
         (let ((res (sys-read fd block-size)))
            (if (eq? res #true) ;; would block
               (if block?
                  (begin
                     ;(interact sid 5)
                     (interact 'iomux (tuple 'read fd))
                     (try-get-block fd block-size #true))
                  res)
               res))) ;; is #false, eof or bvec

      ;; get a block of size up to block size
      (define (get-block fd block-size)
         (try-get-block fd block-size #true))

      (define (maybe-get-block fd block-size)
         (try-get-block fd block-size #false))

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
               ((eof-object? this) (values #true this))
               ((not this) (values #false this))
               (else
                  (let ((n (sizeb this)))
                     (if (eq? n block-size)
                        (values #false this)
                        (lets ((eof-seen? tail (get-whole-block fd (- block-size n))))
                           (cond
                              ((eof-object? tail) (values #true this))
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
                     (mail thread (sys-port->non-blocking fd))
                     #true)
                  (begin
                     ;(interact sid 5) ;; delay rounds
                     (interact 'iomux (tuple 'read fd))
                     (loop rounds))))))

      (define socket-type-tcp 0)
      (define socket-type-udp 1)

      (define (open-tcp-socket port)
         (sys-port->non-blocking (sys-prim 3 port socket-type-tcp #false)))

      (define (open-udp-socket port)
         (sys-port->non-blocking (sys-prim 3 port socket-type-udp #false)))

      ;; port → (ip . bvec) | #false, nonblocking
      (define (check-udp-packet port)
         (sys-prim 10 port #f #f))

      (define (send-udp-packet sock ip port payload)
         (sys-prim 27 sock (cons port ip) payload))

      ;; port → (ip . bvec), blocks thread
      (define (wait-udp-packet port)
         (let ((res (check-udp-packet port)))
            (or res
               (begin
                  ;(interact sid socket-read-delay)
                  (interact 'iomux (tuple 'read port))
                  (wait-udp-packet port)))))

      ;; port → null | ((ip . bvec) ...)
      (define (udp-packets port)
         (let ((sock (open-udp-socket port)))
            (if sock
               (λ ()
                  (let loop ((sock sock))
                     (pair
                        (wait-udp-packet sock)
                        (loop sock))))
               null)))

      (define open-socket open-tcp-socket)

      ;;; TCP connections

      (define (open-connection ip port)
         (cond
            ((not (eq? (type port) type-fix+))
               #false)
            ((and (eq? (type ip) type-vector-raw) (eq? 4 (sizeb ip))) ;; silly old formats
               (sys-port->non-blocking (sys-prim 29 ip port socket-type-tcp)))
            (else
               ;; note: could try to autoconvert formats to be a bit more user friendly
               #false)))

      (define (udp-client-socket)
         (sys-prim 29 #f #f socket-type-udp))

      (define close-port sys-close)


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
            (close-port fd)
            (values r n)))

      ;; sock → #f #f | ip client
      (define (tcp-client sock)
         (let ((res (sys-prim 4 sock #false #false)))
            (if res
               (lets ((ip fd res))
                  (values ip (sys-port->non-blocking fd)))
               (begin
                  ;(interact sid socket-read-delay)
                  (interact 'iomux (tuple 'read sock))
                  (tcp-client sock)))))

      ;; port → ((ip . fd) ... . null|#false), CLOSES SOCKET
      (define (socket-clients sock)
         (lets ((ip cli (tcp-client sock)))
            (if ip
               (pair (cons ip cli) (socket-clients sock))
               null)))

      ;; port → ((ip . fd) ... . null|#false), CLOSES SOCKET
      (define (tcp-clients port)
         (let ((sock (open-tcp-socket port)))
            (if sock
               (λ () (socket-clients sock))
               #false)))

      ;; ip port (bvec ...) → #true n-written | #false error-sym
      (define (tcp-send ip port ll)
         (let ((fd (sys-prim 29 ip port socket-type-tcp)))
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
                  (write-really (raw (reverse out) type-vector-raw) fd)
                  (printer lst 0 null fd)))
            ((null? lst)
               (write-really (raw (reverse out) type-vector-raw) fd))
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

      (define print
         (case-lambda
            ((obj) (print-to stdout obj))
            (xs (printer (foldr render '(#\newline) xs) 0 null stdout))))

      (define write
         (H write-to stdout))

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
         (sys-write stdout str #f))

      (define (system-println str)
         (system-print str)
         (system-print "
      "))

      (define (system-stderr str) ; <- str is a raw or pre-rendered string
         (sys-write stderr str #f))

      ;;;
      ;;; Files <-> vectors
      ;;;

      ;; read all blocks for a port, all but possibly last one having input-block-size bytes
      (define (read-blocks port buff)
         (lets ((eof-seen? val (get-whole-block port input-block-size)))
            (cond
               (eof-seen?
                  (let ((buff (if (eof-object? val) buff (cons val buff))))
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
               ((eof-object? block)
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

      (define wait-write
         (C interact 'wait))

      (define (stream-chunk buff pos tail)
         (if (eq? pos 0)
            (cons (refb buff pos) tail)
            (lets ((next x (fx- pos 1)))
               (stream-chunk buff next
                  (cons (refb buff pos) tail)))))

      (define (sleep ms)
         (interact 'iomux (tuple 'alarm ms)))

      (define (block-stream fd tail?)
         (λ ()
            (let ((block (get-block fd stream-block-size)))
               (cond
                  ((eof-object? block)
                     (if tail?
                        (begin
                           ;; read does not block at eof, so wait explicitly
                           (sleep 1000)
                           (block-stream fd #true))
                        (begin
                           (close-port fd)
                           null)))
                  ((not block)
                     null)
                  (else
                     (cons block
                        (block-stream fd tail?)))))))

      ;; stream blocks close at eof
      (define port->block-stream
         (C block-stream #true))

      ;; stream blocks, wait for more at eof
      (define port->tail-block-stream
         (C block-stream #false))

      ;; include metadata symbols
      (define (port->meta-block-stream fd)
         (let loop ((block? #false))
            (let ((block (try-get-block fd stream-block-size block?)))
               (cond
                  ((eof-object? block)
                     (close-port fd)
                     null)
                  ((not block)
                     (list 'io-error))
                  ((eq? block #true) ;; will block
                     (pair 'block
                        (loop #true)))
                  (else
                     (pair block (loop #false)))))))

      (define (port->byte-stream fd)
         (ledit
            (λ (block ll)
               (stream-chunk block (- (sizeb block) 1) ll))
            (block-stream fd #f)))

      (define (port->tail-byte-stream fd)
         (ledit
            (λ (block ll)
               (stream-chunk block (- (sizeb block) 1) ll))
            (block-stream fd #t)))

      (define (port->meta-byte-stream fd)
         (ledit
            (λ (block ll)
               (if (vector? block)
                  (stream-chunk block (- (sizeb block) 1) ll)
                  (pair block ll)))
            (port->meta-block-stream fd)))

      (define (block-stream->port bs fd)
         (cond
            ((null? bs)
               #true)
            ((pair? bs)
               (if (write-really (car bs) fd)
                  (block-stream->port (cdr bs) fd)
                  #false))
            (else
               (block-stream->port (bs) fd))))

      (define (byte-stream->port bs fd)
         (let loop ((bs bs) (n stream-block-size) (out null))
            (cond
               ((eq? n 0)
                  (if (write-really (list->byte-vector (reverse out)) fd)
                     (loop bs stream-block-size null)
                     #false))
               ((pair? bs)
                  (loop (cdr bs) (- n 1) (cons (car bs) out)))
               ((null? bs)
                  (if (null? out)
                     #true
                     (loop bs 0 out)))
               (else
                  (loop (bs) n out)))))

      (define (byte-stream->lines ll)
         (let loop ((ll ll) (out null))
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
                  (λ ()
                     (loop (ll) out))))))

      (define (lines fd)
         (byte-stream->lines
            (utf8-decoder
               (port->byte-stream fd)
               (λ (self line ll) null))))

      (define (file->byte-stream path)
         (let ((fd (open-input-file path)))
            (if fd
               (port->byte-stream fd)
               #false)))

      (define (fasl-save obj path)
         (vector->file
            (list->vector (fasl-encode obj))
            path))

      (define (fasl-load path fail-val)
         (let ((bs (file->byte-stream path)))
            (if bs
               (fasl-decode bs fail-val)
               fail-val)))

      ;;; new io muxer thread

      (define (delelt lst x) ;; lst x →  lst' | #false if not there
         (let loop ((lst lst) (out null))
            (if (null? lst)
               out
               (lets ((a lst lst))
                  (if (eq? a x)
                     (append lst out)
                     (loop lst (cons a out)))))))

      ;; (... (x . foo) ...) x => (... ...) (x . foo)
      (define (grabelt lst x)
         (let loop ((lst lst) (out null))
            (if (null? lst)
               (values out #false)
               (let ((a (car lst)))
                  (if (eq? x (car a))
                     (values (append (cdr lst) out) a)
                     (loop (cdr lst) (cons a out)))))))

      ;; rs = ((read-fd . read-request-mail) ...)
      ;; ws = ((write-fd . id) ...)
      ;; as = ((wakeup-time . alarm-mail) ...), sorted by wakeup-time

      (define (remove-alarm alarms envelope)
         (if (null? alarms)
            (begin
               (print-to stderr "ERROR: fd read with timeout had no matching alarm")
               null)
            (let ((this (car alarms)))
               (if (eq? (cdr this) envelope)
                  (cdr alarms)
                  (cons this
                     (remove-alarm (cdr alarms) envelope))))))

      (define (remove-alarm-by-fd alarms fd)
         (remove
            (λ (alarm)
               ;; (timeout . #(thread #(read-timeout fd ms)))
               (lets
                  ((msg (cdr alarm))
                   (from req msg)
                   (op port ms msg))
                  (if (eq? port fd)
                     (print-to stderr " - found it"))
                  (eq? port fd)))
            alarms))

      ;; alarm-mail = #(sender #(read-timeout fd ms))

      (define (wakeup rs ws alarms fd reason)
         (cond
            ((eq? reason 1) ;; data ready to be read
               (lets
                  ((rs x (grabelt rs fd))
                   (fd envelope x)
                   (from message envelope))
                  (tuple-case message
                     ((read fd)
                        (mail from message)
                        (values rs ws alarms))
                     ((read-timeout fd timeout)
                        (mail from message)
                        (values rs ws
                           (remove-alarm alarms envelope)))
                     (else
                        (print-to stderr "wakeup: unknown wakeup " message)
                        (values rs ws alarms)))))
            ((eq? reason 2) ;; ready to be written to
               (lets ((ws x (grabelt ws fd))
                      (fd envelope x)
                      (from message envelope))
                  (mail from message)
                  (values rs ws alarms)))
            (else ;; error
               (lets ((rs x (grabelt rs fd))
                      (ws y (grabelt ws fd)))
                  (if x (mail (cdr x) fd))
                  (if y (mail (cdr y) fd))
                  (values rs ws
                     (remove-alarm-by-fd alarms fd))))))

      (define (push-alarm alarms time id)
         (if (null? alarms)
            (list (cons time id))
            (let ((a (car alarms)))
               (if (< (car a) time)
                  (cons a (push-alarm (cdr alarms) time id))
                  (cons (cons time id) alarms)))))

      ;; including time currently causes a circular dependency - resolve later
      (define (time-ms)
         (lets ((ss ms (clock)))
            (+ (* ss 1000) ms)))

      (define (muxer-add rs ws alarms mail)
         (tuple-case (ref mail 2)
            ((read fd)
               (values (cons (cons fd mail) rs) ws alarms))
            ((read-timeout fd ms)
               (values (cons (cons fd mail) rs) ws
                  (push-alarm alarms (+ (time-ms) ms) mail)))
            ((write fd)
               (values rs (cons (cons fd mail) ws) alarms))
            ((alarm ms)
               (values rs ws (push-alarm alarms (+ (time-ms) ms) mail)))
            (else
               (print-to stderr "bad muxer message from " (ref mail 1))
               (values rs ws alarms))))

      (define (muxer rs ws alarms)
         (if (null? alarms)
            ;; No alarms, just maybe IO and messages
            (let ((envelope ((if (and (null? rs) (null? ws)) wait-mail check-mail))))
               (if envelope
                  (lets ((rs ws alarms (muxer-add rs ws alarms envelope)))
                     (muxer rs ws alarms))
                  (lets
                     ((timeout (if (single-thread?) #false 0))
                      (waked x (_poll2 rs ws timeout)))
                     (cond
                        (waked
                           (lets ((rs ws alarms (wakeup rs ws alarms waked x)))
                              (muxer rs ws alarms)))
                        (x 3)
                        (else
                           (set-ticker 0)
                           (muxer rs ws alarms))))))
            (let ((now (time-ms)))
               ;;; alarms and next is not up yet
               (if (< now (caar alarms))
                  (let ((envelope (check-mail)))
                     (if envelope
                        (lets ((rs ws alarms (muxer-add rs ws alarms envelope)))
                           (muxer rs ws alarms))
                        (lets
                           ((timeout
                              (if (single-thread?) (min *max-fixnum* (- (caar alarms) now)) 0))
                            (waked x (_poll2 rs ws timeout)))
                           (cond
                              (waked
                                 (lets ((rs ws alarms (wakeup rs ws alarms waked x)))
                                    (muxer rs ws alarms)))
                              (x 2)
                              (else
                                 (set-ticker 0)
                                 (muxer rs ws alarms))))))
                  ;; the bell tolls
                  (lets
                     ((alarm (car alarms))
                      (time envelope alarm)
                      (id message envelope))
                     (tuple-case message
                        ((alarm ms)
                           (mail id 'alarm)
                           (muxer rs ws (cdr alarms)))
                        ((read-timeout fd ms)
                           ;; remove both the alarm and the read request
                           (mail id 'timeout)
                           (lets ((rs _ (grabelt rs fd)))
                              (muxer rs ws (cdr alarms))))
                        (else
                           (print-to stderr "not sure how to alarm " message)
                           (mail id 'alarm)
                           (muxer rs ws (cdr alarms)))))))))

      (define (start-muxer . id)
         (thread
            (if (null? id) 'iomux (car id))
            (muxer null null null)))

      ;; start normally mandatory threads (apart form meta which will be removed later)
      (define (start-base-threads)
         (start-muxer)
         (wait 1))

      (define (file-size path)
         (let ((port (open-input-file path)))
            (if port
               (let ((end (sys-seek-end port)))
                  (close-port port)
                  end)
               port)))
))
