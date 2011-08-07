;;;
;;; Thread-based IO
;;;

;; todo: bidirectional threads (mainly tcp connections) don't sleep and take too much code

,r "owl/queue.l"
,r "owl/vector.l"

(define-module lib-io

   (export 
      open-output-file        ;; path → thread-id | False
      open-input-file         ;; path → thread-id | False
      open-socket             ;; port → thread-id | False
      open-connection         ;; ip port → thread-id | False
      start-output-thread     ;; fd source → thread-id
      start-input-thread      ;; fd source → thread-id
      fd->id                  ;; file descriptor → thread id
      id->fd                  ;;
      fd?                     ;; _ → bool
      flush-port              ;; fd → _
      close-port              ;; fd → _
      start-sleeper           ;; start a (global) sleeper thread
      sleeper-id              ;; id of sleeper thread
      start-base-threads      ;; start stdio and sleeper threads
      wait-write              ;; fd → ? (no failure handling yet)

      file->vector            ;; vector io, may be moved elsewhere later
      vector->file
      write-vector            ;; vec port
      port->byte-stream       ;; fd → (byte ...) | thunk 
      render fd? 
      fd->id id->fd
   ) 

   (define (fd? x) (eq? (type x) 98)) 
   (define (fd->id fd) (cast fd 12)) 
   (define (id->fd id) (cast id 0))

   (define render
      (λ (self obj tl)
         (if (fd? obj)
            (cons 80 (render-number (id->fd obj) tl 10))
            (render self obj tl))))

   (import lib-queue)
   (import lib-vector merge-chunks vec-leaves) ; needed TEMPORARIly

   ;; use type 12 for fds 

   (define (fclose fd)
      (sys-prim 2 fd F F))

   (define (fopen path mode)
      (cond
         ((c-string path) => 
            (λ (raw) (sys-prim 1 raw mode F)))
         (else False)))

   ;; special way to send messages to stderr directly bypassing the normal IO, which we are implementing here
   (define-syntax debug-not
      (syntax-rules ()
         ((debug . stuff)
            (system-stderr
               (list->vector
                  (foldr renderer '(10) (list "IO: " . stuff)))))))

   (define-syntax debug
      (syntax-rules ()
         ((debug . stuff) True)))

   ;;;
   ;;; File descriptor thread id type 
   ;;;

   ;; moved to beginning of repl for printing purposes
   ;(define (fd->id fd) (cast fd 12))
   ;(define (id->fd id) (cast id 0))
   ;(define (fd? x) (eq? (type x) 98))

   ;; use fd 65535 as the unique sleeper thread name.

   (define sid (fd->id 65535))

   (define sleeper-id sid)

   ;;;
   ;;; Writing threads
   ;;;

   ;; writing thread has a fd and handles all actual IO operations on it. each 
   ;; message sent to it is (atomically) written to the fd in order of received 
   ;; messages. the normal thread mailbox acts as the input buffer when the thread 
   ;; has collected a chunk of data and is trying to write it to the fd. 

   ;; #[0 1 .. n .. m] n → #[n .. m]
   (define (bvec-tail bvec n)
      (raw (map (lambda (p) (refb bvec p)) (iota n 1 (sizeb bvec))) 11 False))

   (define (write-really bvec fd)
      (let ((end (sizeb bvec)))
         (let loop ()
            (let ; ((wrote (fsend fd bvec start end))) ;; vm calls write()
               ((wrote (sys-prim 0 fd bvec end)))
               (cond
                  ((eq? wrote end)
                     True) ;; ok, wrote the whole chunk
                  ((eq? wrote 0) ;; 0 = EWOULDBLOCK
                     (interact sid 2) ;; fixme: adjustable delay rounds 
                     (loop))
                  (wrote ;; partial write
                     (write-really (bvec-tail bvec wrote) fd))
                  (else False)))))) ;; write error or other failure

   ;; how many bytes (max) to add to output buffer before flushing it to the fd
   (define output-buffer-size 4096)

   ;; pack bytes to a raw chunk of memory and start trying to write() it
   (define (flush-output-buffer buff len fd)
      (let ((bvec (raw (reverse buff) 11 False)))
         (write-really bvec fd)))

   ;; fixme: autoflush here on newline
   (define (push-output buff len x fd)
      (cond
         ((eq? len output-buffer-size)
            (flush-output-buffer buff len fd)
            (push-output null 0 x fd))
         ((null? x) 
            (values buff len))
         ((pair? x)
            (push-output (cons (car x) buff) (+ len 1) (cdr x) fd))
         (else
            (error "bad fd input: " x)
            (values buff len))))

   (define (i x) x)
  
   ;; fixme: add a 2-byte jrt instruction. i needed here to force a function call to make the jump fit one byte.
   ;; fixme: switch chunk size selection to favor larger blocks when there is data going 
   (define (make-writer fd source)
      (let loop ((buff null) (len 0))
         (bind (wait-mail)
            (λ (from msg)
               (cond
                  ((pair? msg)
                     (lets 
                        ((flush? (has? msg 10))
                         (buff len (push-output buff len msg fd)))
                        (if flush?
                           (begin
                              (flush-output-buffer buff len fd)
                              (loop null 0))
                           (loop buff len))))
                  ((teq? msg (raw 11)) ;; send a pre-chunked byte vector
                     (if (pair? buff)
                        (flush-output-buffer buff len fd))
                     (write-really msg fd)
                     (loop null 0))
                  ((eq? msg 'wait) ; someone has been waiting for output to finish
                     (mail from 'sync)
                     (loop buff len))
                  ((eq? msg 'flush)
                     (if (pair? buff)
                        (begin
                           (flush-output-buffer buff len fd)
                           (loop null 0))
                        (loop buff len)))
                  ((eq? (i msg) 'close) ;; force function call to work around an issue while switcing opcodes
                     ;; all messages already handled from the inbox -> close at correct position
                     (if (pair? buff)
                        (flush-output-buffer buff len fd))
                     (fclose fd)
                     'closed) ;; <- exit thread
                  ((eq? msg 'info)
                     (mail from (tuple 'write source fd))
                     (loop buff len))
                  ((number? msg) ;; write just one byte
                     (lets ((buff len (push-output buff len (list msg) fd)))
                        (loop buff len)))
                  (else
                     ;; this is an error. later system-print.
                     (loop buff len)))))))

   (define (start-output-thread fd source)
      (let ((id (fd->id fd)))
         (fork-server id (λ () (make-writer fd source)))
         id))

   (define (open-output-file path)
      (let ((fd (fopen path 1)))
         (if fd (start-output-thread fd path) False)))



   ;;;
   ;;; Reading threads
   ;;;

   ;; read in fairly small blocks, mainly because this is the vector leaf node size, 
   ;; so the chunks of memory returned fread can directly be used to construct a 
   ;; vector out of the contents of a file.

   (define (send-next-input thread fd block-size)
      (let loop ((rounds 0)) ;; count for temporary sleep workaround
         (debug "reader: reading fd " fd " for " thread)
         (let ((res (sys-prim 5 fd block-size 0)))
            (cond
               ((eq? res True) ;; would block
                  (debug "reader: fd " fd " has no data, sleeping")
                  (interact sid 5)
                  (loop rounds)) ;; delay rounds not used atm
               (else ;; is False, eof or bvec
                  (debug "reader: read " res " from fd " fd ", sending to " thread)
                  (mail thread res)
                  True)))))

   (define (make-reader fd source)
      (let loop () ;; how many rounds 
         (debug "reader: reader thread " source " (fd " fd ") waiting for requests.")
         (bind (wait-mail)
            (λ (from msg)
               (cond
                  ((eq? msg 'input) ;; input request
                     (debug "reader: thread " from " asks for input from reader " source " (fd " fd ")")
                     (if (send-next-input from fd 256) ;; vectors need 256
                        (loop)
                        (begin
                           (debug "reader: read from fd " fd " for " from " failed. terminating reader.")
                           (mail from False) ;; fixme: does a response even make sense
                           (error "read error " (list 'fd fd 'from source)))))
                  ((teq? msg fix+) ;; 0-65535, read max n+1 bytes, being 1-65536
                     (debug "reader: thread " from " asks for input with max size " msg " from reader " source " (fd " fd ")")
                     (if (send-next-input from fd (+ msg 1))
                        (loop)
                        (begin
                           (debug "reader: read from fd " fd " for " from " failed. terminating reader.")
                           (mail from False) ;; fixme: does a response even make sense
                           (error "read error " (list 'fd fd 'from source)))))
                  ((eq? msg 'close)
                     (debug "reader: thread " from " told me to close.")
                     (fclose fd)
                     'closed)
                  ((eq? msg 'info) 
                     (debug "reader: thread " from " asked my info.")
                     (mail from (tuple 'read source fd))
                     (loop))
                  (else
                     (debug "reader: ERROR - thread " from " send bad mail " msg)
                     ;; later print warning. flush requests may be common.
                     (loop)))))))

   (define (start-input-thread fd source)
      (let ((id (fd->id fd)))
         (fork-server id (λ () (make-reader fd source)))
         id))

   (define (open-input-file path)
      (let ((fd (fopen path 0)))
         (if fd (start-input-thread fd path) False)))



   ;;;
   ;;; Bidirectional channels
   ;;;

   ;; read/write/check for messages non-blockingly. this is a bit less 
   ;; trivial than it sounds.

   ;; if write in progress, try it
   ; ws fd → ws' | False
   (define (maybe-write ws fd)
      (and ws
         (lets 
            ((bvec pos ws)
             (wrote (sys-prim 0 fd bvec (sizeb bvec))) ;; fixme: partial writes not handled
             ; old: (wrote (fsend fd bvec pos (sizeb bvec)))
             )
            (cond
               ((eq? wrote (- (sizeb bvec) pos)) ;; wrote all -> no write op left
                  False)
               (wrote (cons bvec (+ pos wrote)))
               (else ws)))))

   ;; add given non-input request to buffer with output data and events
   ;; output buffer = queue of bytes, bvecs, 'close or #(from sync|info)
   (define (add-output outq env)
      (lets ((from msg env))
         (cond
            ((null? msg) outq) ;; blank write is ok
            ((pair? msg) ;; add a list of bytes to output queue
               (for outq msg (λ (outq byte) (qsnoc byte outq))))
            ((teq? msg (raw 11)) ;; pre-chunked raw byte vector
               (qsnoc msg outq))
            ((eq? msg 'info) (qsnoc env outq))
            ((eq? msg 'close) (qsnoc env outq))
            ((eq? msg 'sync) (qsnoc env outq))
            (else outq)))) ;; fixme: should at least carp something to stderr

   ;; add envelope, if any, to input or output queue
   (define (maybe-add-mail inq outq env)
      (if env
         (if (eq? (ref env 2) 'input)
            (values (qsnoc (ref env 1) inq) outq)
            (values inq (add-output outq env)))
         (values inq outq)))

   ;; if input requests, possibly respond to the first one if data is available
   (define (maybe-read inq fd)
      (if (qnull? inq)
         inq
         (let ((res (sys-prim 5 fd 256 fd)))
            (cond
               ((eq? res True) ;; would block, read nothing
                  inq)
               (else
                  (lets ((thread inq (quncons inq F)))
                     (mail thread res) ; False, eof or bvec
                     inq))))))

   ;; (n ... c b a) → raw byte vector #[a b c ... n] (max len 65536)

   (define (chunk-buffer rbs p)
      (if (null? rbs)
         False
         (tuple (raw (reverse rbs) 11 False) 0)))

   (define (select-buffer outq rbs p info)
      (cond
         ((qnull? outq)
            (if (null? rbs)
               (values outq False)
               (values outq (chunk-buffer rbs p)))) ;; note: auto-flushes 
         ((eq? p output-buffer-size)
            ;; start writing this suitably large chunk of data.
            (values outq (chunk-buffer rbs p)))
         (else
            (lets ((b outq-tl (quncons outq F)))
               (cond
                  ((teq? b fix+)
                     ;; add a byte to output queue
                     (select-buffer outq-tl (cons b rbs) (+ p 1) info))
                  ((tuple? b)
                     (if (null? rbs)
                        (lets ((from req b))
                           (cond
                              ((eq? req 'wait)
                                 (mail from 'sync)
                                 (select-buffer outq-tl rbs p info))
                              ((eq? req 'info)
                                 (mail from info)
                                 (select-buffer outq-tl rbs p info))
                              ((eq? req 'close)
                                 ;; 'close is caught in the io-loop 
                                 (values outq req))
                              (else
                                 ;; bad request
                                 (mail from False)
                                 (select-buffer outq-tl rbs p info))))
                        ;; usually must get rid of the remaining data before handling the request
                        (values outq (chunk-buffer rbs p))))
                  ((teq? b (raw 11)) ;; a pre-chunked chunk
                     (if (null? rbs)
                        ;; can use as such, just add starting position
                        (values outq-tl (tuple b 0))
                        ;; must flush something out of the way first
                        (values outq (chunk-buffer rbs p))))
                  ((eq? b 'close) ;; drop and let caller close the port
                     (values outq-tl b))
                  (else
                     ;; fixme: stderr carp, now just drop
                     (values outq-tl (chunk-buffer rbs p))))))))
                  

   ;; pop messages from output queue and build a bvec to write
   (define (maybe-select-bvec outq ws info)
      (if ws ;; something already being written
         (values outq ws))
         (select-buffer outq null 0 info))

   ;; fixme: interact with sid if both directions are stuck waiting
   (define (make-bidirectional fd source)

      (define info (tuple 'bidirectional source fd))
   
      (define (io-step inq outq ws env)
         (set-ticker 0)
         (lets 
            ((ws (maybe-write ws fd))
             (inq outq (maybe-add-mail inq outq env))
             (inq (maybe-read inq fd))
             (outq ws (maybe-select-bvec outq ws info)))
            (cond
               ((eq? ws 'close) 
                  ;; there has been a fd close request and all writes received before it have finished
                  (fclose fd)
                  (debug "XXXXXXXXXXXXXXXXXXXXXXXX io thread shutting down")
                  'closed)
               (ws ;; running
                  (io-step inq outq ws (check-mail)))
               ((and (qnull? inq) (qnull? outq))
                  ;; no io going on, so block waiting for mail
                  (io-step inq outq ws (wait-mail)))
               (else
                  ;; input or output 
                  (io-step inq outq ws (check-mail))))))

      (io-step qnull qnull False (wait-mail)))


   (define (start-bidirectional-thread fd source)
      (let ((id (fd->id fd)))
         (fork-server id (λ () (debug "MAKE-BIDIRECTIONAL STARTED FOR " id) (make-bidirectional fd source) (debug "MAKE-BIDIRECTIONAL EXITED " id)))
         id))




   ;;;
   ;;; TCP sockets
   ;;;

   (define (send-next-connection thread fd)
      (let loop ((rounds 0)) ;; count for temporary sleep workaround
         (let ((res (sys-prim 4 fd F F)))
            (if res ; did get connection
               (lets ((ip fd res))
                  (mail thread (start-bidirectional-thread fd ip))
                  True)
               (begin
                  (interact sid 5) ;; delay rounds
                  (loop rounds))))))
                  
   (define (make-server fd source)
      (let loop () ;; how many rounds 
         (bind (wait-mail)
            (λ (from msg)
               (cond
                  ((eq? msg 'accept) ;; request a connection
                     (if (send-next-connection from fd)
                        (loop)
                        (begin
                           (mail from False) ;; fixme: does a response even make sense
                           (error "socket read error" (list 'fd fd 'port source)))))
                  ((eq? msg 'close)
                     (fclose fd)
                     'closed)
                  ((eq? msg 'info) 
                     (mail from (tuple 'socket source fd))
                     (loop))
                  (else
                     ;; later print warning. flush requests may be common.
                     (loop)))))))

   (define (start-socket-thread fd source)
      (let ((id (fd->id fd)))
         (fork-server id (λ () (make-server fd source)))
         id))

   (define (open-socket port)
      (let ((sock (sys-prim 3 port F F)))
         (if sock 
            (start-socket-thread sock port)
            False)))


   ;;;
   ;;; TCP connections
   ;;;

   (define (start-socket-thread fd source)
      (let ((id (fd->id fd)))
         (fork-server id (λ () (make-server fd source)))
         id))

   (define (open-connection ip port)
      (cond
         ((not (teq? port fix+))
            False)
         ((and (teq? ip (raw 11)) (eq? 4 (sizeb ip))) ;; silly old formats
            (let ((fd (_connect ip port)))
               (if fd
                  (start-bidirectional-thread fd (tuple 'tcp ip port))
                  False)))
         (else 
            ;; note: could try to autoconvert formats to be a bit more user friendly
            False)))


   ;;;
   ;;; Sleeper thread
   ;;;

   ;; run thread scheduler for n rounds between possibly calling vm sleep()
   (define sleep-check-rounds 10)
   (define ms-per-round 2)

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
         (if (teq? n fix+)
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
      ;; start sleeper thread (used by the io)
      (start-sleeper)
      ;; start stdio threads
      (start-input-thread  0 "stdin")
      (start-output-thread 1 "stdout")
      (start-output-thread 2 "stderr")
      ;; wait for them to be ready (fixme, should not be necessary later)
      (wait 2)
      )

   (define (flush-port fd)
      (mail fd 'flush))

   (define (close-port fd)
      (flush-port fd)
      (mail fd 'close))


   ;;; 
   ;;; Files <-> vectors
   ;;;

   (define (read-blocks port buff last-full?)
      (let ((val (interact port 'input)))
         (cond
            ((eof? val)
               (merge-chunks
                  (reverse buff)
                  (fold + 0 (map sizeb buff))))
            ((not val)
               False)
            (last-full?
               (read-blocks port
                  (cons val buff)
                  (eq? (sizeb val) 256)))
            (else
               ;(show "read-blocks: partial chunk received before " val)
               False))))

   (define (file->vector path) ; path -> vec | False
      (let ((port (open-input-file path)))
         (if port
            (let ((data (read-blocks port null True)))
               (close-port port)
               data)
            (begin
               ;(show "file->vector: cannot open " path)
               False))))

   ;; write each leaf chunk separately (note, no raw type testing here -> can fail)
   (define (write-vector vec port)
      (let loop ((ll (vec-leaves vec)))
         (cond
            ((pair? ll)
               (mail port (car ll))
               (loop (cdr ll)))
            ((null? ll) True)
            (else (loop (ll))))))

   ;; fixme: no way to poll success yet. last message should be ok-request, which are not there yet.
   ;; fixme: detect case of non-bytevectors, which simply means there is a leaf which is not of type (raw 11)
   (define (vector->file vec path)
      (let ((port (open-output-file path)))
         (if port
            (let ((outcome (write-vector vec port)))
               (close-port port)
               outcome)
            False)))

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
         (let ((buff (interact fd 'input)))
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

   (define (file->byte-stream path)
      (let ((fd (open-input-file path)))
         (if fd
            (port->byte-stream fd)
            False)))
)

