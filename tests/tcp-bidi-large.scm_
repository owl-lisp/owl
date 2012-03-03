;; (socket . port)

; ,r "lib/io.scm" (import lib-io)

(define block-size 64)     ;; todo: must add block-size support the channel and vary this above it 
(define data-length 1000)  ;; how much data to transfer

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

(define seed (* (time-ms) (<< (time-ms) 4)))

; (show "Random seed: " seed)

;; data to be sent to both directions
(define data
   (lets
      ((rst (seed->rands seed))
       (rst nums (random-numbers rst 256 data-length)))
      nums))

;; pick a prefix of lst to a vector or a list
;; rst lst → rst' prefix tail, where prefix is a list or a vector
(define (pick-data rst lst)
   (lets
      ((rst n (rand rst block-size))
       (rst vec (rand rst 2))
       (hd (take lst n))
       (tl (drop lst n)))
      (values rst (if (eq? vec 0) hd (list->vector hd)) tl)))

(define (explode bvec tail)
   (append (vec->list bvec) tail))

;; send a stream of known data in chunks, while receiving and and flushing randomly and asynchronously
;; fd rst data → (bvec ...)
(define (io fd rst data left reqs)
   (lets ((rst n (rand rst 4)))
      (cond
         ((and (eq? n 0) (> left 0) (eq? reqs 0)) ;; request data if some is unread and unrequested
            (lets ((rst n (rand rst block-size)))
               ;(mail fd n) ;; request n+1 bytes
               (mail fd 'input) ;; request n+1 bytes
               (io fd rst data left (+ reqs 1))))
         ((eq? n 1) ;; check for arrived data
            (lets ((envelope (check-mail)))
               (if envelope
                  (lets 
                     ((from msg envelope)
                      (reqs (- reqs 1)))
                     (if (eof? msg)
                        (io fd rst data left reqs)
                        (explode msg (io fd rst data (- left (vec-len msg)) reqs))))
                  ;; you have no new email
                  (io fd rst data left reqs))))
         ((and (eq? n 2) (pair? data));; send some data, if left
            (lets ((rst prefix data (pick-data rst data)))
               (mail fd prefix)
               (io fd rst data left reqs)))
         ((eq? n 3) ;; idle for a few thread ticks
            (lets ((rst n (rand rst 2)))
               (wait n)
               (io fd rst data left reqs)))
         ((and (null? data) (= left 0)) ;; all sent and enough received
            (let loop ((reqs reqs)) ;; read responses to remaining requests 
               (if (= reqs 0) 
                  null ;; all done
                  (let ((env (wait-mail)))
                     (if (eof? (ref env 2)) ;; check that we get an eof now
                        (loop (- reqs 1))
                        (error "bad finale message: " env))))))
         (else
            (io fd rst data left reqs)))))

(define rst (seed->rands seed))

(define (compare-bidi fd rst)
   (let ((received-data (io fd rst data (length data) 0)))
      (print (if (equal? received-data data) "Received correct data" "Bad data"))
      ;; leave running to get the possibly queued up responses from fd thread
      (close-port fd)
      (let loop () (wait-mail) (loop))))

(lets
   ((rs (seed->rands (* 123456789 seed)))
    (rs a (rand rs #xffffffffffffff))
    (rs b (rand rs #xffffffffffffff)))

   (fork-server 'socket-thread
      (λ ()
         (let ((cli (interact sock 'accept)))
            (compare-bidi cli (seed->rands a))
            )))

   (let ((conn (open-connection (vector 127 0 0 1) port)))
      (if conn
         (compare-bidi conn (seed->rands b))
         (print "Unable to connect to the local server"))))

