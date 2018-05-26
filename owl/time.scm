(define-library (owl time)

   (export
      elapsed-real-time
      timed
      time
      time-ms
      time-ns)

   (import
      (owl defmac)
      (owl io)
      (owl syscall)
      (owl math)
      (only (owl sys) clock_gettime CLOCK_REALTIME)
      (scheme write))

   (begin

      (define (time-ns)
         (clock_gettime (CLOCK_REALTIME)))

      (define (elapsed-real-time thunk)
         (display "timing: ")
         (lets
            ((ns (time-ns))
             (res (thunk)))
            (print (quotient (- (time-ns) ns) 1000000) "ms")
            res))

      (define-syntax timed
         (syntax-rules ()
            ((timed exp)
               (timed exp (quote exp)))
            ((timed exp comment)
               (lets
                  ((ns (time-ns))
                   (res exp))
                  (print*-to stderr
                     (list comment ": " (quotient (- (time-ns) ns) 1000000) "ms"))
                  res))))

      (define (time)
         (quotient (time-ns) 1000000000))

      (define (time-ms)
         (quotient (time-ns) 1000000))
))
