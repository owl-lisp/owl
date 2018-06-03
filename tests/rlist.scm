
(import
   (owl rlist)
   (only (owl sys) get-heap-bytes-written))

(define (make-rlist n)
   (let loop ((rl (rlist)) (n n))
      (if (eq? n 0)
         rl
         (loop (rcons n rl) (- n 1)))))

;; func args → result n-ns n-alloc
(define (apply-measured func . args)
   (lets
      ((start (time-ns))
       (astart (get-heap-bytes-written))
       (result (apply func args))
       (aend (get-heap-bytes-written)))
      (values
         result
         (- (time-ns) start)
         (- aend astart))))

(define (run-test size print?)
   (lets ((rl make-ns make-alloc (apply-measured make-rlist size)))
      (if print?
         (begin
            ;(print "generation: " make-ns "ns")
            ;(print "generation: " make-alloc "Kw")
            (print size ": " (quotient make-ns size) "ns/elem")
            (print size ": " (quotient make-alloc size) "W/elem")
            ))
     0))

(run-test 100 #f)

(λ (args)
   ;; bin/vm fasl/boot.fasl --run tests/rlist.scm 100 1000 10000 100000 1000000 10000000
   ;; note: since the data structure should scale logarithmically, one could by
   ;; default double the size at each step and run up to a maximum time or heap size
   (map
      (λ (arg)
         (run-test (string->integer arg) #t))
      (cdr args))
   0)
