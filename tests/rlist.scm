
(import
   (only (owl rlist) rcons rget rset rlist) ;; an O(log n) library
   (only (owl sys) get-heap-bytes-written))

;; compare against O(n)
; (define rcons cons)
; (define rget lget)
; (define rset lset)
; (define rlist (lambda x x))
   
(define (make-rlist n)
   (let loop ((rl (rlist)) (n (- n 1)))
      (if (eq? n -1)
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

(define (get-all rl n)
   (if (eq? n -1)
      rl
      (let ((val (rget rl n #f)))
         (if (and val (= n val))
            (get-all rl (- n 1))
            (error "rlist value error" (cons n val))))))

(define (set-all rl n)
   (if (eq? n -1)
      rl
      (let ((rl (rset rl n n)))
         (get-all rl (- n 1)))))

(define (repeat thunk n)
   (let ((val (thunk)))
      (if (eq? n 1)
         val
         (repeat thunk (- n 1)))))

(define (run-test size print?)
   (lets 
      ((rl make-ns make-alloc (apply-measured make-rlist size))
       (rl ref-ns ref-alloc (apply-measured get-all rl (- size 1)))
       (rl set-ns set-alloc (apply-measured set-all rl (- size 1))))
      (if print?
         (begin
            ;(print "generation: " make-ns "ns")
            ;(print "generation: " make-alloc "Kw")
            (print size " gen: " (round (/ make-ns 1000000)) "ms")
            (print size " gen: " (quotient make-ns size) "ns/elem")
            (print size " gen: " (quotient make-alloc size) "W/elem")
            (print size " ref: " (round (/ ref-ns 1000000)) "ms")
            (print size " ref: " (quotient ref-ns size) "ns/elem")
            (print size " ref: " (quotient ref-alloc size) "W/elem")
            (print size " set: " (round (/ set-ns 1000000)) "ms")
            (print size " set: " (quotient set-ns size) "ns/elem")
            (print size " set: " (quotient set-alloc size) "W/elem")
            ))
     0))

; (run-test 100 #f)

(λ (args)
   ;; bin/vm fasl/boot.fasl --run tests/rlist.scm 100 1000 10000 100000 1000000 10000000
   ;; note: since the data structure should scale logarithmically, one could by
   ;; default double the size at each step and run up to a maximum time or heap size
   (map
      (λ (arg)
         (run-test (string->integer arg) #t))
      (cdr args))
   0)
