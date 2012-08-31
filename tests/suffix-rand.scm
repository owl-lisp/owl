;; test a suffix array

; note: end = minimum 

(define (lex-less? vec a b)
   (let ((end (vec-len vec)))
      (let loop ((a a) (b b))
         (cond
            ((= a end) #true)
            ((= b end) #false)
            (else
               (let ((av (vec-ref vec a)) (bv (vec-ref vec b)))
                  (cond
                     ((< av bv) #true)
                     ((= av bv) (loop (+ a 1) (+ b 1)))
                     (else 
                        #false))))))))

(define (naive-array data)
   (list->vector
      (sort 
         (λ (a b) (lex-less? data a b))
         (iota 0 1 (vec-len data)))))

(define (try rst maxlen alphafact)
   (lets
      ((rst len (rand rst maxlen))
       (rst alpha (rand rst (ceil (* alphafact len))))
       (rst nums (random-numbers rst alpha len))
       (data (list->vector nums))
       (sarr (suffix-array data))
       (correct (naive-array data)))
      (if (equal? sarr correct)
         rst
         (begin
            (print "BADNESS: data " data)
            (print " - suffix-array " sarr)
            (print " - correct      " correct)
            'bug))))

(let loop ((rst (seed->rands (expt (time-ms) 4))) (n 0))
   (if (< n 10) 
      (begin
         (print n)
         (loop (try rst 1024 2) (+ n 1)))))

