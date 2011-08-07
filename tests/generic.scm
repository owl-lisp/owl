; depend on order, and do something with zeroes

(define (step a b)
   (+ a (* (+ b 1) 2)))

(define (inc a) (+ a 1))

(define (test rst)
   (lets
      ((rst alpha (rnd rst 512)) ;; leaves having fixnums 0-255 are special in vectors
       (rst n (rnd rst 1000))
       (rst lst (random-numbers rst alpha n))
       (vec (list->vector lst))
       (str (list->string lst)))
      (call/cc
         (λ (ret)
            (let ((fail (λ (why) (print* (list "error -- " why ": lst " lst)) 'bug)))

               ;; fold
               (let ((fold-ok (fold step 0 lst)))
                  (cond
                     ((not (equal? fold-ok (fold step 0 str))) (fail "string fold"))
                     ((not (equal? fold-ok (fold step 0 vec))) (fail "vector fold"))))

               ;; foldr
               (let ((foldr-ok (foldr step 0 lst)))
                  (cond
                     ((not (equal? foldr-ok (foldr step 0 str))) (fail "string foldr"))
                     ((not (equal? foldr-ok (foldr step 0 vec))) (fail "vector foldr"))))
               
               ;; len
               (cond
                  ((not (= n (len lst))) (fail "list length"))
                  ((not (= n (len str))) (fail "string length"))
                  ((not (= n (len vec))) (fail "vector length")))

               ;; map
               (lets
                  ((map-ok (map inc lst))
                   (res-ok (fold step 0 map-ok)))
                  (cond
                     ((not (equal? res-ok (fold step 0 (map inc str)))) (fail "string map"))
                     ((not (equal? res-ok (fold step 0 (map inc vec)))) (fail "vector map"))))

               ;; cat
               (lets
                  ((rst pre (rnd rst 2))
                   (rst alpha (rnd rst 512))
                   (rst n (rnd rst 1000))
                   (rst lstp (random-numbers rst alpha n))
                   (vecp (list->vector lstp))
                   (strp (list->string lstp))
                   (ok (fold step 0 (append lst lstp)))
                   (op (if (eq? pre 0) cat (λ (a b) (cat b a)))))
                  (cond
                     ((not (equal? ok (fold step 0 (op lst lstp)))) (fail (list "cat list with " lstp)))
                     ((not (equal? ok (fold step 0 (op vec vecp)))) (fail (list "cat vec with " lstp)))
                     ((not (equal? ok (fold step 0 (op str strp)))) (fail (list "cat str with " lstp)))))

               ;; rev
               (lets
                  ((rev-ok (reverse lst))
                   (res-ok (fold step 0 rev-ok)))
                  (cond
                     ((not (equal? res-ok (fold step 0 (rev str)))) (fail "string rev"))
                     ((not (equal? res-ok (fold step 0 (rev vec)))) (fail "vector rev"))))
               
               rst)))))


(fold
   (λ (rst n) (print n) (test rst))
   (seed->rands (time-ms))
   (iota 0 1 10))
