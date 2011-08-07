;;;
;;; Owl math module, things after basic arithmetic
;;;

,r "owl/iff.l"
,r "owl/math.l"
,r "owl/sort.l"
,r "owl/list.l"
,r "owl/list-extra.l"
,r "owl/lazy.l"

(define-module lib-math-extra

   (export 
      isqrt sqrt
      expt expt-mod
      ncr npr 
      ! factor prime?
      primes-between
      totient divisor-sum divisor-count
      inv-mod mod-solve dlog dlog-simple
      fib
      )

   (import lib-math)
   (import lib-iff)
   (import lib-list)
   (import lib-list-extra)
   (import lib-iff)
   (import lib-sort)

   ;;;
   ;;; SQUARE ROOTS (stub)
   ;;;

   ; fixme, did not find a good integer sqrt algorithm which would
   ; work with these numerals, so i rolled my own as a quick substitute
   ; bench later

   ; move elsewhere and export, useful for benchmarking
   (define (nbits n f)
      (cond
         ((eq? n 0) f)
         ((teq? n fix+)
            (lets ((hi lo (fx>> n 1)))
               (nbits hi (nat-inc f))))
         (else
            (let ((tl (ncdr n)))
               (if (null? tl)
                  (nbits (ncar n) f)
                  (nbits tl (add f 16)))))))

   (define (isqrt-init n)
      (lets
         ((nb (nbits n 0))
          (val (<< 1 (sub (>> nb 1) 1))))
         (if (eq? (band nb 1) 0)
            val
            (lets ((val2 (<< val 1)) (sq (mul val2 val2)))
               (if (<= sq n) val2 val)))))

   (define (isqrt-fix hi bit n)
      (if (eq? bit 0)
         hi
         (lets ((this (bor hi bit)) (mid (mul this this)))
            (if (> mid n)
               (isqrt-fix hi (>> bit 1) n)
               (isqrt-fix this (>> bit 1) n)))))

   ; largest m where m^2 <= n
   (define (sqrt n)
      (cond
         ((teq? n fix-) (sub 0 (sqrt (sub 0 n))))
         ((teq? n int-) (sub 0 (sqrt (sub 0 n))))
         ((eq? n 0) 0)
         ((eq? n 1) 1)
         (else
            (let ((hi (isqrt-init n)))
               (isqrt-fix hi (>> hi 1) n)))))

   (define (isqrt n)
      (let ((sq (sqrt n)))
         (values sq (sub n (mul sq sq)))))


   ;;; exponentiation

   ; the usual O(lg n) exponentiation

   (define (expt-loop ap p out)
      (cond
         ((eq? p 0) out)
         ((eq? (band p 1) 0)
            (expt-loop (* ap ap) (>> p 1) out))
         (else
            (expt-loop (* ap ap) (>> p 1) (* out ap)))))

   (define (expt a b)
      (cond
         ((eq? b 0) 1)
         ((eq? b 1) a)
         ((eq? b 2) (* a a))
         ((teq? b fix+) (expt-loop a (sub b 1) a))
         ((teq? b int+) (expt-loop a (sub b 1) a))
         ((teq? b fix-) (/ 1 (expt a (negate b))))
         ((teq? b int-) (/ 1 (expt a (negate b))))
         (else (big-bad-args 'expt a b))))

   ; (mod (expt a b) m) = (expt-mod a b m)

   (define (expt-mod-loop ap p out m)
      (cond
         ((eq? p 0) (mod out m))
         ((eq? (band p 1) 0)
            (expt-mod-loop (rem (mul ap ap) m) (>> p 1) out m))
         (else
            (expt-mod-loop (rem (mul ap ap) m) (>> p 1) 
               (rem (mul out ap) m) m))))
         
   (define (expt-mod a b m)
      (cond
         ((eq? b 0) (mod 1 m))
         ((eq? b 1) (mod a m))
         (else
            (expt-mod-loop (rem a m) (sub b 1) a m))))

   ;;;
   ;;; PRIMES AND FACTORING
   ;;;

   ;; primality testing - miller-rabin

   ; n < 9,080,191, a = 31 and 73.
   ; n < 4,759,123,141, a = 2, 7, and 61.
   ; n < 2,152,302,898,747, a = 2, 3, 5, 7, and 11.
   ; n < 3,474,749,660,383, a = 2, 3, 5, 7, 11, and 13.
   ; n < 341,550,071,728,321, a = 2, 3, 5, 7, 11, 13, and 17.

   (define first-primes
      (list->ff 
         (map (lambda (x) (cons x x))
            '(2 3 5 7 11 13 17))))

   ; divide by 2 (shift 1) while even and count shifts
   (define (miller-qk q k)
      (if (eq? (band q 1) 0)
         (miller-qk (>> q 1) (+ k 1))
         (values q k)))

   (define (miller-rabin n x)
      (lets ((q k (miller-qk (- n 1) 0)))
         (let loop ((y (expt-mod x q n)) (j 0))
            (cond
               ((= j k) False)
               ((and (eq? j 0) (eq? y 1)) True)
               ((= y (- n 1)) True)
               ((and (> j 0) (= y 1)) False)
               (else (loop (expt-mod y 2 n) (+ j 1)))))))

   (define (miller-rabin-cases-ok? num tests)
      (fold
         (lambda (status a) (and status (miller-rabin num a)))
         True tests))

   (define assume-riemann-hypothesis? True)

   ; write n as 2^s*d by factoring out powers of 2 from n-1
   ; for all a in [2 .. min(n-1, floor(2*(ln n)^2))]
   ;      if a^d = 1 (mod n)
   ;         next a
   ;         loop r in [0, s-1]
   ;            if (a^(d<<r)) = n-1
   ;               next a
   ;             if out of r
   ;               return composite

   (define (factor-out-twos n)
      (let loop ((n n) (p 0))
         (if (eq? 0 (band n 1))
            (loop (>> n 1) (+ p 1))
            (values n p))))

   ; bound by using a rational approximation e-ish < e

   (define e-ish 25946/9545)   ; log e-ish = 0.999999998

   (define (ln+ n)   ; return a number >= floor(ln(n))
      (let loop ((a 1) (b 1) (p 0))
         (if (> (div a b) n)
            p
            (loop (* a 25946) (* b 9545) (+ p 1)))))

   (define (miller-rabin-det n)
      (lets
         ((np (- n 1))
          (d s (factor-out-twos np))
          (aover (min n (<< (expt (ln+ n) 2) 1))))
         (let loop ((a 2))
            (cond
               ((= a aover) True)
               ((= 1 (expt-mod a d n)) (loop (+ a 1)))
               (else
                  (let loopr ((r (- s 1)))
                     (cond
                        ((= r -1) False)   ; composite 
                        ((= (expt-mod a (<< d r) n) np) (loop (+ a 1)))
                        (else (loopr (- r 1))))))))))

   (define (prime? n)
      (cond
         ((eq? n 1) False)
         ((eq? n 2) True)
         ((eq? 0 (band n 1)) False)
         ((get first-primes n False) True)
         ((< n 1373653) (miller-rabin-cases-ok? n '(2 3)))
         ((< n 9080191) (miller-rabin-cases-ok? n '(31 73)))
         ((< n 4759123141) (miller-rabin-cases-ok? n '(2 7 61)))
         ((< n 2152302898747) (miller-rabin-cases-ok? n '(2 3 5 7 11)))
         ((< n 3474749660383) (miller-rabin-cases-ok? n '(2 3 5 7 11 13)))
         ((< n 341550071728321) (miller-rabin-cases-ok? n '(2 3 5 7 11 13 17)))
         (else (miller-rabin-det n))))

   ;; Atkin sieve 

   (define (atkin-flip ff num)
      (iput ff num (not (iget ff num False))))

   (define (between? a x b)
      (cond
         ((> a x) False)
         ((< b x) False)
         (else True)))

   ; later apply the knowledge about limits
   (define (atkin-candidates lo max)
      (let ((lim (sqrt max)))
         (let loox ((store False) (x 1))
            (if (> x lim)
               store
               (let looy ((store store) (y 1))
                  (if (> y lim)
                     (loox store (+ x 1))
                     ; eww, fix later
                     (lets
                        ((xx (* x x)) 
                         (yy (* y y))
                         (n (+ (* 4 xx) yy))
                         (nm (rem n 12))
                         (store
                           (if (and (between? lo n max) (or (eq? nm 1) (eq? nm 5)))
                              (atkin-flip store n)
                              store))
                         (n (+ (* 3 xx) yy))
                         (nm (rem n 12))
                         (store
                           (if (and (between? lo n max) (eq? nm 7))
                              (atkin-flip store n)
                              store))
                         (n (- n (<< yy 1))))
                        (if (and (> x y) 
                              (and (between? lo n max) (eq? (rem n 12) 11)))
                           (looy (atkin-flip store n) (+ y 1))
                           (looy store (+ y 1))))))))))

   (define (atkin-remove-duplicates-of store prime max)
      (let ((xx (* prime prime)))
         (let loop ((store store) (val xx))
            (cond
               ((> val max) store)
               ((iget store val False)
                  (loop (atkin-flip store val) (+ val xx)))
               (else
                  (loop store (+ val xx)))))))

   (define (atkin-remove-squares max store)
      (ifold
         (lambda (store prime v)
            (if v (atkin-remove-duplicates-of store prime max) store))
         store store))

   (define (atkin-try pows prime)
      (let loop ((n (car pows)) (these 0))
         (if (eq? n 1)
            (if (eq? these 0)   
               pows
               (cons 1 (cons (cons prime these) (cdr pows))))
            (let ((q (ediv n prime)))
               (cond
                  (q (loop q (+ these 1)))
                  ((eq? these 0) pows)
                  (else
                     (cons n    (cons (cons prime these) (cdr pows)))))))))
                     
   (define (atkin-apply store pows)
      (call/cc
         (lambda (done)
            (ifold
               (lambda (out k v)
                  (let ((res (atkin-try out k)))
                     (if (eq? (car res) 1)
                        (done res)
                        res)))
               pows store))))

   ;; primes in the range [lo .. hi] (inclusive)


   (define (atkin-primes-between lo hi)
      (cond
         ((> lo hi) null)
         ; 2 and 3 are special
         ((between? lo 2 hi) (cons 2 (atkin-primes-between 3 hi)))
         ((between? lo 3 hi) (cons 3 (atkin-primes-between 5 hi)))
         (else
            (sort <
               (ifold 
                  (lambda (out k v) (if v (cons k out) out))
                  null
                  (atkin-remove-squares hi
                     (atkin-candidates lo hi)))))))

   (define primes-between atkin-primes-between)

   (define (factor-atkin-between lo hi pows)
      (atkin-apply 
         (atkin-remove-squares hi
            (atkin-candidates lo hi))
         pows))

   (define (atkin-factor-driver pows lo)
      (let ((max (min (<< lo 1) (sqrt (car pows)))))
         (let ((pows (factor-atkin-between lo max pows)))
            (cond
               ((eq? (car pows) 1)   
                  (cdr pows))
               ((>= max (sqrt (car pows)))
                  (cons (cons (car pows) 1) (cdr pows)))
               (else
                  (atkin-factor-driver pows  max))))))

   ; fixme, try different options
   ;   - factor out twos first
   ;   - try low primes 
   ;   - more low primes
   ;  - quick prime? check (maybe miller-rabin (2 3 5))
   ;  - limited pollard-rho
   ;   - full trial division
   ;   - intermediate prime? checks

   (define (factor n)   
      (if (> n 1)
         (let 
            ((pows
               (fold atkin-try (list n)   
                  '(2 3 5 7 11 13 17 19 23 29 31))))
            (if (eq? (car pows) 1)
               (cdr pows)
               (atkin-factor-driver pows 32)))
         null))
   
            
   ;;;
   ;;; UNSORTED
   ;;;

   ; naive factorial

   (define (fact-iter n o)
      (if (eq? n 1)
         o
         (fact-iter (- n 1) (* o n))))

   (define (! n)
      (if (eq? n 0)
         1
         (fact-iter n 1)))

   ;;; npr, number of permutations, naively n!/(n-m)!

   (define (npr-loop n m o)
      (if (eq? m 0)
         o
         (npr-loop (- n 1) (- m 1) (* o n))))

   (define (npr n m)
      (if (eq? m 0)
         0
         (npr-loop (- n 1) (- m 1) n)))

   ;;; ncr, number of combinations, n choose m, simply n!/(m!(n-m)!)

   (define (ncr n m)
      (let ((mp (- n m)))
         (if (> m mp)
            (ncr n mp)
            (/ (npr n m) (! m)))))

   ; Euler's totient, aka phi

   ; phi(p) = p-1 when p is a prime
   ; phi(p^n) = (p-1) * p^(n-1)
   ; phi(ab) = phi(a) * phi(b) when gcd(a,b) = 1

   (define (totient n)
      (if (< n 2)
         1
         (fold
            (lambda (left factor) (- left (/ left (car factor))))
            n (factor n))))

   ; sum of divisors of n, A000203

   (define (divisor-sum num)
      (if (eq? num 1)
         1
         (fold
            (lambda (total factor)
               (* total
                  (/ (- (expt (car factor) (+ (cdr factor) 1)) 1)
                     (- (expt (car factor) 1) 1))))
            1 (factor num))))

   ; number of divisors of n, aka tau, sigma0, A000005
   (define (divisor-count n)
      (if (eq? n 1)
         1
         (fold 
            (lambda (out n) (* out (+ (cdr n) 1)))
            1 (factor n))))


   ;;;
   ;;; Discrete Logarithm
   ;;;

   ;; find ? such that (expt-mod a ? n) = y

   (define (dlp-naive y a n)
      (let loop ((x 0) (seen False))
         (let ((this (expt-mod a x n)))
            (cond
               ((= y this) x)
               ((iget seen this False) False) ; looped, not solvable
               (else (loop (+ x 1) (iput seen this True)))))))

   ;; like naive, but avoids useless multiplications and remainders 
   (define (dlp-simple y a n)
      (let loop ((x 0) (v 1) (seen False))
         (cond
            ((>= v n) (loop x (rem v n) seen))      ; overflow
            ((= v y) x)                             ; solved
            ((iget seen v False) False)             ; looped -> not solvable
            (else                                   ; try next
               (loop (+ x 1) (* v a) (iput seen v v))))))

   ;; like simple, but has O(1) space at the cost of ~1.5x time
   (define (dlp-th-step v a n)
      (let ((v (* a v)))
         (if (>= v n) (rem v n) v)))

   (define (dlp-th y a n)
      (if (= y 1)
         0
         (let loop ((x1 0) (v1 1) (x2 1) (v2 a) (step? False))
            (cond
               ((= v2 y) x2)                          ; hare finds carot \o/
               ((= v1 v2) False)                      ; hare finds tortoise o_O
               (step?                                 ; fast hare is fast
                  (loop x1 v1 (+ x2 1) (dlp-th-step v2 a n) False))
               (else                                    ; enhance
                  (loop 
                     (+ x1 1) (dlp-th-step v1 a n)
                     (+ x2 1) (dlp-th-step v2 a n) True))))))
  

   ;; Shanks' baby-step giant-step algorithm (still not quite working properly)

   (define (carless a b) (< (car a) (car b)))

   (define (find-match b g pred)
      (cond
         ((null? b) False)
         ((null? g) False)
         ((= (caar b) (caar g)) 
            (let ((x (- (cdar g) (cdar b))))
               (if (pred x)
                  x
                  (find-match (cdr b) (cdr g) pred))))
         ((< (caar b) (caar g)) (find-match (cdr b) g pred))
         (else (find-match b (cdr g) pred))))

   ;; a silly mod to avoid some remainders
   (define (bound x n)
      (if (< x n) x (mod x n)))

   ;; this can be done much more efficiently incrementally, but just testing for correctness now
   ;; todo: use incremental construction and an iff to check for matches

   (define (sqrt-ceil n)
      (let ((s (sqrt n)))
         (if (< (* s s) n)
            (+ s 1)
            s)))

   ;; y a n → x, such that y = a^x (mod n)
   (define (dlog-shanks y a n)
      (lets
         ((s (sqrt-ceil n))
          (baby
            (sort carless
               (map (λ (r) (cons (rem (* y (expt-mod a r n)) n) r)) ; (ya^r. r)
                  (iota 5 1 s)))
            ;(sort carless
            ;   (let loop ((ya (bound y n)) (r 0))
            ;      (if (= r s)
            ;         null
            ;         (cons (cons ya r) (loop (bound (* ya a) n) (+ r 1))))))
            )
          (giant
            (sort carless
               (map (λ (t) (cons (expt-mod a (* s t) n) (bound (* t s) n)))
                  (iota 1 1 (+ s 1))))))
         ;; i thought the match would be unique, but there seem to be many and some aren't solutions. not sure yet why.
         (find-match baby giant (λ (x) (= y (expt-mod a x n))))))

   (define dlog-simple dlp-th) ;; a simple reference implementation

   (define dlog dlog-shanks)


   ;;; Fibonacci numbers

   ;; n → f_n, f_n+1
   (define (fibs n)
      (cond
         ((eq? n 0) (values 1 1))
         ((eq? n 1) (values 1 2))
         (else
            (lets
               ((a b (fibs (- (>> n 1) 1)))
                (c (+ a b))
                (aa (* a a)) (bb (* b b)) (cc (* c c)))
               (if (eq? 0 (band n 1))
                  (values (+ aa bb) (- cc aa))
                  (values (- cc aa) (+ bb cc)))))))

   ;; one of the the relatively fast ways to compute fibonacci numbers
   (define (fib n)
      (if (< n 2)
         n
         (lets ((n sn (fibs (- n 1)))) n)))

)

