;;;
;;; a pseudorandom number generator
;;;

;; todo: alternative distributions
;; note - we use mainly primop math here, so this may look a bit odd

(define-library (owl random)

   (export   
      ;; prngs
      lcg-rands           ;; seed (int32) → rands
      
      ;; stream construction
      seed->rands         ;; seed → ll of (digit ...) ;; only the default one, later also merseinne twister, blum blum shub etc alternatives
      rands->bits         ;; (digit ...) → (0|1 ...)
      seed->bits          ;; seed → (bit ...)
      rands->bytes
      seed->bytes

      ;; stream functions
      rand                 ;; rs max → rs' n, 0 <= n < max
      rand-nbit            ;; rs n → rs' i
      rand-log
      rand-elem            ;; rs obj → rs' elem (for lists and vectors)
      rand-subset
      rand-range

      random-numbers      ;; rs x max x i -> rs' (n_1 .. n_i), as in rand
      reservoir-sample    ;; rs x ll x n -> lst', |lst'| <= n
      shuffle             ;; rs x lst -> rs' lst'
      random-permutation  ;; rs x lst -> rs' lst'
      random-subset       ;; rs x lst -> rs' lst' <- lst, same order, each element has 50% chance to be included
      rand-elem           ;; rs x thing -> rs' x element (for some data types)
      rand-occurs?        ;; rs → rs' T|F
      )

   (import
      (owl math)
      (owl lazy)
      (owl list)
      (only (owl syscall) error)
      (owl tuple)
      (owl vector)
      (owl list-extra)
      (owl rlist)
      (owl io)
      (owl syscall)
      (owl sort)
      (owl time)
      (owl defmac))

   (begin
      ;;;
      ;;; Pseudorandom data generators
      ;;;

      ; random data generators implement an infinite stream of positive fixnums, 
      ; which are used by the various functions which need a random data source.
      ; as usual the state variables are explicitly passed into and returned from 
      ; the functions, usually as the first parameter to each direction. these 
      ; could be tucked into a monad some time in the future, but at least for now 
      ; it seems nice to be explicit about the flow of data.

      ;;; Linear Congruential Generater -- old and simple

      ;; x_n+1 = a*x_n + c (mod m)
      ;; max period is m, and is very sensitive to choice of a, c, m
      ;; use a = 1664525, c = 1013904223, m = 2^32 (as suggested in Numerical Recipes according to Wikipedia)
      ;; stream out only the low 16 bits of each step

      (define (lcg-rands seed)
         (let ((seed (band (+ (* seed 1664525) 1013904223) #xffffffff)))
            (if (teq? seed fix+)
               (pair seed (lcg-rands seed))
               (pair (ncar seed) (lcg-rands seed)))))

      ;;; Xorshift (by George Marsaglia, period 2^128 - 1, based on example from Wikipedia)
      ;;; http://www.jstatsoft.org/v08/i14/paper

      ;; reverse digits, cut topmost 0 digits and downgrade to fixnum if possible

      (define (nrev-iter from to)
         (if (null? from)
            to
            (lets ((d ds from))
               (nrev-iter ds (ncons d to)))))

      (define (nrev-fix ds)
         (if (null? ds) 
            0
            (lets ((d ds ds))
               (cond
                  ((eq? d 0) ;; drop leading zeros
                     (nrev-fix ds))
                  ((eq? ds null) ;; downgrade a single digit to fixnum
                     d)
                  (else
                     (nrev-iter ds (ncons d null)))))))

      (define word32 #xffffffff)

      (define (xorshift-128 x y z w)
         (lets 
            ((t (bxor x (band word32 (<< x 11))))
             (x y)
             (y z)
             (z w)
             (w (bxor w (bxor (>> w 19) (bxor t (>> t 8))))))
            (if (teq? w fix+)
               (cons w (cons 0 
                  (λ () (xorshift-128 x y z w))))
               (cons (ncar w) (cons (ncar (ncdr w)) 
                  (λ () (xorshift-128 x y z w)))))))

      (define xors (xorshift-128 123456789 362436069 521288629 88675123))

      ;;; Ad-hoc old random generator. 

      (define rand-modulus 15991) ; no longer a modulus
      (define rand-multiplier 31337)

      (define (rand-walk acc seed out)
         (if (null? seed)
            out
            (lets
               ((lo hi (fx* (ncar seed) rand-multiplier))
                (this over (fx+ lo acc)))
               (rand-walk hi (ncdr seed) (ncons this out)))))

      (define (rand-succ seed)
         (cond
            ; promote natural seeds to random states
            ((teq? seed fix+)
               (let ((seed (ncons 1 (ncons seed null))))
                  (tuple #true (rand-walk rand-modulus seed null) seed)))
            ((teq? seed int+)
               (tuple #true (rand-walk rand-modulus seed null) seed))
            (else
               (lets ((st a b seed))
                  (cond
                     ((= a b)
                        (let ((ap (ncons 1 a)))
                           ;(print "rand loop at " a)
                           (tuple #true (rand-walk rand-modulus ap null) ap)))
                     (st
                        (tuple #false
                           (rand-walk rand-modulus a null)
                           (rand-walk rand-modulus b null)))
                     (else
                        (tuple #true (rand-walk rand-modulus a null) b)))))))

      ;;; Mersenne Twister (missing)

      ;;; Blum-Blum-Shub (import from lib-crypt?)

      (define (adhoc-seed->rands rst)
         (let ((rst (rand-succ rst)))
            (pair (ncar (ref rst 2)) (adhoc-seed->rands rst))))

      (define (bit x n)
         (if (eq? 0 (fxband x n)) 0 1))

      (define (rands->bits rs)
         (lets 
            ((d rs (uncons rs 0))
             (tl (λ () (rands->bits rs))))
            (let loop ((p #b1000000000000000))
               (if (eq? p 0) 
                  tl
                  (cons (bit d p) (loop (>> p 1)))))))

      (define (rands->bytes rs)
         (lets ((digit rs (uncons rs 0)))
            (ilist
               (fxband digit 255)
               (fxband (>> digit 8) 255)
               (λ () (rands->bytes rs)))))

      ;; eww, don't try this at home. to be fixed pretty soon. passed dieharder tests pretty well though.
      (define seed->rands adhoc-seed->rands)

      (define seed->bits 
         (o rands->bits seed->rands))
      
      (define seed->bytes
         (o rands->bytes seed->rands))

      ;; note, a custom uncons could also promote random seeds to streams, but probably better to force 
      ;; being explicit about the choice of prng and require all functions to receive just digit streams.

      ;;;
      ;;; Plain 0-(n-1) rand
      ;;;

      (define (rand-big rs n)
         (if (null? n)
            (values rs null #true)
            (lets 
               ((rs head eq (rand-big rs (ncdr n)))
                (this rs (uncons rs 0)))
               (if eq
                  (let ((val (rem this (+ (ncar n) 1))))
                     (if (eq? val 0)
                        (values rs (if (null? head) null (ncons 0 head)) (eq? (ncar n) 0))
                        (values rs (ncons val head) (eq? val (ncar n)))))
                  (if (eq? this 0)
                     (values rs (if (null? head) null (ncons 0 head)) #false)
                     (values rs (ncons this head) #false))))))

      ; rs n → rs m, 0 <= m < n

      ;; plan: take as many random bits as necessary, check that the value is below limit and pick or recurse

      ;; compute the nearest m = (2^x)-1 with m >= num
      ;; fixme: later by tree comparison
      (define (bitmask num)
         (if (eq? num 0)
            1
            (let loop ((n #xffff))
               (lets ((np _ (fx>> n 1)))
                  (if (lesser? np num) ;; we lost the high bit
                     n
                     (loop np))))))

      (define (rand-fixnum rs n)
         (let loop ((rs rs) (mask (bitmask n)))
            (lets
               ((digit rs (uncons rs #false))
                (m (fxband digit mask)))
               (if (lesser? m n)
                  (values rs m)
                  (loop rs mask)))))

      ;; like rand-fixnum, but <= limit instead of < 
      (define (rand-bignum-topdigit rs n)
         (let loop ((rs rs) (mask (bitmask n)))
            (lets
               ((digit rs (uncons rs #false))
                (m (fxband digit mask)))
               (cond
                  ((lesser? m n) (values rs m))
                  ((eq? m n) (values rs m))
                  (else (loop rs mask))))))
         
      (define (rand-bignum rs n)
         (let loop ((rs rs) (left n) (out null))
            (lets ((digit left left))
               (if (null? left)
                  ;; last bignum digit (most significant bits) -> grab a suitable fixnum and compare
                  (lets 
                     ((rs top (rand-bignum-topdigit rs digit))
                      (try (nrev-fix (cons top out))))
                     (if (< try n)
                        ;; we made 0 <= try < n
                        (values rs try)
                        (rand-bignum rs n)))
                  (lets ((digit rs (uncons rs #false)))
                     (loop rs left (cons digit out)))))))

      ;; todo: add (n-bits fixnum), use it to construct fixnum and bignum top digits, and compare&reject instead of modulo to avoid bias
      ;; + compare speed to the old one!
      ;; todo: check if limit is an exact power of two, and if so, use bitwise. if not, we know how many bits to generate, which is also necessary now.
      (define (rand rs max)
         (if (eq? max 0)
            (values rs 0) ;; don't use any rands here
            (type-case max
               (fix+ (rand-fixnum rs max))
               (int+ (rand-bignum rs max))
               (else (error "bad rand limit: " max)))))

      ;; a quick skew check. the modulo issue caused a >10% skew in some cases earlier
      ;(let loop ((rs (seed->rands (time-ms))) (n 0) (sum 0) (lim 10000000000))
      ;   (if (eq? 0 (band 1023 n))
      ;      (let ((avg (div sum (max n 1))))
      ;         (print
      ;            (list "at " n " sum " sum " avg " avg " delta percent " 
      ;               (let ((perc (div (* 100 (abs (- (>> lim 1) avg))) (>> lim 1))))
      ;                  perc)))))
      ;   (lets ((rs val (rand rs lim)))
      ;      (loop rs (+ n 1) (+ sum val) lim)))

      ;;;
      ;;; Random selection
      ;;;

      ;; picking one element (non-lazy)
      (define (rand-elem rs obj)
         (cond
            ((pair? obj)
               (lets ((rs n (rand rs (length obj))))
                  (values rs (lref obj n))))
            ((tuple? obj)
               (lets ((rs n (rand rs (size obj))))
                  (values rs (ref obj (+ n 1)))))
            ((vector? obj)
               (lets ((rs n (rand rs (vec-len obj))))
                  (values rs (vec-ref obj n))))
            (else
               (error "rand-elem: what be " obj))))

      ;; select all from lst with a 1-bit in corresponding position
      (define (select-members lst bits this out)
         (cond
            ((null? lst) out)
            ((eq? this (band bits this))
               (select-members lst (- bits this) this 
                  (cons (car lst) out)))
            ((eq? this #x8000) ; highest fixnum bit
               (select-members (cdr lst) (ncdr bits) 1 out))
            (else
               (select-members (cdr lst) bits (<< this 1) out))))

      ; random exactly n-bit number
      (define (rand-nbit rs n)
         (if (eq? n 0)
            (values rs 0)
            (lets
               ((hi (<< 1 (- n 1)))
                (rs val (rand rs hi)))
               (values rs (bor val hi)))))

      ;; select with bits of a random number (to save some rands)
      (define (random-subset rs l)
         (if (null? l)
            (values rs null)
            (lets
               ((n (length l))
                (rs bits (rand-nbit rs (+ n 1))))
               (values rs (reverse (select-members l bits 1 null))))))

      ;;;
      ;;; Reservoir sampler
      ;;;

      ;; todo: check reservoir sampler distribution. could have an off by one.

      (define return-selection rlist->list)

      ; → rs' selection
      (define (reservoir-sampler rs ll n p res)
         (cond
            ((null? ll)
               (values rs (return-selection res)))
            ((pair? ll)
               (lets 
                  ((rs x (rand rs p))
                   (res (if (< x n) (rset res x (car ll)) res)))
                  (reservoir-sampler rs (cdr ll) n (+ p 1) res)))
            (else 
               (reservoir-sampler rs (ll) n p res))))

      ;; populate initial n elements to reservoir and start sampler if full
      (define (reservoir-init rs ll n p res)
         (cond
            ((null? ll) 
               (values rs (return-selection res)))
            ((= n p) 
               (reservoir-sampler rs ll n (+ n 1) res))
            ((pair? ll) (reservoir-init rs (cdr ll) n (+ p 1) (rcons (car ll) res)))
            (else (reservoir-init rs (ll) n p res))))

      ;; rs ll n → rs' lst
      (define (reservoir-sample rs ll n)
         (reservoir-init rs ll n 0 null))



      ; rs lst → rs' sublist, each element having 50% chance of being in the sublist
      (define (rand-subset rs l)
         (if (null? l)
            (values rs null)
            (lets
               ((n (length l))
                (rs bits (rand-nbit rs (+ n 1))))
               (values rs
                  (reverse (select-members l bits 1 null))))))

      ; a number with log_2(n) instead of n evenly distributed in range
      (define (rand-log rs n)
         (if (= n 0)
            0
            (lets
               ((rs n (rand rs n))
                (rs n (rand-nbit rs n)))
               (values rs n))))

      (define (rand-range rs lo hi)
         (if (< lo hi) 
            ;; fixme: is this indeed ok?
            (lets ((rs o (rand rs (- hi lo))))
               (values rs (+ o lo)))
            (error "rnd-range: bad range: " (list lo hi))))

      ;(define data (iota 0 1 10))
      ;(let loop ((rst (expt (time-ms) 3)))
      ;   (print " => " (reservoir-sample rst data 5))
      ;   (loop (rand-succ rst)))
      '(let loop ((rs (seed->rands (expt (time-ms) (+ 1 (band (time-ms) 7))))))
         (lets ((rs n (rand rs 100000000)))
            (print " => " n)
            (wait 100)
            (loop rs)))

      ;; shuffling (random permutations)

      ; give random (fixnum) labels to elements, sort and take values. recurse for ranges with equal keys.
      ; rst x done x n -> rst' x ((i . n) . done)
      (define (shuffle-label rs done val)
         (lets ((n rs (uncons rs 0)))
            (values rs (cons (cons n val) done))))

      (define (carless a b) (lesser? (car a) (car b))) ; labels are fixnums, so use eq-like comparison

      (define (shuffle-merge rs pairs tail rec)
         (if (null? pairs)
            (values rs tail)
            (lets
               ((this (caar pairs))
                (these pairs (take-while (λ (x) (eq? (car x) this)) pairs)))
               (if (null? (cdr these)) ; leaf case, just one
                  (shuffle-merge rs pairs (cons (cdar these) tail) rec)
                  (lets ((rs tail (shuffle-merge rs pairs tail rec)))
                     (rec rs (map cdr these) tail))))))

      (define (shuffler rs lst tail)
         (if (null? lst)
            (values rs tail) 
            (lets
               ((rs opts (fold2 shuffle-label rs null lst))
                (opts (sort carless opts)))
               (shuffle-merge rs opts tail shuffler))))

      (define (shuffle rs lst)
         (if (null? lst)
            (values rs lst)
            (shuffler rs lst null)))

      (define random-permutation shuffle)

      (define (random-numbers rs bound count)
         (let loop ((rs rs) (out null) (count count))
            (if (= count 0)
               (values rs out)
               (lets ((rs n (rand rs bound)))
                  (loop rs (cons n out) (- count 1))))))

      ; grab directly low 8 bits of each rand (same would happend with (rand rs 256))
      (define (random-bvec rs n)
         (let loop ((rs rs) (out null) (n n))
            (if (eq? n 0)
               (values rs (raw out 11 #true)) ; reverses to keep order
               (lets 
                  ((d rs (uncons rs 0))
                   (n _ (fx- n 1))) 
                  (loop rs (cons (fxband d 255) out) n)))))

      (define (random-data-file rs path)
         (let 
            ((port (open-output-file path))
             (block (* 1024 32)) ; write in 32kb blocks
             (megs (* 1024 500))) ; ~1GB is enough for dieharder and smallcrush, 500 might be enough for crush?
            (if port
               (let loop ((rs rs) (n (* megs (* 1024 1024))))
                  (print* (list path ": left " n " bytes"))
                  (if (eq? n 0)
                     (close-port port)
                     (lets ((rs bytes (random-bvec rs block)))
                        (if (write-byte-vector port bytes)
                           (loop rs (- n block))
                           #false))))
               (begin
                  (print "failed to open " path)
                  #false))))


      (define (rand-occurs? rs prob)
         (type-case prob
            (rat 
               (lets ((nom denom prob))
                  (if (eq? nom 1) ;; 1/n -> rand n and check for 0
                     (lets ((rs n (rand rs denom)))
                        (values rs (eq? n 0)))
                     (lets ((rs n (rand rs denom)))
                        (values rs (< n nom))))))
            (fix- (values rs #false))
            (int- (values rs #false))
            (else
               (if (eq? prob 0)
                  (values rs #false)
                  (values rs #true))))) ;; <- natural number > 0 -> >= 100%

      ;(lets ((rs l (shuffle (seed->rands 42) (iota 0 1 100))))
      ;   (print " xxx " l))

      ;;;
      ;;; Random stream tests
      ;;;

      (define (prng-speed str)
         (let 
            ((start (time-ms))
             (ndigits (* 1024 64))) ; make 1mb 
            (let loop ((str str) (n ndigits))
               (if (eq? n 0)
                  (print (floor (/ (* ndigits 16) (- (time-ms) start))) " bits/ms")
                  (lets ((d rs (uncons str 0)))
                     (loop rs (- n 1)))))))

      ;; add basic statistical tests here
      ;;  - n-bit repetition frequencies
      ;;  - every nth bit bias
      ;;  - check that a stream of (rand rs n) stays near n/2

      '(begin
         (begin
            (display " * blank    ")
            (prng-speed (liter (λ (x) x) 42)))
         (begin
            (display " * default  ")
            (prng-speed (seed->rands 42)))
         (begin
            (display " * bigseed  ")
            (prng-speed (seed->rands 12412421412948214981249184921841572357239582359723592735019842395723509843698734954735092384239752398573468724981498)))
         (begin
            (display " * xors     ")
            (prng-speed xors))
      )


     ;; make files to test the prngs
     ; (random-data-file (lcg-rands 0) "/tmp/random.lcg")
     ; (random-data-file xors "/tmp/random.xors")
     ; (random-data-file (seed->rands 12312312313) "/tmp/random.adhoc")

))

