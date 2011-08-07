;;;
;;; a pseudorandom number generator
;;;

;; bug: modulo-based random number choosing is broken (see issue #26)
;; todo: unit tests
;; todo: alternative distributions

(define-module lib-random

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
      rnd                 ;; rs max → rs' n, 0 <= n < max
      rnd-nbit            ;; rs n → rs' i
      rnd-log
      rnd-elem            ;; rs obj → rs' elem (for lists and vectors)
      rnd-subset
      rnd-range

      random-numbers      ;; rs x max x i -> rs' (n_1 .. n_i), as in rand
      reservoir-sample    ;; rs x ll x n -> lst', |lst'| <= n
      shuffle             ;; rs x lst -> rs' lst'
      random-permutation  ;; rs x lst -> rs' lst'
      random-subset       ;; rs x lst -> rs' lst' <- lst, same order, each element has 50% chance to be included
      rand-elem           ;; rs x thing -> rs' x element (for some data types)

      ;;; old versions (to be removed later)
      rand                ;; rst x max -> rst' x val, val <- [0 .. max-1]
      rand-succ           ;; rst -> rst'  ;; <- to be removed and replaced with (make-random-<type> <seed>) etc
      rand-nbit           ;; rst x nbits -> rst' x val, being exactly n bits wide
      rand-log            ;; rst x max-bits -> rst' x val
      rand-range          ;; rst x lo x hi -> rst' x n <= lo < hi
      ; rand-range-uniform (= rand-range)
      ; rand-range-normal
      )

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

   ;;; Ad-hoc old random generator

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
               (tuple True (rand-walk rand-modulus seed null) seed)))
         ((teq? seed int+) 
            (tuple True (rand-walk rand-modulus seed null) seed))
         (else
            (lets ((st a b seed))
               (cond
                  ((= a b)
                     (let ((ap (ncons 1 a)))
                        ;(show "rand loop at " a)
                        (tuple True (rand-walk rand-modulus ap null) ap)))
                  (st   
                     (tuple False
                        (rand-walk rand-modulus a null)
                        (rand-walk rand-modulus b null)))
                  (else   
                     (tuple True (rand-walk rand-modulus a null) b)))))))

   ;;; Xorshift (by George Marsaglia, period 2^128 - 1, based on example from Wikipedia)
   ;;; http://www.jstatsoft.org/v08/i14/paper

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


   ;;; Mersenne Twister (missing)

   ;;; Blum-Blum-Shub (import from lib-crypt?)


   ; rst n -> rst' i, i in [0...n-1]

   ; walk up to the significant digits and fall down having either
   ;   - constructed exactly equal high part, so the current digit 
   ;     is limited by the digit of the current number
   ;   - constructed at least one smaller high digit, so any digit 
   ;     (like the low 16-bit one from rst) will do

   (define (rand-big rst n)
      (if (null? n)
         (values rst null True)
         (lets ((rst head eq (rand-big rst (ncdr n))))
            (if eq
               (let ((val (rem (ncar (ref rst 2)) (+ (ncar n) 1))))
                  (if (eq? val 0)
                     (values (rand-succ rst) 
                        (if (null? head) null (ncons 0 head))
                        (eq? (ncar n) 0))
                     (values
                        (rand-succ rst)
                        (ncons val head)
                        (eq? val (ncar n)))))
               (let ((this (ncar (ref rst 2))))
                  (if (eq? this 0)
                     (values (rand-succ rst) 
                        (if (null? head) null (ncons 0 head))
                        False)
                     (values (rand-succ rst) 
                        (ncons this head)
                        False)))))))

   ;; todo: rand returns 0 with maximum 0. consistent with other systems? see also rand-range etc
   (define (rand rst max)
      (let ((rst (rand-succ rst))) ; step and ensure tupleness
         (type-case max
            (fix+
               (let ((this (ncar (ref rst 2))))
                  (if (eq? max 0)
                     (values rst 0)
                     (values rst (fx% this max)))))
            (int+
               (lets ((rst n eq (rand-big rst max)))
                  (cond
                     (eq (rand rst max))
                     ((null? n) (values rst 0))
                     ((null? (ncdr n)) (values rst (ncar n)))
                     (else (values rst n)))))
            (fix- 
               (lets ((rst n (rand rst (- 0 max))))
                  (values rst (- 0 n))))
            (int- 
               (lets ((rst n (rand rst (- 0 max))))
                  (values rst (- 0 n))))
            (else
               (error "bad rand limit: " max)))))

   ;make a 500mb file for dieharder test
   ;(define fd (open-output-file "inc-new.bin"))
   ;(let loop ((rst 0) (n (* 500 (* 1024 1024))))
   ;   (if (eq? 0 (band n #xffff)) (print (list 'left n 'bytes)))
   ;   (if (eq? n 0)
   ;      (close-port fd)
   ;      (lets   ((rst b (rand rst 256)))
   ;         (mail fd b)
   ;         (loop rst (- n 1)))))

   ;;;
   ;;; Shareable part
   ;;;

   (define (random-list-elt rst lst)
      (lets ((rst pos (rand rst (length lst))))   
         (values rst (lref lst pos))))

   (define (random-tuple-elt rst obj)
      (lets ((rst pos (rand rst (size obj))))
         (if (eq? pos 0)
            (values rst (ref obj (size obj)))
            (values rst (ref obj pos)))))


   (define (rand-stream rst max)
      (lets ((rst this (rand rst max)))
         (pair this
            (rand-stream rst max))))

   ; random exactly n-bit number
   (define (rand-nbit rst n)
      (if (eq? n 0)
         (values rst 0)
         (lets
            ((hi (<< 1 (- n 1)))
             (rst val (rand rst hi)))
            (values rst (bor val hi)))))

   ; a number with log_2(n) instead of n evenly distributed in range
   (define (rand-log rst n)
      (if (= n 0)
         0
         (lets
            ((rst n (rand rst n))
             (rst n (rand-nbit rst n)))
            (values rst n))))

   (define (rand-range rst lo hi)
      (if (> lo hi) 
         (error "rand-range: bad range: " (list lo hi))
         (lets ((rst o (rand rst (- hi lo))))
            (values rst (+ o lo)))))

   ;;; Testing
   ;
   '(lets
      ((max 10000000000)
       (goal (div max 2))
       (max (* goal 2)))

      (print (list max goal))
      (let loop ((rst (rand-succ 1)) (sum 0) (n 0))
         (receive (rand rst max)
            (lambda (rst r)
               (lets
                  ((sum (+ sum r))
                   (n (+ n 1))
                   (avg (div sum n)))
                  (if (eq? (band n #xff) 0)
                     (show " * " (list 'at n 'val r 'avg avg 'dist (div (* 100 (- avg goal)) goal) 'perc)))
                  (loop rst sum n))))))


   (define (rand-elem rst obj)
      (cond
         ((pair? obj)
            (lets ((rst n (rand rst (length obj))))
               (values rst (lref obj n))))
         ((vector? obj)
            (lets ((rst n (rand rst (vec-len obj))))
               (values rst (vec-ref obj n))))
         (else
            (error "rand-elem: what be " obj))))


   ;;;
   ;;; random files output to test with diehard(er) & ent 
   ;;;
   ;
   ;
   ;(define (populate-file port state size)
   ;   (let loop ((state state) (left size) (pos 0) (buff null))
   ;      (cond
   ;         ((eq? left 0)
   ;            (if (null? buff)
   ;               (begin
   ;                  (mail port buff)
   ;                  True)
   ;               True))
   ;         ((eq? pos #x4fff)
   ;            (mail port buff)
   ;            (show " left " left)
   ;            (loop state left 0 null))
   ;         (else
   ;            (receive (rand state 256)
   ;               (lambda (state byte)   
   ;                  (loop state (- left 1) (+ pos 1) (cons byte buff))))))))
   ;
   ;(define (random-file path state size)
   ;   (show "opening file " path)
   ;   (let ((port (open-output-file path)))
   ;      (show " -> " port)
   ;      (if port 
   ;         (begin
   ;            (populate-file port state size)
   ;            (close-port port)
   ;            True)
   ;         False)))
   ;





   ;;;
   ;;; Stream-based variants
   ;;;

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

   (define (rnd-big rs n)
      (if (null? n)
         (values rs null True)
         (lets 
            ((rs head eq (rnd-big rs (ncdr n)))
             (this rs (uncons rs 0)))
            (if eq
               (let ((val (rem this (+ (ncar n) 1))))
                  (if (eq? val 0)
                     (values rs (if (null? head) null (ncons 0 head)) (eq? (ncar n) 0))
                     (values rs (ncons val head) (eq? val (ncar n)))))
               (if (eq? this 0)
                  (values rs (if (null? head) null (ncons 0 head)) False)
                  (values rs (ncons this head) False))))))

   ; rs n → rs m, 0 <= m < n

   (define (rnd rs max)
      (type-case max
         (fix+
            (lets ((n rs (uncons rs 0)))
               (if (eq? max 0)
                  (values rs 0)
                  (values rs (fx% n max)))))
         (int+
            (lets ((rs n eq (rnd-big rs max)))
               (cond
                  (eq (rnd rs max))
                  ((null? n) (values rs 0)) 
                  ((null? (ncdr n)) (values rs (ncar n)))
                  (else (values rs n)))))
         (else
            (error "bad rand limit: " max))))
  
   ;;;
   ;;; Random selection
   ;;;

   ;; picking one element (non-lazy)
   (define (rnd-elem rs obj)
      (cond
         ((pair? obj)
            (lets ((rs n (rnd rs (length obj))))
               (values rs (lref obj n))))
         ((tuple? obj)
            (lets ((rs n (rnd rs (size obj))))
               (values rs (ref obj (+ n 1)))))
         ((vector? obj)
            (lets ((rs n (rnd rs (vec-len obj))))
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

   ;; select with bits of a random number (to save some rands)
   (define (random-subset rst l)
      (if (null? l)
         null
         (lets
            ((n (length l))
             (rst bits (rand-nbit rst (+ n 1))))
            (reverse
               (select-members l bits 1 null)))))

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
               ((rs x (rnd rs p))
                (res (if (< x n) (rset res x (car ll)) res)))
               (reservoir-sampler rs (cdr ll) n (+ p 1) res)))
         (else 
            (reservoir-sampler rs (ll) n p res))))

   ;; populate initial n elements to reservoir and start sampler if full
   (define (reservoir-init rs ll n p res)
      (cond
         ((null? ll) 
            (values rs (return-selection res)))
         ((= n p) (reservoir-sampler rs ll n p res))
         ((pair? ll) (reservoir-init rs (cdr ll) n (+ p 1) (rcons (car ll) res)))
         (else (reservoir-init rs (ll) n p res))))

   ;; rs ll n → rs' lst
   (define (reservoir-sample rs ll n)
      (reservoir-init rs ll n 0 null))



   ; random exactly n-bit number
   (define (rnd-nbit rs n)
      (if (eq? n 0)
         (values rs 0)
         (lets
            ((hi (<< 1 (- n 1)))
             (rs val (rnd rs hi)))
            (values rs (bor val hi)))))

   ; rs lst → rs' sublist, each element having 50% chance of being in the sublist
   (define (rnd-subset rs l)
      (if (null? l)
         (values rs null)
         (lets
            ((n (length l))
             (rs bits (rnd-nbit rs (+ n 1))))
            (values rs
               (reverse (select-members l bits 1 null))))))

   ; a number with log_2(n) instead of n evenly distributed in range
   (define (rnd-log rs n)
      (if (= n 0)
         0
         (lets
            ((rs n (rnd rs n))
             (rs n (rnd-nbit rs n)))
            (values rs n))))

   (define (rnd-range rs lo hi)
      (if (< lo hi) 
         ;; fixme: is this indeed ok?
         (lets ((rs o (rnd rs (- hi lo))))
            (values rs (+ o lo)))
         (error "rnd-range: bad range: " (list lo hi))))

   ;(define data (iota 0 1 10))
   ;(let loop ((rst (expt (time-ms) 3)))
   ;   (show " => " (reservoir-sample rst data 5))
   ;   (loop (rand-succ rst)))
   '(let loop ((rs (seed->rands (expt (time-ms) (+ 1 (band (time-ms) 7))))))
      (lets ((rs n (rnd rs 100000000)))
         (show " => " n)
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
            (lets ((rs n (rnd rs bound)))
               (loop rs (cons n out) (- count 1))))))

   ; grab directly low 8 bits of each rand (same would happend with (rnd rs 256))
   (define (random-bvec rs n)
      (let loop ((rs rs) (out null) (n n))
         (if (eq? n 0)
            (values rs (raw out 11 True)) ; reverses to keep order
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
                     (mail port bytes)
                     (loop rs (- n block)))))
            (begin
               (show "failed to open " path)
               False))))
         

   ;(lets ((rs l (shuffle (seed->rands 42) (iota 0 1 100))))
   ;   (show " xxx " l))

   ;;;
   ;;; Random stream tests
   ;;;

   (define (prng-speed str)
      (let 
         ((start (time-ms))
          (ndigits (* 1024 64))) ; make 1mb 
         (let loop ((str str) (n ndigits))
            (if (eq? n 0)
               (show (floor (/ (* ndigits 16) (- (time-ms) start))) " bits/ms")
               (lets ((d rs (uncons str 0)))
                  (loop rs (- n 1)))))))

   ;; add basic statistical tests here
   ;;  - n-bit repetition frequencies
   ;;  - every nth bit bias
   ;;  - check that a stream of (rnd rs n) stays near n/2

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


)
