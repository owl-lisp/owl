;; RANDOM -- tag to run this as part of random-tests

;; todo: maybe test api should return rs'?

;; DSL

(define-syntax translate 
   (syntax-rules (∀ ∊ → ↔ ← =)
      ((translate rs a → . b) (if a (translate rs . b) #true))
      ((translate rs var ← defn . rest) 
         (let ((var defn)) (translate rs . rest)))
      ((translate rs a ↔ b) (if a b (not b)))
      ((translate rs a = b) (equal? a b))
      ((translate rs ∀ var ∊ gen . rest)
         (lets ((rs var (gen rs)))
            (translate rs . rest)))
      ((translate rs ∀ var next ... ∊ gen . rest)
         (lets ((rs var (gen rs)))
            (translate rs ∀ next ... ∊ gen . rest)))
      ((translate rs term) term)))

(define-syntax theorem
   (syntax-rules ()
      ((theorem name . stuff)
         (cons (quote name)
            (λ (rs) (translate rs . stuff))))))

(define-syntax theorems
   (syntax-rules (theorem)
      ((theorems theorem thing ... theorem . rest)
         (cons (theorem thing ...) (theorems theorem . rest)))
      ((theorems . stuff)
         (list stuff))))



;; Generators 

(define elem-ip 20) ;; inverse probability of stopping element addition for linear random data structures

(define (Bool rs)
   (lets ((d rs (uncons rs 0)))
      ;; get one rand, pick low bit
      (values rs (eq? 1 (band d 1)))))

(define (Byte rs)
   (rand rs 256))

(define (Short rs)
   (lets ((digit rs (uncons rs 0)))
      (values rs digit)))

(define (Nat rs)
   (lets
      ((rs b (rand rs 128))
       (b (max b 10)))
      (rand-log rs b)))

(define (Int rs)
   (lets
      ((rs sign (rand rs 2))
       (rs n (Nat rs)))
      (values rs 
         (if (eq? sign 0)
            (- 0 n)
            n))))

(define (Rat rs)
   (lets
      ((rs a (Int rs))
       (rs b (Nat rs)))
      (values rs
         (if (eq? b 0)
            0
            (/ a b))))) ; <- could also make explicitly one for which gcd(a,b) = 1

(define (Comp rs)
   (lets
      ((rs r (Rat rs))
       (rs i (Rat rs)))
      (values rs
         (if (eq? i 0)
            r
            (complex r i)))))

; any number
(define (Num rs)
   (lets ((rs n (rand rs 4)))
      ((cond
         ((eq? n 0) Nat)
         ((eq? n 1) Int)
         ((eq? n 2) Rat)
         (else Comp)) rs)))


(define (List-of thing)
   (λ (rs)
      (lets ((rs n (rand rs elem-ip)))
         (if (eq? n 0)
            (values rs null)
            (lets 
               ((rs head (thing rs))
                (rs tail ((List-of thing) rs)))
               (values rs (cons head tail)))))))

(define List (List-of Byte))

(define (Ff-of thing)
   (λ (rs)
      (let loop ((rs rs) (out #false))
         (lets ((rs n (rand rs elem-ip)))
            (if (eq? n 0)
               (values rs out)
               (lets ((rs x (thing rs)))
                  (loop rs (put out x x))))))))



;; Theory 

(define (nonzero? a) 
   (not (eq? a 0)))

(define tests

   (theorems

      theorem prime-1
         ∀ a ∊ Nat 
            (< a 100000000) → 
               (prime? a) → (= 1 (length (factor a)))
     
      theorem factor-1
         ∀ a ∊ Nat
            (and (< 1 a) (< a 1000000)) → 
               a = (fold * 1 (map (λ (p) (expt (car p) (cdr p))) (factor a)))

      theorem add-comm
         ∀ a b ∊ Num 
            (+ a b) = (+ b a)

      theorem add-assoc
         ∀ a b c ∊ Num 
            (+ a (+ b c)) = (+ (+ a b) c)

      theorem add-3
         ∀ a ∊ Num 
            a = (+ a 0)

      theorem mul-add-double
         ∀ a ∊ Num 
            (+ a a) = (* a 2)

      theorem mul-distrib
         ∀ a b c ∊ Num
            (* a (+ b c)) = (+ (* a b) (* a c))
            
      theorem mul-add-1
         ∀ a b ∊ Num 
            (* a (+ b 1)) = (+ (* a b) a)

      theorem add-cancel
         ∀ a b ∊ Num 
            a = (- (+ a b) b)

      theorem div-cancel-1
         ∀ a b ∊ Num 
            (nonzero? b) → a = (* (/ a b) b)

      theorem div-cancel-2
         ∀ a b ∊ Num 
            (nonzero? b) → a = (/ (* a b) b)

      theorem div-twice 
         ∀ a b ∊ Num 
            (nonzero? b) → (/ (/ a b) b) = (/ a (* b b))

      theorem div-self
         ∀ a ∊ Num 
            (nonzero? a) → 1 = (/ a a)

      theorem mul-comm
         ∀ a b ∊ Num 
            (* a b) = (* b a)
      
      theorem mul-assoc
         ∀ a b c ∊ Num 
            (* a (* b c)) = (* (* a b) c)

      theorem shift-cancel
         ∀ a ∊ Nat ∀ b ∊ Byte 
            a = (>> (<< a b) b)

      theorem gcd-swap
         ∀ a b ∊ Int 
            (gcd a b) = (gcd b a)

      theorem rev-1
         ∀ l ∊ List 
            l = (reverse (reverse l))

      theorem reverse-fold
         ∀ l ∊ List 
            (reverse l) = (fold (λ (a b) (cons b a)) null l)

      theorem foldr-copy
         ∀ l ∊ List 
            l = (foldr cons null l)

      theorem zip-map
         ∀ l ∊ List 
            l = (map car (zip cons l l))

      theorem ncr-def 
         ∀ a b ∊ Byte 
            (>= a b) → (ncr a b) = (/ (! a) (* (! b) (! (- a b))))

      theorem halve-1
         ∀ l ∊ List 
            l = (lets ((hd tl (halve l))) (append hd tl))

      theorem sort-rev
         ∀ l ∊ (List-of Byte) 
            (sort < l) = (reverse (sort > l))

      theorem ff-del
         ∀ f ∊ (Ff-of Byte) ∀ a b ∊ Byte
            b = (get (del (put f a a) a) a b)
      
      theorem ff-put
         ∀ f ∊ (Ff-of Byte) ∀ a b ∊ Byte
            b = (get (put f a b) a #false)
            
      theorem ff-keys-sorted
         ∀ f ∊ (Ff-of Short)
            ks ← (keys f) ;; inorder 
               ks = (sort < ks)

      theorem ff-fold-foldr
         ∀ f ∊ (Ff-of Short)
            (ff-foldr (λ (out k v) (cons k out)) null f) = (reverse (ff-fold (λ (out k v) (cons k out)) null f))

      theorem sqrt-1
         ∀ a ∊ Nat
            a = (sqrt (* a a))

      theorem square-1
         ∀ a b ∊ Num
            S ← (λ (x) (* x x))
            (S (* a b)) = (* (S a) (S b))

      theorem quotrem-1
         ∀ a b ∊ Int
            (nonzero? b) →  
               a = (lets ((q r (quotrem a b))) (+ (* q b) r))

      theorem expt-1
         ∀ a ∊ Num ∀ p ∊ Byte
            (and (< 0 p) (< p 10)) → 
               (expt a p) = (* a (expt a (- p 1)))

      theorem totient-1
         ∀ a ∊ Nat
            (and (< 1 a) (< a 100000)) → 
               (prime? a) ↔ (= (phi a) (- a 1))

      theorem fasl-1
         ∀ f ∊ (Ff-of Num)
            f = (fasl-decode (fasl-encode f) 'bad)

      theorem bisect-1
         ∀ a n m ∊ Nat
            b ← (+ a n)
            c ← (+ b (+ m 1))
            b = (bisect (λ (p) (>= p b)) a c)

))



;; Practice

(define (get-seed)
   (let ((fd (open-input-file "/dev/urandom"))) ;; #false if not there
      (if fd
         (let ((data (get-block fd 16)))
            (close-port fd)
            (if (vector? data)
               (vec-fold (λ (n d) (+ d (<< n 8))) 0 data)
               (time-ms)))
         (time-ms))))

(define (failures rs)
   (fold
      (λ (failed test)
         (if ((cdr test) rs) ;; this is ok
            failed
            (cons (car test) failed)))
      null tests))

(let 
   ((seed (get-seed))
    ; (seed 42)
    )
    (let loop ((n 20) (rs (seed->rands seed)))
      (if (= n 0)
         (print "All OK!")
         (let ((fails (failures rs)))
            (if (null? fails)
               (loop (- n 1) (lets ((d rs (uncons rs 0))) rs))
               (show "FAILED: " fails))))))

;; for --run
(λ (args)
   (let ((seed (get-seed)))
      (show "Starting random continuous test, seed " seed)
      (let loop ((n 0) (rs (seed->rands seed)))
         (if (eq? 0 (band n 31))
            (show " - " n))
         (lets
            ((fails (failures rs))
             (d rs (uncons rs 0)))
            (if (null? fails)
               (loop (+ n 1) rs)
               (begin
                  (show "TESTS FAILED: " (list 'fails fails 'seed seed 'n n))
                  #false))))))


