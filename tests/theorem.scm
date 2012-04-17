;; RANDOM -- tag to run this as part of random-tests

;; DSL

(define-syntax translate 
   (syntax-rules (∀ ∊ → ↔ =)
      ((translate rs a → . b) (if a (translate rs . b) #true))
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

(define (Byte rs)
   (rand rs 256))

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

(define (List rs)
   (lets ((rs n (rand rs 20)))
      (if (eq? n 0)
         (values rs null)
         (lets ((rs tl (List rs)))
            (values rs (cons n tl))))))

(define (List-of thing)
   (λ (rs)
      (lets ((rs n (rand rs 20)))
         (if (eq? n 0)
            (values rs null)
            (lets 
               ((rs head (thing rs))
                (rs tail ((List-of thing) rs)))
               (values rs (cons head tail)))))))

;; Theory 

(define tests

   (theorems

      theorem prime-1
         ∀ a ∊ Nat (< a 100000000) → (prime? a) → (= 1 (length (factor a)))
      
      theorem add-1 
         ∀ a b ∊ Num (+ a b) = (+ b a)

      theorem add-2
         ∀ a b c ∊ Num (+ a (+ b c)) = (+ (+ a b) c)

      theorem add-3
         ∀ a ∊ Num a = (+ a 0)

      theorem mul-add-double
         ∀ a ∊ Num (+ a a) = (* a 2)

      theorem mul-add-1
         ∀ a b ∊ Num (* a (+ b 1)) = (+ (* a b) a)

      theorem add-cancel
         ∀ a b ∊ Num a = (- (+ a b) b)

      theorem div-cancel-1
         ∀ a b ∊ Num (not (= b 0)) → a = (* (/ a b) b)

      theorem div-cancel-2
         ∀ a b ∊ Num (not (= b 0)) → a = (/ (* a b) b)

      theorem div-twice 
         ∀ a b ∊ Num (not (= b 0)) → (/ (/ a b) b) = (/ a (* b b))

      theorem div-self
         ∀ a ∊ Num (not (= a 0)) → 1 = (/ a a)

      theorem mul-2
         ∀ a b ∊ Num (* a b) = (* b a)
      
      theorem mul-3
         ∀ a b c ∊ Num (* a (* b c)) = (* (* a b) c)

      theorem shift-cancel
         ∀ a ∊ Nat ∀ b ∊ Byte a = (>> (<< a b) b)

      theorem gcd-swap
         ∀ a b ∊ Int (gcd a b) = (gcd b a)

      theorem rev-1
         ∀ l ∊ List l = (reverse (reverse l))

      theorem reverse-fold
         ∀ l ∊ List (reverse l) = (fold (λ (a b) (cons b a)) null l)

      theorem foldr-copy
         ∀ l ∊ List l = (foldr cons null l)

      theorem zip-map
         ∀ l ∊ List l = (map car (zip cons l l))

      theorem ncr-def 
         ∀ a b ∊ Byte (>= a b) → (ncr a b) = (/ (! a) (* (! b) (! (- a b))))

      theorem halve-1
         ∀ l ∊ List l = (lets ((hd tl (halve l))) (append hd tl))

      theorem sort-rev
         ∀ l ∊ (List-of Byte) (sort < l) = (reverse (sort > l))

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

(define (test)
   (lets
      ((seed (get-seed))
       ;(seed theonethatfails)
       (rs (seed->rands seed))
       (failed
         (fold
            (λ (failed test)
               (if ((cdr test) rs) ;; this is ok
                  failed
                  (cons (car test) failed))) ;; save name if it failed
            null
            tests)))
      (if (null? failed)
         #true
         (begin
            (print* (list "Tests " failed " failed for seed " seed "."))
            #false))))

(if (fold (λ (ok n) (and ok (test))) #true (iota 0 1 20))
   (print "All OK!")
   (print "Bad kitty!!1"))

