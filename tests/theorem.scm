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

(define (Nat rs)
   (lets
      ((rs b (rand rs 128))
       (b (max b 10)))
      (rand-log rs b)))

(define (List rs)
   (lets ((rs n (rand rs 20)))
      (if (eq? n 0)
         (values rs null)
         (lets ((rs tl (List rs)))
            (values rs (cons n tl))))))


;; Theory 

(define tests

   (theorems

      theorem fact-1
         ∀ a ∊ Nat (< a 1000000) → (= 2 (length (factor a))) ↔ (prime? a)

      theorem add-1 
         ∀ a b ∊ Nat (+ a b) = (+ b a)

      theorem add-2
         ∀ a b c ∊ Nat (+ a (+ b c)) = (+ (+ a b) c)

      theorem add-3
         ∀ a ∊ Nat a = (+ a 0)

      theorem mul-1
         ∀ a ∊ Nat (+ a a) = (* a 2)

      theorem mul-2
         ∀ a b ∊ Nat (* a b) = (* b a)
      
      theorem mul-3
         ∀ a b c ∊ Nat (* a (* b c)) = (* (* a b) c)

      theorem rev-1
         ∀ l ∊ List l = (reverse (reverse l))

      theorem halve-1
         ∀ l ∊ List l = (lets ((hd tl (halve l))) (append hd tl))

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

(lets
   ((seed (get-seed))
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
      (print "All OK!")
      (print* (list "Tests " failed " failed for seed " seed "."))))

