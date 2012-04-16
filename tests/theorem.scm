
(define-syntax translate 
   (syntax-rules (∀ ∊ → ↔ =)
      ((translate rs a → b) (if a b #true))
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

(define (Nat rs)
   (rand rs 1000))

(define (List rs)
   (lets ((rs n (rand rs 10)))
      (if (eq? n 0)
         (values rs null)
         (lets ((rs tl (List rs)))
            (values rs (cons n tl))))))

(define tests
   (list
      (theorem fact-1
         ∀ a ∊ Nat (= 1 (length (factor a))) ↔ (prime? a))

      (theorem add-1 
         ∀ a b ∊ Nat (+ a b) = (+ b a))

      (theorem add-2
         ∀ a b c ∊ Nat (+ a (+ b c)) = (+ (+ a b) c))

      (theorem add-3
         ∀ a ∊ Nat a = (+ a 0))

      (theorem mul-1
         ∀ a ∊ Nat (+ a a) = (* a 2))

      (theorem mul-2
         ∀ a b ∊ Nat (* a b) = (* b a))
      
      (theorem mul-3
         ∀ a b c ∊ Nat (* a (* b c)) = (* (* a b) c))

      (theorem rev-1
         ∀ l ∊ List l = (reverse (reverse l)))

      (theorem halve-1
         ∀ l ∊ List l = (lets ((hd tl (halve l))) (append hd tl)))

))

(map 
   (λ (test)
      (print* (list " - " (car test) ": " ((cdr test) (seed->rands (time-ms))))))
   tests)

