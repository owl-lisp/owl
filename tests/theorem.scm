#!/usr/bin/ol --run

;; RANDOM -- tag to run this as part of random-tests

;; todo: run each in a thread to avoid slow tests taking most of the time

;; DSL

;; theorem :: rs → rs' bindings ok?

(define-syntax translate 
   (syntax-rules (∀ ∊ → ↔ ← =)
      ((translate rs a → . b) 
         (lets 
            ((rs env-a ar (translate rs a)))
            (if ar
               (lets ((rs env-b br (translate rs . b)))
                  (values rs (append env-a env-b) br))
               (values rs env-a #true))))
      ((translate rs var ← defn . rest) 
         (let ((var defn))
            (lets ((rs env res (translate rs . rest)))
               (values rs (cons (cons (quote var) var) env) defn))))
      ((translate rs a ↔ b) 
         (lets 
            ((rs env-a ar (translate rs a))
             (rs env-b br (translate rs b))
             (env (append env-a env-b)))
            (values rs env (if ar br (not br)))))
      ((translate rs a = b) 
         (values rs null (equal? a b)))
      ((translate rs ∀ var ∊ gen . rest)
         (lets 
            ((rs var (gen rs))
             (rs env res (translate rs . rest)))
            (values rs (cons (cons (quote var) var) env) res)))
      ((translate rs ∀ var next ... ∊ gen . rest)
         (lets 
            ((rs var (gen rs))
             (rs env res (translate rs ∀ next ... ∊ gen . rest)))
            (values rs (cons (cons (quote var) var) env) res)))
      ((translate rs term) 
         (values rs null term))))

(define-syntax theorem
   (syntax-rules ()
      ((theorem name . stuff)
         (cons (quote name)
            (λ (rs) (translate rs . stuff))))))

(define-syntax theory
   (syntax-rules (theorem)
      ((theory theorem thing ... theorem . rest)
         ;; n>1 left
         (cons (theorem thing ...) (theory theorem . rest)))
      ((theory . stuff)
         ;; last one
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

(define (Rlist-of thing)
   (λ (rs)
      (lets ((rs n (rand rs elem-ip)))
         (if (eq? n 0)
            (values rs null)
            (lets 
               ((rs head (thing rs))
                (rs tail ((Rlist-of thing) rs)))
               (values rs (rcons head tail)))))))

(define List (List-of Byte))

(define Rlist (Rlist-of Byte))

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

   (theory

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
     
      theorem rlist-car-cons
         ∀ a ∊ Byte ∀ r ∊ Rlist
            a = (rcar (rcons a r))

      ;theorem rlist-set-get
      ;   ∀ r ∊ Rlist ∀ a → Byte
      ;      p ∊ (range r)
      ;      p → a = (rget (rset r p a) p)               
               
      ;; testing failures
      ; theorem all-even ∀ a ∊ Nat 0 = (band a 1)

))



;; Practice

(define (random-seed)
   (let ((fd (open-input-file "/dev/urandom"))) ;; #false if not there
      (if fd
         (let ((data (get-block fd 16)))
            (close-port fd)
            (if (vector? data)
               (vec-fold (λ (n d) (+ d (<< n 8))) 0 data)
               (time-ms)))
         (time-ms))))

(define (failures rs)
   (let loop ((rs rs) (tests tests) (failed null))
      (if (null? tests)
         (values rs failed)
         (lets ((rs env ok ((cdar tests) rs)))
            (if ok
               (begin
                  ;; unquote to see successful bindings
                  ;(print (list (caar tests) 'ok 'with env))
                  (loop rs (cdr tests) failed))
               (loop rs (cdr tests) 
                  (cons (cons (caar tests) env) failed)))))))

;; run a few rounds at load/compile time, like in in $ make random-test
(let ((seed (random-seed)))
    (let loop ((n 20) (rs (seed->rands seed)))
      (if (= n 0)
         (print "All OK!")
         (lets ((rs fails (failures rs)))
            (if (null? fails)
               (loop (- n 1) rs)
               (show "FAILED: " fails))))))

(import (owl args))

(define (string->natural str)
   (let ((x (string->integer str)))
      (if (< x 0) #false x)))

(define cl-handler
   (cl-rules
    `((seed "-s" "--seed" cook ,string->natural)
      (rounds "-n" "--rounds" cook ,string->natural comment "give for finite test")
      (help "-h" "--help"))))

;; for --run
(λ (args)
   (process-arguments (cdr args) cl-handler "boo"
      (λ (dict unknown)
         (cond
            ((not (null? unknown))
               (show "Pray tell what are " unknown)
               1)
            ((getf dict 'help)
               (print "Usage:")
               (print (format-rules cl-handler))
               0)
            (else
               (lets
                  ((seed (or (getf dict 'seed)  (random-seed)))
                   (end (getf dict 'rounds))) ; #false if not given
                  (show "Starting random continuous test, seed " seed)
                  (if end
                     (show "Will run up to " end)
                     (print "Will run forever"))
                  (let loop ((n 0) (rs (seed->rands seed)))
                     (if (eq? 0 (band n 31))
                        (show " - " n))
                     (lets ((rs fails (failures rs)))
                        (if (null? fails)
                           (if (equal? n end)
                              (begin
                                 (print "Finished successfully")
                                 0)
                              (loop (+ n 1) rs))
                           (begin
                              (show "TESTS FAILED: " (list 'fails fails 'seed seed 'n n))
                              2))))))))))


