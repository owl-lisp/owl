#!/usr/bin/ol --run

;; todo: move to (owl proof) after dust settles

;; DSL - still being prototyped
; 
;  Theorem = (∀ var ... ∊ set)* Term
;  Term = Term ⇒ Term                    -- implies
;       | Term ⇔ Term                    -- if and only if
;       | Exp = Exp                      -- compare exps with equal? to determine outcome
;       | Exp                            -- return value (as in #f or non-#f) of Exp
;       | var ∊ (list | number) Term     -- pick one at random  (should use ◅, ⇐, ⇠, or ⇦ instead to avoid confusion?)
;       | var ← Exp Term                 -- assign within the following
;       | Term ∧ Term          <- likewise
;       | Term ∨ Term          <- ditto
;       | (Term)               <- not there yet
;       | ∀ var ... ∊ set Term <- -||-
; 

;; Params

(define elem-ip 20) ;; inverse probability of stopping element addition for linear random data structures
(define max-bits 128)


;; theorem :: rs → rs' bindings ok?

; f :: ? → _, keys
(define (domain x)
   (cond
      ((rlist? x)  (iota 0 1 (rlen x)))
      ((list? x)   (iota 0 1 (length x)))
      ((string? x) (iota 0 1 (string-length x)))
      ((vector? x) (iota 0 1 (vec-len x)))
      ((ff? x)     (keys x))
      (else (error "domain: what is " x))))

; f :: _ → ?, values
(define (range x)
   (cond
      ((rlist? x)  (rlist->list x))
      ((list? x)   x)
      ((string? x) (string->list x))
      ((vector? x) (vector->list x))
      ((ff? x)     (ff-fold (λ (out k v) (cons v out)) null x))
      (else (error "range: what is " x))))

;; rs (thing_1 ...) def → rs' thing_i | rs def
(define (choose rs l)
   (cond
      ((eq? l 0)
         (error "cannot take a random number below: " l))
      ((number? l)
         (rand rs l))
      ((null? l)
         (error "cannot take a random element of empty list: " l))
      (else
         (lets ((rs n (rand rs (length l))))
            (values rs (lref l n))))))

;; todo: precedence: ¬ ∧ ∨ ∀ ∃ ⇒  
;; todo:
;; todo: is the ∀ a ∊ A P == ∀ a (a ∊ A) ⇒ P worth having a nonstandard syntax?
;; plan: (translate rs env thing ... OP . rst) -> translate left and right sides around op, convert to (OP-value first rest)
;; type-specific translators to avoid extra checks?

;; node :: rs env → rs' env' result

(define-syntax translate 
   (syntax-rules (∀ ∊ → ↔ ← ⇒ ⇔ = ∧ ∨)
      ((translate rs a ⇒ . b) ;; we want a R b R c to be a R (b R c)
         (translate rs a → . b)) 
      ((translate rs a ⇔ . b)
         (translate rs a ↔ . b))
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
               (values rs (cons (cons (quote var) var) env) res))))
      ((translate rs var ∊ exp . rest) 
         (lets 
            ((rs var (choose rs exp))
             (rs env res (translate rs . rest)))
            (values rs (cons (cons (quote var) var) env) res)))
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

#| theorem name:
 |   ...
 | proof:
 |   ...
 |#

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
      ((rs b (rand rs max-bits))
       (b (max b 6)))
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
      (let loop ((rs rs) (out empty))
         (lets ((rs n (rand rs elem-ip)))
            (if (eq? n 0)
               (values rs out)
               (lets ((rs x (thing rs)))
                  (loop rs (put out x x))))))))

(import (owl iff))

(define (Iff rs)
   (let loop ((rs rs) (out #empty))
      (lets ((rs n (rand rs elem-ip)))
         (if (eq? n 0)
            (values rs out)
            (lets ((rs x (Nat rs)))
               (loop rs (iput out x x)))))))

;; FIXME: have full range and correct holes
(define (UChar rs)
   (rand rs #xffff))

(define (String rs)
   (let loop ((rs rs) (out null))
      (lets ((rs n (rand rs elem-ip)))
         (if (eq? n 0)
            (values rs (list->string out))
            (lets ((rs b (UChar rs)))
               (loop rs (cons b out)))))))


;; Theory 

(define (nonzero? a) 
   (not (eq? a 0)))

(define tests-1

   (theory

      theorem prime-1
         ∀ a ∊ Nat 
            (< a 100000000) ⇒ 
               (prime? a) ⇒ (= 1 (length (factor a)))
     
      theorem factor-1
         ∀ a ∊ Nat
            (and (< 1 a) (< a 1000000)) ⇒ 
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
            (nonzero? b) ⇒ a = (* (/ a b) b)

      theorem div-cancel-2
         ∀ a b ∊ Num 
            (nonzero? b) ⇒ a = (/ (* a b) b)

      theorem div-twice 
         ∀ a b ∊ Num 
            (nonzero? b) ⇒ (/ (/ a b) b) = (/ a (* b b))

      theorem div-self
         ∀ a ∊ Num 
            (nonzero? a) ⇒ 1 = (/ a a)

      theorem mul-comm
         ∀ a b ∊ Num 
            (* a b) = (* b a)
      
      theorem mul-assoc
         ∀ a b c ∊ Num 
            (* a (* b c)) = (* (* a b) c)

      theorem shift-cancel
         ∀ a ∊ Nat ∀ b ∊ Byte 
            a = (>> (<< a b) b)

      theorem gcd-comm
         ∀ a b ∊ Int 
            (gcd a b) = (gcd b a)

      theorem gcd-assoc
         ∀ a b c ∊ Int
            (gcd a (gcd b c)) = (gcd (gcd a b) c)

      theorem gcd-lcm
         ∀ a b ∊ Nat
            (* a b) = (* (gcd a b) (lcm a b))

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
            (>= a b) ⇒ (ncr a b) = (/ (! a) (* (! b) (! (- a b))))

      theorem halve-1
         ∀ l ∊ List 
            l = (lets ((hd tl (halve l))) (append hd tl))

      theorem sort-rev
         ∀ l ∊ (List-of Byte) 
            (sort < l) = (reverse (sort > l))

      theorem ff-del
         ∀ f ∊ (Ff-of Byte) ∀ a b ∊ Byte
            b = (get (del (put f a a) a) a b)
     
      theorem ff-del-all
         ∀ f ∊ (Ff-of Byte)
            empty = (ff-fold (λ (ff key val) (del ff key)) f f)

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

      theorem sqrt-square
         ∀ a ∊ Nat
            a = (sqrt (* a a))

      theorem sqrt-exact
         ∀ a ∊ Int
            a = (lets ((b r (exact-integer-sqrt a))) (+ (* b b) r))

      theorem square-1
         ∀ a b ∊ Num
            S ← (λ (x) (* x x))
            (S (* a b)) = (* (S a) (S b))

      theorem quotrem-1
         ∀ a b ∊ Int
            (nonzero? b) ⇒  
               (lets ((q r (quotrem a b))) 
                  (and (< (abs r) (abs b)) (= (+ (* q b) r) a)))

      theorem expt-1
         ∀ a ∊ Num ∀ p ∊ Byte
            (and (< 0 p) (< p 10)) ⇒ 
               (expt a p) = (* a (expt a (- p 1)))

      theorem totient-1
         ∀ a ∊ Nat
            (and (< 1 a) (< a 100000)) ⇒ 
               (prime? a) ⇔ (= (phi a) (- a 1))

))

(define tests-2 ;; had to split to multiple lists because the register allocator ran out of space (again... fix it!)
   (theory

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

      theorem rlist-rfoldr-copy
         ∀ r ∊ Rlist
            r = (rfoldr rcons null r)

      theorem rlist-rfold-reverse
         ∀ r ∊ Rlist
            r = (rrev (rfold (λ (o x) (rcons x o)) null r))

      theorem rlist-set-get-map
         ∀ r ∊ (Rlist-of Byte)
            (rmap (λ (x) (+ x 1)) r) 
             = (fold 
                  (lambda (rp i) (rset rp i (+ 1 (rget rp i 'bad))))
                  r 
                  (iota 0 1 (rlen r)))

      theorem rlist-convert
         ∀ l ∊ List 
            l = (rlist->list (list->rlist l))

      theorem rlist-cons-moves 
         ∀ r ∊ Rlist
            l ← (rlen r)
            (> l 0) ⇒
               p ∊ l
               (rget r p #t) = (rget (rcons 0 r) (+ p 1) #f)

      theorem iff-put
         ∀ i ∊ Iff ∀ n ∊ Nat
            (iget (iput i n 'foo) n 'bar) = 'foo

      theorem iff-gen
         ∀ l ∊ (List-of Nat)                                     ; (k_1 k_2 ...)
            i ← (fold (λ (i k) (iput i k (+ k 1))) #empty l)     ; iff of k_n ⇒ k_n+1
            (ifold (λ (ok k v) (and ok (= k (- v 1)))) #true i)  ; check all ff[k_n] = k_n+1

      theorem lazy-1
         ∀ n ∊ Byte
            (fold + 0 (iota 0 1 n)) = (lfold + 0 (liota 0 1 n))
      
      theorem lazy-2
         ∀ n ∊ Byte
            (zip cons (iota 0 1 n) (iota n -1 0)) 
               = (force-ll (lzip cons (liota 0 1 n) (liota n -1 0)))

      theorem lazy-3
         ∀ n ∊ Byte
            (reverse (iota 0 1 n)) = (lfoldr cons null (liota 0 1 n))

      theorem str-1
         ∀ l ∊ (List-of Short)
            l = (string->list (list->string l))

      theorem str-2
         ∀ s ∊ String
            s = (bytes->string (string->bytes s))

      theorem list-keep-remove
         ∀ l ∊ List
            (sort < l) = (sort < (append (keep odd? l) (remove odd? l))) 

      theorem pick-test
         ∀ l ∊ (List-of Byte)
            (pair? l) ⇒  
               e ∊ l
               (< e 256)

      theorem bor-band-self
         ∀ a ∊ Nat (bor a a) = (band a a)

      theorem bxor-self
         ∀ a ∊ Nat (bxor a a) = 0

      theorem borband
         ∀ a b c ∊ Nat
            (band (bor a b) c) = (bor (band a c) (band b c))

      theorem bxor-inv
         ∀ a b ∊ Nat
            a = (bxor (bxor a b) b)

))

(define tests
   (foldr append null 
      (list 
         tests-1 
         tests-2)))


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
               (begin
                  ;(print (list (caar tests) 'bad 'with env))
                  (loop rs (cdr tests) 
                     (cons (cons (caar tests) env) failed))))))))

;; run a few rounds at load/compile time, like in in $ make random-test
(let ((seed (random-seed)))
    (let loop ((n 20) (rs (seed->rands seed)))
      (if (= n 0)
         (print "All OK!")
         (lets ((rs fails (failures rs)))
            (if (null? fails)
               (loop (- n 1) rs)
               (print "FAILED: " fails))))))

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
               (print "Pray tell what are " unknown)
               1)
            ((getf dict 'help)
               (print "Usage:")
               (print (format-rules cl-handler))
               0)
            (else
               (lets
                  ((seed (or (getf dict 'seed)  (random-seed)))
                   (end (getf dict 'rounds))) ; #false if not given
                  (print "Starting random continuous test, seed " seed)
                  (if end
                     (print "Will run up to " end)
                     (print "Will run forever"))
                  (let loop ((n 0) (rs (seed->rands seed)))
                     (if (eq? 0 (band n 31))
                        (print " - " n))
                     (lets ((rs fails (failures rs)))
                        (if (null? fails)
                           (if (equal? n end)
                              (begin
                                 (print "Finished successfully")
                                 0)
                              (loop (+ n 1) rs))
                           (begin
                              (print "TESTS FAILED: " (list 'fails fails 'seed seed 'n n))
                              2))))))))))


