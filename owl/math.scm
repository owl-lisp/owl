;;;
;;; Owl math module, first iteration
;;;

;; todo: split this to separate libraries for fixnums, integers, rationals etc?
;; todo: add a simple school-long-division using fxqr to get the top digit usually fairly right and see where the breakeven point is for other methods

;; todo: factor this to smaller libraries

;; wishlist: sin, cos, tan, arc_, pi, e, log, ... (as iterators)
;; todo: remove the cast instruction (only used here iirc)
;; wishlist: complex numbers not implemented yet
;; fixme: at least main base bignum functions (+ - * =) should handle the full dispatch

(define-library (owl math)

   (export 
      number? fixnum? integer?
      + - * = / 
      << < <= = >= > >>   
      band bor bxor
      div ediv rem mod quotrem mod divmod 
      add nat-inc sub mul big-bad-args negate
      even? odd?
      gcd gcdl lcm
      min max minl maxl
      quotient quot
      floor ceiling ceil abs
      sum product
      numerator denumerator
      log log2
      render-number
      fx% ;; temp export until it is removed
      zero? exact? inexact?
      real? complex? rational?
      exact->inexact inexact->exact
      negative? positive?
      denominator numerator
      remainder modulo
      truncate round
      rational complex
      *max-fixnum*
      )

   (import
      (owl defmac)
      (owl list)
      (owl syscall)
      (owl ff))

   (begin

      (define (zero? x) (eq? x 0))

      (define (fixnum? x) 
         (if (size x) ;; <- FIXME - remove after new type no longer collides with functions
            #false
            (let ((t (type x)))
               (or 
                  (eq? t type-fix+)
                  ;(eq? t type-fix-) ;; <- FIXME - breaks build, someone isn't expecting negative fixnums
                  ))))

      (define (exact? n) #true)    ;; RnRS compat
      (define (inexact? n) #false)
      ;; signaling an error would also make sense for these, but as compat
      ;; functions returning the argument as suggested in filed bug
      (define (exact->inexact n) n) 
      (define (inexact->exact n) n)

      ;; deprecated primop
      ;(define-syntax fxdivmod
      ;   (syntax-rules ()
      ;      ((fxdivmod a b)
      ;         (lets ((q1 q2 r (fxqr 0 a b)))
      ;            (values q2 r)))))

      (define-syntax define-traced 
         (syntax-rules ()
            ((define-traced (name arg ...) . whatever)
               (define (name arg ...)
                  (print (list (quote name) (list (quote arg) '= arg) ...))
                  . whatever))))

      (define (nrev-walk num to)
         (if (eq? num null)
            to
            (nrev-walk
               (ncdr num)
               (ncons (ncar num) to))))

      (define (nrev num)
         (nrev-walk num null))

      (define (big-bad-args op a b)
         (error "Bad math:" (list op a b)))

      (define (big-unimplemented op a b)
         (error "Math too high:" (list op a b)))


      ;;;
      ;;; COMPARISON
      ;;;

      (define (big-digits-equal? a b)
         (cond
            ((eq? a b) #true)      ; shared tail or both empty
            ((eq? a null) #false)
            ((eq? b null) #false)
            ((eq? (ncar a) (ncar b))
               (big-digits-equal? (ncdr a) (ncdr b)))
            (else #false)))

      (define (big-less a b lower)
         (cond
            ((eq? a b)    ; both ended or shared tail
               lower)
            ((eq? a null) #true)
            ((eq? b null) #false)
            (else
               (let ((ad (ncar a)) (bd (ncar b)))
                  (cond
                     ((lesser? ad bd)
                        (big-less (ncdr a) (ncdr b) #true))
                     ((eq? ad bd)
                        (big-less (ncdr a) (ncdr b) lower))
                     (else
                        (big-less (ncdr a) (ncdr b) #false)))))))

      (define (int< a b)
         (type-case a
            (fix+
               (type-case b
                  (fix+   (lesser? a b))
                  (int+ #true)
                  (else #false)))
            (fix-
               (type-case b
                  (fix+ #true)
                  (fix-
                     (if (eq? a b)
                        #false
                        (lesser? b a)))
                  (int+ #true)
                  (else #false)))
            (int+
               (type-case b
                  (int+ (big-less a b #false))
                  (else #false)))
            (int-   
               (type-case b
                  (int-
                     (if (big-less a b #false)
                        #false
                        #true))
                  (else #true)))
            (else
               (big-bad-args 'int< a b))))

      ;        =       (compare-numbers a b #false #true #false)
      ;(define (> a b)  (compare-numbers a b #false #false #true))
      ;(define (>= a b) (compare-numbers a b #false #true #true))
      ;(define (< a b) (compare-numbers a b #true #false #false))
      ;(define (<= a b) (compare-numbers a b #true #true #false))



      ; a slightly optimized = 

      (define (= a b)
         (type-case a
            (fix+ (eq? a b))
            (fix- (eq? a b))
            (int+
               (type-case b
                  (int+ (big-digits-equal? a b))
                  (else #false)))
            (int-
               (type-case b
                  (int- (big-digits-equal? a b))
                  (else #false)))
            (rat    
               (type-case b
                  (rat
                     ;; todo: add eq-simple to avoid this becoming recursive
                     (if (= (ncar a) (ncar b))
                        (= (ncdr a) (ncdr b))
                        #false))
                  (else #false)))
            (comp
               (if (teq? b comp)
                  (and (= (ref a 1) (ref b 1)) 
                       (= (ref a 2) (ref b 2)))
                  #false))
            (else 
               (big-bad-args '= a b))))

      ; later just, is major type X

      (define (number? a)
         (type-case a
            (fix+ #true)
            (fix- #true)
            ;(int+ #true)
            ;(int- #true)
            ;(rat  #true)
            ;(comp #true)
            (else 
               ;; major type 9, being all non-fixnum numbers
               (eq? 72 (fxband (type-old a) 248))
               ;(eq? 9 (fxband 63 (type a)))
               )))

      (define (integer? a)
         (type-case a
            (fix+ #true)
            (fix- #true)
            (int+ #true)
            (int- #true)
            (else #false)))

      (define (negative? a)
         (type-case a
            (fix+ #false)
            (fix- #true)
            (int+ #false)
            (int- #true)
            (rat  
               (type-case (ncar a)
                  (fix+ #false)
                  (fix- #true)
                  (int+ #false)
                  (int- #true)
                  (else (error "Bad number: " a))))
            (else (error 'negative? a))))

      (define (positive? a) 
         (if (negative? a) #false #true))

      ;; RnRS compat
      (define real? number?)
      (define complex? number?)
      (define rational? number?)



      ;;;
      ;;; ADDITION
      ;;;

      (define big-one (ncons 1 null))

      (define *max-fixnum* #xffff)

      (define (nat-inc n)
         (cond
            ((teq? n fix+)
               (if (eq? n *max-fixnum*)
                  (ncons 0 big-one)
                  (lets ((n x (fx+ n 1))) n)))
            ((teq? n int+)
               (let ((lo (ncar n)))
                  (if (eq? lo *max-fixnum*)
                     (ncons 0 (nat-inc (ncdr n)))
                     (lets ((lo x (fx+ lo 1)))
                        (ncons lo (ncdr n))))))
            ((eq? n null)
               big-one)
            (else
               (big-bad-args 'inc n n))))

      (define (nlen n)
         (cond
            ((teq? n fix+) 1)
            ((teq? n fix-) 1)
            (else
               (let loop ((n n) (i 0))
                  (if (null? n)
                     i
                     (loop (ncdr n) (nat-inc i)))))))

      (define (add-number-big a big)
         (lets 
            ((b bs big)
             (new overflow? (fx+ a b)))
            (if overflow?
               (if (eq? bs null)
                  (ncons new big-one)
                  (ncons new (add-number-big 1 bs)))
               (ncons new bs))))

      (define (add-big a b carry)
         (cond
            ((eq? a null)
               (if (eq? b null)
                  (if carry big-one null)
                  (if carry (add-number-big 1 b) b)))
            ((eq? b null)
               (if carry (add-number-big 1 a) a))
            (else
               (lets ((r o (fx+ (ncar a) (ncar b))))
                  (if carry
                     (lets ((r o2 (fx+ r 1)))
                        (cond
                           (o (ncons r (add-big (ncdr a) (ncdr b) #true)))
                           (o2 (ncons r (add-big (ncdr a) (ncdr b) #true)))
                           (else (ncons r (add-big (ncdr a) (ncdr b) #false)))))
                     (ncons r
                        (add-big (ncdr a) (ncdr b) o)))))))

      (define-syntax add-small->positive 
         (syntax-rules () 
            ((add-small->positive a b)
               (lets ((r overflow? (fx+ a b)))
                  (if overflow? (ncons r big-one) r)))))




      ;;;
      ;;; SUBSTRACTION
      ;;;

      (define-syntax sub-small->pick-sign
         (syntax-rules ()
            ((sub-small->pick-sign a b)
               (lets ((r uf? (fx- a b)))
                  (if uf?
                     (lets ((r _ (fx- b a))) ;; could also fix here by adding or bitwise
                        (cast r 32))
                     r)))))

      ; bignum - fixnum -> either
      (define (sub-big-number a b leading?)
         (lets ((r underflow? (fx- (ncar a) b)))
            (cond
               (underflow?
                  (let ((tail (sub-big-number (ncdr a) 1 #false)))
                     (cond
                        (tail (ncons r tail))   ; otherwise tail went to 0
                        (leading? r)
                        ((eq? r 0) #false)
                        (else (ncons r null)))))
               ((eq? r 0)
                  (let ((tail (ncdr a)))
                     (if (eq? tail null)
                        (if leading? r #false)
                        (ncons r tail))))
               (else 
                  (ncons r (ncdr a))))))

      ; a - B = a + -B = -(-a + B) = -(B - a)

      (define (sub-number-big a b first?)
         (let ((res (sub-big-number b a #true)))
            ; res is either fixnum or bignum
            (type-case res
               (fix+ (cast res 32))
               (else (cast res 41)))))


      ; substract from a, which must be bigger

      (define (sub-digits a b borrow? leading?)
         (cond
            ((eq? a null)
               #false)
            ((eq? b null)
               (if borrow?
                  (sub-big-number a 1 leading?)
                  a))
            (else
               (lets ((r u? (fx- (ncar a) (ncar b))))
                  (if borrow?
                     (lets ((r u2? (fx- r 1)))
                        (let ((tail
                           (cond
                              (u? (sub-digits (ncdr a) (ncdr b) #true #false))
                              (u2? (sub-digits (ncdr a) (ncdr b) #true #false))
                              (else
                                 (sub-digits (ncdr a) (ncdr b) #false #false)))))
                           (cond
                              (tail (ncons r tail))
                              (leading? r)
                              ((eq? r 0) #false)
                              (else (ncons r null)))))

                     (let ((tail (sub-digits (ncdr a) (ncdr b) u? #false)))
                        (cond
                           (tail (ncons r tail))
                           (leading? r)
                           ((eq? r 0) #false)
                           (else (ncons r null)))))))))


      ; A - B = -(B - A)

      (define (sub-big a b)
         (cond
            ((big-less a b #false) 
               (let ((neg (sub-digits b a #false #true)))
                  (cond
                     ((eq? neg 0) neg)
                     ((teq? neg fix+) (cast neg 32))
                     (else (cast neg 41)))))
            (else
               (sub-digits a b #false #true))))

      ; add bits, output is negative

      (define (add-small->negative a b)
         (lets ((r overflow? (fx+ a b)))
            (if overflow?
               (cast (ncons r big-one) 41)
               (cast r 32))))


      ; for changing the (default positive) sign of unsigned operations 
      (define-syntax negative 
         (syntax-rules (teq? imm cast if fix+)
            ((negative (op . args))
               (let ((foo (op . args)))
                  (negative foo)))
            ((negative x)
               (if (teq? x fix+)
                  (cast x 32)
                  (cast x 41)))))
               
      (define-syntax rational
         (syntax-rules ()
            ((rational a b) (mkt 73 a b))))
      
      (define (negate num)   
         (type-case num
            (fix+ 
               (if (eq? num 0)
                  0
                  (cast num 32)))   ;; a  -> -a
            (fix- (cast num 0))   ;; -a ->  a
            (int+                ;;  A -> -A
               (mkt 41 (ncar num) (ncdr num)))
            (int-             ;; -A -> A
               (ncons (ncar num) (ncdr num)))
            (rat
               (lets ((a b num))
                  (rational (negate a) b)))
            (else
               (big-bad-args 'negate num #false))))



      ;;;
      ;;; Addition and substraction generics 
      ;;;

      (define (add a b)
         (type-case a
            (fix+ ; a signed fixnum
               (type-case b
                  (fix+ (add-small->positive a b))            ;; +a + +b -> c | C
                  (fix- (sub-small->pick-sign a b))         ;; +a + -b -> +c | -c, underflow determines sign
                  (int+ (add-number-big a b))               ;; +a + +B -> +C
                  (int- (sub-number-big a b #true))         ;; +a + -B -> -c | -C
                  (else (big-bad-args 'add a b))))
            (fix-
               (type-case b
                  (fix+ (sub-small->pick-sign b a))         ;; -a + +b == +b + -a -> as above (no need to recurse)
                  (fix- (add-small->negative a b))         ;; -a + -b -> -c | -C
                  (int+ (sub-big-number b a #true))            ;; -a + +B == +B - +a -> sub-big-number 
                  (int- (cast (add-number-big a b) 41))   ;; -a + -B == -C == -(a + B)   
                  (else (big-bad-args 'add a b))))
            (int+
               (type-case b
                  (fix+ (add-number-big b a))               ;; +A + +b -> +C
                  (fix- (sub-big-number a b #true))            ;; +A + -b == -b + +A -> as above
                  (int+ (add-big a b #false))                  ;; +A + +B == +C
                  (int- (sub-big a b))                     ;; +A + -B == +c | -c | +C | -C
                  (else (big-bad-args 'add a b))))
            (int-
               (type-case b
                  (fix+ (sub-number-big b a #true))            ;; -A + +b == +b + -A -> as above
                  (fix- (cast (add-number-big b a) 41))      ;; -A + -b == -b + -A = -C -> as above
                  (int+ (sub-big b a))                     ;; -A + +B == +B + -A -> as above
                  (int- (cast (add-big a b #false) 41))      ;; -A + -B == -(A + B)
                  (else (big-bad-args 'add a b))))
            (else 
               (big-bad-args 'add a b))))

      ; substraction is just just opencoded (+ a (negate b))

      ;; substraction for at most bignum integers (needed for the more complex ones)
      (define (subi a b)
         (type-case a
            (fix+ ; a signed fixnum
               (type-case b
                  (fix+   (sub-small->pick-sign a b))         ;; +a - +b -> as +a + -b
                  (fix- (add-small->positive a b))         ;; +a - -b -> as +a + +b
                  (int+ (sub-number-big a b #true))            ;; +a - +B -> as +a + -B
                  (int-   (add-number-big a b))            ;; +a - -B -> as +a + +B
                  (else (big-bad-args '- a b))))
            (fix-
               (type-case b
                  (fix+ (add-small->negative a b))            ;; -a - +b -> as -a + -b
                  (fix- (sub-small->pick-sign b a))         ;; -a - -b -> as -a + +b
                  (int+ (cast (add-number-big a b) 41))   ;; -a - +B -> as -a + -B
                  (int- (sub-big-number b a #true))         ;; -a - -B -> as -a + +B
                  (else (big-bad-args '- a b))))
            (int+
               (type-case b
                  (fix+ (sub-big-number a b #true))            ;; +A - +b -> as +A + -b
                  (fix- (add-number-big b a))               ;; +A - -b -> as +A + +b
                  (int+ (sub-big a b))                     ;; +A - +B -> as +A + -B
                  (int- (add-big a b #false))                  ;; +A - -B -> as +A + +B
                  (else (big-bad-args '- a b))))
            (int-
               (type-case b
                  (fix+ (cast (add-number-big b a) 41))      ;; -A - +b -> as -A + -b
                  (fix- (sub-number-big b a #true))            ;; -A - -b -> as -A + +b
                  (int+ (cast (add-big a b #false) 41))         ;; -A - +B -> as -A + -B
                  (int- (sub-big b a))                     ;; -A - -B -> as -A + +B
                  (else (big-bad-args '- a b))))
            (else 
               (big-bad-args '- a b))))

         


      ;;;
      ;;; BITWISE OPERATIONS
      ;;;

      ; fxband, fxbor, fx<<, fx>>
      ; fxband, fxor -> result
      ; fx<< -> hi + lo
      ; fx>> -> hi + lo

      (define (shift-right-walk this rest n first?)
         (if (eq? rest null)
            (cond
               (first?  this)
               ((eq? this 0) #false)
               (else
                  (ncons this null)))
            (let ((next (ncar rest)))
               (lets ((hi lo (fx>> next n)))
                  (lets
                     ((this (fxbor this lo))
                      (tail (shift-right-walk hi (ncdr rest) n #false)))
                     (cond
                        (tail (ncons this tail))
                        ((eq? this 0)
                           (if first? 0 #false))
                        (else 
                           (if first? this 
                              (ncons this null)))))))))

      (define (shift-right a n)
         (if (eq? a null) 
            0
            (lets ((hi lo (fx>> (ncar a) n)))
               (shift-right-walk hi (ncdr a) n #true))))

      ; words known to be fixnum
      (define (drop-digits a words)
         (cond
            ((eq? words 0) a)
            ((eq? a null) a)
            (else
               (lets ((words _ (fx- words 1)))
                  (drop-digits (ncdr a) words)))))

      ; optimize << and >> since they will be heavily used in subsequent ops

      (define hi-12 #b1111111111110000)
      (define lo-4  #b0000000000001111)

      (define (>> a b)
         (type-case b 
            (fix+
               (let ((wor (fxband b hi-12)) (bits  (fxband b lo-4)))
                  (if (eq? wor 0) 
                     (type-case a
                        (fix+ (receive (fx>> a bits) (lambda (hi lo) hi)))
                        (fix- (receive (fx>> a bits) (lambda (hi lo) (if (eq? hi 0) 0 (negate hi)))))
                        (int+ (shift-right a bits))
                        (int- (negative (shift-right a bits)))
                        (else (big-bad-args '>> a b)))
                     (lets ((wo lo (fx>> wor 4)))
                        (type-case a
                           (fix+   0)
                           (fix- 0)
                           (int+ (shift-right (drop-digits a wo) bits))
                           (int- 
                              (negative 
                                 (shift-right (drop-digits a wo) bits)))
                           (else (big-bad-args '>> a b)))))))
            (int+
               (if (eq? a 0)
                  0
                  (>> (>> a hi-12) (subi b hi-12))))
            (else
               (big-bad-args '>> a b))))

      ; make a digit with last as low bits
      (define (shift-left num n last)
         (if (eq? num null)
            (if (eq? last 0)
               null
               (ncons last null))
            (lets ((hi lo (fx<< (ncar num) n)))
               (ncons (fxbor last lo)
                  (shift-left (ncdr num) n hi)))))

      ; << quarantees n is a fixnum
      (define (extend-digits num n)
         (if (eq? n 0)
            num
            (lets ((n _ (fx- n 1)))
               (extend-digits (ncons 0 num) n))))

      ; fixme: a << b = a * 2^b for other numbers
      ; thus #b0.0001 << 4 = 1

      (define (<< a b)
         (cond
            ((eq? a 0) 0)
            ((teq? b fix+)
               (let ((wor (fxband b hi-12)) (bits  (fxband b lo-4)))
                  (lets ((words lo (fx>> wor 4)))
                     (type-case a
                        (fix+
                           (lets ((hi lo (fx<< a bits)))
                              (if (eq? hi 0)
                                 (if (eq? words 0)
                                    lo
                                    (extend-digits (ncons lo null) words))
                                 (if (eq? words 0)
                                    (ncons lo (ncons hi null)) 
                                    (extend-digits 
                                       (ncons lo (ncons hi null)) 
                                       words)))))
                        (fix-
                           (lets ((hi lo (fx<< a bits)))
                              (if (eq? hi 0)
                                 (if (eq? words 0)
                                    (cast lo 32)
                                    (cast 
                                       (extend-digits (ncons lo null) words) 
                                       41))
                                 (cast 
                                    (extend-digits 
                                       (ncons lo (ncons hi null)) words) 
                                    41))))
                        (int+
                           (extend-digits (shift-left a bits 0) words))
                        (int-
                           (cast (extend-digits (shift-left a bits 0) words) 41))
                        (else
                           (big-bad-args '<< a b))))))
            ((teq? b int+)
               (<< (<< a hi-12) (subi b hi-12)))
            (else
               (big-bad-args '<< a b))))
               

      (define (big-band a b)
         (cond
            ((eq? a null) 0)
            ((eq? b null) 0)
            (else
               (lets
                  ((this (fxband (ncar a) (ncar b)))
                   (tail (big-band (ncdr a) (ncdr b))))
                  (cond
                     ((eq? tail 0) this)
                     ((teq? tail fix+)
                        (ncons this (ncons tail null)))
                     (else
                        (ncons this tail)))))))

      ;; answer is quaranteed to be a bignum
      (define (big-bor a b)
         (cond
            ((eq? a null) b)
            ((eq? b null) a)
            (else
               (lets
                  ((this (fxbor (ncar a) (ncar b)))
                   (tail (big-bor (ncdr a) (ncdr b))))
                  (ncons this tail)))))

      ;; → null | bignum
      (define (big-bxor-digits a b)
         (cond
            ((null? a) b)
            ((null? b) a)
            (else
               (lets
                  ((this (fxbxor (ncar a) (ncar b)))
                   (tail (big-bxor-digits (ncdr a) (ncdr b))))
                  (if (null? tail)
                     (if (eq? this 0)
                        null
                        (ncons this tail))
                     (ncons this tail))))))

      (define (big-bxor a b)
         (let ((r (big-bxor-digits a b)))
            (cond
               ;; maybe demote to fixnum
               ((null? r) 0) 
               ((null? (ncdr r)) (ncar r))
               (else r))))

      ; not yet defined for negative
      (define (band a b)
         (type-case a
            (fix+
               (type-case b
                  (fix+ (fxband a b))
                  (int+ (fxband a (ncar b)))
                  (else
                     (big-bad-args 'band a b))))
            (int+
               (type-case b
                  (fix+
                     (fxband (ncar a) b))
                  (int+
                     (big-band a b))
                  (else
                     (big-bad-args 'band a b))))
            (else
               (big-bad-args 'band a b))))

      (define (even? n) (eq? 0 (band n 1)))
      (define (odd?  n) (eq? 1 (band n 1)))

      (define (bor a b)
         (type-case a
            (fix+
               (type-case b
                  (fix+ (fxbor a b))
                  (int+ 
                     (ncons (fxbor a (ncar b))
                        (ncdr b)))
                  (else
                     (big-bad-args 'bor a b))))
            (int+
               (type-case b
                  (fix+
                     (ncons (fxbor b (ncar a))
                        (ncdr a)))
                  (int+
                     (big-bor a b))
                  (else
                     (big-bad-args 'bor a b))))
            (else
               (big-bad-args 'bor a b))))

      (define (bxor a b)
         (type-case a
            (fix+
               (type-case b
                  (fix+ (fxbxor a b))
                  (int+ 
                     (ncons (fxbxor a (ncar b)) (ncdr b)))
                  (else
                     (big-bad-args 'bxor a b))))
            (int+
               (type-case b
                  (fix+
                     (ncons (fxbxor b (ncar a)) (ncdr a)))
                  (int+
                     (big-bxor a b))
                  (else
                     (big-bad-args 'bxor a b))))
            (else
               (big-bad-args 'bxor a b))))



      ;;;
      ;;; MULTIPLICATION 
      ;;;

      ; O(n), basic multiply bignum b by fixnum a with carry

      (define (mult-num-big a b carry)
         (cond
            ((eq? b null)
               (if (eq? carry 0)
                  null
                  (ncons carry null)))
            ((eq? carry 0)
               (lets ((lo hi (fx* a (ncar b))))
                  (ncons lo
                     (mult-num-big a (ncdr b) hi))))
            (else
               (lets
                  ((lo hi (fx* a (ncar b)))
                   (lo o? (fx+ lo carry)))
                  (if o?
                     (lets ((hi o? (fx+ hi 1)))
                        (ncons lo
                           (mult-num-big a (ncdr b) hi)))
                     (ncons lo
                        (mult-num-big a (ncdr b) hi)))))))

      ; O(1), fixnum multiply overflowing to bignum

      ;(define (mult-fixnums a b)
      ;   (receive (fx* a b)
      ;      (lambda (lo hi)
      ;         (if (eq? hi 0)
      ;            lo
      ;            (ncons lo (ncons hi null))))))

      (define-syntax mult-fixnums
         (syntax-rules ()
            ((mult-fixnums a b)
               (lets ((lo hi (fx* a b)))
                  (if (eq? hi 0)
                     lo
                     (ncons lo (ncons hi null)))))))
      

      ;;;
      ;;; Big multiplication
      ;;;

      ; current approach: karatsuba + schoolboy algorithm for small numbers

      ; ensure bigness
      (define (bigen x)
         (if (eq? (type x) type-fix+)
            (ncons x null)
            x))

      ; a + (b << ex*16)
      (define (add-ext a b ex)
         (cond
            ((eq? ex 0) (if (null? a) (bigen b) (add a b)))
            ((null? a)
               (ncons 0
                  (add-ext null b (subi ex 1))))
            ((teq? a fix+) (add-ext (ncons a null) b ex))
            ((teq? ex fix+)
               (lets 
                  ((ex u (fx- ex 1))
                   (d ds a))
                  (ncons d (add-ext ds b ex))))
            (else
               (ncons (ncar a)
                  (add-ext (ncdr a) b (subi ex 1))))))

      ; fixme, should just keep jumbo digits for for added versions and 
      ;        perform the carrying just once in a final pass. add merges 
      ;        and high parts (if any) of the digits are the carriables.
      ; can be used for small bignums
      (define (mul-simple a b)
         (if (null? a)
            null
            (lets
                ((digit (ncar a))
                (head (ncons 0 (mul-simple (ncdr a) b)))
                (this (mult-num-big digit b 0)))
               (add head this))))

      ; downgrade to fixnum if length 1
      (define (fix n)
         (if (null? (ncdr n)) (ncar n) n))

      ; drop leading zeros, reverse digits and downgrade to fixnum if possible
      (define (fixr n)
         (if (null? n) 
            0
            (lets ((d ds n))
               (cond
                  ((null? ds) d)
                  ((eq? d 0) (fixr ds))
                  (else (nrev n)))))
         ;(fix (nrev n))
         )

      ; cut numbers from the midpoint of smaller while counting length (tortoise and hare akimbo)
      (define (splice-nums ah bh at bt rat rbt s? l)
         (cond
            ((null? ah)
               (values (fix at) (fixr rat) (fix bt) (fixr rbt) l))
            ((null? bh)
               (values (fix at) (fixr rat) (fix bt) (fixr rbt) l))
            (s?
               (splice-nums (ncdr ah) (ncdr bh) at bt rat rbt #false l))
            (else
               (lets 
                  ((a at at) 
                   (b bt bt)
                   (l over (fx+ l 1))) ; fixme, no bignum len
                  (splice-nums (ncdr ah) (ncdr bh)
                     at bt (ncons a rat) (ncons b rbt) #true l)))))

      (define (kara a b)
         (cond
            ;; O(1) leaf cases
            ((eq? a 0) 0)
            ((eq? b 0) 0)
            ((eq? a 1) b)
            ((eq? b 1) a)

            ;; O(n) or O(1) leaf cases
            ((teq? a fix+) (if (teq? b fix+) (mult-fixnums a b) (mult-num-big a b 0)))
            ((teq? b fix+) (mult-num-big b a 0))
            ((null? (ncdr a)) 
               (if (null? (ncdr b))
                  (mult-fixnums (ncar a) (ncar b))
                  (mult-num-big (ncar a) b 0)))
            ((null? (ncdr b)) (mult-num-big (ncar b) a 0))

            ;; otherwise divide et imperial troopers
            (else
               (lets
                  ; 3O(n)
                  ((ah at bh bt atl
                     (splice-nums a b a b null null #true 0)))
                  (if (lesser? atl 30) 
                     (mul-simple a b)
                     (lets
                         ; 3F(O(n/2)) + 2O(n/2)
                        ((z2 (kara ah bh))
                         (z0 (kara at bt))
                         (z1a 
                           (lets ((a (add ah at)) (b (add bh bt)))
                              (kara a b)))
                         ; 2O(n)
                         (z1 (subi z1a (add z2 z0)))
                         ; two more below
                         (x (if (eq? z1 0) z0 (add-ext z0 z1 atl))))
                        (if (eq? z2 0)
                           x
                           (add-ext x z2 (<< atl 1)))))))))

      ;(define mult-big mul-simple)   ; for debugging only!
      (define mult-big kara)

      (define (muli a b)
         (cond 
            ; are these actually useful?
            ((eq? a 0) 0)
            ;((eq? a 1) b)
            ((eq? b 0) 0)
            ;((eq? b 1) a)
            (else
               (type-case a
                  (fix+
                     (type-case b
                        (fix+ (mult-fixnums a b))                  ; +a * +b
                        (fix- (negative (mult-fixnums a b)))      ; +a * -b
                        (int+ (mult-num-big a b 0))               ; +a * +B
                        (int- (negative (mult-num-big a b 0)))   ; +a * -b
                        (else (big-bad-args 'mul a b))))
                  (fix-
                     (type-case b
                        (fix+ (negative (mult-fixnums a b)))      ; -a * +b -> -c | -C
                        (fix- (mult-fixnums a b))                  ; -a * -b -> +c | +C
                        (int+ (cast (mult-num-big a b 0) 41))   ; -a * +B -> -C
                        (int- (mult-num-big a b 0))            ; -a * -B -> +C
                        (else (big-bad-args 'mul a b))))
                  (int+
                     (type-case b
                        (fix+ (mult-num-big b a 0))            ; +A * +b -> +C
                        (fix- (cast (mult-num-big b a 0) 41))    ; +A * -b -> -C
                        (int+ (mult-big a b))               ; +A * +B -> +C
                        (int- (cast (mult-big a b) 41))      ; +A * -B -> -C
                        (else (big-bad-args 'mul a b))))
                  (int-   
                     (type-case b
                        (fix+ (cast (mult-num-big b a 0) 41))      ; -A * +b -> -C
                        (fix- (mult-num-big b a 0))               ; -A * -b -> +C
                        (int+ (cast (mult-big a b) 41))      ; -A * +B -> -C
                        (int- (mult-big a b))                  ; -A * -B -> +C
                        (else (big-bad-args 'mul a b))))
                  (rat
                     (type-case b
                        (rat  (big-bad-args 'mul a b))         ; handle this before mul for now
                        (else (muli b a))))                  ; otherwise use other branches
                  (else (big-bad-args 'mul a b))))))

      ;;; comparison (rationals need mul)

      ;; todo: rational comparison is dumb.. first one should check the signs, then whether log_2(ab') < log_2(ba'), which is way faster than multiplication, and only as a last resort do the actual multiplication. also, more common comparisons should be inlined here.
      (define (< a b)
         (cond
            ; add short type paths here later
            ((teq? a rat)
               (if (teq? b rat)
                  ; a/a' < b/b' <=> ab' < ba'
                  (int< (muli (ncar a) (ncdr b)) (muli (ncar b) (ncdr a)))
                  ; a/a' < b <=> a < ba'
                  (int< (ncar a) (muli b (ncdr a)))))
            ((teq? b rat)
               ; a < b/b' <=> ab' < b
               (int< (muli a (ncdr b)) (ncar b)))
            (else 
               (int< a b))))

      (define (denominator n)
         (if (teq? n rat)
            (ncdr n)  ;; always positive
            1))

      (define (numerator n)
         (if (teq? n rat)
            (ncar n)  ;; has the sign if negative
            1))

      (define (<= a b)
         (or (< a b) (= a b)))

      (define (> a b) (< b a))
      (define (>= a b) (<= b a))

      (define (min a b) (if (< a b) a b))
      (define (max a b) (if (< a b) b a))

      (define (minl as) (fold min (car as) (cdr as)))
      (define (maxl as) (fold max (car as) (cdr as)))

      ;;;
      ;;; DIVISION 
      ;;;



      ; walk down a and compute each digit of quotient using the top 2 digits of a
      (define (qr-bs-loop a1 as b out)
         (if (null? as)
            (if (null? (ncdr out))
               (values (ncar out) a1)
               (values out a1))
            (lets
               ((a2 as as)
                (q1 q2 r (fxqr a1 a2 b)))
               (if (eq? q1 0)
                  (qr-bs-loop r as b (ncons q2 out))
                  (qr-bs-loop r as b (ncons q2 (ncons q1 out)))))))

      (define (qr-big-small a b) ; -> q r
         (cond
            ((eq? b 0)
               (big-bad-args 'qr-big-small a b))
            ;((null? (ncdr (ncdr a))) ; (al ah) b -> can use fxqr primop
            ;   (let ((tl (ncdr a)))
            ;      (fxqr (ncar tl) (ncar a) b)))
            (else
               (lets 
                  ((ra (nrev a))
                   (a as ra))
                  (qr-bs-loop a as b null)))))

      ; once upon a time
      ;(define (big-divmod a b)
      ;   (let loop ((a a) (q 0))
      ;      (if (>= a b)
      ;         (loop (- a b) (+ q 1))
      ;         (values q a))))

      ; decrease b
      (define (shift-local-down a b n)
         (cond
            ((eq? n 0) 0)
            ((eq? a b) (subi n 1))
            ((lesser? b a) n)
            (else 
               (lets ((b over (fx>> b 1)))
                  (shift-local-down a b (subi n 1))))))

      ; increase b
      (define (shift-local-up a b n)
         (cond
            ((eq? a b) (subi n 1))
            ((lesser? a b) (subi n 1))
            (else 
               (lets ((over b (fx<< b 1)))
                  (if (eq? over 0)
                     (shift-local-up a b (nat-inc n))
                     (subi n 1))))))

      (define (div-shift a b n)
         (if (teq? a fix+)   
            0
            (let ((na (ncdr a)) (nb (ncdr b)))
               (cond
                  ((null? na)
                     (if (null? nb)
                        (let ((b-lead (ncar b)))
                           (if (eq? b-lead *max-fixnum*)
                              (if (eq? n 0)
                                 0
                                 (shift-local-down (ncar a) #x7fff (subi n 1)))
                              (let ((aa (ncar a)) (bb (add b-lead 1)))
                                 ; increment b to ensure b'000.. > b....
                                 (cond
                                    ((lesser? aa bb)
                                       (shift-local-down aa bb n))
                                    (else
                                       (shift-local-up aa bb n))))))
                        ; divisor is larger
                        0))
                  ((null? nb)
                     (div-shift (ncdr a) b (add n 16)))
                  (else
                     (div-shift (ncdr a) (ncdr b) n))))))
               
      (define (nat-quotrem-finish a b out)
         (let ((next (subi a b)))
            (if (negative? next)
               (values out a)
               (nat-quotrem-finish next b (nat-inc out)))))
    
      (define (nat-quotrem a b)
         (let loop ((a a) (out 0))
            (let ((s (div-shift a b 0)))
               (cond
                  ; hack warning, -1 0 1 are lesser of 2, but not -2 
                  ; (tag bits including sign are low)
                  ((lesser? s 2)
                     (nat-quotrem-finish a b out))
                  (else
                     (let ((this (<< b s)))
                        (loop (subi a this) (add out (<< 1 s)))))))))

      (define (div-big->negative a b)
         (lets ((q r (nat-quotrem a b)))
            (negate q)))

      ;; fixme: big division is ugly and slow
      (define (div-big-qr a b)
         (lets ((q r (nat-quotrem a b))) q))



      ;;; 
      ;;; REMAINDER
      ;;;

      ;; mainly manually partial evaling remainder separately, since a fast one is needed for now for gcd and rational math

      (define (nat-rem-finish a b)
         (let ((ap (subi a b)))
            (if (negative? ap)
               a
               (nat-rem-finish ap b))))

      ;; substract large b*2^n's until a < b
      (define (nat-rem-simple a b)
         (let loop ((a a))
            (let ((s (div-shift a b 0)))
               (cond
                  ; hack warning, -1 0 1 are lesser of 2, but not -2 
                  ; (tag bits including sign are low)
                  ((lesser? s 2)
                     (nat-rem-finish a b))
                  (else
                     (loop (subi a (<< b s))))))))

      ;; reverse number remainder

      (define (rsub a b) ; -> a' borrow|null
         (cond
            ((null? b) (values a #false)) ; ok if borrowable
            ((null? a) (values a null)) ; fail
            (else
               (lets
                  ((d (subi (ncar a) (ncar b))) ; fix+ or fix-
                   (tl dr (rsub (ncdr a) (ncdr b))))
                  (cond
                     ((null? dr) (values tl dr)) ; failed below
                     (dr
                        (let ((d (subi d 1))) ; int- (of was -*max-fixnum*), fix- or fix+
                           (if (negative? d)
                              (values (ncons (add d #x10000) tl) #true) ; borrow
                              (values (ncons d tl) #false))))
                     ((teq? d fix-) ; borrow
                        (values (ncons (add d #x10000) tl) #true))
                     (else
                        (values (ncons d tl) #false)))))))

      (define (drop-zeros n)
         (cond
            ((null? n) n)
            ((eq? 0 (ncar n)) (drop-zeros (ncdr n)))
            (else n)))

      (define (rev-sub a b) ; bignum format a' | #false
         (lets ((val fail? (rsub a b)))
            (if fail? 
               #false
               (drop-zeros val))))

      ;; reverse number multiplication by digit

      (define (rmul n d) ; tl x carry-up
         (if (null? n)
            (values null 0)
            (lets
               ((x tl n) 
                (lo hi (fx* x d))
                (tl carry (rmul (ncdr n) d))
                (lo over (fx+ lo carry)))
               (if over
                  (lets ((hi over (fx+ hi 1)))
                     (values (ncons lo tl) hi))
                  (values (ncons lo tl) hi)))))

      (define (rmul-digit n d) ; -> bignum
         (cond
            ((eq? d 0) (ncons 0 null))
            ((eq? d 1) n)
            (else
               (lets ((tl carry (rmul n d)))
                  (if (eq? carry 0)
                     tl
                     (ncons carry tl))))))
            
      (define (rrem a b) ; both should be scaled to get a good head for b
         (cond
            ((null? a) a)
            ((null? (ncdr a)) a)
            ((lesser? (ncar b) (ncar a))
               (lets
                  ((h over (fx+ (ncar b) 1))
                   (_ f r (fxqr 0 (ncar a) h))
                   (bp (rmul-digit b f))
                   (ap (rev-sub a bp)))
                  (if ap (rrem ap b) a)))
            ((rev-sub a b) =>
               (lambda (ap) (rrem ap b)))
            (else
               (lets
                  ((h o (fx+ (ncar b) 1))
                   (f r (qr-big-small (ncons (ncar (ncdr a)) (ncons (ncar a) null)) h)) ; FIXME, use fxqr instead
                   )
                  (if (teq? f fix+) 
                     (lets
                        ((bp (rmul-digit b f))
                         (ap (rev-sub a bp))
                         (ap (or ap (rev-sub a (ncons 0 bp)))))
                        (if ap (rrem ap b) a))
                     (lets
                        ((f (cadr f))
                         (bp (rmul-digit b f))
                         ;(ap (rev-sub a bp))
                         (ap #false)
                         (ap (or ap (rev-sub a (ncons 0 bp)))))
                        (if ap (rrem ap b) a)))))))

      (define (nat-rem-reverse a b)
         (if (< a b)
            a
            (lets ((rb (nrev b)))
               (if (lesser? #b0000111111111111 (ncar rb))
                  ; scale them to get a more fitting head for b
                  ; and also get rid of the special case where it is *max-fixnum*
                  (>> (nat-rem-reverse (<< a 8) (<< b 8)) 8)
                  (let ((r (rrem (nrev a) rb)))
                     (cond
                        ((null? r) 0)
                        ((null? (ncdr r)) (ncar r))
                        (else (nrev r))))))))


      ;(define nat-rem nat-rem-simple)    ; better for same sized numers
      (define nat-rem nat-rem-reverse)    ; better when b is smaller


      ;;;
      ;;; Exact division
      ;;;

      ;; this algorithm is based on the observation that the lowest digit of 
      ;; the quotient in division, when the remainder will be 0, depends only 
      ;; on the lowest bits of the divisor and quotient, which allows the 
      ;; quotient to be built bottom up using only shifts and substractions.

      ; bottom up exact division, base 2

      (define (div-rev st out)
         (if (null? st) out
            (div-rev (ncdr st) (ncons (ncar st) out))))

      (define (div-finish n)
         (cond
            ((null? (ncdr n)) (ncar n))
            ((eq? (ncar n) 0) (div-finish (ncdr n)))
            (else (div-rev n null))))

      ; fixme, postpone and merge shifts and substraction

      ; later (sub-shifted a b s) in 1-bit positions
      ; b is usually shorter, so shift b right and then substract instead 
      ; of moving a by s

      (define bit-poss
         (list->ff
            (map (lambda (n) (cons (<< 1 n) n))
               '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))))

      ;; FIXME - keep bit-pos in a variable to avoid a frequent lookup. ff change had major speed effect in whole arith.scm
      (define (divex bit a b out)
         (cond
            ((teq? a fix-) #false) ;; not divisible
            ((teq? a int-) #false) ;; not divisible
            ((eq? a 0) (div-finish out))
            ((eq? (band a bit) 0) ; O(1)
               (if (eq? bit 32768)
                  (lets 
                     ((a (ncdr a))
                      (a (if (null? (ncdr a)) (ncar a) a)))
                     (divex 1 a b (ncons 0 out)))
                  (lets ((_ bit (fx<< bit 1)))
                     (divex bit a b out))))
            (else ; shift + substract = amortized O(2b) + O(log a)
               (divex bit (subi a (<< b (get bit-poss bit #false)))
                  b (ncons (fxbor bit (ncar out)) (ncdr out))))))

      (define divex-start (ncons 0 null))

      ; FIXME: shifts are O(a+b), switch to bit walking later to get O(1)

      (define (nat-divide-exact a b)
         (if (eq? (band b 1) 0)
            (if (eq? (band a 1) 0) 
               ;; drop a power of two from both
               (nat-divide-exact (>> a 1) (>> b 1))
               #false) ;; not divisible
            (divex 1 a b divex-start)))

      (define (maybe-negate a)
         (if a (negate a) a))

      ; int nat -> int | #false
      (define (divide-exact a b)
         (type-case a
            (fix- (maybe-negate (divide-exact (negate a) b)))
            (int- (maybe-negate (divide-exact (negate a) b)))
            (else (nat-divide-exact a b))))

      (define ediv divide-exact)


      ;; the same can be generalized for base B, where 2^16 is convenient given that it is the 
      ;; base in which bignums are represented in owl. the lowest digit will have 

      ;; fixme, add ^

      
      ;;; alternative division

      (define (div-big-exact a b) (ediv (subi a (nat-rem a b)) b))

      (define div-big div-big-exact)

      ;;; continue old general division

      (define (div-fixnum->negative a b)
         (lets ((_ q r (fxqr 0 a b)))
            (if (eq? q 0)
               q
               (cast q 32))))

      (define (div-big-num->negative a b)
         (lets ((q r (qr-big-small a b)))
            (type-case q
               (fix+ (cast q 32))
               (else (cast q 41)))))

      ; fixme, could just call quotrem -> q
      ; should output rationals later, inputs always within int
      ; note, now *only* used in rationalize and exported for integer math
      (define (div a b)
         (if (eq? b 0)
            (big-bad-args 'div a b)
            (type-case a
               (fix+
                  (type-case b
                     (fix+ (lets ((_ q r (fxqr 0 a b))) q))   ; +a / +b -> +c
                     (fix- (div-fixnum->negative a b))                  ; +a / -b -> -c | 0
                     (int+ 0)                                             ; +a / +B -> 0
                     (int- 0)                                             ; +a / -B -> 0
                     (else (big-bad-args 'div a b))))
               (fix-
                  (type-case b
                     (fix+ (div-fixnum->negative a b))                  ; -a / +b -> -c | 0
                     (fix- (lets ((_ q r (fxqr 0 a b))) q))             ; -a / -b -> +c
                     (int+ 0)                                           ; -a / +B -> 0
                     (int- 0)                                             ; -a / -B -> 0
                     (else (big-bad-args 'div a b))))
               (int+
                  (type-case b
                     (fix+ (lets ((q r (qr-big-small a b))) q))   ; +A / +b -> +c | +C
                     (fix- (div-big-num->negative a b))            ; +A / -b -> -c | -C
                     (int+ (div-big a b))                           ; +A / +B -> 0 | +c | +C
                     (int- (div-big->negative a (negate b)))      ; +A / -B -> 0 | -c | -C
                     (else (big-bad-args 'div a b))))
               (int-
                  (type-case b
                     (fix+ (div-big-num->negative a b))            ; -A / +b -> -c | -C
                     (fix- (lets ((q r (qr-big-small a b))) q))    ; -A / -b -> +c | +C
                     (int+ (div-big->negative (negate a) b))                     ; -A / +B -> 0 | -c | -C
                     (int- (div-big (negate a) (negate b)))                              ; -A / -B -> 0 | +c | +C
                     (else (big-bad-args 'div a b))))   
               (else (big-bad-args 'div a b)))))

      (define-syntax fx%
         (syntax-rules ()
            ((fx% a b)
               (lets ((q1 q2 r (fxqr 0 a b))) r))))

      (define (rem a b)
         (type-case a
            (fix+
               (type-case b
                  (fix+ (fx% a b))
                  (fix- (fx% a b))
                  (int+ a) 
                  (int- a)
                  (else (big-bad-args 'mod a b))))
            (fix-
               (type-case b
                  (fix+ (negate (fx% a b)))
                  (fix- (negate (fx% a b)))
                  (int+ a)
                  (int- a)
                  (else (big-bad-args 'mod a b))))
            (int+
               (type-case b
                  (fix+ (receive (qr-big-small a b) (lambda (q r) r)))
                  (fix- (receive (qr-big-small a b) (lambda (q r) r)))
                  (int+ (nat-rem a b))
                  (int- (nat-rem a (negate b)))
                  (else (big-bad-args 'mod a b))))
            (int-
               (type-case b
                  (fix+ 
                     (receive (qr-big-small a b) 
                        (lambda (q r) (negate r))))
                  (fix-    
                     (receive (qr-big-small a b) 
                        (lambda (q r) (negate r))))
                  (int+ (negate (nat-rem (negate a) b)))
                  (int- (negate (nat-rem (negate a) (negate b))))
                  (else (big-bad-args 'rem a b))))
            (else (big-bad-args 'rem a b))))

      (define remainder rem)
      (define quotient div)

      ; fixme, only true when the signs are the same, but left here as a placeholder

      ;(print "math.scm: fix mod")
      (define mod rem)

      ; required when (quotrem a b) -> q,r and b != 0
      ;   a = q*b + r
      ;    |r| < |b|
      ;    -a/b = a/-b = -(a/b)

      ; note: rem has sign of a, mod that of b

      (define (quotrem a b)
         (if (eq? b 0)
            (big-bad-args 'quotrem a b)
            (type-case a
               (fix+ 
                  (type-case b
                     (fix+ (receive (fxqr 0 a b) (lambda (_ q r) (values q r))))
                     (int+ (values 0 a))
                     (fix- (receive (fxqr 0 a b) (lambda (_ q r) (values (negate q) r))))
                     (int- (values 0 a))
                     (else (big-bad-args 'quotrem a b))))
               (int+
                  (type-case b
                     (fix+ (receive (qr-big-small a b) (lambda (q r) (values q r))))
                     (int+ (nat-quotrem a b))
                     (fix- (receive (qr-big-small a b) (lambda (q r) (values (negate q) r))))
                     (int- (receive (nat-quotrem a (negate b)) 
                              (lambda (q r) (values (negate q) r))))
                     (else (big-bad-args 'quotrem a b))))
               (fix- 
                  (type-case b
                     (fix+ 
                        (receive (fxqr 0 a b) (lambda (_ q r) (values (negate q) (negate r)))))
                     (fix- (receive (fxqr 0 a b) (lambda (_ q r) (values q (negate r)))))
                     (int+ (values 0 a))
                     (int- (values 0 a))
                     (else (big-bad-args 'quotrem a b))))   
               (int-
                  (type-case b
                     (fix+
                        (lets ((q r (qr-big-small a b)))
                           (values (negate q) (negate r))))
                     (fix- (receive (qr-big-small a b) (lambda (q r) (values q (negate r)))))
                     (int+ (receive (nat-quotrem (negate a) b) 
                              (lambda (q r) (values (negate q) (negate r)))))
                     (int- (receive (nat-quotrem (negate a) (negate b)) 
                              (lambda (q r) (values q (negate r)))))
                     (else (big-bad-args 'quotrem a b))))
               (else
                  (big-bad-args 'quotrem a b)))))

      ;(print "math.scm: fix quotrem")
      (define divmod quotrem)

      ;;;
      ;;; GCD (lazy binary new)
      ;;;

      ;; fixme: add a 32-vs-16-bit euclid 

      ;; euclid's gcd
      (define (gcd-euclid a b)
         (if (eq? b 0)
            a
            (gcd-euclid b (rem a b))))

      ;; lazy gcd

      ; O(1), shift focus bit 
      (define (gcd-drop n)
         (let ((s (car n)))   
            (cond
               ((eq? s 32768)
                  (let ((n (cdr n)))
                     ; drop a digit or zero
                     (if (teq? n fix+)
                        (cons 1 0)
                        (let ((tl (ncdr n)))
                           (if (null? (ncdr tl))
                              (cons 1 (ncar tl))
                              (cons 1 tl))))))
               (else   
                  (lets ((hi lo (fx<< s 1)))
                     (cons lo (cdr n)))))))

      (define gcd-shifts 
         (list->ff 
            (map (lambda (x) (cons (<< 1 x) x))
               '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))))
      
      ; O(1) clear low bits -> do also sub-adjusting ...
      (define (clear i n) ; clear low n-1 bits
         (if (eq? n 1)
            i
            (subi i (band i (get gcd-shifts n #false)))))

      (define (lazy-gcd a b n)
         (let ((av (cdr a)) (bv (cdr b)))
            (cond
               ((eq? av 0) (<< bv n))
               ((eq? bv 0) (<< av n))
               ((eq? (band av (car a)) 0) ; a even
                  (if (eq? (band bv (car b)) 0) ; a and b even
                     (lazy-gcd (gcd-drop a) (gcd-drop b) (add n 1))
                     (lazy-gcd (gcd-drop a) b n)))
               ((eq? (band bv (car b)) 0) ; a is odd, u is even
                  (lazy-gcd a (gcd-drop b) n))
               (else
                  (lets
                     ((av (>> av (get gcd-shifts (car a) 0)))
                      (bv (>> bv (get gcd-shifts (car b) 0)))
                      (x (subi av bv)))
                     (if (negative? x)
                        (lazy-gcd (cons 2 (negate x)) (cons 1 av) n)
                        (lazy-gcd (cons 2 x) (cons 1 bv) n)))))))

      (define (nat-gcd a b) (lazy-gcd (cons 1 a) (cons 1 b) 0))
      ;(define nat-gcd gcd-euclid)

      ;; signed wrapper for nat-gcd
      (define (gcd a b)
         (cond
            ; negates should be inlined 
            ((teq? a fix-) (gcd (negate a) b))
            ((teq? a int-) (gcd (negate a) b))
            ((teq? b fix-) (gcd a (negate b)))
            ((teq? b int-) (gcd a (negate b)))
            ((eq? a b) a)
            (else (nat-gcd a b))))
               
      (define (gcdl ls) (fold gcd (car ls) (cdr ls)))


      ;;;
      ;;; RATIONALS and COMPLEX (stub)
      ;;;

      (define-syntax complex
         (syntax-rules ()
            ((complex a b) (mkt 105 a b))))

      ; normalize, fix sign and construct rational
      (define (rationalize a b)
         (let ((f (gcd a b)))
            (if (eq? f 1)
               (cond
                  ((teq? b fix-) (rational (negate a) (negate b)))
                  ((teq? b int-) (rational (negate a) (negate b)))
                  (else (rational a b)))
               (rationalize (div a f) (div b f)))))

      ;; if dividing small fixnums, do it with primops
      (define (divide-simple a b)
         (if (teq? b fix+) ; negative (if any) always at a
            (cond
               ((teq? a fix+)
                  (lets ((_ q r (fxqr 0 a b)))
                     (if (eq? r 0)
                        q
                        #false)))
               (else #false))
            #false))

      ; fixme, to change real soon now 

      (define (divide a b)
         (cond
            ((eq? a 0) 0)
            ((teq? b fix-) (divide (negate a) (negate b)))
            ((teq? b int-) (divide (negate a) (negate b)))
            ((divide-simple a b) => (lambda (x) x))
            (else 
               (let ((f (gcd a b)))
                  (cond
                     ((eq? f 1) 
                        (if (eq? b 1)
                           a
                           (rational a b)))
                     ((= f b)
                        (divide-exact a f))
                     (else 
                        (rational
                           (divide-exact a f)
                           (divide-exact b f))))))))



      ;;;
      ;;; Generic arithmetic routines
      ;;;

      ;; generic addition, switching from most common type (?) to more complex ones (no pun untended)

      ;; rational case: a/b + c, gcd(a,b) = 1 => gcd(a+bc, b) = 1 -> no need to renormalize
      (define (add a b)
         (type-case a
            (fix+ 
               (type-case b
                  (fix+  (add-small->positive a b))
                  (int+  (add-number-big a b))
                  (fix-  (sub-small->pick-sign a b))
                  (int-  (sub-number-big a b #true))
                  (rat   (lets ((x z b)) (rational (add (muli a z) x) z)))
                  (comp  (lets ((x y b)) (complex (add a x) y)))
                  (else (big-bad-args '+ a b))))
            (int+
               (type-case b
                  (fix+ (add-number-big b a))
                  (int+ (add-big a b #false))
                  (fix- (sub-big-number a b #true))
                  (int- (sub-big a b))
                  (rat  (lets ((x z b)) (rational (add (muli a z) x) z)))
                  (comp (lets ((x y b)) (complex (add a x) y)))
                  (else (big-bad-args '+ a b))))
            (fix-
               (type-case b
                  (fix+ (sub-small->pick-sign b a))
                  (fix- (add-small->negative a b))
                  (int+ (sub-big-number b a #true))
                  (int- (cast (add-number-big a b) 41))
                  (rat  (lets ((x z b)) (rational (add (muli a z) x) z)))
                  (comp (lets ((x y b)) (complex (add a x) y)))
                  (else (big-bad-args '+ a b))))
            (int-
               (type-case b
                  (fix+ (sub-number-big b a #true))
                  (fix- (cast (add-number-big b a) 41))
                  (int+ (sub-big b a))
                  (int- (cast (add-big a b #false) 41))
                  (rat  (lets ((x z b)) (rational (add (muli a z) x) z)))
                  (comp (lets ((x y b)) (complex (add a x) y)))
                  (else (big-bad-args '+ a b))))
            (rat 
               (type-case b
                  (rat
                     ; a'/a" + b'/b" = a'b" + b'a" / a"b"
                     (let ((ad (ncdr a)) (bd (ncdr b)))
                        (if (eq? ad bd)   
                           ; a/x + b/x = (a+b)/x, x within fixnum range
                           (divide (add (ncar a) (ncar b)) ad)
                           (let ((an (ncar a)) (bn (ncar b)))   
                              (divide 
                                 (add (muli an bd) (muli bn ad))
                                 (muli ad bd))))))
                  (comp
                     (lets ((br bi b))
                        (complex (add a br) bi)))
                  (else
                     ; a'/a" + b = (a'+ba")/a"
                     (rational (add (ncar a) (muli b (ncdr a))) (ncdr a)))))
            (comp
               (if (teq? b comp)
                  ;; A+ai + B+bi = A+B + (a+b)i
                  (lets 
                     ((ar ai a)
                      (br bi b)
                      (r (add ar br))
                      (i (add ai bi)))
                     (if (eq? i 0) r (complex r i)))
                  (lets
                     ((ar ai a))
                     (complex (add ar b) ai))))
            (else 
               (big-bad-args '+ a b))))

      (define (sub a b)
         (type-case a
            (fix+
               (type-case b
                  (fix+ (sub-small->pick-sign a b))
                  (fix- (add-small->positive a b))
                  (int+ (sub-number-big a b #true))
                  (int- (add-number-big a b))
                  (rat  (let ((bl (ncdr b))) (sub (rational (muli a bl) bl) b)))
                  (comp (lets ((br bi b)) (complex (sub a br) (negate bi))))
                  (else (big-bad-args '- a b))))
            (fix-
               (type-case b
                  (fix+ (add-small->negative a b))
                  (fix- (sub-small->pick-sign b a))
                  (int+ (cast (add-number-big a b) 41))
                  (int- (sub-big-number b a #true))
                  (rat  (let ((bl (ncdr b))) (sub (rational (muli a bl) bl) b)))
                  (comp (lets ((br bi b)) (complex (sub a br) (negate bi))))
                  (else (big-bad-args '- a b))))
            (int+
               (type-case b
                  (fix+ (sub-big-number a b #true))
                  (fix- (add-number-big b a))
                  (int+ (sub-big a b))
                  (int- (add-big a b #false))
                  (rat  (let ((bl (ncdr b))) (sub (rational (muli a bl) bl) b)))
                  (comp (lets ((br bi b)) (complex (sub a br) (negate bi))))
                  (else (big-bad-args '- a b))))
            (int-
               (type-case b
                  (fix+ (cast (add-number-big b a) 41))
                  (fix- (sub-number-big b a #true))
                  (int+ (cast (add-big a b #false) 41))
                  (int- (sub-big b a))
                  (rat  (let ((bl (ncdr b))) (sub (rational (muli a bl) bl) b)))
                  (comp (lets ((br bi b)) (complex (sub a br) (negate bi))))
                  (else (big-bad-args '- a b))))
            (rat
               (type-case b
                  (rat 
                     ; a'/a" - b'/b" = a'b" - b'a" / a"b"
                     (let ((ad (ncdr a)) (bd (ncdr b)))
                        (if (eq? ad bd)   
                           ; a/x - b/x = (a-b)/x, x within fixnum range
                           (divide (subi (ncar a) (ncar b)) ad)
                           (let ((an (ncar a)) (bn (ncar b)))   
                              (divide 
                                 (subi (muli an bd) (muli bn ad))
                                 (muli ad bd))))))
                  (comp
                     (lets ((br bi b)) (complex (sub a br) (negate bi))))
                  (else
                     ; a'/a" - b = (a'-ba")/a"
                     (rational (subi (ncar a) (muli b (ncdr a))) (ncdr a)))))
            (comp
               (if (teq? b comp)
                  (lets 
                     ((ar ai a) 
                      (br bi b)
                      (r (sub ar br))
                      (i (sub ai bi)))
                     (if (eq? i 0)
                        r
                        (complex r i)))
                  (lets ((ar ai a))
                     (complex (sub ar b) ai))))
            (else
               (big-bad-args '- a b))))

      ;; todo: complex construction should be a macro that checks for the nonimaginary part
      ;; todo: no different multiplication for known up to rational etc

      (define mul muli)

      (define (mul a b)
         (cond
            ((eq? a 0) 0)
            ((eq? b 0) 0)
            (else
               (type-case a
                  (fix+
                     (type-case b
                        (fix+ (mult-fixnums a b))                 ; +a * +b
                        (int+ (mult-num-big a b 0))               ; +a * +B
                        (fix- (negative (mult-fixnums a b)))      ; +a * -b
                        (int- (negative (mult-num-big a b 0)))    ; +a * -b
                        (rat  (divide (mul a (ncar b)) (ncdr b)))
                        (comp 
                           (lets ((br bi b) (r (mul a br)) (i (mul a bi)))
                              (if (eq? i 0) r (complex r i))))
                        (else (big-bad-args 'mul a b))))
                  (fix-
                     (type-case b
                        (fix+ (negative (mult-fixnums a b)))      ; -a * +b -> -c | -C
                        (int+ (cast (mult-num-big a b 0) 41))   ; -a * +B -> -C
                        (fix- (mult-fixnums a b))                  ; -a * -b -> +c | +C
                        (int- (mult-num-big a b 0))            ; -a * -B -> +C
                        (rat  (divide (mul a (ncar b)) (ncdr b)))
                        (comp 
                           (lets ((br bi b) (r (mul a br)) (i (mul a bi)))
                              (if (eq? i 0) r (complex r i))))
                        (else (big-bad-args 'mul a b))))
                  (int+
                     (type-case b
                        (fix+ (mult-num-big b a 0))            ; +A * +b -> +C
                        (int+ (mult-big a b))               ; +A * +B -> +C
                        (fix- (cast (mult-num-big b a 0) 41))    ; +A * -b -> -C
                        (int- (cast (mult-big a b) 41))      ; +A * -B -> -C
                        (rat  (divide (mul a (ncar b)) (ncdr b)))
                        (comp 
                           (lets ((br bi b) (r (mul a br)) (i (mul a bi)))
                              (if (eq? i 0) r (complex r i))))
                        (else (big-bad-args 'mul a b))))
                  (int-   
                     (type-case b
                        (fix+ (cast (mult-num-big b a 0) 41))      ; -A * +b -> -C
                        (int+ (cast (mult-big a b) 41))      ; -A * +B -> -C
                        (fix- (mult-num-big b a 0))               ; -A * -b -> +C
                        (int- (mult-big a b))                  ; -A * -B -> +C
                        (rat  (divide (mul a (ncar b)) (ncdr b)))
                        (comp 
                           (lets ((br bi b) (r (mul a br)) (i (mul a bi)))
                              (if (eq? i 0) r (complex r i))))
                        (else (big-bad-args 'mul a b))))
                  (rat
                     (type-case b
                        (rat  
                           (divide (mul (ncar a) (ncar b)) (mul (ncdr a) (ncdr b))))
                        (comp 
                           (lets ((br bi b) (r (mul a br)) (i (mul a bi)))
                              (if (eq? i 0) r (complex r i))))
                        (else 
                           (divide (mul (ncar a) b) (ncdr a)))))
                  (comp 
                     (if (teq? b comp)
                        (lets 
                           ((ar ai a)
                            (br bi b)
                            (r (sub (mul ar br) (mul ai bi)))
                            (i (add (mul ai br) (mul ar bi))))
                           (if (eq? i 0) r (complex r i)))
                        (lets
                           ((ar ai a)
                            (r (mul ar b))
                            (i (mul ai b)))
                           (if (eq? i 0) r (complex r i)))))
                  (else 
                     (big-bad-args '* a b))))))

      ;; todo: division lacks short circuits
      (define (/ a b)
         (cond    
            ((eq? b 0)
               (error "division by zero " (list '/ a b)))
            ((teq? a comp)
               (if (teq? b comp)
                  (lets 
                     ((ar ai a) 
                      (br bi b)
                      (x (add (mul br br) (mul bi bi)))
                      (r (/ (add (mul ar br) (mul ai bi)) x))
                      (i (/ (sub (mul ai br) (mul ar bi)) x))) 
                     (if (eq? i 0) r (complex r i)))
                  (lets
                     ((ar ai a)
                      (x (mul b b))
                      (r (/ (mul ar b) x))
                      (i (/ (mul ai b) x)))
                     (if (eq? i 0) r (complex r i)))))
            ((teq? b comp)
               (lets 
                  ((br bi b)
                   (x (add (mul br br) (mul bi bi)))
                   (re (/ (mul a br) x))
                   (im (/ (sub 0 (mul a bi)) x)))
                  (if (eq? im 0) re (complex re im))))
            ((teq? a rat)
               (if (teq? b rat)
                  ; a'/a" / b'/b" = a'b" / a"b'
                  (divide
                     (mul (ncar a) (ncdr b)) 
                     (mul (ncdr a) (ncar b)))
                  ; a'/a" / b = a'/ba"
                  (divide (ncar a) (mul (ncdr a) b))))
            ((teq? b rat)
               ; a / b'/b" = ab"/n
               (divide (mul a (ncdr b)) (ncar b)))
            (else 
               (divide a b))))


      ;;;
      ;;; Basic math extra stuff
      ;;;

      (define (abs n)
         (type-case n
            (fix+ n)
            (fix- (cast n 0))
            (int+ n)
            (int- (ncons (ncar n) (ncdr n)))
            (rat (if (negative? n) (sub 0 n) n))
            (else (error "bad math: " (list 'abs n)))))

      (define (floor n)
         (if (teq? n rat)
            (lets ((a b n))
               (if (negative? a)
                  (negate (nat-inc (div (abs a) b)))
                  (div a b)))
            n))
      
      (define (ceiling n)
         (if (teq? n rat)
            (lets ((a b n))
               (if (negative? a)
                  (div a b)
                  (nat-inc (floor n))))
            n))

      (define (truncate n)
         (if (teq? n rat)
            (lets ((a b n))
               (if (negative? a)
                  (negate (div (negate a) b))
                  (div a b)))
            n))

      (define (round n)
         (if (teq? n rat)
            (lets ((a b n))
               (if (eq? b 2)
                  (if (negative? a)
                     (>> (sub a 1) 1)
                     (>> (nat-inc a) 1))
                  (div a b)))
            n))

      (define (sum l) (fold add (car l) (cdr l)))
      (define (product l) (fold mul (car l) (cdr l)))

      ;;;
      ;;; Alternative names
      ;;;

      (define quot div) ; switch the other way around later
      (define ceil ceiling)
      (define quotient quot)

      ; for all numbers n == (/ (numerator n) (denumerator n))

      (define (numerator n)
         (type-case n
            (rat (ncar n))
            (else n)))

      (define (denumerator n)
         (type-case n
            (rat (ncdr n))
            (else 1)))



      ;;;
      ;;; logarithms, here meaning (log n a) = m, being least natural number such that n^m >= a
      ;;;

      ;; naive version, multiply successively until >=
      (define (log-loop n a m i)
         (if (< m a)
            (log-loop n a (mul m n) (add i 1))
            i))

      ;; least m such that n^m >= a
      (define (log-naive n a)
         (log-loop n a 1 0))

      ;; same, but double initial steps (could recurse on remaining interval, cache steps etc for further speedup)
      (define (logd-loop n a m i)
         (if (< m a)
            (let ((mm (mul m m)))
               (if (< mm a)
                  (logd-loop n a mm (add i i))
                  (log-loop n a (mul m n) (add i 1))))
            i))

      (define (logn n a)
         (cond
            ((>= 1 a) 0)
            ((< a n) 1)
            (else (logd-loop n a n 1))))

      ;; special case of log2 

      ; could do in 8 comparisons with a tree
      (define (log2-fixnum n)
         (let loop ((i 0))
            (if (< (<< 1 i) n)
               (loop (add i 1))
               i)))

      (define (log2-msd n)
         (let loop ((i 0))
            (if (<= (<< 1 i) n)
               (loop (add i 1))
               i)))

      (define (log2-big n digs)
         (let ((tl (ncdr n)))
            (if (null? tl)
               (add (log2-msd (ncar n)) (mul digs 16))
               (log2-big tl (add digs 1)))))

      (define (log2 n)
         (cond
            ((teq? n int+) (log2-big (ncdr n) 1))
            ((teq? n fix+ )
               (if (< n 0) 1 (log2-fixnum n)))
            (else (logn 2 n))))

      (define (log n a)
         (cond
            ((eq? n 2) (log2 a))
            ((<= n 1) (big-bad-args 'log n a))
            (else (logn n a))))

      ;(import-old lib-test)
      ;(test
      ;   (lmap (λ (i) (lets ((rst n (rand i 10000000000000000000))) n)) (lnums 1))
      ;   (λ (n) (log-naive 3 n))
      ;   (λ (n) (log 3 n)))
      ;(import-old lib-test)
      ;(test
      ;   (lmap (λ (i) (lets ((rst n (rand i #x20000))) n)) (lnums 1))
      ;   (λ (n) (log 2 n))
      ;   (λ (n) (log2 n)))

      ; note: it is safe to use div, which is faster for bignums, because by definition
      ; the product is divisble by the gcd. also, gcd 0 0 is not safe, but since (lcm 
      ; a a) == a, handlin this special case and and a small optimization overlap nicely.

      (define (lcm a b) 
         (if (eq? a b)
            a
            (div (abs (mul a b)) (gcd a b))))


      ;;;
      ;;; Rendering numbers
      ;;;

      (define (char-of digit)
         (add digit (if (< digit 10) 48 87)))

      (define (render-digits num tl base)
         (fold (λ (a b) (cons b a)) tl 
            (unfold (λ (n) (lets ((q r (quotrem n base))) (values (char-of r) q))) num zero?)))

      ;; move to math.scm

      (define (render-number num tl base)
         (cond
            ((teq? num rat)
               (render-number (ref num 1)
                  (cons 47
                     (render-number (ref num 2) tl base))
                  base))
            ((teq? num comp)
               ;; todo: imaginary number rendering looks silly, written in a hurry
               (lets ((real imag num))
                  (render-number real 
                     (cond
                        ((eq? imag 1) (ilist #\+ #\i tl))
                        ((eq? imag -1) (ilist #\- #\i tl))
                        ((< imag 0) ;; already has sign
                           (render-number imag (cons #\i tl) base))
                        (else
                           (cons #\+
                              (render-number imag (cons #\i tl) base))))
                     base)))
            ((< num 0)
               (cons 45
                  (render-number (sub 0 num) tl base)))
            ((< num base)
               (cons (char-of num) tl))
            (else
               (render-digits num tl base))))

      (define (mod a b)
         (if (negative? a)
            (if (negative? b)
               (remainder a b)
               (let ((r (remainder a b)))
                  (if (eq? r 0)
                     r
                     (add b r))))
            (if (negative? b)
               (let ((r (remainder a b)))
                  (if (eq? r 0)
                     r
                     (add b r)))
               (remainder a b))))

      (define modulo mod)


      ;;;
      ;;; Variable arity versions
      ;;;

      ;; FIXME: these need short circuiting 

      ;; + → add
      (define +
         (case-lambda
            ((a b) (add a b))
            (xs (fold add 0 xs))))

      ;; - → sub
      (define -
         (case-lambda 
            ((a b) (sub a b))
            ((a) (sub 0 a))
            ((a b . xs) 
               (sub a (add b (fold add 0 xs))))))
      
      ;; * → mul
      (define *
         (case-lambda 
            ((a b) (mul a b))
            ((a b . xs) (mul a (mul b (fold mul 1 xs))))
            ((a) a)
            (() 1)))

      (define bin-div /)

      (define /
         (case-lambda
            ((a b) (bin-div a b))
            ((a) (bin-div 1 a))
            ((a . bs) (bin-div a (product bs)))))

      ;; fold but stop on first false
      (define (each op x xs)
         (cond
            ((null? xs) #true)
            ((op x (car xs)) 
               (each op (car xs) (cdr xs)))
            (else #false)))

      ;; the rest are redefined against the old binary ones

      (define (vararg-predicate op) ;; turn into a macro
         (case-lambda
            ((a b) (op a b))
            ((a . bs) (each op a bs))))

      (define = (vararg-predicate =)) ;; short this later
      (define < (vararg-predicate <))
      (define > (vararg-predicate >))

      (define <= (vararg-predicate <=))
      (define >= (vararg-predicate >=))

      ;; ditto for foldables
      (define (vararg-fold op zero)
         (case-lambda
            ((a b) (op a b))
            ((a) a)
            ((a . bs) (fold op a bs))
            (() (or zero (error "No arguments for " op)))))

      (define min (vararg-fold min #false))
      (define max (vararg-fold max #false))
      (define gcd (vararg-fold gcd 0))
      (define lcm (vararg-fold lcm 1))

))
