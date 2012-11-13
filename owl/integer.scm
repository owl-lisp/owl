;; fixnums -> integers + fixed arity versions for building rationals etc

(define-library (owl integer)

   (export 
      *max-fixnum*
      *fixnum-bits*)

   (import
      (owl defmac)
      (owl syscall))

   (begin

      (define-syntax unimplemented
         (syntax-rules ()
            ((unimplemented name)
               (define name
                  (lambda args
                     (error "unimplemented math: " (cons 'name args)))))))

      (define-syntax missing
         (syntax-rules ()
            ((missing name arg ...)
               (error "Math too high: " (list (quote name) arg ...)))))

      ;; bignums are lists of fixnums (of a distinct type) starting from the least 
      ;; significant digit. fixnum size may vary, so it needs to be checked.

      ;; check how many fixnum bits the vm supports with fx<<
      ;; idea is to allow the vm to be compiled with different ranges, initially fixed to 24
      (define *max-fixnum*
         (let loop ((f 0))
            (lets
               ((o f (fx<< f 1)) ;; repeat (f<<1)|1 until overflow
                (f (fxbor f 1)))
               (if (eq? o 0)
                  (loop f)
                  f))))

      ;; count the number of bits in *max-fixnum*
      (define *fixnum-bits*
         (let loop ((f *max-fixnum*) (n 0))
            (if (eq? f 0)
               n
               (lets 
                  ((f _ (fx>> f 1))
                   (n _ (fx+ n 1)))
                  (loop f n)))))

      (define big-one (ncons 1 (ncons 0 null)))

      ;; up to bignum math, rationals t
      (define (add a b add)
         (case (type a)
            (type-fix+
               (case (type b)
                  (type-fix+
                     (lets ((c o (fx+ a b)))
                        (if o ;; check for overflow to fixnum range
                           (ncons c (ncons a null))
                           c)))
                  (else
                     (missing add a b))))
            (else
               (missing add a b))))

      (define +
         (case-lambda
            ((a b) (add a b add))
            (as (fold (λ (a b) (add a b add)) 0 as))))

))
