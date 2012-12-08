;;;
;;; Number stores (radix trees with a ff at each node)
;;;

;; todo: extend to work for negative numbers
;; todo: no unit tests. add a quick lib-compare test asap.

(define-library (owl iff)

   (export iget iput ifold iff->list)
   
   (import 
      (owl defmac)
      (owl ff)
      (owl list))

   (begin

      (define tag #false) ;; non-digit special ff key

      (define (iputl ff num val)
         (if (null? num)
            (put ff tag val)
            (let ((these (get ff (ncar num) empty)))
               (put ff (ncar num)
                  (iputl these (ncdr num) val)))))

      (define (iput ff num val)
         (if (eq? (type num) type-fix+)
            (let ((small (get ff tag empty)))
               (put ff tag
                  (put small num val)))
            (iputl ff num val)))

      (define (igetl ff num def)
         (if ff
            (if (null? num)
               (get ff tag def)
               (igetl (get ff (ncar num) empty) (ncdr num) def))
            def))

      (define (iget ff num def)
         (if (eq? (type num) type-fix+)
            (get (get ff tag empty) num def)
            (igetl ff num def)))

      ; private allocated things are private

      (define iff-nan '(kansas))

      ; order may change later

      (define (nrev out in)
         (if (null? in)
            out
            (nrev (ncons (ncar in) out) (ncdr in))))

      (define (iff-walk op st ff taken)
         (lets
            ((this (get ff tag iff-nan))
             (st (if (eq? this iff-nan) st  
                     (op st (nrev null taken) this))))
            (ff-fold
               (λ (st digit more)
                  (if digit
                     (iff-walk op st more (ncons digit taken))
                     st))
               st ff)))
            
      (define (ifold op st ff)
         (ff-fold
            (λ (st k v)
               (if k 
                  (iff-walk op st v (ncons k null))
                  st))
            (ff-fold op st (get ff tag empty))
            ff))

      (define (iff->list iff)
         (ifold (lambda (tail n v) (cons (cons n v) tail)) null iff))

))

