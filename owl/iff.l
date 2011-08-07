;;;
;;; Number stores (radix trees with a ff at each node)
;;;

;; todo: extend to work for negative numbers
;; todo: no unit tests. add a quick lib-compare test asap.

,r "owl/ff.l"

(define-module lib-iff

   (export iget iput ifold iff->list)
   
   (import lib-ff)

   (define (iputl ff num val)
      (if (null? num)
         (put ff False val)
         (let ((these (get ff (ncar num) False)))
            (put ff (ncar num)
               (iputl these (ncdr num) val)))))

   (define (iput ff num val)
      (if (teq? num fix+)
         (let ((small (get ff False False)))
            (put ff False
               (put small num val)))
         (iputl ff num val)))

   (define (igetl ff num def)
      (if ff
         (if (null? num)
            (get ff False def)
            (igetl (get ff (ncar num) False) (ncdr num) def))
         def))

   (define (iget ff num def)
      (if (teq? num fix+)
         (get (get ff False False) num def)
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
         ((this (get ff False iff-nan))
          (st (if (eq? this iff-nan) st  
                  (op st (nrev null taken) this))))
         (ff-fold
            (lambda (st digit more)
               (if digit
                  (iff-walk op st more (ncons digit taken))
                  st))
            st ff)))
         
   (define (ifold op st ff)
      (ff-fold
         (lambda (st k v)
            (if k 
               (iff-walk op st v (ncons k null))
               st))
         (ff-fold op st (get ff False False))
         ff))

   (define (iff->list iff)
      (ifold (lambda (tail n v) (cons (cons n v) tail)) null iff))

)

