
(define-library (owl render)

   (import
      (owl defmac)
      (owl string)
      (owl list)
      )
   
   (export 
      serialize       ;; obj tl → (byte ... . tl), lazy 
      )

   (begin

      (define (serialize obj tl)
         (cond
            (else
               (append (string->list "#<WTF>") tl))))
))
