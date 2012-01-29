(define-library (owl equal)

   (import 
      (owl defmac)
      (owl string)
      (owl symbol)
      (owl math))

   (export
      equal?
      eqv?)

   (begin
      (define (eq-fields a b eq pos)
         (cond
            ((eq? pos 0)
               #true)
            ((eq (ref a pos) (ref b pos))
               (lets ((pos x (fx- pos 1)))
                  (eq-fields a b eq pos)))
            (else #false)))

      (define (eq-bytes a b pos)
         (if (eq? (refb a pos) (refb b pos))
            (if (eq? pos 0)
               #true
               (receive (fx- pos 1)
                  (λ pos x (eq-bytes a b pos))))
            #false))

      ;; fixme: ff:s should have a separate equality test too
      ;; fixme: byte vector paddings not here

      ;; raw brute force object equality
      (define (equal? a b)
         (cond
            ((eq? a b)
               #true)
            ((string? a)
               (and (string? b) (string-eq? a b)))
            ((symbol? a) #false) ; would have been eq?, because they are interned
            ((pair? a)
               (if (pair? b)
                  (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
                  #false))
            (else
               (let ((sa (size a)))
                  (cond
                     ; a is immediate -> would have been eq?
                     ((eq? sa 0) 
                        #false)
                     ; same size
                     ((eq? sa (size b))
                        (let ((ta (type a)))
                           ; check equal types
                           (if (eq? ta (type b))
                              (if (eq? (fxband ta 2048) 0)
                                 ; equal ntuples, check fields
                                 (eq-fields a b equal? sa)
                                 ; equal raw objects, check bytes
                                 (lets
                                    ((ea (sizeb a)) ; raw objects may have padding bytes, so recheck the sizes
                                 (eb (sizeb b)))
                                    (if (eq? ea eb)
                                       (if (eq? ea 0)
                                          #true
                                          (eq-bytes a b (- ea 1)))
                                       #false)))
                              #false)))
                     (else #false))))))

      (define ≡ equal?)

      (define eqv? equal?)))
