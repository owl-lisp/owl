;;;
;;; Suffix array and tree construction
;;; 

(define-library (owl suffix)

   (export 
      suffix-array      ;; iter -> suffix array (not here yet)
      suffix-list       ;; iter -> suffix list
   )

   (import
      (owl defmac)
      (owl math)
      (owl list)
      (owl sort)
      (owl list-extra)
      (owl vector)
      (owl string)
      (owl iff)
      (owl lazy))


   (begin

      (define sentinel "telomerase") ; something unique as in eq?

      (define (carless a b) 
         (let ((a (car a)) (b (car b)))
            (cond
               ((eq? a sentinel) #true) ; sentinel is teh minimum
               ((eq? b sentinel) #false) ; ditto
               (else (lesser? a b)))))

      (define (cdr< a b) (< (cdr a) (cdr b))) 
      (define (car< a b) (< (car a) (car b)))

      ;;;
      ;;; Functional qsufsort (see Larsson & Sadakane's "Faster Suffix Sorting" paper for the original imperative algorithm)
      ;;;

      (define (invert bs) 
         (map car (sort cdr< (iff->list bs))))

      (define (get-run x vs out)
         (cond
            ((null? vs) (values out vs))
            ((eq? (caar vs) x) (get-run x (cdr vs) (cons (cdar vs) out)))
            (else (values out vs))))

      (define (chunk vs bs tl n) ; -> ls' + bs'
         (if (null? vs)
            (values tl bs)
            (lets
               ((l vs (get-run (car (car vs)) vs null)) ; <- ie get a tree node
                (ln (length l))
                (lid (+ n ln))
                (bs (fold (λ (bs p) (iput bs p lid)) bs l))
                (vs bs (chunk vs bs tl (+ n ln))))
               (if (null? (cdr l))
                  (values vs bs)
                  (values (cons l vs) bs)))))

      ;; todo: if a tree-sort was used, the nodes could directly be used for chunk
      ;; todo: if iffs would allow in-order iteration, they could be used here to sort and walk over chunks directly
      (define (ssort-bucket l bs tl n)
         (chunk 
            (sort car< (map (λ (p) (cons (iget bs (+ p n) sentinel) p)) l)) ; was (sort carless ..)
            bs tl (- (iget bs (car l) #false) (length l))))

      (define (ssort-step ss bs n) ; -> ls' + bs'
         (if (null? ss)
            (values ss bs)
            (lets 
               ((bucket (car ss))
                (tl bs (ssort-step (cdr ss) bs n)))
               (ssort-bucket bucket bs tl n))))

      (define (ssort-steps ls bs n)
         (if (null? ls) 
            (invert bs)
            (lets ((ls bs (ssort-step ls bs n)))
               (ssort-steps ls bs (* n 2)))))

      (define (add-poss lst p)
         (if (pair? lst)
            (cons (cons (car lst) p)
               (add-poss (cdr lst) (+ p 1)))
            null))

      (define (ssort-doubling lst)
         (lets
            ((sb (add-poss lst 0))
             (sb (cons (cons sentinel (length sb)) sb)) ; add sentinel
             (sb (sort carless sb))
             (ls bs (chunk sb #empty null -1)))
            (cdr (ssort-steps ls bs 1)))) ; drop the sentinel (is just length of list at car)

      (define suffix-list ssort-doubling)

      (define (suffix-array thing)
         (list->vector
            (suffix-list
               (cond
                  ((vector? thing) (vector->list thing))
                  ((string? thing) (string->list thing))
                  (else thing)))))))

