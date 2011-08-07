;;;
;;; Suffix array and tree construction
;;; 

;; todo: string-sort is usually good, and doubling is always good but on average 3x slower -> use string-sort for first 2^n steps?
;; todo: allow constructing also a suffix tree (or provide a convenient iterator)

,r "owl/lazy.l"

(define-module lib-suffix

   (export 
      suffix-array      ;; iter -> suffix array (not here yet)
      suffix-list         ;; iter -> suffix list
      ;suffix-tree      ;; iter -> suffix tree (not here)

      ;; construction
      ssort-doubling      ;; functional qsufsort (default)
      ssort-string       ;; mergesort using string comparison (bad)
      ssort-ternary      ;; ternary partitioning radix sort (bad)
      )

   (import lib-iff)
   (import lib-lazy force lzip lnums)

   (define sentinel "telomerase") ; something unique as in eq?

   ;;; misc utils

   (define (carless a b) 
      (let ((a (car a)) (b (car b)))
         (cond
            ((eq? a sentinel) True) ; sentinel is teh minimum
            ((eq? b sentinel) False) ; ditto
            (else (lesser? a b)))))

   (define (cdr< a b) (< (cdr a) (cdr b))) 
   (define (car< a b) (< (car a) (car b)))

   ;;;
   ;;; Comparison suffix sort
   ;;;

   (define (lex-less? a b)
      (cond
         ((null? a) True)
         ((null? b) False)
         (else
            (lets ((a as a) (b bs b))
               (cond
                  ((lesser? a b) True) ; was <
                  ((eq? a b) (lex-less? as bs)) ; was =
                  (else False))))))

   (define (indexed-suffixes lst)
      (let loop ((lst lst) (pos 0) (out null))
         (cond
            ((pair? lst)
               (loop (cdr lst) (+ pos 1) (cons (cons pos lst) out)))
            ((null? lst) out)
            (else (loop (lst) pos out)))))

   (define (ssort-string lst)
      (map car
         (sort 
            (λ (a b) (lex-less? (cdr a) (cdr b))) 
            (indexed-suffixes lst))))


   ;;;
   ;;; Ternary partitioning generic list ssort
   ;;;

   (define (middle a b c)
      (cond
         ((> a b) (middle b a c))
         ((> b c) (middle a c b))
         (else b)))

   (define (len>? l n)
      (cond
         ((null? l) False)
         ((= n 0) True)
         (else (len>? (cdr l) (- n 1)))))

   (define (head l) (if (null? (cdr l)) sentinel (cadr l)))

   (define (choose-pivot il)
      (if (len>? il 2)
         (lets ((a (car il)) (il (cdr il)) (b (car il)) (il (cdr il)) (c (car il)))
            (middle (head a) (head b) (head c)))
         (head (car il))))
      
   (define (partition il pivot)
      (let loop ((il il) (pre null) (same null) (post null))
         (if (null? il)
            (values pre same post)
            (lets
               ((this (car il))
                (pos l this))
               (cond
                  ((or (null? l) (lesser? (car l) pivot))
                     (loop (cdr il) (cons this pre) same post))
                  ((eq? (car l) pivot)
                     (loop (cdr il) pre (cons (cons pos (cdr l)) same) post))
                  (else
                     (loop (cdr il) pre same (cons this post))))))))

   (define (radix-step il tail)
      (cond
         ((null? il) tail)
         ((null? (cdr il)) (cons (caar il) tail))
         (else
            (lets 
               ((pivot (choose-pivot il))
                (pre these post (partition il pivot)))
               (radix-step pre
                  (radix-step these
                     (radix-step post tail)))))))
      
   (define (ssort-ternary lst)
      (radix-step (indexed-suffixes lst) null))


   ;;;
   ;;; Functional qsufsort (see larsson & sadakane's "faster suffix sorting" paper for the original imperative algorithm)
   ;;;

   (define (invert bs) (map car (sort cdr< (iff->list bs))))

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
         bs tl (- (iget bs (car l) False) (length l))))

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
          (ls bs (chunk sb False null -1)))
         (cdr (ssort-steps ls bs 1)))) ; drop the sentinel (is just length of list at car)

   ;;;
   ;;; Exported default versions
   ;;;

   (define suffix-list ssort-doubling)

   (define (suffix-array thing)
      (list->vector
         (suffix-list
            (cond
               ((vector? thing) (vector->list thing))
               ((string? thing) (string->list thing))
               (else thing)))))

)

