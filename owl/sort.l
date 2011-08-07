; todo:
;   - the default sort could be stable

(define-module lib-sort

   (export sort isort quicksort mergesort)

   ;;;
   ;;; quicksort, never use this, worst case O(n^2) on sorted data
   ;;;

   (define (quicksort op lst) 
      (let loop ((x lst))
         (if (null? x) null 
            (let ((this (car x))) 
               (let diffx ((x (cdr x)) (left null) (right null))
                  (cond 
                     ((null? x)
                        (let ((left (loop left)))
                           (append left (cons this (loop right)))))
                     ((op (car x) this)
                        (diffx (cdr x) (cons (car x) left) right))
                     (else
                        (diffx (cdr x) left (cons (car x) right)))))))))

   ;;;
   ;;; insertion sort, a good fallback on small sorts
   ;;;

   (define (insert lst val op)
      (cond
         ((null? lst) (list val))
         ((op val (car lst)) (cons val lst))
         (else (cons (car lst) (insert (cdr lst) val op)))))

   (define (isort op lst)
      (let loop ((out null) (lst lst))
         (if (null? lst)
            out
            (loop (insert out (car lst) op) (cdr lst)))))

   ;;;
   ;;; bottom up mergesort, a stable O(n log n) worst case sort
   ;;;

   (define (merge op a b)
      (cond
         ((null? a) b)
         ((null? b) a)
         ((op (car b) (car a)) (cons (car b) (merge op a (cdr b))))
         (else (cons (car a) (merge op (cdr a) b)))))

   (define (merger op l)
      (if (null? l) 
         null
         (let ((a (car l)) (l (cdr l)))
            (if (null? l)
               (list a)
               (let ((b (car l)) (l (cdr l)))
                  (cons (merge op a b) (merger op l)))))))

   ; as an optimization to (map (lambda (x) (list x)) l), chunk the list 
   ; initially to <chunk-size> lists using insertion sort steps

   (define chunk-size 10)

   (define (chunker op l out n)
      (cond
         ((null? l)
            (if (null? out) null (list out)))
         ((eq? n chunk-size)
            (cons out
               (chunker op (cdr l) (list (car l)) 1)))
         (else
            (lets ((n o (fx+ n 1)))
               (chunker op (cdr l) (insert out (car l) op) n)))))

   (define (merge-pairs op l)
      (if (null? (cdr l))
         (car l)
         (merge-pairs op (merger op l))))

   (define (mergesort op l)
      (if (null? l) 
         null
         (let ((l (chunker op l null 0)))
            (if (null? (cdr l))
               (car l)
               (merge-pairs op l)))))

   ;;;
   ;;; the default sort is always fast and stable
   ;;;

   (define (sort op lst)
      (cond
         ((null? lst) lst)
         ((null? (cdr lst)) lst)
         (else (mergesort op lst))))

)

;;; a quick correctness test, assuming i got all the corner cases of isort right =D
;(import lib-sort)
;(import lib-random)
;(define (index l n) (if (null? l) null (cons (cons (car l) n) (index (cdr l) (+ n 1)))))
;(define (comp a b) (< (car a) (car b)))
;(define (well-ordered? l)
;   (call/cc (lambda (fail)
;      (if (null? l) True
;         (for (car l) (cdr l)
;            (lambda (a b)
;               (if (= (car a) (car b))
;                  (if (< (cdr a) (cdr b))
;                     b
;                     (begin (show "order fail: " (list 'a a 'b b)) (fail False)))
;                  b)))))))
;(let loop ((rst 111111111111) (n 1))
;   (show " * check " n)
;   (if (< n 1000)
;      (lets
;         ((rst len (rand rst 100))
;          (rst max (rand rst 100))
;           (lst (random-numbers rst max len))
;          (isorted (isort < lst))
;          (sorted  (sort < lst)))
;         (cond
;            ((not (equal? isorted sorted))
;               (show "bad sort at " lst)
;               (show " sorts to " sorted)
;               (show " correct  " isorted)
;               (! 10000)
;               (error "badness " "sort fail"))
;            ((not (well-ordered? (sort comp (index lst 1))))
;               (show "unstable sort at " lst)
;               (show "sorts to " (sort comp (index lst 1)))
;               (! 10000)
;               (error "badness " "sort order fail"))
;            (else
;               (loop (rand-succ rst) (+ n 1)))))
;      (print "all ok (so far)")))

;;; a quick speed test
;(import lib-sort)
;(import lib-random)
;(define (try-sort op lst)
;   (let loop ((n 0)) (flush-port 1) 
;      (if (= n 50) (mail stdout 10) (begin (mail 1 42) (op < lst) (loop (+ n 1))))))
;(let ((sorter sort))
;   (display " random: ") (try-sort sorter (random-numbers 1 1000 20000))
;   (display "ordered: ") (try-sort sorter (iota 0 1 20000)) 
;   (display "reverse: ") (try-sort sorter (iota 20000 -1 0)))


