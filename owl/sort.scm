; todo:
;   - the default sort could be stable

(define-library (owl sort)

   (export sort isort quicksort mergesort)

   (import 
      (owl math)
      (owl list)
      (owl defmac))

   (begin
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

))
