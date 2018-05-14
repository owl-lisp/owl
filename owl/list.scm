(define-library (owl list)

   (export
      null pair? null?
      caar cadr cdar cddr
      list?
      zip fold foldr map for-each
      memq assq last
      fold-map foldr-map
      append reverse keep remove
      every any
      smap unfold
      find find-tail
      take-while                ;; pred, lst -> as, bs
      fold2
      halve
      edit                      ;; op lst → lst'
      interleave
      ╯°□°╯

      diff union intersect)

   (import
      (owl defmac)
      (owl primop)
      (owl proof)
      (owl syscall)
      (owl boolean))

   (begin

      ;; constants are always inlined, so you pay just one byte of source for readability

      (define null '())

      ;; any -> bool
      (define (pair? x) (eq? type-pair (type x)))

      ;; any -> bool
      (define null? (C eq? null))

      (define-syntax withcc
         (syntax-rules ()
            ((withcc name proc)
               (call/cc (λ (name) proc)))))

      ;; '((a . b) . c) -> a
      (define caar (B car car))
      ;; '(a . (b . c)) -> b
      (define cadr (B car cdr))
      ;; '((a . b) . c) -> b
      (define cdar (B cdr car))
      ;; '(a . (b . c)) -> c
      (define cddr (B cdr cdr))

      ;; any -> bool, check if a thing is a linked list, O(n)
      (define (list? l)
         (cond
            ((null? l) #true)
            ((pair? l) (list? (cdr l)))
            (else #false)))

      ;; fn as bs -> ((fn a b) ...), zip values of lists together with a function
      (define (zip op a b)
         (cond
            ((null? a) null)
            ((null? b) null)
            (else
               (let ((hd (op (car a) (car b))))
                  (cons hd (zip op (cdr a) (cdr b)))))))

      ;; op state lst -> state', walk over a list from left and compute a value

      (define (fold op state lst)
         (if (null? lst)
            state
            (fold op
               (op state (car lst))
               (cdr lst))))

      (example
         (zip cons '(1 2 3) '(a b c d)) = '((1 . a) (2 . b) (3 . c)))

      (define (unfold op st end?)
         (if (end? st)
            null
            (lets ((this st (op st)))
               (cons this (unfold op st end?)))))

      ;; op s1 s2 lst -> s1' s2', fold with 2 states
      (define (fold2 op s1 s2 lst)
         (if (null? lst)
            (values s1 s2)
            (lets ((s1 s2 (op s1 s2 (car lst))))
               (fold2 op s1 s2 (cdr lst)))))

      ;; op st lst -> st', compute a value from the right
      ;;    (foldr - 0 '(1 2 3)) = 2
      (define (foldr op st lst)
         (if (null? lst)
            st
            (op (car lst)
               (foldr op st (cdr lst)))))

      (example (foldr cons null '(a b c)) = '(a b c))

      ;; fn lst -> lst', run a function to all elements of a list
      (define (map fn lst)
         (if (null? lst)
            null
            (lets
               ((hd tl lst)
                (hd (fn hd))) ;; compute head first
               (cons hd (map fn tl)))))

      (example
         (map not '(#false #false #true)) = '(#true #true #false))

      ;; fn lst -> _, run a function to all elements of a list for side effects
      (define (for-each op lst)
         (if (null? lst)
            null
            (begin
               (op (car lst))
               (for-each op (cdr lst)))))

      (define (memq x lst)
         (cond
            ((null? lst) #false)
            ((eq? (car lst) x) lst)
            (else (memq x (cdr lst)))))

      ;; lst k -> #false | value, get a value from an association list
      (define (assq k lst)
         (cond
            ((null? lst) #false)
            ((eq? k (car (car lst))) (car lst))
            (else (assq k (cdr lst)))))

      (example
         (assq 'a '((a . 1) (b . 2))) = '(a . 1)
         (assq 'c '((a . 1) (b . 2))) = #false)


      ;; last list default -> last-elem | default, get the last value of a list
      (define (last l def)
         (fold (λ (a b) b) def l))

      (example
         (last '(1 2 3) 'a) = 3
         (last '() 'a) = 'a)

      (define (app a b app)
         (if (null? a)
            b
            (cons (car a) (app (cdr a) b app))))

      (define (appl l appl)
         (if (null? (cdr l))
            (car l)
            (app (car l) (appl (cdr l) appl) app)))

      ;; append list ... -> list', join lists
      ;;    (append '(1) '() '(2 3)) = '(1 2 3)
      (define append
         (case-lambda
            ((a b) (app a b app))
            ((a b . cs) (app a (app b (appl cs appl) app) app))
            ((a) a)
            (() null)))


      (example
         (append '(1 2 3) '(a b c)) = '(1 2 3 a b c))

      ; todo: update to work like ledit
      (define (edit op l)
         (if (null? l)
            l
            (let ((x (op (car l))))
               (if x
                  (append x (edit op (cdr l)))
                  (cons (car l) (edit op (cdr l)))))))

      ;(define (reverse l) (fold (λ (r a) (cons a r)) null l))

      (define (rev-loop a b)
         (if (null? a)
            b
            (rev-loop (cdr a) (cons (car a) b))))

      ;; lst -> lst', reverse a list
      (define reverse (C rev-loop null))

      (example
         (reverse '(1 2 3)) = '(3 2 1))

      ;; misc

      (define (find pred lst)
         (and
            (pair? lst)
            (if (pred (car lst))
               (car lst)
               (find pred (cdr lst)))))

      (example
         (find null? '(1 2 3)) = #f
         (find null? '(1 ())) = ())

      (define (find-tail pred lst)
         (and
            (pair? lst)
            (if (pred (car lst))
               lst
               (find-tail pred (cdr lst)))))

      (define (take-while pred lst)
         (let loop ((lst lst) (taken null))
            (cond
               ((null? lst) (values (reverse taken) null))
               ((pred (car lst)) (loop (cdr lst) (cons (car lst) taken)))
               (else (values (reverse taken) lst)))))

      (define (keep pred lst)
         (foldr (λ (x tl) (if (pred x) (cons x tl) tl)) null lst))

      (define (remove pred lst)
         (keep (B not pred) lst))

      (let ((l '(1 2 () 3 () 4)))
         (example
            (keep null? l) = '(() ())
            (remove null? l) = '(1 2 3 4)))

      (define (every pred lst)
         (or (null? lst) (and (pred (car lst)) (every pred (cdr lst)))))

      (define (any pred lst)
         (and (pair? lst) (or (pred (car lst)) (any pred (cdr lst)))))

      (let ((l '(#t #f ())))
         (example
            (any null? l) = #true
            (every null? l) = #false))

      ; map carrying one state variable down like fold
      (define (smap op st lst)
         (if (null? lst)
            null
            (lets ((st val (op st (car lst))))
               (cons val
                  (smap op st (cdr lst))))))

      (define (fold-map o s l)
         (let loop ((s s) (l l) (r null))
            (if (null? l)
               (values s (reverse r))
               (lets ((s a (o s (car l))))
                  (loop s (cdr l) (cons a r))))))

      (define (foldr-map o s l)
         (if (null? l)
            (values s null)
            (lets
               ((a (car l))
                (s l (foldr-map o s (cdr l))))
               (o a s))))

      (define (diff a b)
         (cond
            ((null? a) a)
            ((memq (car a) b)
               (diff (cdr a) b))
            (else
               (cons (car a)
                  (diff (cdr a) b)))))

      (define (union a b)
         (cond
            ((null? a) b)
            ((memq (car a) b)
               (union (cdr a) b))
            (else
               (cons (car a)
                  (union  (cdr a) b)))))

      (define (intersect a b)
         (cond
            ((null? a) null)
            ((memq (car a) b)
               (cons (car a)
                  (intersect (cdr a) b)))
            (else
               (intersect (cdr a) b))))

      (let ((abc '(a b c)) (def '(d e f)) (cd '(c d)))
         (example
            (diff abc abc) = ()
            (union abc def) = '(a b c d e f)
            (intersect abc cd) = '(c)
            (diff abc cd) = (diff abc (intersect abc cd))))

      (define (interleave mid lst)
         (if (null? lst)
            null
            (let loop ((a (car lst)) (as (cdr lst)))
               (if (null? as)
                  (list a)
                  (ilist a mid (loop (car as) (cdr as)))))))

      ;; lst → a b, a ++ b == lst, length a = length b | length b + 1
      (define (halve lst)
         (let walk ((t lst) (h lst) (out null))
            (if (null? h)
               (values (reverse out) t)
               (let ((h (cdr h)))
                  (if (null? h)
                     (values (reverse (cons (car t) out)) (cdr t))
                     (walk (cdr t) (cdr h) (cons (car t) out)))))))
      (lets ((l '(a b c d e f)))
         (example
            l = (lets ((head tail (halve l))) (append head tail))))

      (example
         (interleave 'x '(a b c)) = '(a x b x c)
         (interleave 'x '()) = ()
         (halve '(a b c d)) = (values '(a b) '(c d))
         (halve '(a b c d e)) = (values '(a b c) '(d e)))

      (define ╯°□°╯ reverse)
))
