(define-library (owl list)

   (export 
      null pair? null?
      caar cadr cdar cddr
      caaar caadr cadar caddr 
      cdaar cdadr cddar cdddr
      list?      
      zip for fold foldr map for-each
      has? getq last drop-while
      mem
      fold-map foldr-map
      append reverse keep remove 
      all some
      smap unfold
      take-while                ;; pred, lst -> as, bs
      fold2
      first
      halve
      ╯°□°╯
      
      diff union intersect 

      ;; and now for something fairly different
      caaaar
      caaadr
      caadar
      caaddr
      cadaar
      cadadr
      caddar
      cadddr
      cdaaar
      cdaadr
      cdadar
      cdaddr
      cddaar
      cddadr
      cdddar
      cddddr
      )

   (import
      (owl defmac)
      (owl primop)
      (owl boolean))

   (begin

      ;; constants are always inlined, so you pay just one byte of source for readability

      (define null '())

      (define (pair? x) (eq? type-pair (type x)))

      (define (null? x) (eq? x null))

      (define-syntax withcc
         (syntax-rules ()
            ((withcc name proc)
               (call/cc (λ (name) proc)))))

      (define (caar x) (car (car x)))
      (define (cadr x) (car (cdr x)))
      (define (cdar x) (cdr (car x)))
      (define (cddr x) (cdr (cdr x)))

      (define (caaar x) (car (car (car x))))
      (define (caadr x) (car (car (cdr x))))
      (define (cadar x) (car (cdr (car x))))
      (define (caddr x) (car (cdr (cdr x))))
      (define (cdaar x) (cdr (car (car x))))
      (define (cdadr x) (cdr (car (cdr x))))
      (define (cddar x) (cdr (cdr (car x))))
      (define (cdddr x) (cdr (cdr (cdr x))))

      (define (caaaar x) (car (car (car (car x)))))
      (define (caaadr x) (car (car (car (cdr x)))))
      (define (caadar x) (car (car (cdr (car x)))))
      (define (caaddr x) (car (car (cdr (cdr x)))))
      (define (cadaar x) (car (cdr (car (car x)))))
      (define (cadadr x) (car (cdr (car (cdr x)))))
      (define (caddar x) (car (cdr (cdr (car x)))))
      (define (cadddr x) (car (cdr (cdr (cdr x)))))
      (define (cdaaar x) (cdr (car (car (car x)))))
      (define (cdaadr x) (cdr (car (car (cdr x)))))
      (define (cdadar x) (cdr (car (cdr (car x)))))
      (define (cdaddr x) (cdr (car (cdr (cdr x)))))
      (define (cddaar x) (cdr (cdr (car (car x)))))
      (define (cddadr x) (cdr (cdr (car (cdr x)))))
      (define (cdddar x) (cdr (cdr (cdr (car x)))))
      (define (cddddr x) (cdr (cdr (cdr (cdr x)))))

      (define (list? l)
         (cond
            ((null? l) #true)
            ((pair? l) (list? (cdr l)))
            (else #false)))

      (define (zip op a b)
         (cond
            ((null? a) null)
            ((null? b) null)
            (else
               (let ((hd (op (car a) (car b))))
                  (cons hd (zip op (cdr a) (cdr b)))))))

      ; (for st l op) == (fold op st l)
      ; just usually less indentation clutter 

      (define (for st l op)
         (if (null? l)
            st
            (for (op st (car l)) (cdr l) op)))

      (define (fold op state lst) 
         (if (null? lst) 
            state 
            (fold op 
               (op state (car lst))
               (cdr lst))))

      (define (unfold op st end?)
         (if (end? st)
            null
            (lets ((this st (op st)))
               (cons this (unfold op st end?)))))

      (define (fold2 op s1 s2 lst)
         (if (null? lst)
            (values s1 s2)
            (lets ((s1 s2 (op s1 s2 (car lst))))
               (fold2 op s1 s2 (cdr lst)))))

      (define (foldr op st lst)
         (if (null? lst)
            st
            (op (car lst)
               (foldr op st (cdr lst)))))

      (define (map fn lst)
         (if (null? lst)
            null
            (lets 
               ((hd tl lst)
                (hd (fn hd))) ;; compute head first
               (cons hd (map fn tl)))))

      (define (for-each op lst)
         (if (null? lst)
            null
            (begin
               (op (car lst))
               (for-each op (cdr lst)))))

      (define (has? lst x)
         (cond
            ((null? lst) #false)
            ((eq? (car lst) x) lst)
            (else (has? (cdr lst) x))))

      (define (getq lst k)
         (cond
            ((null? lst) #false)
            ((eq? k (car (car lst))) (car lst))
            (else (getq (cdr lst) k))))

      (define (last l def)
         (fold (λ (a b) b) def l)) 

      (define (mem cmp lst elem)
         (cond
            ((null? lst) #false)
            ((cmp (car lst) elem) lst)
            (else (mem cmp (cdr lst) elem))))

      ;(define (append a b) (foldr cons b a))

      (define (app a b app)
         (if (null? a)
            b
            (cons (car a) (app (cdr a) b app))))
      
      (define (appl l appl)
         (if (null? (cdr l))
            (car l)
            (app (car l) (appl (cdr l) appl) app)))

      (define append
         (case-lambda 
            ((a b) (app a b app))
            ((a b . cs) (app a (app b (appl cs appl) app) app))
            ((a) a)
            (() null)))

      ;(define (reverse l) (fold (λ (r a) (cons a r)) null l))

      (define (rev-loop a b)
         (if (null? a)
            b
            (rev-loop (cdr a) (cons (car a) b))))

      (define (reverse l) (rev-loop l null))   

      ;; misc

      (define (drop-while pred lst)
         (cond
            ((null? lst) lst)
            ((pred (car lst))
               (drop-while pred (cdr lst)))
            (else lst)))

      (define (take-while pred lst)
         (let loop ((lst lst) (taken null))
            (cond
               ((null? lst) (values (reverse taken) null))
               ((pred (car lst)) (loop (cdr lst) (cons (car lst) taken)))
               (else (values (reverse taken) lst)))))

      (define (keep pred lst)
         (foldr (λ (x tl) (if (pred x) (cons x tl) tl)) null lst))

      (define (remove pred lst)
         (keep (o not pred) lst))

      (define (all pred lst)
         (withcc ret
            (fold (λ (ok x) (if (pred x) ok (ret #false))) #true lst)))

      (define (some pred lst) 
         (withcc ret
            (fold (λ (_ x) (let ((v (pred x))) (if v (ret v) #false))) #false lst)))

      ; map carrying one state variable down like fold
      (define (smap op st lst)
         (if (null? lst)
            null
            (lets ((st val (op st (car lst))))
               (cons val
                  (smap op st (cdr lst))))))


      ; could also fold
      (define (first pred l def)
         (cond
            ((null? l) def)
            ((pred (car l)) (car l))
            (else (first pred (cdr l) def))))

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
            ((has? b (car a))
               (diff (cdr a) b))
            (else
               (cons (car a)
                  (diff (cdr a) b)))))

      (define (union a b)
         (cond
            ((null? a) b)
            ((has? b (car a))
               (union (cdr a) b))
            (else
               (cons (car a)
                  (union  (cdr a) b)))))

      (define (intersect a b)
         (cond
            ((null? a) null)
            ((has? b (car a))
               (cons (car a)
                  (intersect (cdr a) b)))
            (else
               (intersect (cdr a) b))))

      ;; lst → a b, a ++ b == lst, length a = length b | length b + 1
      (define (halve lst)
         (let walk ((t lst) (h lst) (out null))
            (if (null? h)
               (values (reverse out) t)
               (let ((h (cdr h)))
                  (if (null? h)
                     (values (reverse (cons (car t) out)) (cdr t))
                     (walk (cdr t) (cdr h) (cons (car t) out)))))))


   (define ╯°□°╯ reverse)     

))
