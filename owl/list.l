(define-module lib-list

   (export 
      null pair? null?
      caar cadr cdar cddr
      caaar caadr cadar caddr 
      cdaar cdadr cddar cdddr
      list?      
      zip for fold foldr map for-each
      has? getq lst last drop-while
      mem
      append reverse keep remove 
      all some
      smap unfold
      take-while                ;; pred, lst -> as, bs
      fold2
      first
      render
      diff union intersect 
      )

   (define null Null)

   (define (pair? x) (eq? (type x) 14))
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

   (define (list? l)
      (cond
         ((null? l) True)
         ((pair? l) (list? (cdr l)))
         (else False)))

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
         (cons st
            (unfold op (op st) end?))))

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
      (foldr (λ (a bs) (cons (fn a) bs)) null lst))

   (define (for-each op lst)
      (if (null? lst)
         null
         (begin
            (op (car lst))
            (for-each op (cdr lst)))))

   (define (has? lst x)
      (cond
         ((null? lst) False)
         ((eq? (car lst) x) lst)
         (else (has? (cdr lst) x))))

   (define (getq lst k)
      (cond
         ((null? lst) False)
         ((eq? k (car (car lst))) (car lst))
         (else (getq (cdr lst) k))))

   (define (last l def)
      (fold (λ (a b) b) def l)) 

   (define (mem cmp lst elem)
      (cond
         ((null? lst) False)
         ((cmp (car lst) elem) lst)
         (else (mem cmp (cdr lst) elem))))

   (define (append a b) (foldr cons b a))

   ;(define (reverse l) (fold (λ (r a) (cons a r)) null l))

   (define (rev-loop a b)
      (if (null? a)
         b
         (rev-loop (cdr a) (cons (car a) b))))

   (define (reverse l) (rev-loop l null))   

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
         (fold (λ (ok x) (if (pred x) ok (ret False))) True lst)))

   (define (some pred lst) 
      (withcc ret
         (fold (λ (_ x) (let ((v (pred x))) (if v (ret v) False))) False lst)))

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

   ;; rendering

   (define (render-list-content render node tl)
      (cond 
         ((null? node) tl)
         ((pair? node)
            (cons 32
               (render render (car node) (render-list-content render (cdr node) tl))))
         (else 
            (ilist 32 46 32 (render render node tl)))))

   (define render
      (lambda (self obj tl)
         (cond
            ((null? obj)
               (ilist 40 41 tl))
            ((pair? obj)
               (cons 40 (cdr (render-list-content self obj (cons 41 tl)))))
            (else
               (render self obj tl)))))


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


)
