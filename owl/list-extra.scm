(define-library (owl list-extra)

   (export
      lset ldel length
      led ledn lins
      take drop iota
      list-ref
      list-tail
      make-list
      split ;; lst n → head tail
      )

   (import
      (owl defmac)
      (owl math)
      (owl list)
      (owl proof)
      (owl syscall))

   (begin

      (define (list-ref lst pos)
         (cond
            ((null? lst) (error "list-ref: out of list" pos))
            ((eq? pos 0) (car lst))
            (else (list-ref (cdr lst) (- pos 1)))))

      (define (lset lst pos val)
         (cond
            ((null? lst) (error "list-set: out of list setting " val))
            ((eq? pos 0) (cons val (cdr lst)))
            (else
               (cons (car lst)
                  (lset (cdr lst) (- pos 1) val)))))

      (define (ldel lst pos)
         (cond
            ((null? lst) (error "list-del: out of list, left " pos))
            ((eq? pos 0) (cdr lst))
            (else (cons (car lst) (ldel (cdr lst) (- pos 1))))))

      ;; list edit node - apply op to list (not element) at pos pos, allowing last null
      (define (ledn lst pos op)
         (cond
            ((eq? pos 0) (op lst))
            ((null? lst) (error "ledn: out of list, remaining " pos))
            (else
               (lets ((hd tl lst))
                  (cons hd (ledn tl (- pos 1) op))))))

      ;; list edit - apply op to value at given pos
      (define (led lst pos op)
         (cond
            ((null? lst) (error "led: out of list, remaining " pos))
            ((eq? pos 0) (cons (op (car lst)) (cdr lst)))
            (else
               (lets ((hd tl lst))
                  (cons hd (led tl (- pos 1) op))))))

      ;; insert value to list at given position
      (define (lins lst pos val)
         (cond
            ((eq? pos 0) (cons val lst))
            ((null? lst) (error "lins: out of list, left " pos))
            (else
               (lets ((hd tl lst))
                  (cons hd (lins tl (- pos 1) val))))))

      (example
         (list-ref '(a b c) 1) = 'b
         (lset '(a b c) 1 'x) = '(a x c)
         (ldel '(a b c) 1) = '(a c)
         (led '(1 2 3) 1 (C * 10)) = '(1 20 3)
         (ledn '(1 2 3) 1 (H cons 'x)) = '(1 x 2 3)
         (lins '(a b c) 1 'x) = '(a x b c))

      (define (length lst)
         (fold (λ (n v) (+ n 1)) 0 lst))

      ; take at n (or less) elemts from list l

      (define (take l n)
         (cond
            ((eq? n 0) null)
            ((null? l) null)
            (else (cons (car l) (take (cdr l) (- n 1))))))

      (define (drop l n)
         (cond
            ((eq? n 0) l)
            ((null? l) l)
            (else (drop (cdr l) (- n 1)))))

      (example
         (length '(a b c)) = 3
         (take '(a b c) 2) = '(a b)
         (take '(a) 100) = '(a)
         (drop '(a b c) 2) = '(c)
         (drop '(a) 100) = '())

      (define (iota-up p s e)
         (if (< p e)
            (cons p (iota-up (+ p s) s e))
            null))

      (define (iota-down p s e)
         (if (> p e)
            (cons p (iota-down (+ p s) s e))
            null))

      (define (iota from step to)
         (cond
            ((> step 0)
               (if (< to from) null (iota-up from step to)))
            ((< step 0)
               (if (> to from) null (iota-down from step to)))
            ((= from to)
               null)
            (else
               (error "bad iota: " (list 'iota from step to)))))

      (example
         (iota 0 1 5) = '(0 1 2 3 4)
         (iota 10 -2 0) = '(10 8 6 4 2))

      (define (list-tail lst n)
         (if (eq? n 0)
            lst
            (list-tail (cdr lst) (- n 1))))

      (define (make-list n thing)
         (let loop ((n n) (out null))
            (if (eq? n 0)
               out
               (loop (- n 1) (cons thing out)))))

      (define (split l n)
         (let loop ((l l) (o null) (n n))
            (cond
               ((null? l)
                  (values (reverse o) l))
               ((eq? n 0)
                  (values (reverse o) l))
               (else
                  (loop (cdr l) (cons (car l) o) (- n 1))))))

      (example
         (list-tail '(a b c) 1) = '(b c)
         (make-list 3 'x) = '(x x x)
         (split '(a b c d) 2) = (values '(a b) '(c d)))
))
