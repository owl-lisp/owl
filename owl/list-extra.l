(define-module lib-list-extra

	(export lref lset length
		take drop iota)

	(import lib-math) ; basix arithmetic

	;;; List ops using numbers

	(define (lref lst pos)
		(cond
			((null? lst) (error "lref: out of list" pos))
			((eq? pos 0) (car lst))
			(else (lref (cdr lst) (- pos 1)))))

	(define (lset lst pos val)
		(cond
			((null? lst) (error "list-set: out of list setting " val))
			((eq? pos 0) (cons val (cdr lst)))
			(else
				(cons (car lst)
					(lset (cdr lst) (- pos 1) val)))))

	(define (length lst)
		(fold (λ (n v) (+ n 1)) 0 lst))

	; take at n (or less) elemts from list l

	(define (take l n)
		(cond	
			((eq? n 0) null)
			((null? l) null)
			(else (cons (car l) (take (cdr l) (- n 1))))))

	; drop n elements (or less) from list l

	(define (drop l n)
		(cond
			((eq? n 0) l)
			((null? l) l)
			(else (drop (cdr l) (- n 1)))))

	; fixme, iotas should be unfolds

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

)
