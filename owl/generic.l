;;;
;;; Generic versions of some common operations related to iterable things
;;;

;; todo: add rlists

; currently generic-ified modules
;	- lists
;	- lazy lists (handled here directly)
;	- vectors
;	- strings

; todo operations
;	- rev(erse)

,r "owl/rlist.l"
(define-module lib-generic 
	(export 
		fold		;; (fold op st X) → (op (.. (op (op st a) b) ..) n)
		foldr		;; (foldr op st X) → (op a (op b ... (op n st)))
		len		;; (len x) -> nat
		map		;; (map f x) -> x'
      iter     ;; iterate something to a ll
      iterr    ;; ditto from the end
      cat      ;; a x b → a++b
      rev      ;; (a b ...) → (... b a)
      drop     ;; l n → l'
	)

	(import lib-rlist)

	(define (fold op st x)
		(cond
			((pair? x) (fold op (op st (car x)) (cdr x))) 	;; shortest path for list case
			((null? x) st)
			;; fixme: switch function? test to major type test later
			((eq? #b110 (fxband (type x) #b11111110)) 		;; could also be a lazy thing yielding the value
				(fold op st (x)))
			((not x) st) ; empty ff or similar
			((ff? x) (ff-fold op st x))
			((vector? x) (vec-fold op st x))
			((string? x) (str-fold op st x))
			((teq? x (alloc 10)) (rfold op st x)) ; random access list 
			(else (error "fold: what is a " x))))

	(define (foldr op st x)
		(cond
			((pair? x) (let ((a (car x))) (op a (foldr op st (cdr x)))))
			((null? x) st)
			;; fixme: switch function? test to inlined major type test later
			((eq? #b110 (fxband (type x) #b11111110)) 		;; could also be a lazy thing yielding the value
				(foldr op st (x)))
			((not x) st) ; empty ff or similar
			((ff? x) (ff-foldr op st x))
			((vector? x) (vec-foldr op st x))
			((string? x) (str-foldr op st x))
			((teq? x (alloc 10)) (rfoldr op st x)) ; random access list 
			(else (error "foldr: what is a " x))))

	(define (map fn x)
		(cond
			((pair? x) (let ((a (car x))) (cons (fn a) (map fn (cdr x)))))
			((null? x) x)
			;; fixme: switch function? test to inlined major type test later
			((eq? #b110 (fxband (type x) #b11111110)) 		;; could also be a lazy thing yielding the value
				(map fn (x)))
			((not x) x) ; empty ff or similar
			((ff? x) (ff-map x fn)) ; note reversed order for now
			((teq? x (alloc 10)) (rmap fn x)) ; random access list
			((vector? x) (vec-map fn x))
			((string? x) (str-map fn x))
			(else (error "map: what is a " x))))

	(define (listlike-len l n)
		(cond
			((pair? l)
				(listlike-len (cdr l) (+ n 1)))
			((null? l) n)
			((eq? #b110 (fxband (type l) #b11111110)) 		;; could also be a lazy thing yielding the value
				(listlike-len (l) n))
			(else 
				(error "len: what's a " l))))

	(define (len x)
		(cond
			((pair? x) (listlike-len (cdr x) 1))
			((vector? x) (vec-len x))
			((string? x) (string-length x))
			((teq? x (alloc 10)) (rlen x))
			(else (listlike-len x 0))))

   ;; assume the same type
   (define (cat a b)
      (cond
         ((pair? a) (append a b))
         ((null? a) b)
         ((vector? a) (vec-cat a b))
         ((string? a) (str-app a b))
         (else 
            (error "cat: what is " a))))

   (define (iter l)
      (cond
         ((pair? l) l)
         ((null? l) l)
         ((string? l) (str-iter l))
         ((vector? l) (vec-iter l))
         (else (error "iter: how do i iterate " l))))

   (define (rev l)
      (cond
         ((pair? l) (reverse l))
         ((null? l) l)
         ((string? l) (str-rev l))
         ((vector? l) (vec-rev l))
         (else (error "rev: how do i reverse " l))))

   ;; forcing reverse
   (define (lrev l o)
      (cond
         ((null? l) o)
         ((pair? l) (lrev (cdr l) (cons (car l) o)))
         (else (lrev (l) o))))

   (define (iterr l)
      (cond
         ((pair? l) (lrev l null))
         ((null? l) l)
         ((string? l) (str-iterr l))
         ((vector? l) (vec-iterr l))
         (else (error "iter: how do i iterate " l))))

   ;; lazy drop
   (define (drop l n)
      (cond
         ((eq? n 0) l)
         ((null? l) l)
         ((pair? l) 
            (drop (cdr l) (- n 1)))
         (else (drop (l) n))))

	; should also define a (ref obj key default) here 
)
