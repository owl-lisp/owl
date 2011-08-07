;;;
;;; Closure inliner stub
;;;

; after a value has been evaluated, all bindings in it are cast in stone.
; owl closures already have a literals section which is used to add direct
; pointers to all necessary values which are known statically. it would be 
; just plain stupid to keep carrying values using environments to where 
; they are needed after they are statically known after evaluation, especially 
; since it costs O(n) memory and time to do it, even though the n is rather 
; small.

; also, this would be a great place to check and specialize calls to 
; the known values which were not known at compile time.

; optimization pass, has no effect on computations, modulo bugs

;; todo: needed assembly side also

(define-module lib-inline
	(export 
		inline-closure
		inline-closures
		)

	; battle plan
	;	- main pass: find all closures and convert them
	;  - inliner:
	;		+ take a single closure
	;     + take the values of it's closure aside
	;		+ make a new literals-tuple and extend it with the contents of the closure
	;		+ disasm the code
	;		+ walk the code and convert all references to the environment-register to 
	;       point to literals at nliterals + offset
	;		+ keep track of register contents in the process
	;		+ on each call position
	;			+ if the called value is known, check arity and pick a suitable direct goto-instruction to bypass a full apply

	(define inline-closure (λ (x) x))
	(define inline-closures (λ (a b) (values a b)))

	(define (closure? val) (eq? (type val) 518))
	(define (proc? val) (eq? (type val) 262))
	(define (code? val) (eq? (type val) 2054))

	(define (inline-closure ob)
		(if (closure? val)
			(lets
				((proc (ref val 1))
				 (code (ref proc 1)))


			False))


)
