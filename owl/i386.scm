;;;
;;; a (highly experimental) bytecode2assembly translator
;;;

;; the current version of owl does not aim (yet) to fully support itself.
;; this is one of the long-term goals though.

(define-module lib-i386
	(export 
		compile-to-asm          ;; obj extras → False | asm-code-string
	)

   (define alloc-types
      (list->ff
         '((1 . pair))))

   ;; represent some immediate as a string in C
	(define (represent val fail)
		(cond
			;((eq? val null) "INULL")
			;((eq? val True) "ITRUE")
			;((eq? val False) "IFALSE")
			((teq? val fix+) ;; $<num>, being a tagged fixnum like in owl
				(bytes->string
               (cons 36 (render (+ 2 (<< val 12)) null))))
			(else 
				(show "represent: cannot yet handle " val)
				(fail))))

	;; fixme: raw? cut n pasted here
	(define (raw? obj) (eq? (fxband (type obj) #b100000000110) #b100000000110))

	; -> list of bytes | False
	(define (code->bytes code extras)
		(if (and (function? code) (raw? code))
			(let ((bytes (map (λ (p) (refb code p)) (iota 0 1 (sizeb code)))))
				(if (eq? (cadr bytes) 0) ;; (<arity> 0 <hi8> <lo8>) == call extra instruction
               (lets
                  ((opcode (+ (<< (caddr bytes) 8) (car (cdddr bytes))))
                   (bytecode (get extras opcode False)))
                  (if bytecode
                     (code->bytes bytecode extras) ;; <- vanilla bytecode (modulo boostrap bugs)
                     (error "code->bytes: cannot find original bytecode for opcode " opcode)))
               bytes))
			False))

   (define (unknown bs regs fail)
      ;(show " - cgen does not grok opcode " (car bs))
      (fail))

   (define (get2 l) ; (a b . tl)
      (let ((tl (cdr l))) 
         (values (car l) (car tl) (cdr tl))))

   (define (get3 l)
      (lets ((b c tl (get2 (cdr l))))
         (values (car l) b c tl)))

   (define (get4 l)
      (lets ((b c d tl (get3 (cdr l))))
         (values (car l) b c d tl)))

   (define (get5 l)
      (lets ((b c d e tl (get4 (cdr l))))
         (values (car l) b c d e tl)))

   (define (get6 l)
      (lets ((b c d e f tl (get5 (cdr l))))
         (values (car l) b c d e f tl)))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; translator function dispatch ff

	(define translators 
      (list->ff
         (list
            ;; not here yet
            )))

   ;; regs is a ff of partial knowledge going downwards about things currently in registers
   ;; → (obs ... . tail)
   (define (emit-asm ops regs fail tail)
      (if (null? ops)
         tail
         (lets ((res tl regs ((get translators (car ops) unknown) ops regs fail)))
            (cond
               (else ;; instruction compiled, handle the rest
                  (append res (emit-asm tl regs fail tail)))))))

   ;; obj extras → False | (arity . c-code-string), to become #[arity 0 hi8 lo8] + c-code in vm
	(define (compile-to-asm code extras)
		(if (and (function? code) (raw? code)) ;; todo: add bytecode? elsewhere
         (let ((ops (code->bytes code extras)))
            (call/cc
					(λ (ret)
                  (cons (car ops)
                     (list->string
                        (foldr render null
                           (emit-c (cdr ops) False (λ () (ret False)) null)))))))
         False))

   (compile-to-asm (λ (args) 42))

)

;; planning 
;
; .globl _start
; _start:
;     pushl $halt    <- vm halt label (all labels aligned!)
;     pushl <null>   <- args (irrelevant for now) <-- no, substract enough to get space for regs and then use
;                       a more pushy register user might be the way to go in the next incarnation of the runtim
;     mov $<tag-of-entry>, %eax
;     mov $2, %ebx   <- nargs check on call
;     jmp apply
; 
; apply:
;     <check that eax is alloc (pointer>)>
;     <based on type fill 0-2 registers and follow field 1 to code>
;     mov <code>, $eax
;     jmp *%eax           ; <- maybe remove tag if using them here
; 
; clos1:
;     <check-ebx>         <- nargs check
;     <code>
;     <set up eax and ebx> + jmp apply || in case of known call, set up regs and jmp *%eax

