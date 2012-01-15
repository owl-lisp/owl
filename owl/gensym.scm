

(define-module lib-gensym

   (export gensym fresh)

   ; now in lib-intern
   ;(define (string->symbol str) (interact 'intern str))
   ;(define (symbol->string x) (ref x 1))

   ; return the gensym id of exp (number) or False

   (define (count-gensym-id str pos end n)
      (if (= pos end)
         n
         (let ((this (refb str pos)))   
            (cond
               ((and (< 47 this) (< this 58))
                  (count-gensym-id str (+ pos 1) end (+ (* n 10) (- this 48))))
               (else False)))))

   (define (gensym-id exp)
      (if (symbol? exp)
         (let ((str (symbol->string exp)))
            (let ((len (string-length str)))
               (if (and (> len 1) (eq? (refb str 0) 103))
                  (count-gensym-id str 1 len 0)
                  False)))
         False))

   (define (max-gensym-id exp max)
      (cond  
         ((pair? exp)
            (if (eq? (car exp) 'quote)
               max
               (max-gensym-id (cdr exp)
                  (max-gensym-id (car exp) max))))
         ((gensym-id exp) =>
            (lambda (id)
               (if (> id max) id max)))
         (else max)))

   (define (max-ast-id exp max)
      (tuple-case exp
         ((var sym)
            (max-gensym-id sym max))
         ((lambda formals body)
            (max-ast-id body
               (max-gensym-id formals max)))
         ((call rator rands)
            (max-ast-id rator
               (fold 
                  (lambda (max exp) (max-ast-id exp max))
                  max rands)))
         ((value val) max)
         ((receive op fn)
            (max-ast-id op
               (max-ast-id fn max)))
         ((branch kind a b then else)
            (max-ast-id a (max-ast-id b
               (max-ast-id then (max-ast-id else max)))))
         ((values vals)
            (fold (lambda (max exp) (max-ast-id exp max)) max vals))
         (else
            (error "gensym: max-ast-id: what is this: " exp)
            max)))


   (define (gensym exp)
      (lets 
         ((id (+ 1 (if (tuple? exp) (max-ast-id exp 0) (max-gensym-id exp 0))))
          (digits (cons 103 (render render id null))))
         (string->symbol (runes->string digits))))

   (define (fresh free)
      (values free (gensym free)))

   ;(gensym 1)
   ;(gensym '(1 2 3))
   ;(gensym '(g1 (g2 g9999) . g10000000000000))
)

