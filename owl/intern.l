;;;
;;; Symbol interning and conversions
;;;

(define-module lib-intern
   (export
      bytes->symbol
      string->symbol
      symbol->string
      put-symbol
      initialize-symbol-interner
      string->uninterned-symbol
      intern-symbols
      )

   (define symbol-store
      (tuple False 'lambda False))

   ; hack warning, could use normal = and < here, but 
   ; using primitives speeds up parsing a bit

   ; False = s1 is less, 0 = equal, 1 = s1 is more
   (define (walk s1 s2)
      (cond
         ((null? s1)
            (cond
               ((pair? s2) False)
               ((null? s2) 0)
               (else (walk s1 (s2)))))
         ((pair? s1)
            (cond
               ((pair? s2)
                  (lets 
                     ((a as s1)
                      (b bs s2))
                     (cond
                        ((eq? a b) (walk as bs))
                        ((lesser? a b) False)
                        (else True))))
               ((null? s2) 1)
               (else (walk s1 (s2)))))
         (else (walk (s1) s2))))
            
   (define (compare s1 s2)
      (walk (str-iter s1) (str-iter s2)))

   ; FIXME, add a typed ref instruction

   (define (string->uninterned-symbol str)
      (mkt 4 str))

   (define (symbol->string ob)
      (ref ob 1))

   ; lookup node str sym -> node' sym'

   (define (maybe-lookup-symbol node str)   
      (if node
         (lets
            ((this (symbol->string (ref node 2)))
             (res (compare str this)))
            (cond
               ((eq? res 0) ; match
                  (ref node 2))
               (res
                  (maybe-lookup-symbol (ref node 1) str))
               (else
                  (maybe-lookup-symbol (ref node 3) str))))
         False))


   (define (put-symbol node sym)
      (if node
         (lets
            ((this (ref node 2))
             (res (compare (symbol->string sym) (symbol->string this))))
            (cond
               ((eq? res 0)
                  (set node 2 sym))
               (res
                  (set node 1 
                     (put-symbol (ref node 1) sym)))
               (else
                  (set node 3
                     (put-symbol (ref node 3) sym)))))
         (tuple False sym False)))
      
   ;; note, only leaf strings for now
   (define (string->interned-symbol root str)
      (let ((old (maybe-lookup-symbol root str)))
         (if old
            (values root old)
            (let ((new (string->uninterned-symbol str)))
               (values (put-symbol root new) new)))))

   (define symbol-store (tuple False))

   ; interner is started before the repl at vm boot


   (define (new-symbol-interner root)
      (bind (wait-mail)
         (λ sender msg
            (if (string? msg)
               (lets ((root sym (string->interned-symbol root msg)))
                  (mail sender sym)
                  (new-symbol-interner root))
               (begin
                  (mail sender 'bad-kitty)
                  (new-symbol-interner root))))))


   (define (string->symbol str)
      (interact 'intern str))

   ; this will be forked as 'interner
   ; to bootstrap, collect all symbols from the entry procedure, intern
   ; them, and then make the intial threads with an up-to-date interner

   ; ready to be forked 
   (define (initialize-symbol-interner root)
      (λ (new-symbol-interner root)))

   (define (bytes->symbol bytes)
      (string->symbol 
         (runes->string 
            (reverse bytes))))

   (define (intern-symbols sexp)
      (cond
         ((symbol? sexp)
            (string->symbol (ref sexp 1)))
         ((pair? sexp)
            (cons (intern-symbols (car sexp)) (intern-symbols (cdr sexp))))
         (else sexp)))
      
)
