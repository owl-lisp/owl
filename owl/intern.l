;;;
;;; Value interning and conversions
;;;

; have mappings:
;  - strings->symbol
;  - bytes->bytecode (where returned bytecode may use spacial primops)
;  (- intern-char     -> x → x')

(define-module lib-intern
   (export
      bytes->symbol
      string->symbol
      symbol->string
      initialize-interner
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

   ;; node is an unbalanced trie of symbols (F | (Tuple L sym R))
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


   (define (string->symbol str)
      (interact 'intern str))

   ;;;
   ;;; BYTECODE INTERNING
   ;;;

   (define is-less False)
   (define is-equal True)
   (define is-greater null)

   (define (compare-bytes a b pos end)
      (if (eq? pos end)
         is-equal
         (let ((ab (refb a pos)) (bb (refb b pos)))
            (cond
               ((eq? ab bb) (compare-bytes a b (+ pos 1) end))
               ((lesser? ab bb) is-less)
               (else is-greater)))))

   ;; shorter is less, otherwase lexical comparison from start
   (define (compare-code a b)
      (lets
         ((as (sizeb a))
          (bs (sizeb b)))
         (cond 
            ((eq? as bs) (compare-bytes a b 0 as))
            ((lesser? as bs) is-less)
            (else is-greater))))

   ;; fixme: should occasionally balance the tree

   ;; codes bcode value → codes'
   (define (insert-code codes bcode value)
      (if codes
         (ff-bind codes
            (λ (l k v r)
               (let ((res (compare-code k bcode)))
                  (cond
                     ((eq? res is-equal)
                        (mkblack l bcode value r))
                     ((eq? res is-less)
                        (mkblack (insert-code l bcode value) k v r))
                     (else
                        (mkblack l k v (insert-code r bcode value)))))))
         (mkblack False bcode value False)))

    ;; codes bcode → bcode' | False
    (define (lookup-code codes bcode)
      (if codes
         (ff-bind codes
            (λ (l k v r)
               (let ((res (compare-code k bcode)))
                  (cond
                     ((eq? res is-equal) v)
                     ((eq? res is-less) (lookup-code l bcode))
                     (else (lookup-code r bcode))))))
         False))

   ;; codes bcode → codes(') bcode(')
   (define (intern-code codes bcode)
      (let ((res (lookup-code codes bcode)))
         (if res
            (values codes res)
            (values (insert-code codes bcode bcode) bcode))))

   ; this will be forked as 'interner
   ; to bootstrap, collect all symbols from the entry procedure, intern
   ; them, and then make the intial threads with an up-to-date interner

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
      
   ;; thread with string → symbol, ...
   (define (interner root codes)
      (lets
         ((env (wait-mail))
          (sender msg env))
         (cond
            ((string? msg) ;; find an old symbol or make a new one
               (lets ((root sym (string->interned-symbol root msg)))
                  (mail sender sym)
                  (interner root codes)))
            ((bytecode? msg) ;; find an old equal bytecode sequence, extended wrapper, or add a new code fragment
               (lets ((codes code (intern-code codes msg)))
                  (mail sender code)
                  (interner root codes)))
            (else
               (mail sender 'bad-kitty)
               (interner root codes)))))

   ;; make a thunk to be forked as the thread
   (define (initialize-interner symbol-list codes)
      (let 
         ((sym-root (fold put-symbol False symbol-list))
          (code-root (fold (λ (codes pair) (insert-code codes (car pair) (cdr pair))) False codes)))
         (λ () (interner sym-root code-root))))

)
