
;; making printable representations

(define-library (owl print)

   (import
      (owl defmac)
      (owl string)
      (owl list)
      (owl list-extra)
      (owl boolean)
      (owl symbol)
      (owl tuple)
      (owl function)
      (owl rlist)
      (only (owl vector) byte-vector? vector? vector->list)
      (only (owl math) render-number number?)
      (only (owl string) render-string string?)
      )
   
   (export 
      serialize       ;; obj tl → (byte ... . tl), lazy, suitable for write
      render          ;; obj tl → (byte ... . tl), lazy, usual output
      )

   (begin

      (define (render obj tl)
         (cond

            ((null? obj)
               (ilist #\( #\) tl))

            ((number? obj)
               (render-number obj tl 10))

            ((string? obj)
               (render-string obj tl))

            ((pair? obj)
               (cons #\(
                  (cdr
                     (let loop ((obj obj) (tl (cons #\) tl)))
                        (cond
                           ((null? obj) tl)
                           ((pair? obj)
                              (cons #\space 
                                 (render (car obj) (loop (cdr obj) tl))))
                           (else
                              (ilist #\space #\. #\space (render obj tl))))))))

            ((boolean? obj)
               (append (string->list (if obj "#true" "#false")) tl))
               
            ((symbol? obj)
               (render (symbol->string obj) tl))

            ;; these are a subclass of vectors in owl
            ((byte-vector? obj)
               (ilist #\u #\8 (render (vector->list obj) tl)))

            ((vector? obj)
               (cons #\# (render (vector->list obj) tl)))

            ((function? obj)
               (append (string->list "#<function>") tl))

            ((tuple? obj)
               (ilist 40 84 117 112 108 101 32
                  (render (ref obj 1)
                     (fold
                        (λ (tl pos) (cons 32 (render (ref obj pos) tl)))
                        (cons 41 tl)
                        (iota (size obj) -1 1)))))

            ;; fixme: not parsed yet
            ((rlist? obj)
               (ilist #\# #\r (render (rlist->list obj) tl)))

            (else 
               (append (string->list "#<WTF>") tl)))) ;; What This Format?

;      (define render (λ (self obj tl) (if (function? obj) (ilist 35 60 (self self (let ((name (interact 'intern (tuple 'get-name obj)))) (or name "function")) (cons 62 tl))) (render self obj tl))))))

      (define serialize render)
))
