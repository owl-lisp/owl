
;; making printable representations

(define-library (owl render)

   (import
      (owl defmac)
      (owl string)
      (owl list)
      (owl list-extra)
      (owl boolean)
      (owl symbol)
      (owl ff)
      (owl tuple)
      (owl function)
      (owl rlist)
      (owl syscall)
      (owl lazy)
      (owl math)
      (only (owl fasl) object-closure)
      (only (owl vector) byte-vector? vector? vector->list)
      (only (owl math) render-number number?)
      (only (owl string) render-string string?))
   
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
            ;((byte-vector? obj)
            ;   (ilist #\# #\u #\8 (render (vector->list obj) tl)))

            ((vector? obj)
               (cons #\# (render (vector->list obj) tl)))

            ((function? obj)
               ;; anonimas
               ;(append (string->list "#<function>") tl)
               (let ((symp (interact 'intern (tuple 'get-name obj))))
                  (if symp
                     (ilist #\# #\< (render symp (cons #\> tl)))
                     (render "#<function>" tl))))

            ((tuple? obj)
               (ilist 40 84 117 112 108 101 32
                  (render (ref obj 1)
                     (fold
                        (λ (tl pos) (cons 32 (render (ref obj pos) tl)))
                        (cons 41 tl)
                        (iota (size obj) -1 1)))))

            ((rlist? obj) ;; fixme: rlist not parsed yet
               (ilist #\# #\r (render (rlist->list obj) tl)))

            ((ff? obj) ;; fixme: ff not parsed yet this way
               (cons #\# (render (ff->list obj) tl)))

            (else 
               (append (string->list "#<WTF>") tl)))) ;; What This Format?


      ;;; serialize suitably for parsing, not yet sharing preserving

      (define (ser obj sh tl)
         (cond

            ((getf sh obj) =>
               (λ (id) 
                  (ilist #\# #\< (ser id sh (cons #\> tl)))))

            ((null? obj)
               (ilist #\' #\( #\) tl))

            ((number? obj)
               (render-number obj tl 10))

            ((string? obj)
               (cons #\" 
                  (render-quoted-string obj 
                     (cons #\" tl))))

            ((pair? obj)
               (cons #\(
                  (cdr
                     (let loop ((obj obj) (tl (cons #\) tl)))
                        (cond
                           ((null? obj) tl)
                           ((pair? obj)
                              (cons #\space 
                                 (ser (car obj) sh (loop (cdr obj) tl))))
                           (else
                              (ilist #\space #\. #\space (ser obj sh tl))))))))

            ((boolean? obj)
               (append (string->list (if obj "#true" "#false")) tl))
               
            ((symbol? obj)
               (render (symbol->string obj) tl))

            ;; these are a subclass of vectors in owl
            ;((byte-vector? obj)
            ;   (ilist #\# #\u #\8 (render (vector->list obj) tl)))

            ((vector? obj)
               (cons #\# (ser sh (vector->list obj) tl)))

            ((function? obj)
               ;; anonimas
               ;(append (string->list "#<function>") tl)
               (let ((symp (interact 'intern (tuple 'get-name obj))))
                  (if symp
                     (ilist #\# #\< (render symp (cons #\> tl)))
                     (render "#<function>" tl))))

            ((tuple? obj)
               (ilist 40 84 117 112 108 101 32
                  (render (ref obj 1)
                     (fold
                        (λ (tl pos) (cons 32 (render (ref obj pos) tl)))
                        (cons 41 tl)
                        (iota (size obj) -1 1)))))

            ((rlist? obj) ;; fixme: rlist not parsed yet
               (ilist #\# #\r (ser sh (rlist->list obj) tl)))

            ((ff? obj) ;; fixme: ff not parsed yet this way
               (cons #\# (ser sh (ff->list obj) tl)))

            (else 
               (append (string->list "#<WTF>") tl))))

      (define (maybe-quote val lst)
         (if (or (number? val) (string? val) (boolean? val) (function? val))
            lst
            (cons #\' lst)))

      ;; val → ff of (ob → n)
      (define (label-shared-objects val)
         (lets
            ((refs (object-closure #false val))
             (shares 
               (ff-fold 
                  (λ (shared ob refs) (if (eq? refs 1) shared (cons ob shared)))
                  null refs)))
            (let loop ((out #false) (shares shares) (n 1))
               (if (null? shares)
                  out
                  (loop (put out (car shares) n) (cdr shares) (+ n 1))))))

      (define (serialize val tl)
         (ser val (label-shared-objects val) tl))
))
