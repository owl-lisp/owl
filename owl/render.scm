
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

      ;; hack: positive id = not written yet, negative = written, so just output a reference
      (define (ser sh obj tl)
         (cond

            ((getf sh obj) =>
               (λ (id) 
                  (if (< id 0) ;; already written, just refer
                     (values sh 
                        (ilist #\# #\< (render (abs id) (ilist #\> #\# tl))))
                     (lets
                        ((sh (del sh obj))           ;; avoid doing this again
                         (sh tl (ser sh obj tl))     ;; render normally
                         (sh (put sh obj (- 0 id)))) ;; mark written
                        (values sh
                           (ilist #\# #\< 
                              (render id 
                                 (ilist #\> #\= tl))))))))

            ((null? obj)
               (values sh 
                  (ilist #\' #\( #\) tl)))

            ((number? obj)
               (values sh 
                  (render-number obj tl 10)))

            ((string? obj)
               (values sh 
                  (cons #\" 
                     (render-quoted-string obj 
                        (cons #\" tl)))))

            ((pair? obj)
               (lets ((sh tl
                  (let loop ((sh sh) (obj obj) (tl tl))
                     (cond
                        ((null? obj) 
                           (values sh 
                              (cons 41 tl)))
                        ((pair? obj)
                           (lets
                              ((sh tl (loop sh (cdr obj) tl)))
                              (ser sh (car obj) 
                                 (if (eq? (car tl) 41) ;; no space before closing paren
                                    tl
                                    (cons #\space tl)))))
                        (else
                           (lets ((sh tl (ser sh obj (cons 41 tl))))
                              (values sh 
                                 (ilist #\. #\space tl))))))))
                  (values sh (cons 40 tl))))

            ((boolean? obj)
               (values sh 
                  (append (string->list (if obj "#true" "#false")) tl)))
               
            ((symbol? obj)
               (values sh 
                  (render (symbol->string obj) tl)))

            ;; these are a subclass of vectors in owl
            ;((byte-vector? obj)
            ;   (ilist #\# #\u #\8 (render (vector->list obj) tl)))

            ((vector? obj)
               (lets ((sh tl (ser sh (vector->list obj) tl)))
                  (values sh
                     (cons #\# tl))))

            ((function? obj)
               ;; anonimas
               ;(append (string->list "#<function>") tl)
               (let ((symp (interact 'intern (tuple 'get-name obj))))
                  (values sh 
                     (if symp
                        (ilist #\# #\< (render symp (cons #\> tl)))
                        (render "#<function>" tl)))))

            ;((tuple? obj)
            ;   (ilist 40 84 117 112 108 101 32
            ;      (render (ref obj 1)
            ;         (fold
            ;            (λ (tl pos) (cons 32 (render (ref obj pos) tl)))
            ;            (cons 41 tl)
            ;            (iota (size obj) -1 1)))))

            ((rlist? obj) ;; fixme: rlist not parsed yet
               (lets ((sh tl (ser sh (rlist->list obj) tl)))
                  (values sh 
                     (ilist #\# #\r tl))))

            ((ff? obj) ;; fixme: ff not parsed yet this way
               (lets ((sh tl (ser sh (ff->list obj) tl)))
                  (values sh 
                     (ilist #\# tl))))

            (else 
               (values sh
                  (append (string->list "#<WTF>") tl)))))

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
                  (λ (shared ob refs) 
                     ;; (#<1>= #<1>=#<+>) isn't too useful, so not sharing functions
                     (if (or (eq? refs 1) (function? ob))
                        shared
                        (cons ob shared)))
                  null refs)))
            (let loop ((out #false) (shares shares) (n 1))
               (if (null? shares)
                  out
                  (loop (put out (car shares) n) (cdr shares) (+ n 1))))))

      (define (serialize val tl)
         (lets
            ((sh (label-shared-objects val))
             (sh lst (ser sh val tl)))
            (maybe-quote val lst)))
))
