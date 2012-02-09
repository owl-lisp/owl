
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

      ; laziness changes:
      ;  - use explicit CPS to 'return' 
      ;  - emit definition on first encounter

      (define (ser sh obj k)
         (cond

            ((getf sh obj) =>
               (λ (id) 
                  (if (< id 0) ;; already written, just refer
                     (ilist #\# (render (abs id) (cons #\# (k sh))))
                     (ilist #\# 
                        (render id
                           (ilist #\# #\=
                              (ser (del sh obj) obj
                                 (λ (sh)
                                    (k (put sh obj (- 0 id)))))))))))

            ((null? obj)
               (ilist #\' #\( #\) (k sh)))

            ((number? obj)
               (render-number obj (k sh) 10))

            ((string? obj)
               (cons #\" 
                  (render-quoted-string obj 
                     (cons #\" (k sh)))))

            ((pair? obj)
               (cons 40
                  (let loop ((sh sh) (obj obj))
                     (cond
                        ((null? obj)
                           ;; run of the mill list end
                           (cons 41 (k sh)))
                        ((getf sh obj) =>
                           (λ (id)
                              (ilist #\. #\space #\#
                                 (render (abs id)
                                    (cons #\#
                                       (if (< id 0)
                                          (cons 41 (k sh))
                                          (ser (del sh obj) obj 
                                             (λ (sh)
                                                (cons 41
                                                   (k 
                                                      (put sh obj 
                                                         (- 0 id))))))))))))
                        ((pair? obj) 
                           ;; render car, then cdr
                           (ser sh (car obj)
                              (λ (sh)
                                 (if (null? (cdr obj))
                                    (loop sh (cdr obj))
                                    (cons #\space (loop sh (cdr obj)))))))
                        (else 
                           ;; improper list
                           (ilist #\. #\space 
                              (ser sh obj
                                 (λ (sh) (cons 41 (k sh))))))))))

            ((boolean? obj)
               (append (string->list (if obj "#true" "#false")) (k sh)))
               
            ((symbol? obj)
               (render (symbol->string obj) (k sh)))

            ((vector? obj)
               (cons #\#
                  (ser sh (vector->list obj) k)))

            ((function? obj)
               ;; anonimas
               ;(append (string->list "#<function>") tl)
               (let ((symp (interact 'intern (tuple 'get-name obj))))
                  (if symp
                     (ilist #\# #\< (render symp (cons #\> (k sh))))
                     (render "#<function>" (k sh)))))

            ;; not sure yet what the syntax for these should be
            ;((tuple? obj)
            ;   (ilist 40 84 117 112 108 101 32
            ;      (render (ref obj 1)
            ;         (fold
            ;            (λ (tl pos) (cons 32 (render (ref obj pos) tl)))
            ;            (cons 41 tl)
            ;            (iota (size obj) -1 1)))))

            ((rlist? obj) ;; fixme: rlist not parsed yet
               (ilist #\# #\r (ser sh (rlist->list obj) k)))

            ((ff? obj) ;; fixme: ff not parsed yet this way
               (cons #\# (ser sh (ff->list obj) k)))

            (else 
               (append (string->list "#<WTF>") (k sh)))))

      (define (self-quoting? val)
         (or (number? val) (string? val) (boolean? val) (function? val)))

      ;; could drop val earlier to possibly gc it while rendering 
      (define (maybe-quote val lst)
         (if (self-quoting? val) 
            lst
            (cons #\' lst)))

      ;; val → ff of (ob → node-id)
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
         (force-ll
            (maybe-quote val
               (ser (label-shared-objects val) val 
                  (λ (sh) tl)))))
))
