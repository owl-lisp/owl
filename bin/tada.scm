#!/usr/bin/ol --run 

;; totally automatic documentation aggregator (TADA)
;; initial minimal viable version
;; usage: tada owl-library.scm -> documentation

(import (owl sexp))

;; simple expression pattern match 

(define (match exp pat)
   (define (walk exp pat found)
      (cond
         ((not found)
            #false)
         ((eq? pat '?)
            (cons exp found))
         ((pair? pat)
            (if (pair? exp)
               (walk (car exp) (car pat)
                  (walk (cdr exp) (cdr pat) found))
               #false))
         ((eq? exp pat)
            found)
         (else
            #false)))
   (let ((res (walk exp pat null)))
      (if res
         (list->tuple res)
         #false)))

(define (match-1 exp pat)
   (let ((res (match exp pat)))
      (if res (ref res 1) #false)))

(define (maybe-definition-args defn)
   (if (m/^ *\(define +\(/ defn)  ; )) 
      (s/\).*// ; (
         (s/.*\(define +\([^ ]+ *// defn)) ; ))
      #false))

(define (maybe fn val) 
   (if val (fn val) val))

(define (maybe-put ff k v)
   (if v (put ff k v) ff))

(define (find-args defn comms)
   (or (maybe s/,.*// (first m/.* -> / comms #false))
       (maybe-definition-args defn)))

(define (find-description comms)
   (maybe s/.*, *// (first m/.* -> .*,/ comms #false)))   
   
(define (examples comms)
   (map s/   //
      (keep m/^   .* = / comms)))

(define (metadata-entry info name defn-line prev-comments)
   (-> info
      (maybe-put 'name name)
      (maybe-put 'description (find-description prev-comments))
      (maybe-put 'args (find-args defn-line prev-comments))
      (maybe-put 'examples (examples prev-comments))))
         
(define (add-comment-metadatas meta path)
   (let loop ((ls (lines (open-input-file path))) (cs null) (metas meta))
      (lets ((line ls (uncons ls #false)))
         (if line
            (cond
               ((m/^ *;; / line)
                  (loop ls (cons (s/^ *;; // line) cs) metas))
               ((m/^ *\(define / line) ;) happy paren balancer
                  (let ((name (s/^ *\(define \(?([^ ]+).*/\1/ line))) ; ))
                     (loop ls null 
                        (lets
                            ((name (string->symbol name))
                             (info (getf metas name)))
                            (if info
                               (put metas name 
                                  (metadata-entry info name line (reverse cs)))
                               metas)))))
               (else
                  (loop ls null metas)))
            metas))))

(define safe-chars 
   (fold 
      (lambda (ff x) (put ff x x)) 
      #empty 
      (list #\space #\- #\_ #\: #\( #\) #\?)))

(define (html-safe s)
   (list->string
      (foldr
         (lambda (char tl)
            (cond
               ((<= #\a char #\z) (cons char tl))
               ((<= #\0 char #\9) (cons char tl))
               ((<= #\A char #\Z) (cons char tl))
               ((getf safe-chars char) (cons char tl))
               (else (render (str "&#" char ";") tl))))
         null
         (string->list s))))

(define (export? x) 
   (and (pair? x) (list? x) (eq? (car x) 'export)))

(define (begin? x)
   (and (pair? x) (list? x) (eq? (car x) 'begin)))

(define (add-defn-metadata meta exp)
   (cond
      ((match exp '(define (? . ?) . ?)) =>
         (lambda (ms)
            (let ((val (getf meta (ref ms 1))))
               (if val ;; exported, collecting data for it
                  (put meta (ref ms 1) (put val 'args (ref ms 2)))
                  meta))))
      (else meta)))

(define (pick-exports exps body)
   (lets 
      ((meta 
         (fold 
            (lambda (ff exp) 
               (put ff exp (put #empty 'name exp)))
             #empty exps)))
      (fold add-defn-metadata meta (or body null))))

;; sexp -> #f | (libname . initial-metadata-ff)
(define (maybe-tada-module sexp)
   (let ((res (match sexp '(define-library ? . ?))))
      (if res
         (lets ((library (ref res 1))
                (exports
                   (fold 
                      (lambda (found x) 
                         (or found (if (export? x) (cdr x) #false)))
                      #false (ref res 2)))
                (body 
                   (fold 
                      (lambda (found x) (or found (if (begin? x) (cdr x) #false)))
                      #false (ref res 2))))
            (if (and library exports)
               (values library (pick-exports exports body))
               (values #f #f)))
        (values #f #f))))

(define (meta-of sym ms)
   (cond
      ((null? ms) null)
      ((equal? (symbol->string sym) (caar ms))
         (cdar ms))
      (else
         (meta-of sym (cdr ms)))))
   
(define (tada-add out path)
   (print-to stderr (str "Reading " path))
   (lets ((sexps (force-ll (read-ll (open-input-file path)))))
      (print-to stderr (str " - " (length sexps) " exps"))
      (if (= (length sexps) 1)
         (lets
            ((libname info (maybe-tada-module (car sexps)))
             (metas (if info (add-comment-metadatas info path) #f)))
            (if (and info metas)
               (cons (cons libname metas) out)
               out))
         (begin
            (print-to stderr "Warning: not tadaing " path)
            out))))

(lambda (args)
   (for-each
      (lambda (node)
         (print "*" (html-safe (str (car node ))) "*")
         (ff-fold
            (lambda (_ name info)
               (lets 
                  ((args (getf info 'args)) 
                   (examples (get info 'examples null))
                   (desc (getf info 'description)))
                  (print 
                     (html-safe 
                        (str " - "
                           (if args
                              (if (m/ -> / args)
                                 (str "(" name " " (s/ *->/) ->/ args))
                                 (str "(" name " " args ")"))
                              name)
                           (if desc (str ", _" desc "_") ""))))
                  (for-each 
                     (lambda (exp) (print "    - " (html-safe exp)))
                      examples)))
            #f (cdr node))
         (print))
     (reverse (fold tada-add null (cdr args))))
   0)

