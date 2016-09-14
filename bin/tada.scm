#!/usr/bin/ol --run 

;; totally automatic documentation aggregator (TADA)
;; initial minimal viable version
;; usage: tada owl-library.scm -> documentation

(import (owl sexp))

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


; could find these from the parsed result, but using a silly line-based approach
; here since we're reading the comments also at the same time this way
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

(define (metadata-entry name defn-line prev-comments)
   (-> #empty
      (maybe-put 'name name)
      (maybe-put 'description (find-description prev-comments))
      (maybe-put 'args (find-args defn-line prev-comments))
      (maybe-put 'examples (examples prev-comments))))
         
(define (find-metadatas path)
   (let loop ((ls (lines (open-input-file path))) (cs null) (metas #empty))
      (lets ((line ls (uncons ls #false)))
         (if line
            (cond
               ((m/^ *;; / line)
                  (loop ls (cons (s/^ *;; // line) cs) metas))
               ((m/^ *\(define / line) ;) happy paren balancer
                  (let ((name (s/^ *\(define \(?([^ ]+).*/\1/ line))) ; ))
                     (loop ls null 
                        (let ((name (string->symbol name)))
                           (put metas name
                              (metadata-entry name line (reverse cs)))))))
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
   (and (pair? x) 
      (list? x) 
      (eq? (car x) 'export)))

(define (maybe-tada-module sexp)
   (let ((res (match sexp '(define-library ? . ?))))
      (if res
         (lets ((library (ref res 1))
                (exports
                   (fold (lambda (found x) (or found (if (export? x) (cdr x) #false)))
                      #false (ref res 2))))
              (if (and library exports)
                 (cons library exports)
                 #f)))))

(define (meta-of sym ms)
   (cond
      ((null? ms) null)
      ((equal? (symbol->string sym) (caar ms))
         (cdar ms))
      (else
         (meta-of sym (cdr ms)))))

(define (pick-metadatas exps metas)
   (map
      (lambda (sym) (cons sym (meta-of sym metas)))
      exps))

(define (output-documentation name info)
   (print " - *" (html-safe (str name)) "* "
      (get info 'args "")
      (let ((desc (getf info 'description)))
         (if desc
            (str " /" desc "/")
            "")))
   (for-each 
      (lambda (exp)
         (print "   - " exp))
      (get info 'examples null)))
   
(define (format-metadatas lib exps metas)
   (lets ((exported (fold (lambda (ff name) (put ff name name)) #empty exps)))
      (print "*" (html-safe (str lib)) "*")
      (ff-fold
         (lambda (_ name info)
            (if (getf exported name)
               (output-documentation name info)))
         #false metas)
      (print)))


(define (tada path)
   (print-to stderr (str "Reading " path))
   (lets ((sexps (force-ll (read-ll (open-input-file path)))))
      (print-to stderr (str " - " (length sexps) " exps"))
      (if (= (length sexps) 1)
         (lets
            ((info (maybe-tada-module (car sexps)))
             (metas (find-metadatas path)))
            (if (and info metas)
               (format-metadatas (car info) (cdr info) metas)))
         (print-to stderr "Warning: not tadaing " path))))

(lambda (args)
   (map tada (cdr args)))

