#!/usr/bin/ol --run 

;; totally automatic documentation aggrecator (TADA)
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

(define (find-metadatas path)
   (let loop ((ls (lines (open-input-file path))) (cs null) (metas null))
      (lets ((line ls (uncons ls #false)))
         (if line
            (cond
               ((m/^ *;; / line)
                  (loop ls (cons (s/^ *;; // line) cs) metas))
               ((m/^ *\(define / line) ;) happy paren balancer
                  (let ((name (s/^ *\(define \(?([^ ]+).*/\1/ line))) ; ))
                     (loop ls null (cons (cons name (reverse cs)) metas))))
               (else
                  (loop ls null metas)))
            metas))))

(define (html-safe s)
   (list->string
      (foldr
         (lambda (char tl)
            (cond
               ((eq? char #\<) (ilist #\& #\l #\t #\; tl))
               ((eq? char #\>) (ilist #\& #\g #\t #\; tl))
               ((eq? char #\*) (ilist #\& #\a #\m #\p #\; tl))
               (else (cons char tl))))
         null
         (string->list s))))

(define (maybe-tada-module sexp)
   (let ((res (match sexp '(define-library ? ? ? ?))))
      (if res
         (lets ((library (ref res 1))
                (exports (or (match-1 (ref res 2) '(export . ?))
                             (match-1 (ref res 3) '(export . ?))
                             (match-1 (ref res 4) '(export . ?)))))
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
   
(define (format-metadatas lib exps metas)
   (print "## " (html-safe (str lib)))
   (print "")
   (for-each 
      (lambda (row)
         (print "*" (html-safe (str (car row))) "*")
         (for-each
            (lambda (comment-line)
               (print " - " (html-safe comment-line)))
            (cdr row))
         (print))
      (pick-metadatas exps metas))
   (print))

(define (tada path)
   ;(print "Reading " path)
   (lets ((sexps (force-ll (read-ll (open-input-file path)))))
      ;(print "Read " (length sexps) " exps")
      (if (= (length sexps) 1)
         (lets
            ((info (maybe-tada-module (car sexps)))
             (metas (find-metadatas path)))
            (if (and info metas)
               (format-metadatas (car info) (cdr info) metas)))
         #false)))

(lambda (args)
   (map tada (cdr args)))

