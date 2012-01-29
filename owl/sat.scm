;; SAT solving is a fun simple NP-complete problem. I haven't gotten an excuse to work on or even 
;; study the more advanced techniques, but adding at least a quick and dirty simple solver to 
;; get something for testing.

(define-module lib-sat

   (export 
      satisfy     ;; cnf → assignment | #false
      )

   ;; note - leading ands and ors are just ignored. add a term → cnf translater later and switch the adt to something more sensible 

   ;; todo: choose the terms and variables greedily
   ;; todo: use a different representation (probably a bit vector with and/or/xor against preprocessed clauses?)
   ;; todo: term->cnf
   ;; todo: cnf->solver-state

   (define (fill-or lst dict true cont)
      (if (null? lst)
         (and true (cont dict))
         (let ((term (car lst)))
            (if (pair? term)
               ;; this is (not term)
               (let ((term (cadr term)) (not #true))
                  (let ((val (get dict term null)))
                     (cond
                        ((null? val)
                           ;; try both options
                           (or (fill-or (cdr lst) (put dict term #true) true cont) ;; not #true -> keep old
                              (fill-or (cdr lst) (put dict term #false) #true cont))) ;; this was true
                        (val (fill-or (cdr lst) dict true cont))
                        (else (fill-or (cdr lst) dict #true cont)))))
               ;; note, could share branch
               (let ((val (get dict term null)))
                  (cond
                     ((null? val)
                        (or (fill-or (cdr lst) (put dict term #true) #true cont)
                           (fill-or (cdr lst) (put dict term #false) true cont)))
                     (val (fill-or (cdr lst) dict #true cont))
                     (else (fill-or (cdr lst) dict true cont))))))))


   (define (solver cnf dict)
      (if (null? cnf)
         (ff->list dict)
         (fill-or (cdar cnf) dict #false ; drop the or
            (λ (dict) (solver (cdr cnf) dict)))))

   ;; naive backtracking solver for testing

   (define (sat-solve-backtrack cnf)
      (solver (cdr cnf) #false)) ; drop the and

   (define satisfy sat-solve-backtrack))
     
