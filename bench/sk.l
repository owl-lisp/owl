
;; normal order evaluation in combinatory logic (sans sharing)

(define s (lambda (a) (lambda (b) (lambda (c) (cons (cons a c) (cons b c))))))
(define k (lambda (a) (lambda (b) a)))
(define i ((s k) k))

(define (lc-eval term)
   (if (pair? term)
      (lc-eval 
         ((lc-eval (car term))
            (cdr term)))
      term))

(define (app rator rand) 
   ((rator) rand))

;; a simple ast for lambda-terms

(define (abstract var term)
   (cond
      ((pair? term) 
         (if (eq? (car term) 'lambda)
            (let ((new-var (cadr term)) (body (cddr term)))
               (abstract var (abstract new-var body)))
            (cons (cons s (abstract var (car term))) (abstract var (cdr term)))))
      ((eq? var term) i)
      (else (cons k term))))

(define (lc->cl node)
   (if (pair? node)
      (if (eq? (car node) 'lambda)
         (let ((var (cadr node)) (body (cddr node)))
            (abstract (cadr node) (cddr node)))
         (cons (lc->cl (car node)) (lc->cl (cdr node))))
      node))

;; normal scheme sexps

; can't remember if folds are in r5rs compat, so using ugly manual recursion here

(define (sexp-lambda formals body)
   (if (null? formals)
      body
      (sexp-lambda (cdr formals) (cons 'lambda (cons (car formals) body)))))

(define (sexp-apply hd tl)
   (if (null? tl)
      hd
      (sexp-apply (cons hd (car tl)) (cdr tl))))

(define (sexp->lc sexp)
   (cond
      ((list? sexp)
         (cond
            ((eq? (car sexp) 'lambda) ;; (lambda (formal ...) body)
               (sexp-lambda (reverse (cadr sexp)) (sexp->lc (caddr sexp))))
            ((eq? (car sexp) 'let) ;; (let ((formal value) ...) body)
               (let ((defns (cadr sexp)) (body (caddr sexp)))
                  (sexp->lc
                     (cons (list 'lambda (map car defns) body)
                        (map cadr defns)))))
            (else
               (let ((sexp (map sexp->lc sexp)))
                  (sexp-apply (car sexp) (cdr sexp))))))
      (else sexp)))

;; run a small recursive function (which takes a while in SK lacking memoization)

(define lc-prog
   '(let ;; basic combinators 
      ((i (lambda (x) x))
       (k (lambda (x y) x))
       (s (lambda (x y z) (x z (y z))))
       (t (lambda (a b) a))
       (f (lambda (a b) b))
       (y (lambda (a) ((lambda (a) (a a)) (lambda (b) (a (b b))))))
       (bi (lambda (a b c) (c a b)))
       (if (lambda (a b c) (a b c))))
      (let 
         ((tail (y (lambda (self x) (x t (x f) (self (x f)))))))
         (tail (bi f (bi f (bi f (bi t 42))))))))

(define (test args)
   (print (list (lc-eval (lc->cl (sexp->lc lc-prog))))))

test
