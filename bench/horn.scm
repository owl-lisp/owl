;;;
;;; A simple interpreted Prolog-style language benchmark
;;;

; tags: values

(define ? 0)

(define (seek env val)
	(cond
		((null? env) #f)
		((eq? (caar env) val) (cdar env))
		(else (seek (cdr env) val))))

(define (unify x y env)
	(define (unify-var var val env)
		(cond
			((eq? var ?)
				env)
			((seek env var) =>
				(lambda (binding)
					(unify binding val env)))
			((and (number? val) (seek env val)) =>
				(lambda (binding)
					(unify var binding env)))
			; note, occurs check omitted
			(else
				(cons (cons var val) env))))
	(cond
		((not env) env)
		((eq? x y) env)
		((number? x)
			(unify-var x y env))
		((number? y)
			(unify-var y x env))
		((and (pair? x) (pair? y))
			(unify (cdr x) (cdr y)
				(unify (car x) (car y) env)))
		(else #f)))


(define (apply-env exp env)
	(cond
		((not env) env)
		((and (number? exp) (seek env exp)) => 
			(lambda (binding)
				(apply-env binding env)))
		((pair? exp)
			(cons
				(apply-env (car exp) env)
				(apply-env (cdr exp) env)))
		(else exp)))

(define (substitute exp env)
	(if (null? env)
		exp
		(apply-env exp env)))

(define (unifier x y)
	(substitute x (unify x y null)))

(define (freshen-symbol x)
	(+ x 1000000000000000000))

;;; reinstantiate a rule (it's logic variables) 

(define (instantiate-rule rule)
	(receive
		(let loop ((rule rule) (instantiated null))
			(cond
				((pair? rule)
					(receive (loop (car rule) instantiated)
						(lambda (head instantiated)
							(receive (loop (cdr rule) instantiated)
								(lambda (tail instantiated)
									(values (cons head tail) instantiated))))))
				((seek instantiated rule) => 
					(lambda (new)
						(values new instantiated)))
				((number? rule)
					(let ((new (freshen-symbol rule)))
						(values new (cons (cons rule new) instantiated))))
				(else 
					(values rule instantiated))))
		(lambda (rule new)
			rule)))

(define (prove db exp)

	(define (prove-terms goals subst sk fk)
		(if (null? goals)
			(sk subst fk)
			(prove-term (car goals) subst
				(lambda (new-subst new-fk)
					(prove-terms (cdr goals) new-subst sk new-fk))
				fk)))

	(define (prove-term goal subst sk fk)
		(let loop ((rules db))
			(if (null? rules)
				(fk)
				(prove-term-with-rule goal (car rules) subst sk
					(lambda ()
						(loop (cdr rules)))))))

	(define (prove-term-with-rule goal the-rule subst sk fk)
		(let* 
			((rule (instantiate-rule the-rule))
			 (head (car rule))
			 (subgoals (cdr rule))
			 (new-subst (unify head (substitute goal subst) '())))
			(if new-subst
				(prove-terms subgoals
					(append subst new-subst)
					sk fk)
				(fk))))

	(define (end-cont)
		(print "}")
		#f)

	(define (forcer fk count)
		(cond
			((not (function? fk))
				'done)
			((= count 20)
				(print "and so forth"))
			(else
				(let ((new (fk)))
					(forcer new (+ count 1))
				))))

; solve-term goal db

   (define expected 42)

	(define (solve-term goal)
		(for-each display (list "solving " goal " {"))
		(display "
")
		(prove-term goal null
			(lambda (subst fk)
            (let ((res (substitute goal subst)))
               (print " - " (substitute goal subst))
               (if (equal? res expected)
                  (print "correct: (42)")))
				(forcer fk 1))
			end-cont))

	(solve-term exp))

(define (process- db exp)
	(if (eq? (car exp) 'rule)
		(begin
			(print " + " exp)
			(append db (list (cdr exp))))
		(begin
			(prove db exp)
			db)))

(define logic-vars
	'((_ . 0) (a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6)))

(define (preprocess rule)
	(cond
		((pair? rule)
			(cons 
				(preprocess (car rule)) 
				(preprocess (cdr rule))))
		((seek logic-vars rule) => 
			(lambda (id) id))
		(else
			rule)))
				
(define (minilog exps)
   (print " => " (car (reverse (fold process- '() (map preprocess exps)))))
   (list (+ 40 2)))

;; () = 0, (x . a) = 1 + a, _ = anything

(define (test args)
	(minilog '(

		(rule (= (x . a) (x . b) c)
			(= a b c))

		(rule (= () () true))
		(rule (= (x . _) () false))
		(rule (= () (x . _) false))

		(rule (+ () a a))
		;(rule (+ a () a))
		(rule (+ (x . a) b (x . c)) 
			(+ a b c))

		(rule (- a b c) 
			(+ c b a))

		(rule (* () a ()))
		;(rule (* a () ()))
	 
		(rule (* (x . a) b c)
			(* a b d)
			(+ d b c)
         )
	
      (rule (/ a b c) 
         (* b c a))

		(rule (fakt () (x)))
	 
		(rule (fakt (x . c) b)
			(fakt c d)
			(* c d f)
         (+ f d b))
   
      (rule (sqrt a b)
         (* b b a))

      (rule (self a b) ;; a! / (a-1)! = a
         (fakt a c)
         (- a (x) d)
         (fakt d e)
         (/ c e b)
         )

      (fakt (x x x) a) ; 3! = 120
      (fakt (x x x x) a) ; 4! = 120
      (fakt (x x x x x) a) ; 5! = 120
      (fakt (x x x x x) a) ; 5! = 120
      (fakt (x x x x x) a) ; 5! = 120
      (fakt (x x x x x) a) ; 5! = 120
      (sqrt a (x x x x x))
      (sqrt a (x x x x x))
      (sqrt a (x x x x x))
      (sqrt a (x x x x x))

      (self (x x x) a)
		))
   (print "OK"))

test



