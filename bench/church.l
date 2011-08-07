;;;
;;; benchmarking church numerals
;;;

; tags: macro

(define-syntax lam
  (syntax-rules ()
    ((lam a b) (lambda (a) b))))

(define-syntax def
  (syntax-rules ()
   ((def var value app)
      ((lam var app) value))
    ((def var value . rest)
      ((lam var rest) value))))

(define unchurch
	(lam c
		((c (lam x (+ x 1))) 0)))

(define (compute)
	(unchurch 
		(def w
		 (lam a (a a))
	  
	  def y
		 (lam f
			(w (lam x (f (lam a 
			  ((x x) a))))))
	  
	  def i
		 (lam a a)
	  
	  def c0 
		 (lam f (lam x x))
	  
	  def c1 
		 (lam f (lam x (f x)))
	  
	  def c2 
		 (lam f (lam x (f (f x))))
	  
	  def c+
		 (lam a (lam b 
			(lam f (lam x 
			  ((a f) ((b f) x))))))
	  
	  def c*
		 (lam a (lam b
			(a ((lam c ((c+ c) b)) c0))))
	  
	  def true
		 (lam a (lam b a))
	  
	  def false
		 (lam a (lam b b))
	  
	  def c0?
		 (lam a ((a (lam b false)) true))
	  
	  def c--
		 (lam a 
			(lam f (lam x 
			  (((a (lam b (lam c (c (b f)))))
				 (lam a x))
				 i))))
	  
	  def c++
		 (lam a
			(lam f (lam x ((a f) (f x)))))
	  
	  def square 
		 (c* c2)
	 
	  def twice c2

	  ((twice (twice square)) c2))))

(define (test args)
	(display "crunching: ")
	(let loop ((n 50) (last 0))
		(if (= n 0)
         (list (- last 65494))
			(begin
				(display "* ")
				(loop (- n 1) (compute))))))

test

