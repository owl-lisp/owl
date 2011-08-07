;; normal sexp
(print (eval '(+ 1 2) *toplevel*))

;; linked code
(print (eval (list + 1 2) *toplevel*))

;; nested
(print (eval (list eval '(+ 1 2) *toplevel*) *toplevel*))

;; silly
(let ((add (fasl-encode +)))
	(eval (list 'print (list (list 'fasl-decode (list 'quote add) 42) 1 2)) *toplevel*))
