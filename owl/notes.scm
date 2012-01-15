;;;
;;; a small tool for processing notes from code 
;;;

;; todo: can now use line streaming (add to lib-io) and regexp matching + splitting
;; todo: align notes

; $ owl --notes thingy.scm
; thingy.scm:123 - fixme - add carry to low bits 
; thingy.scm:129 - fixme - does not work for n > 255
; thingy.scm:243 - todo - would be nice to be below O(n^n!)

;; not to be exported to toplevel, just for $ owl --notes [source] ...
(define-module lib-notes

	(import lib-lazy)
	(export 
		collect-notes 	;; paths -> (note ...)
		show-notes		;; (note ..) -> side effects
		show-notes-of  ;; paths -> side effects
	)

	; use a simple automata to read through data in one pass (ugly but reasonably fast, modulo owl)
	;  collect -> reading a comment, buffer holds (char_n ... char_n0 <label>)
	;  label -> reading part after \n;; and waiting for a :, buffer holds reverse bytes
	;  skip -> waiting for newline
	;  newline -> waiting for ;
	;  nl-colon -> waiting for another colon

	(define max-note-length 80)

	;; fixme: notes can fail for bad utf8
	(define (flush-note taken lb path line)
		(lets
			((label bytes lb)
			 (text (bytes->string (take bytes max-note-length))))
			;(print* (list " ==> made note, label " label ", text " text " <=="))
			(cons (tuple path line label text) taken)))

	;; todo: drop note if label does not look like one
	;; note = #(path line label text)
	(define (get-notes-from ll path)
		;(show "Getting notes from " path)
		; state = #(lineno state buff taken)
		(reverse
			(ref
				(lfold
					(λ (st byte) ; note, not utf8 decoded
						(lets ((line state buff taken st))
							(cond
								;; jump to fresh line state and maybe save note
								((eq? byte 10) ; newline
									(if (eq? state 'collect)
										(tuple (+ line 1) 'newline null 
											(flush-note taken (reverse buff) path line))
										(tuple (+ line 1) 'newline null taken)))
								((eq? state 'skip) st) ; move along
								((eq? state 'collect) ; collecting a labeled note
									(tuple line state (cons byte buff) taken))
								((and (eq? state 'newline) (eq? byte 59)) ; \n;
									(tuple line 'nl-colon null taken))
								((and (eq? state 'nl-colon) (eq? byte 59)) ; \n;;
									(tuple line 'label null taken))
								((and (eq? state 'newline) (has? '(9 32) byte)) ;; allow indented notes
									st)
								((and (eq? state 'label) (eq? byte 59)) ; skip if too many semicolons
									(tuple line 'skip null taken))
								((eq? state 'label)
									(if (eq? byte 58)
										(let ((label (bytes->string (reverse buff)))) ; <- will fail if bad utf8
											(tuple line 'collect (list label) taken))
										(tuple line 'label (cons byte buff) taken)))
								(else 
									(tuple line 'skip null taken)))))
					(tuple 1 'newline null null) ll)
				4)))

	; paths -> (#(path line type text)  ...)
	(define (collect-notes paths)
		(lets
			((pfds (map (λ (path) (cons path (open-input-file path))) paths))
			 (pfds (keep (λ (pair) (if (cdr pair) True (begin (show "not readable: " (car pair)) False))) pfds)))
			(foldr append null
				(map 
					(λ (pfd) 
						(lets ((path fd pfd)) 
							(get-notes-from (port->byte-stream fd) path)))
					pfds))))

	(define (label-lex-less? sa sb)
		(let loop ((a (str-iter (ref sa 3))) (b (str-iter (ref sb 3))))
			(cond
				((null? a) 
					;; b may be uncomputed so must check to avoid reversing the equal part
					(cond
						((null? b) False)
						((pair? b) True)
						(else (loop a (b)))))
				((null? b) False)
				((not (pair? a)) (loop (a) b))
				((not (pair? b)) (loop a (b)))
				((lesser? (car a) (car b)) True)
				((eq? (car a) (car b)) (loop (cdr a) (cdr b)))
				(else False))))

	(define (show-notes notes)
		(fold
			(λ (last note)
				(lets ((path line type text note))
					(if (not (equal? type last))
						(print type))
					(print* (list "   " path ":" line " -" text))
					type))
			42 (sort label-lex-less? notes)))

	(define show-notes-of 
		(o show-notes collect-notes))

	(export collect-notes)
)

;(import lib-notes show-notes-of)
;(show-notes-of '("owl.scm"))
