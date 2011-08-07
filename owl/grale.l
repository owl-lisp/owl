;;;
;;; Support for (very basic) 8-bit graphics
;;;

; pre pre alpha

; colors are RRRGGGBB

; $ make bin/grale
; $ owl
; > ,load "lib/grale.scm"

; written mainly because I want to make and play some simple games

(define-module lib-grale
	(export 
		;; base stuff
		start-grale			; 
		grale-init			; width, height -> bool
		grale-put 			; x, y, col (no result)
		grale-update		; x, y, width, height (no result)
		grale-wait-event	; -> event
		grale-check-event	; -> event | False
		grale-fill-rect	; x, y, w, h, col 
		grale-paint			; x y (data)
		grale-puts			; x y col (n move ...)
		grale-timer			; ms -> bool
		grale-wait-timer	; -> n, being the number of occurred signals since last read
		grale-show-cursor	; thunk
		grale-hide-cursor ; thunk

		;; support stuff
		font-8px	;; partial, not monospace, not there yet
		owl-logo		;; a test for puts
		build-sprite	;; (width pixel ...) -> False | (len move ...)
		grale-put-text		; font, x, y, col, text
		grale-text-width	; font, string -> n
		cursor
	)

	;(define grale-server-path "bin/grale")
	(define grale-server-path "grale") ; assume in $PATH, usually for me at $HOME/bin

	;; events
	;;  - 1 btn x y, mouse button down

	(define (natural in) ; -> in' ok? val
		(let loop ((in in) (n 0))
			(cond
				((null? in)
					(values in False False))
				((pair? in)
					(lets
						((a (car in))
						 (ap (fxband a 127)))
						(if (eq? a ap)
							(if (eq? n 0)
								(values (cdr in) True a)
								(values (cdr in) True (+ (<< n 7) ap)))
							(loop (cdr in) (+ (<< n 7) ap)))))
				(else (loop (in) n)))))

	(define no-event False)

	;; todo: switch to a ff of parsing combinators
	(define (event-response in)
		(lets
			((in ok type (natural in)))
			(if ok
				(cond
					((eq? type 1) ; mouse click - btn x y
						(lets
							((in ok btn (natural in))
							 (in ok x (natural in))
							 (in ok y (natural in)))
							(if ok
								(values in True (tuple 'click btn x y))
								(values in False False))))
					((eq? type 2) ; mouse motion - x y
						(lets
							((in ok x (natural in))
							 (in ok y (natural in)))
							(if ok
								(values in True (tuple 'mouse-move x y))
								(values in False False))))
					((eq? type 3) ; key down - unicode-codepoint
						(lets ((in ok cp (natural in)))
							(values in ok (tuple 'key cp))))
					((eq? type 127) ; unknown <id>
						(lets
							((in ok type (natural in)))
							(values in ok (tuple 'unknown type))))
					((eq? type 0)
						(values in ok no-event))
					(else
						(error "Connection out of sync or unknown (to owl) event type from grale: " type)))
				(values in False False))))

	(define (init-response in)
		(cond
			((null? in) (values in False False))
			((pair? in) 
				(values (cdr in) True 
					(if (eq? (car in) 0)
						(tuple 'connected)
						(tuple 'failed))))
			(else (init-response (in)))))

	(define response-parsers
		(list->ff
			(list
				(cons 1 init-response)
				(cons 4 event-response)
				(cons 11 natural) 			; bool actually
				(cons 12 natural)				; number of occurred alarms (>= 1)
			)))

	(define (grale-thread out in)
		(lets
			((env (wait-mail))
			 (msg (ref env 2)))
			(cond
				((pair? msg)
					;(show " -> to grale " msg)
					(mail out msg)
					(let ((res-parse (get response-parsers (car msg) False)))
						(if res-parse
							(begin
								(flush-port out) ; make sure the request was actually sent
								(lets ((in ok? response (res-parse in)))
									(if ok?
										(begin
											(mail (ref env 1) response)
											(grale-thread out in))
										(show "an erroc occurred when talking to grale. response belonged to " env))))
							(grale-thread out in))))
				((eq? msg 'flush)
					;(print " -> flushing grale buffer")
					(flush-port out)
					(grale-thread out in))
				(else
					;(show "funny grale mail " msg)
					(grale-thread out in)))))

	;; encoding (sending to grale)

   (define low7 #b01111111)

   (define (biggish->bytes num done)
      (if (< num 127)
         (cons (+ num 128) done)
         (biggish->bytes (>> num 7)
            (cons (+ 128 (band num low7)) done))))

   (define (number->bytes num tail)
      (if (lesser? num 128)
         (cons num tail)
         (biggish->bytes (>> num 7)
            (cons (band num #b01111111) tail))))

	(define (grale-init w h)
		(interact 'grale
			(cons 1 (number->bytes w (number->bytes h null)))))

	(define (grale-put x y c)
		(mail 'grale
			(cons 2 (number->bytes x (number->bytes y (list (fxband c 255)))))))

	(define (grale-update x y w h)
		(mail 'grale
			(cons 3 (foldr number->bytes null (list x y w h 10)))))

	(define (grale-wait-event) 
		(interact 'grale '(4 1)))
	
	(define (grale-paint x y lst)
		(mail 'grale
			(cons 6 (number->bytes x (number->bytes y lst)))))
		
	(define (grale-puts x y col lst)
		(mail 'grale
			(cons 7 (number->bytes x (number->bytes y (cons col lst))))))
		
	(define (grale-check-event) 
		(interact 'grale '(4 0)))

	(define (grale-fill-rect x y w h col)
		(mail 'grale
			(ilist 5 col (foldr number->bytes null (list x y w h)))))

	(define (grale-timer ms)
		(interact 'grale
			(cons 11 (number->bytes ms '(10))))) ; 10 no-op to flush and make sure the request is sent right away

	(define (grale-wait-timer)
		(interact 'grale '(12)))

	(define (grale-hide-cursor)
		(mail 'grale '(13 0)))

	(define (grale-show-cursor)
		(mail 'grale '(13 1)))

	;; fixme: report grale error if the external grale server cannot be started
	;; todo: try to start bin/grale and then one from $PATH? or give the command always as an argument?
	(define (start-grale)
		(fork-server 'grale
			(λ ()
				(let ((node (fork-process (list grale-server-path))))
					(if node
						(grale-thread (ref node 2) (port->byte-stream (ref node 3)))
						(begin
							(show "cuold not start grale server with command " grale-server-path)
							False))))))


	; dx dy -> move-byte | False
	(define (move dx dy)
		(if (and (<= dx 7) (>= dx -7) (<= dy 7) (>= dy -7))
			(+ (<< (+ dx 8) 4) (+ dy 8))
			False))

	(define (maybe-cons hd maybe-tl)
		(if maybe-tl (cons hd maybe-tl) False))
	
	(define (jump a b w)
		(lets
			((ay ax (quotrem a w))
			 (by bx (quotrem b w)))
			(move (- ax bx) (- ay by))))

	; -> moves | False
	(define (build-moves pos lst width)
		(if (null? lst)
			null 
			(fold
				(λ (win to)
					(or win 
						(let ((this (jump pos to width)))
							(if this
								(maybe-cons this 
									(build-moves to (remove (λ (x) (eq? x to)) lst) width))
								False))))
				False lst)))

	; list of tokens -> (len move ...), intended for drawing single-coloured images (letters etc) using grale-puts
	; lst = (width thing ...) where
	;	x = draw this pixel
	;  - = nothing here
	;  + = start drawing here (where the puts x/y coordinates point to) but don't paint anything there
	;  o = start drawing here, and also colour it

	; -> (len move ...) | False
	(define (build-sprite lst)
		(show " - building " lst)
		(lets
			((width (car lst))
			 (lst (cdr lst))
			 (lst (zip cons (iota 0 1 (length lst)) lst)) ; index positions
			 (lst (remove (λ (p) (eq? (cdr p) '-)) lst)) ; drop blanks
			 (start ; find starting place
			 	(fold 
					(λ (found this) (if (has? '(+ o) (cdr this)) (car this) found))
					False lst))
			 (lst  ; drop start information and get just drawables
			 	(fold
					(λ (out p) 
						(cond
							((eq? (cdr p) '+) out) ; drop from paintable pixels
							((eq? (cdr p) 'o) (cons (car p) out)) ; paintable start pixel
							(else (cons (car p) out))))
					null lst)))
			(cond
				((not start)
					(show "no start symbol found in sprite data " lst)
					False)
				((build-moves start lst width) =>
					(λ (moves) ; \o/
						(show " - sprite encoded as " moves)
						(number->bytes (length moves) moves)))
				(else
					(show "this sprite cannot be drawn easily (drawing each pixel just once): " lst)
					False))))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(define lamb
		(build-sprite
			'(5 x x - - - 
				 - - x - - 
				 - - x - - 
				 - - x - - 
				 - x - x - 
				 o - - - x)))

	(define cursor
		(build-sprite
		 '(3 x - x
			  - x -
			  - x -
			  - x -
			  - x -
			  - x -
			  - x -
			  o - x)))

	;; variable width fonts are kept as (width . spritedata) 
	;; to be translated later to a ff of codepoint -> (width . spritedata)

	; keep the size at car 
	(define (build-char data) (cons (car data) (build-sprite data)))

	(define font8-chars-numeric	
		(map build-char 
			'((5 - x x - - 
				  x - - x -
				  x - x x -
				  x - - x -
				  x x - x -
				  x - - x -
				  - x x - -
				  + - - - -)

			  (3 - x - 
				  x x - 
				  - x - 
				  - x - 
				  - x - 
				  - x - 
				  - x - 
				  + - - )

			  (4 - x - - 
				  x - x -
				  - - x -
				  - - x -
				  - x - -
				  x - - -
				  x x x -
				  + - - -)

			  (4 x x - - 
				  - - x -
				  - - x -
				  - x - -
				  - - x -
				  - - x -
				  x x - -
				  + - - -)
			  
			  (4 x - x - 
				  x - x -
				  x - x -
				  x x x -
				  - - x -
				  - - x -
				  - - x -
				  + - - -)

			  (4 x x x - 
				  x - - -
				  x - - -
				  x x - -
				  - - x -
				  - - x -
				  x x - -
				  + - - -)

			  (5 - x x - - 
				  x - - - -
				  x - - - -
				  x x x - -
				  x - - x -
				  x - - x -
				  - x x - -
				  + - - - -)
			  
			  (4 x x x - 
				  - - x -
				  - - x -
				  - x - -
				  x - - -
				  x - - -
				  x - - -
				  + - - -)

			  (4 x x x - 
				  x - x -
				  x - x -
				  - x - -
				  x - x -
				  x - x -
				  x x x -
				  + - - -)

			  (5 - x x - - 
				  x - - x -
				  x - - x -
				  - x x x - 
				  - - - x -
				  x - - x -
				  - x x - -
				  + - - - -)
				)))

	(define font8-chars-alpha-uppercase
		(map build-char
			'((5 - x x - -
				  x - - x - 
				  x - - x -
				  x - - x -
				  x x x x -
				  x - - x -
				  x - - x -
				  + - - - -)

			  (5 x x x - -
				  x - - x - 
				  x - - x -
				  x x x - -
				  x - - x -
				  x - - x -
				  x x x - -
				  + - - - -)

			  (5 - x x - -
				  x - - x - 
				  x - - - -
				  x - - - -
				  x - - - -
				  x - - x -
				  - x x - -
				  + - - - -)

			  (5 x x x - -
				  x - - x - 
				  x - - x -
				  x - - x -
				  x - - x -
				  x - - x -
				  x x x - -
				  + - - - -)

			  (5 x x x x -
				  x - - - - 
				  x - - - -
				  x x x x -
				  x - - - -
				  x - - - -
				  x x x x -
				  + - - - -)

			  (4 x x x x
				  x - - - 
				  x - - -
				  x x x -
				  x - - -
				  x - - -
				  x - - -
				  + - - -)

			  (5 - x x - -
				  x - - x - 
				  x - - - -
				  x - x x -
				  x - - x -
				  x - - x -
				  - x x - -
				  + - - - -)

			  (5 x - - x -
				  x - - x - 
				  x - - x -
				  x x x x -
				  x - - x -
				  x - - x -
				  x - - x -
				  + - - - -)

			  (3 - x -
				  - x - 
				  - x -
				  - x -
				  - x -
				  - x -
				  - x -
				  + - -)

			  (5 - - - x -
				  - - - x - 
				  - - - x -
				  - - - x -
				  - - - x -
				  x - - x -
				  - x x - -
				  + - - - -)

			  (5 x - - x -
				  x - x - - 
				  x x - - -
				  x - - - -
				  x x - - -
				  x - x - -
				  x - - x -
				  + - - - -)

			  (5 x - - - -
				  x - - - - 
				  x - - - -
				  x - - - -
				  x - - - -
				  x - - - -
				  x x x x -
				  + - - - -)

			  (6 x - - - x -
				  x x - x x - 
				  x - x - x -
				  x - - - x -
				  x - - - x -
				  x - - - x -
				  x - - - x -
				  + - - - - -)

			  (5 x - - x -
				  x x - x -
				  x x - x -
				  x - x x -
				  x - x x -
				  x - - x -
				  x - - x -
				  + - - - -)

			  (5 - x x - -
				  x - - x - 
				  x - - x -
				  x - - x -
				  x - - x -
				  x - - x -
				  - x x - -
				  + - - - -)

			  (5 x x x - -
				  x - - x - 
				  x - - x -
				  x x x - -
				  x - - - -
				  x - - - -
				  x - - - -
				  + - - - -)

			  (5 - x x - -
				  x - - x - 
				  x - - x -
				  x - - x -
				  x - - x -
				  x - - x -
				  - x x - -
				  + - - x -)

			  (5 x x x - -
				  x - - x - 
				  x - - x -
				  x x x - -
				  x - - x -
				  x - - x -
				  x - - x -
				  + - - - -)

			  (5 - x x - -
				  x - - x -
				  x - - - -
				  - x x - -
				  - - - x -
				  x - - x -
				  - x x - -
				  + - - - -)

			  (5 x x x x x
				  - - x - - 
				  - - x - -
				  - - x - -
				  - - x - -
				  - - x - -
				  - - x - -
				  + - - - -)

			  (5 x - - x -
				  x - - x - 
				  x - - x -
				  x - - x -
				  x - - x -
				  x - - x -
				  - x x - -
				  + - - - -)

			  (5 x - - - x
				  x - - - x 
				  x - - - x
				  x - - - x
				  x - - - x
				  - x - x -
				  - - x - -
				  + - - - -)

			  (6 x - - - x -
				  x - - - x - 
				  x - - - x -
				  x - - - x -
				  x - x - x -
				  x - x - x -
				  - x - x - -
				  + - - - - -)

			  (5 x - - - x
				  x - - - x 
				  - x - x -
				  - - x - -
				  - x - x -
				  x - - - x
				  x - - - x
				  + - - - -)

			  (5 x - - - x
				  x - - - x 
				  - x - x -
				  - - x - -
				  - - x - -
				  - - x - -
				  - - x - -
				  + - - - -)

			  (5 x x x x -
				  x - - - - 
				  - x - - -
				  - - x - -
				  - - - x -
				  - - - x -
				  x x x x -
				  + - - - -)

	)))

	(define font8-chars-alpha-lowercase
		(map build-char
			'((5 - - - - -
				  - - - - - 
				  - x x - -
				  - - - x -
				  - x x x -
				  x - - x -
				  - x x x -
				  + - - - -)

			  (5 x - - - -
				  x - - - - 
				  x x x - - 
				  x - - x -
				  x - - x - 
				  x - - x - 
				  - x x - - 
				  + - - - -)

			  (4 - - - - 
				  - - - - 
				  - x x - 
				  x - - -
				  x - - - 
				  x - - - 
				  - x x - 
				  + - - -)

			  (5 - - - x -
				  - - - x - 
				  - x x x - 
				  x - - x -
				  x - - x - 
				  x - - x - 
				  - x x x - 
				  + - - - -)

			  (5 - - - - -
				  - - - - -
				  - x x - - 
				  x - - x -
				  x x x - - 
				  x - - - - 
				  - x x x - 
				  + - - - -)

			  (4 - - x x
				  - x - -
				  x x x -
				  - x - -
				  - x - -
				  - x - -
				  - x - -
				  + - - -)

			  (5 - - - - -
				  - - - - -
				  - x x - -
				  x - - x -
				  x - - x -
				  - x x x -
				  - - - x -
				  + x x - -)

			  (5 x - - - -
				  x - - - -
				  x x x - -
				  x - - x -
				  x - - x -
				  x - - x -
				  x - - x - 
				  + - - - -)

			  (2 x -
				  - -
				  x -
				  x -
				  x -
				  x -
				  x - 
				  + -)

			  (3 - x -
				  - - -
				  - x -
				  - x -
				  - x -
				  - x -
				  - x - 
				  o - - )

			  (5 x - - - -
				  x - - - -
				  x - - x -
				  x - x - -
				  x x - - -
				  x - x - -
				  x - - x - 
				  + - - - -)
			
			  (3 x x -
				  - x -
				  - x -
				  - x -
				  - x -
				  - x -
				  - x - 
				  + - -)

			  (6 - - - - - -
				  - - - - - -
				  x x - x - -
				  x - x - x -
				  x - x - x -
				  x - x - x -
				  x - x - x -
				  + - - - - -)

			  (5 - - - - -
				  - - - - -
				  x x x - -
				  x - - x -
				  x - - x -
				  x - - x -
				  x - - x -
				  + - - - -)

			  (5 - - - - -  
				  - - - - -  
				  - x x - -
				  x - - x -
				  x - - x - 
				  x - - x - 
				  - x x - - 
				  + - - - -)

			  (5 - - - - -
				  - - - - -
				  - x x - -
				  x - - x -
				  x - - x -
				  x - - x -
				  x x x - -
				  o - - - -)

			  (5 - - - - -
				  - - - - -
				  - x x - -
				  x - - x -
				  x - - x -
				  x - - x -
				  - x x x -
				  + - - x -)

			  (4 - - - -  
				  - - - -  
				  - x x -
				  x - - -
				  x - - - 
				  x - - - 
				  x - - - 
				  + - - -)

			  (5 - - - - -
				  - - - - -
				  - x x x -
				  x - - - -
				  - x x - -
				  - - - x -
				  x x x - -
				  + - - - -)

			  (4 - x - -
				  x x x -
				  - x - -
				  - x - -
				  - x - -
				  - x - -
				  - x - - 
				  + - - -)

			  (5 - - - - -  
				  - - - - -  
				  x - - x -
				  x - - x -
				  x - - x - 
				  x - - x - 
				  - x x x - 
				  + - - - -)

			  (4 - - - -  
				  - - - -  
				  x - x -
				  x - x -
				  x - x - 
				  x - x - 
				  - x - - 
				  + - - -)

			  (6 - - - - - -
				  - - - - - -
				  x - x - x - 
				  x - x - x - 
				  x - x - x -
				  x - x - x -
				  - x - x - -
				  + - - - - -)

			  (4 - - - -  
				  - - - -  
				  x - x -
				  x - x -
				  - x - - 
				  x - x - 
				  x - x - 
				  + - - -)

			  (5 - - - - -
				  - - - - -
				  x - - x -
				  x - - x -
				  x - - x -
				  - x x x -
				  - - - x -
				  o x x - -)

			  (4 - - - -
				  - - - -
				  x x x -
				  - - x -
				  - x - -
				  x - - -
				  x x x -
				  + - - -)
				)))

	(define font8-chars-special
		(map 
			(λ (p) (cons (car p) (build-char (cdr p))))
			'((228 
			     5 x - - x -
					 - - - - - 
					 - x x - -
					 - - - x -
					 - x x x -
					 x - - x -
					 - x x x -
					 + - - - -)

			(246 
			    5 x - - x -  
					- - - - -  
					- x x - -
					x - - x -
					x - - x - 
					x - - x - 
					- x x - - 
					+ - - - -)

		  (40  3 - x - 
		         x - - 
		         x - - 
		         x - - 
		         x - - 
		         x - - 
		         x - - 
		         + x -)

		  (46  3 - - - 
		         - - - 
		         - - - 
		         - - - 
		         - - - 
		         - - - 
		         - x - 
		         + - -)

		  (44  3 - - - 
		         - - - 
		         - - - 
		         - - - 
		         - - - 
		         - - - 
		         - x - 
		         o - -)

		  (58  3 - - - 
		         - - - 
		         - - - 
		         - x - 
		         - - - 
		         - x - 
		         - - - 
		         + - -)

		  (41  3 x - - 
		         - x - 
		         - x - 
		         - x - 
		         - x - 
		         - x - 
		         - x - 
		         o - -)
		  
		  (-1 6
		         - - - - - -
		         - - x x - -
		         - x x x x -
		         - x x x x -
		         - x x x x -
		         - x x x x -
		         - - x x - - 
		         + - - - - -)

		  (32 4  +))))       ; space width is 6 pixies

	(define owl-logo
		(build-sprite
			'(11 - x - - - - - - - x - 
				  - x x x x x x x x x -
				  - - x x x - x x x - -
				  - - x - - x - - x - - 
				  - x x - x x x - x x - 
				  - x - x x x x x - x -
				  x x x - - x - - x x x
				  x x x x - + - x x x x
				  x x x - x - x - x x x
				  x x - x - x - x - x x
				  x x x - x - x - x x x
				  - x - x - x - x - x - 
				  - x x - x - x - x - -
				  - - x x x x x x x - - 
				  - - - x x - x x - - -)))

	(define (index l n)
		(if (null? l)
			null
			(cons (cons n (car l))
				(index (cdr l) (+ n 1)))))

	;; ff of codepoint -> (width . (len . moves)), to be passed to puts
	(define font-8px
		(list->ff
			(foldr append null
				(list
					(index font8-chars-numeric 48)
					(index font8-chars-alpha-lowercase 97)
					(index font8-chars-alpha-uppercase 65)
					font8-chars-special
					))))

	(define (grale-text-width font text)
		(str-fold
			(λ (w char)
				(cond
					((get font char False) => (λ (c) (+ w (car c))))
					((get font char -1) => (λ (c) (+ w (car c))))
					(else w)))
			0 text))

	; fill a single line for now
	(define (grale-put-text font x y col text)
		(str-fold
			(λ (x char)
				(cond
					((get font char False) =>
						(λ (info)
							(grale-puts x y col (cdr info))
							(+ x (car info))))
					((get font -1 False) =>
						(λ (info)
							(grale-puts x y col (cdr info))
							(+ x (car info))))
					(else x)))
			x text))

)



;
;(import lib-grale)
;(start-grale)
;
;
;(define (put-stupid x y col)
;	(grale-put x y col)
;	(grale-update x y 1 1))
;
;(define (pack-moves lst)
;	(cons (length lst)
;		(foldr
;			(λ (node tl)
;				(lets ((dx dy col node))
;					(cons (+ (<< (+ 8 dx) 4) (+ 8 dy))
;						(cons col tl))))
;			null lst)))
;
;;      o o o
;;    o       o
;;    o   x   o
;;    o       o
;;      o o o
;
;(define w #b11111111)
;
;(define crosshair
;	(pack-moves
;		(list
;			(tuple  0  0 w)
;			(tuple  0 -2 w)
;			(tuple -1  0 w)
;			(tuple -1  1 w)
;			(tuple  0  1 w)
;			(tuple  0  1 w)
;			(tuple  1  1 w)
;			(tuple  1  0 w)
;			(tuple  1  0 w)
;			(tuple  1 -1 w)
;			(tuple  0 -1 w)
;			(tuple  0 -1 w)
;			(tuple -1 -1 w)
;		)))
;
;(define (test w h)
;	(show "connecting to grale: " (grale-init w h))
;
;	'(for-each
;		(λ (x)
;			(for-each
;				(λ (y)
;					(grale-put x y 
;						(band 
;							(if (eq? 0 (band x y))
;								(cond
;									((eq? 0 (rem (>> y 2) 3))
;										#b11100000)
;									((eq? 1 (rem (>> y 3) 3))
;										#b00011100)
;									(else
;										#b00000011))
;								(band #b01001001
;									(+ x y)))
;							255)
;						))
;				(iota 0 1 h)))
;		(iota 0 1 w))
;
;	(grale-put-text font-8px 100 100 #b11111100 "Ainakin Bertta Cecil Demo Ensin Faarao Gabriel Hänen Itse Joku Kaapo Laakso")
;	(grale-put-text font-8px 100 120 #b00011100 "Miksi Nakki Ooppera Pisara Quartus Rasti Saparo Tappi Ulina Vaaksa Wanha")
;	(grale-put-text font-8px 100 140 #b11100000 "Xenon Ympäri Zaphod")
;	(grale-update 0 0 w h)
;
;	(let loop ((tx 0) (ty 13))
;		(let ((ev (grale-wait-event)))
;			(if ev
;				(begin
;					(if (tuple? ev)
;						(tuple-case ev
;							((click btn x y)
;								(cond
;									((eq? btn 1)
;										;(for-each (λ (d) (put-stupid (+ x d) y 255) (put-stupid x (+ y d) 255)) (iota -5 1 6))
;										(grale-paint x y crosshair)
;										(grale-update (- x 10) (- y 10) 20 20)
;										)
;									((eq? btn 3)
;										(for-each
;											(λ (dy)
;												(for-each 
;													(λ (dx)
;														(let ((x (+ x dx)) (y (+ y dy)))
;															(grale-put x y (band 255 (bxor x y)))))
;													(iota -20 1 21)))
;											(iota -20 1 21))
;										(grale-update (- x 20) (- y 20) 40 40))
;									((eq? btn 2)
;										(for-each
;											(λ (dy)
;												(for-each 
;													(λ (dx)
;														(let ((x (+ x dx)) (y (+ y dy)))
;															(grale-put x y (band 255 (band x y)))))
;													(iota -20 1 21)))
;											(iota -20 1 21))
;										(grale-update (- x 20) (- y 20) 40 40))
;									(else
;										;(grale-fill-rect (- x 10) (- y 10) 20 20 255)
;										;(grale-update (- x 10) (- y 10) 20 20)
;										(grale-puts x y 255 owl-logo)
;										(grale-update 0 0 w h)))
;								(loop tx ty))
;							((mouse-move x y)
;								(put-stupid x y #b00010000)
;								(loop tx ty))
;							((key cp)
;								(if (eq? cp 13) ; carriage, return!
;									(loop 0 (+ ty 10))
;									(lets 
;										((char (get font-8px cp False))
;										 (col (if (has? '(40 41) cp) #b11111100 #b00011100)))
;										(if char
;											(let ((xp (+ tx (car char))))
;												(if (< xp w)
;													(begin
;														(grale-puts tx ty col (cdr char))
;														(grale-update tx (- ty 8) (car char) 9)
;														(loop xp ty))
;													(let ((ty (+ ty 10)))
;														(show "printing char to " (cons 0 ty))
;														(grale-puts 0 ty col (cdr char))
;														(grale-update 0 (- ty 8) (car char) 9)
;														(loop (car char) ty))))
;											(begin
;												(show " - no char for cp " cp)
;												(loop tx ty))))))
;							((unknown opcode)
;								; drop
;								(loop tx ty))
;							(else
;								(show "owl: ignored funny event " ev)
;								(loop tx ty)))
;						(loop tx ty)))))))
;
;(test 700 500)
;
;
;
;
