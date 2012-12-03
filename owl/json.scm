;;;
;;; a quick and dirty JSON parser (imported from work, will be hacked mainly separately)
;;;

; todo:
;	- json-rpc part under experimental hacking
;	- encoding, assumed UTF-8 everywhere. no support for UTF-16 and pals.
;	- need to be more greedy at some places to avoid unnecessary backtracking
;	- pick a way to handle failures (data structure or a cont)
;	- fuzz with radamsa
;		- an obvious dos attack comes from big exponents 

; JSON - a simpler alternative to XML, almost as simple as S-exprs

(define-module lib-json

	(export json-decode json-encode)

	(import (owl parse))
	(import-old lib-unicode encode-point)

	;;;
	;;; Decoding
	;;;

	(define (between? lo x hi)
		(and (<= lo x) (<= x hi)))

	;; numbers

	(define get-sign
		(get-any-of 
			(get-imm 43)
			(get-imm 45) 
			(get-epsilon 43)))

	(define (digit? x) (between? 48 x 57))

	(define (bytes->number bs)
		(fold (lambda (out d) (+ (* out 10) (- d 48))) 0 bs))

	(define get-natural
		(let-parses
			((digits (get-greedy-kleene+ (get-byte-if digit?))))
			(bytes->number digits)))

	(define (rationalize num digits base)
		(let ((shift (expt base (length digits))))
			(/ (+ (* num shift) (bytes->number digits)) shift)))

	(define (cook-number sign main frac exp)
		(cond
			((eq? sign 45) (- 0 (cook-number #false main frac exp)))
			(frac (cook-number sign (rationalize main frac 10) #false exp))
			(exp (* main (expt 10 exp)))
			(else main)))

	(define get-exponent-sign
		(let-parses
			((e (get-either (get-imm 101) (get-imm 69)))
			 (sign	
			 	(get-any-of
					(get-word "-" -1)
					(get-word "+" +1)
					(get-epsilon +1))))
			sign))
			 
	(define get-number 
		(let-parses
			((sign get-sign)
			 (main get-natural)
			 (frac 
			 	(get-either
					(let-parses
						((skip (get-imm 46)) ; dot
						 (frac (get-greedy-kleene+ (get-byte-if digit?))))
						frac)
					(get-epsilon #false)))
			(exp
				(get-either
					(let-parses
						((s get-exponent-sign)
						 (exp get-natural))
						(* s exp))
					(get-epsilon #false))))
			(cook-number sign main frac exp)))

	;; strings

	; fixme, share from below
	(define string-specials
		(list->ff
			(map (λ (x) (cons (cdr x) (car x)))
				'((34 .  34) ; "
				  (92 .  92) ; \
				  (47 .  47) ; /
				  ( 8 .  98) ; [b]ackspace
				  (12 . 102) ; [f]ormfeed
				  (10 . 110) ; [n]ewline
				  (13 . 114) ; carriage [r]eturn
				  ( 9 . 116) ; horizontal [t]ab
				  ))))

	(define hex-val
		(list->ff
			(append (zip cons (iota 48 1 58) (iota 0 1 10)) ; 0-9
				(append 
					(zip cons (iota 97 1 103) (iota 10 1 16))	; a-f
					(zip cons (iota 65 1 71)  (iota 10 1 16)))))) ; A-F

	(define get-hex 
		(let-parses ((digit (get-byte-if (λ (x) (get hex-val x #false)))))
			(get hex-val digit 0)))

	(define get-quoted-char
		(let-parses
			((skip (get-imm 92))
			 (char (get-byte-if (λ (x) (get string-specials x #false)))))
			(get string-specials char #false)))

	(define get-hex16-char
		(let-parses
			((skip (get-imm 92))
			 (skip (get-imm 117))
			 (a get-hex)
			 (b get-hex)
			 (c get-hex)
			 (d get-hex))
			(bor (<< a 12) (bor (<< b 8) (bor (<< c 4) d)))))

	(define get-string
		(let-parses
			((skip (get-imm 34))
			 (chars
				(get-greedy-kleene*
					(get-any-of
						(get-rune-if (lambda (x) (not (has? '(34 92) x))))
						get-quoted-char
						get-hex16-char)))
			 (skip (get-imm 34)))
			(runes->string chars)))

	(define maybe-whitespace
		(get-greedy-kleene*
			(get-byte-if
				(λ (b) (has? '(9 10 13 32) b)))))

	;; boilerplate reduction for compound forms 

	(define (get-comma-list-of get-elem lp rp type)
		(let-parses
			((skip (get-imm lp))
			 (commad-vals 	
			 	(get-kleene* 	 ;fixme, could not be greedy here?
					(let-parses
						((skip maybe-whitespace)
						 (thing get-elem)
						 (skip maybe-whitespace)
						 (skip (get-imm 44)))
						thing)))
			 (skip maybe-whitespace)
			 (final-val 
			 	(get-either get-elem (get-epsilon 'none)))
			 (skip maybe-whitespace)
			 (skip (get-imm rp)))
			(cons type
				(if (eq? final-val 'none)
					commad-vals
					(append commad-vals (list final-val))))))

	;; arrays 
						 
	(define (get-array get)
		(get-comma-list-of (get) 91 93 'array))

	;; objects

	(define (get-object get)
		(get-comma-list-of
			(let-parses
				((skip maybe-whitespace)
				 (key get-string)
				 (skip maybe-whitespace)
				 (skip (get-imm 58))
				 (skip maybe-whitespace)
				 (value (get)))
				(cons key value))
			123 125 'object))

	;; special literals

	(define get-literal
		(get-either
			(get-word "null" 'nothing)
			(get-either
				(get-word "true" #true)
				(get-word "false" #false))))

	;; entry

	(define (get-json-exp)
		(let-parses
			((skip maybe-whitespace)
			 (value 
			 	(get-either
					(get-either get-string 
						(get-either get-number get-literal))
					(get-either (get-array get-json-exp) (get-object get-json-exp)))))
			value))

	(define (json-decode data)
		((get-json-exp) 
			(cond
				((string? data) (string->runes data))
				((list? data) data)
				(else (error "json-decode: funny data: " data)))
			(lambda (data fail val) (values #true val data))
			(lambda (data reason) (values #false reason #false))))



	;;; 
	;;; Encoding
	;;; 

	; wanted (equal? (json-decode (json-encode E)) E), when E in range

	(define (jsonify-array lst tail json)
		(cons 91
			(foldr
				(lambda (obj tail)
					(json obj
						(if (eq? (car tail) 93)
							tail
							(cons 44 tail))))
				(cons 93 tail) lst)))

	(define string-special-chars 
		(list->ff
			'((34 .  34) ; "
			  (92 .  92) ; \
			  (47 .  47) ; /
			  ( 8 .  98) ; [b]ackspace
			  (12 . 102) ; [f]ormfeed
			  (10 . 110) ; [n]ewline
			  (13 . 114) ; carriage [r]eturn
			  ( 9 . 116) ; horizontal [t]ab
			  )))

	(define (hex-digit n)
		(if (< n 10)
			(+ n 48)
			(+ n 87)))

	(define (hex-digits char)
		(if (= char (band char #xffff))
			(values
				(hex-digit (>> char 12))
				(hex-digit (band (>> char 8) #xf))
				(hex-digit (band (>> char 4) #xf))
				(hex-digit (band char #xf)))
			(error "json encode: unicode code point too high for JSON: " char)))

	; fixme, use UTF-8 for other chars, since \uxxxx only handles the low 16 bits

	(define (jsonify-string str tail)
		(cons 34
			(str-foldr
				(lambda (char tail)
					(cond
						((get string-special-chars char #false) =>
							(λ (char) (ilist 92 char tail)))
						(else
							(encode-point char tail))))
				(cons 34 tail) str)))

	(define (jsonify-object lst tail json)
		(error "jsonify-object: not yet implemented: " lst))

	; fixme, rationals with finite decimal expansions *are* jsonifiable without loss of information
	; but one probably would like a finite approximation anyway since these turn into floats (?)
	; so just grab a suitable exponent and some meaningful digits later

	(define (compatible-number? n)
		(case (type n)
			(type-fix+ #true)
			(type-fix- #true)
			(type-int+ #true)
			(type-int- #true)
			(else #false))) ; namely rationals and later complex

	(define (jsonify-number n tail)
		(if (compatible-number? n)
			(render n tail)
			(error "jsonify: cannot encode number " n)))

	(define (jsonify val tail)
		(cond
			((string? val)
				(jsonify-string val tail))
			((pair? val)
				(cond
					((eq? (car val) 'array)
						(jsonify-array (cdr val) tail jsonify))
					((eq? (car val) 'object)
						(jsonify-object (cdr val) tail jsonify))
					(else
						(error "jsonify: bad pair head: " (car val)))))
			((number? val)
				(jsonify-number val tail))
			((eq? val #true) (render "true" tail))
			((eq? val #false) (render "false" tail))
			((eq? val 'nothing) (render "null" tail))
			(else (error "jsonify: cannot encode " val))))

	(define (json-encode val)
		(jsonify val null))

)

; JSON-RPC - a simpler alternative to XML-RPC, almost as good as the equivalent with S-exprs

(define-module lib-json-rpc

	(import-old lib-json)

	(export loltron)

	(define (loltron x)
		(print "lol " x))

	;;; 
	;;; Client connections
	;;; 

	; json rpc thread operation
	;	- wait for input or messages from local threads
	;	- send notifications to subscribers or drop
	;	- keep the id counter and match against responses

	(define (json-rpc-terminate reason fd pending)
		(close-port fd)
		(if reason (print "json-rpc connection closed: " reason))
		(if pending (print "WARNING: there are still pending requests which will remain stuck waiting."))
		'json-rpc-closed)

	; event-driven operation 

	(define (json-rpc-handler fd pending-reqs id input)
		(bind (accept-mail)
			(λ (from msg)
				(cond
					((eq? from fd)
						(cond
							((eof? msg)
								(json-rpc-terminate "remote end terminated connection" fd pending-reqs))
							((not msg)
								(json-rpc-terminate "connection error" fd pending-reqs))
							(else
								(print "json-rpc-handler: i got some data!!!1 \o/")
								(lets
									((pending-reqs id input 	
										(handle-input msg pending-reqs id input)))
									(json-rpc-handler fd pending-reqs id input)))))
					((json-rpc-serialize msg id) =>
						(λ (message)
							(lets ((bytes cont id message))
								(mail fd bytes)
								(json-rpc-handler 
									fd
									(if cont
										...)))))
					...))))
						

	(define (initialize-json-rpc-thread fd)
		(mail fd 'input)	; accept input (notifications can come sponateously)
		(json-rpc-handler fd #false 0 null))
	
	; host x port → thread-name | #false
	(define (json-rpc-connection host port) 
		(let ((fd (connect host port)))
			(if fd
				(fork-named (tuple 'json-rpc host port 'at fd)
					(λ () (initialize-json-rpc-thread fd)))
				(begin
					(print "json-rpc: connection failed to " (cons host port))
					#false))))


	; Syntax:
	; --> data sent to service
	; <-- data coming from service
	; Procedure Call with positional parameters:
	; --> {"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}
	; <-- {"jsonrpc": "2.0", "result": 19, "id": 1}
	; 
	; --> {"jsonrpc": "2.0", "method": "subtract", "params": [23, 42], "id": 2}
	; <-- {"jsonrpc": "2.0", "result": -19, "id": 2}
	; Procedure Call with named parameters:
	; --> {"jsonrpc": "2.0", "method": "subtract", "params": {"subtrahend": 23, "minuend": 42}, "id": 3}
	; <-- {"jsonrpc": "2.0", "result": 19, "id": 3}
	; 
	; --> {"jsonrpc": "2.0", "method": "subtract", "params": {"minuend": 42, "subtrahend": 23}, "id": 4}
	; <-- {"jsonrpc": "2.0", "result": 19, "id": 4}
	; Notification:
	; --> {"jsonrpc": "2.0", "method": "update", "params": [1,2,3,4,5]}
	; 
	; --> {"jsonrpc": "2.0", "method": "foobar"}
	; Procedure Call of non-existent procedure:
	; --> {"jsonrpc": "2.0", "method": "foobar", "id": "1"}
	; <-- {"jsonrpc": "2.0", "error": {"code": -32601, "message": "Procedure not found."}, "id": "1"}
	; Procedure Call with invalid JSON:
	; --> {"jsonrpc": "2.0", "method": "foobar, "params": "bar", "baz]
	; <-- {"jsonrpc": "2.0", "error": {"code": -32700, "message": "Parse error."}, "id": null}
	; Procedure Call with invalid JSON-RPC:
	; --> [1,2,3]
	; <-- {"jsonrpc": "2.0", "error": {"code": -32600, "message": "Invalid Request."}, "id": null}
	; 
	; --> {"jsonrpc": "2.0", "method": 1, "params": "bar"}
	; <-- {"jsonrpc": "2.0", "error": {"code": -32600, "message": "Invalid Request."}, "id": null}
	; Batched Call:
	; --> [ {"jsonrpc": "2.0", "method": "sum", "params": [1,2,4], "id": "1"},
	;       {"jsonrpc": "2.0", "method": "notify_hello", "params": [7]},
	;       {"jsonrpc": "2.0", "method": "subtract", "params": [42,23], "id": "2"},
	;       {"foo": "boo"},
	;       {"jsonrpc": "2.0", "method": "foo.get", "params": {"name": "myself"}, "id": "5"},
	;       {"jsonrpc": "2.0", "method": "get_data", "id": "9"} ]
	; 
	; <-- [ {"jsonrpc": "2.0", "result": 7, "id": "1"},
	;       {"jsonrpc": "2.0", "result": 19, "id": "2"},
	;       {"jsonrpc": "2.0", "error": {"code": -32600, "message": "Invalid Request."}, "id": null},
	;       {"jsonrpc": "2.0", "error": {"code": -32601, "message": "Method not found."}, id: "5"},
	;       {"jsonrpc": "2.0", "result": ["hello", 5], "id": "9"} ]
	; Batched Call itself fails:
	; --> [ {"jsonrpc": "2.0", "method": "sum", "params": [1,2,4], "id": "1"},
	;       {"jsonrpc": "2.0", "method" ]
	; <-- {"jsonrpc": "2.0", "error": {"code": -32700, "message": "Parse error."}, "id": null}
)


;; a few tests

(import-old lib-json)

(print "DECODING ------------------------------------------------")

(for-each 
	(lambda (x)
		(print "at " x)
		(lets ((ok? value tail (json-decode x)))
			(if ok?
				(begin
					(print " parsed " value) 
					(print " tail " tail))
				(print " *** PARSE FAILED ***"))))
	(list
		"1 "
		"123 "
		"\"abba\"  "
		"[]"
		"[42]"
		"[1, 2, 3]"
		"[1, \"lol\", [1, 2, 3], [] ]"
		"\"lol\\btron\""
		"\"\\u002A\\u002a\\u002A\""
		"{\"a\": \"yes\", \"b\" : \"no\"}"
		"{
	  \"firstName\": \"John\",
	  \"lastName\": \"Smith\",
	  \"address\": {
			\"streetAddress\": \"21 2nd Street\",
			\"city\": \"New York\",
			\"state\": \"NY\",
			\"postalCode\": 10021
	  },
	  \"phoneNumbers\": [
			{ \"type\": \"home\", \"number\": \"212 555-1234\" },
			{ \"type\": \"fax\", \"number\": \"646 555-4567\" }
	  ],
	  \"newSubscription\": false,
	  \"companyName\": null
		 }"
	  "[123, 123.456, -123, -123.551, 0.001, 1e10, 0.0003e+10, 1234567E-10]"
	  "[1e-2, 1e-1, 1e0, 1e1, 1e2]"
		))

(print "ENCODING ----------------------------------------------------")

(for-each
	(lambda (obj)
		(display obj)
		(print " => " (bytes->string (json-encode obj)))
		(print ""))
	(list
		#true
		#false
		(list 'array #true #false)
		(list 'array #true (list 'array #false #false) #true)
		(list 'array 1 2 3 4)
		(list 'array -10000000000000000 -100 0 100 +100000000000000000)
		"loltron"
		"lol\tron"
		"lol
tron"
		"lol λ tron"
		"Ω ≡ (λx.(x x) λx.(x x))"
))

