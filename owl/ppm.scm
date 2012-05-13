;;;
;;; Support for reading PPM images (initial quick hack)
;;;

(define-module lib-ppm
	(export
		read-ppm 			; path → #(rgb888-pixel-list width height) | #false
		)

	; drop anything up to and including the next newine (if any)
	(define (pop-comment bs)
		(cond
			((null? bs) bs)
			((pair? bs)
				(if (eq? (car bs) 10)
					(cdr bs)
					(pop-comment (cdr bs))))
			(else (pop-comment (bs)))))

	(define whitespace-chars (list 9 10 32))

	; bs -> bs' after possibly dropping a single whitespace 
	(define (pop-whitespace bs)
		(cond
			((null? bs) bs)
			((pair? bs)
				(cond
					((has? whitespace-chars (car bs))
						(cdr bs))
					((eq? (car bs) 35) ; # .... \n
						(pop-comment bs))
					(else bs)))
			(else (pop-whitespace (bs)))))

	(define (pop-whitespaces bs)
		(let ((bsp (pop-whitespace bs)))
			(if (eq? bs bsp)
				bsp
				(pop-whitespaces bsp))))

	; bs + predicate -> bs' + kleene*-matched-bytes
	(define (get-bytes bs pred)
		(if (pair? bs)
			(if (pred (car bs))
				(lets
					((this (car bs))
					 (bs tl (get-bytes (cdr bs) pred)))
					(values bs (cons this tl)))
				(values bs null))
			(get-bytes (bs) pred)))

	(define (decimal-char-byte? b)
		(and (>= b 48) (<= b 57)))

	(define (bytes->natural bs)
		(fold (λ (h d) (+ (* h 10) d)) 0 
			(map (λ (b) (- b 48)) bs)))

	; bs -> bs' + n|0
	(define (get-decimal bs)
		(lets 
			((bs (pop-whitespaces bs))
			 (bs these (get-bytes bs decimal-char-byte?)))
			(values bs (bytes->natural these))))

	(define (magic-byte? b) 
		(or (decimal-char-byte? b) (eq? b 80)))

	; P6
	; <whitespaces> blanks, tabs, crs, lfs etc
	; <width> -- ascii decimal
	; <height> -- ascii decimal
	; <maxval> -- ascii 1-65535 (if more than 255, read 2 bytes of pixels)
	; <a single whitespace character>
	; width*height times 3/6-byte triplets representing red, green and blue intensities

	; bs -> bs val|#false
	(define (get-byte bs)
		(cond
			((null? bs)
				(print "ppm: out of data")
				(values bs #false))
			((pair? bs)
				(values (cdr bs) (car bs)))
			(else (get-byte (bs)))))

	(define (make-pixel-reader get)
		(λ (bs)
			(lets
				((bs r (get bs))
				 (bs g (get bs))
				 (bs b (get bs)))
				(if b
					;; fixme: assumed 8-bit here
					(values bs
						(bor (bor (<< r 16) (<< g 8)) b))
					(values bs #false)))))

	;; fixme: rgb8 color bounding missing (very simple but ENOTIME, just pass the multiplier to both readers, 1 for 255)
	(define (get-rgb8 maxval)
		(if (= maxval 255)
			get-byte
			(error "ppm: rgb8 cannot yet handle maxval " maxval)))

	;; fixme: rgb16 missing (and simple)
	(define (get-rgb16 maxval)
		(error "ppm: rgb16" "i'm not implemented"))

	(define (get-pixels bs get n)
		(let loop ((bs bs) (n n) (out null))
			(if (= n 0)
				(reverse out)
				(lets ((bs this (get bs)))
					(if this
						(loop bs (- n 1) (cons this out))
						#false)))))

	; note, this will somewhat misleadingly report bad parses as having values 0
	(define (parse-ppm-p6 bs)
		(lets
			((bs width (get-decimal bs))
			 (bs height (get-decimal bs))
			 (bs maxval (get-decimal bs))
			 (bs (pop-whitespace bs)))
			(cond
				((or (< width 1) (< height 1) (> width 65536) (> height 65536))
					(print "refusing to load image of proportions " (cons width height))
					#false)
				((= maxval 0)
					(print "ppm: maximum color cannot be 0")
					#false)
				((> maxval 65535)
					(print "ppm: too many colours: " maxval)
					#false)
				(else
					(lets
						((get-color (if (< maxval 256) (get-rgb8 maxval) (get-rgb16 maxval)))
						 (get-pixel (make-pixel-reader get-color))
						 (data (get-pixels bs get-pixel (* width height))))
						(if data
							(tuple data width height)
							#false))))))

	(define (parse-ppm bs)
		(lets ((bs magic (get-bytes bs magic-byte?)))
			(cond
				((equal? magic '(80 54))
					(parse-ppm-p6 bs))
				;; fixme: only supports P6 atm
				(else
					(print "bad magic in file: " magic)
					#false))))

	(define (read-ppm path)
		(let ((port (open-input-file path)))
			(if port
				(lets ((stuff (parse-ppm (port->byte-stream port))))
					(close-port port)
					stuff)
				(begin
					(print "failed to open " path)
					#false))))

)

;(import-old lib-ppm)
;(read-ppm "/tmp/black.ppm")
