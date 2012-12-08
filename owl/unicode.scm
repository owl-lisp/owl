;;;
;;; Unicode and UTF-8
;;;

;; todo: remove dependencies to remaining few bignum ops
;; fixme: check for code point validity in string conversions
;; fixme: force the string conversion errors to be handled explicitly

(define-library (owl unicode)

   (export
      utf8-encode             ;; ll -> list | #false
      utf8-decode             ;; ll -> list | #false
      utf8-sloppy-decode      ;; ll -> list
      ;utf8-normalize         ;; ll -> list
      utf8-decoder            ;; byte-ll fail → cp-ll | (cp ... . (fail self line data))
      encode-point
      two-byte-point
      three-byte-point
      four-byte-point
      last-code-point
      valid-code-point?)

   (import
      (except (owl list) render)
      (owl list-extra)
      (owl defmac)
      (owl lazy)
      (owl math)
      (only (owl syscall) error))

   (begin
      ;; UTF-8
      ; overall idea: each unicode code point is represented as a leading byte 
      ; possibly followed by extra bytes. a leading byte in ASCII range 0-127 
      ; represents the corresponding code point. for higher values the high bits 
      ; are used as a tag to mark how many of the following bytes also have bits 
      ; belonging to this code point, and the subsequent ones have extension tag
      ; and some payload bits.
      ;
      ; codepoint = [typetag|high-bits] [01|lower-bits]*
      ; 0xxxxxxx -- 7-bit        -- 7-bit codepoints as in ASCII
      ; 10xxxxxx -- continuation -- extra-payload bits for large code points 
      ; 110xxxxx -- 2-byte char  -- leading 5 bits (followed by 1 cont bytes)
      ; 1110xxxx -- 3-byte char  -- leading 4 bits (followed by 2 cont bytes)
      ; 11110xxx -- 4-byte char  -- leading 3 bits (followed by 3 cont bytes)


      ; for example: bytes 206 187 in input
      ;
      ;         .-> top 5 bits of value
      ;         |        .-> low 6 bits of value
      ;     .---|    .---|
      ;  11001110 10111011
      ;  '-|abcde '-|fghijk
      ;    |        '-> extra payload tag bits
      ;    '-> 2-byte tag bits
      ;
      ;  abcdefghijk = #b01110111011 = 955 = code point of the letter λ
      ;

      ;;;
      ;;; Encoding
      ;;;

      (define last-code-point (- #b1000000000000000000000 1))

      ; -> ll' rout'
      ;(define (extra-encode rout val tag n here)
      ;   (if (eq? n 0) ; make the last leading byte out of val
      ;      (cons (bor tag val) rout)
      ;      (lets
      ;         ((here (band val here)) ; take the current payload bits
      ;          (val (>> val 6)))
      ;         (cons
      ;            (bor #b10000000 here) ; add extra byte tag
      ;            (extra-encode rout val tag (- n 1))))))

      (define (valid-code-point? val)
         (cond
            ((eq? (type val) type-fix+) #true) ; 0 <= n < 65536
            ((eq? (type val) type-int+) ;; 0 <= n <= last-code-point
               (<= val last-code-point))
            (else #false)))

      ;(define (encode-point rout val)
      ;   (cond
      ;      ((lesser? val 128) val) ; the usual suspect
      ;      ((< val #b100000000000) ; fits in 5+6 bits with 2-byte encoding
      ;         (extra-encode rout val #b11000000 1 #b11111))
      ;      ((< val #b10000000000000000) ; fits in 4 + 2*6 bits with 3-byte encoding
      ;         (extra-encode rout val #b11100000 2 #b1111))
      ;      ((< val #b1000000000000000000000) ; fits in 3 + 3*6 bits with 4-byte encoding
      ;         (extra-encode rout val #b11110000 #b111))
      ;      (else #false)))

      (define extension #b10000000)

      ; grab low 6 bits of a number
      (define (ext n)
         (fxbor extension 
            (band n #b111111)))

      (define (encode-point point tl)
         (cond
            ((< point #x80) ; ascii, fits 7 bits
               (cons point tl))
            ((< point #b100000000000) ; 5 + 6 bits
               (ilist
                  (bor (band (>> point 6) #x1f) #xc0)
                  (ext point)
                  tl))
            ((< point #b10000000000000000) ; 4 + 2*6 bits
               (ilist
                  (bor (band (>> point 12) #x0f) #xe0)
                  (ext (>> point 6))
                  (ext point)
                  tl))
            ((< point #b1000000000000000000000) ; 3 + 3*6 bits
               (ilist
                  (bor (band (>> point 18) #b111) #xf0)
                  (ext (>> point 12))
                  (ext (>> point 6))
                  (ext point)
                  tl))
            (else
               (error "utf8 encode: code point too high " point))))

      ; ll -> list | #false
      (define (utf8-encode thing)
         (foldr encode-point null thing))


      ;;;
      ;;; Decoding
      ;;;

      ;; compute the minimal sizes using the encoder to avoid silly bugs
      (define (min-nbyte n)
         (let loop ((cp 1))
            (if (= (length (encode-point cp null)) n)
               cp
               (loop (<< cp 1)))))

      (define min-2byte (min-nbyte 2))
      (define min-3byte (min-nbyte 3))
      (define min-4byte (min-nbyte 4))

      ;; read n extension bytes and add values as low bits to val
      ; lst n val min -> val'|#false lst', val' >= min
      (define (get-extension-bytes lst n val min)
         (cond
            ((eq? n 0)
               (if (< val min)
                  (values #false lst) ; <- overly wide encoding
                  (values val lst)))
            ((null? lst)
               (values #false lst))
            ((pair? lst)
               (let ((hd (car lst)))
                  (if (eq? #b10000000 (fxband #b11000000 hd))
                     (get-extension-bytes (cdr lst) (- n 1) (bor (<< val 6) (band hd #b111111)) min)
                     ;; not a valid continuation byte -> error
                     (values #false lst))))
            (else
               (get-extension-bytes (lst) n val min))))

      ;; bs -> char x bs' | #false x bs', being after the last bad value or already null
      (define (decode-char lst)
         (let ((hd (car lst)))
            (cond
               ((eq? 0 (fxband hd #b10000000))
                  (values hd (cdr lst)))
               ((eq? #b11000000 (fxband hd #b11100000))
                  ; 2-byte char with top bits 110, 5 bits here
                  (get-extension-bytes (cdr lst) 1 (fxband #b11111 hd) min-2byte))
               ((eq? #b11100000 (fxband #b11110000 hd))
                  ; 3-byte char, 4 bits here
                  (get-extension-bytes (cdr lst) 2 (fxband #b1111 hd) min-3byte))
               ((eq? #b11110000 (fxband #b11111000 hd))
                  ; 4-byte char, 3 bits here
                  (get-extension-bytes (cdr lst) 3 (fxband #b111 hd) min-4byte))
               (else
                  ; invalid leading byte
                  (values #false lst)))))

      ;; bs default-codepoint -> unicode-code-points | #false if error and no default-codepoint
      (define (decoder lst out default)
         (cond
            ((null? lst)
               (reverse out))
            ((pair? lst)
               (lets ((val lst (decode-char lst)))
                  (cond
                     (val ;; code point taken successfully
                        (decoder lst (cons val out) default))
                     (default ;; broken value replaced by given default one, if given
                        (decoder lst (cons default out) default))
                     (else ;; return #false for broken UTF-8 when no replacing symbol is given
                        #false))))
            (else
               (decoder (lst) out default))))

      ; ll -> lst' | #false
      (define (utf8-decode lst)
         (decoder lst null #false))

      ; ll -> lst', rewrite all errors with #
      (define (utf8-sloppy-decode lst)
         (decoder lst null 35))


      ; byte-ll fail → cp-ll | (cp ... . (fail self line data))
      ; self :: data(') line → continue

      (define (utf8-decoder ll fail)
         (let loop ((ll ll) (line 0))
            (cond
               ((null? ll)
                  null)
               ((pair? ll)
                  (lets ((val ll (decode-char ll)))
                     (cond
                        (val
                           (if (eq? val #\newline)
                              (pair val (loop ll (+ line 1)))
                              (pair val (loop ll line))))
                        ;; drop bom here later
                        (else
                           (fail loop line ll)))))
               (else
                  (loop (ll) line)))))


      ;;;
      ;;; Testing
      ;;;

      ;;; encoding code points to byte-lists


      ;;; decoding byte-lists to code points 

      (define (two-byte-point a b)
         (bor (<< (fxband a #x1f) 6) (fxband b #x3f)))

      (define (three-byte-point a b c)
         (bor (bor (<< (fxband a #x0f) 12) (<< (fxband b #x3f) 6)) 
            (fxband c #x3f)))

      (define (four-byte-point a b c d)
         (bor 
            (bor (<< (fxband a #x07) 18) (<< (fxband b #x3f) 12)) 
            (bor (<< (fxband c #x3f)  6)     (fxband d #x3f))))

))



         
