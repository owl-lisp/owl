;;;
;;; Compression and decompression 
;;;

;; testing some compression techniques. it would be nice to compress the C-code
;; stored in repl, maybe the whole fasl image etc. 

;; todo: implement later <technique>-[compress|decompress] for raw data, and keep compress/decompress for packing arbitrary objects
;; todo: arbitrary object compression should also handle object interning and ff rebalancing (implement in lib-fasl and switch to using such decoding here)

(define-module lib-compress
   (export 
      compress    ; compress an arbitrary owl object (fasl encode + some compression)
      decompress  ; decompress an arbitrary owl object (decompress + fasl decode)
      )

   ;; move-to-front compressor
   ;  - find the longest repeated byte sequence(s) 
   ;  - compute savings obtained by a move-to-front encoding
   ;  - pick one of them if some causes data to be saved
   ;  - recurse while progress
   ;
   ;; encoding plan
   ; - <nat> = [0]abcdefg <- 7-bit number
   ;         | [1]abcdefg <- next 7 high bits of a number, and more follow
   ; - compressed = (<rounds> byte ...)
   ;                    '-> number of compression rounds performed (0 = decompressed data at cdr) -> compressing something non-compressable gives a 1-byte overhead
   ; - each fruitful compression round (when using just mtf) causes
   ;     (<rounds> byte ...) -> (<rounds+1> <len> byte0 .. byten <n> <pos0> .. <posn> byte ...)
   ;     == the byte sequence <byte0> .. <byten> is to be inserted to positions <pos0>, <pos0> + <pos1>, <pos0>+<pos1>+<pos2>, ...

   ;;;
   ;;; Compression
   ;;;

   ; idea: we want a subsequence of vec with a large size*frequency (after removing self overlaps)
   ;; to find one, the suffix array gives one good solution. each equal substring is chunked together 
   ;; in it, so we can pick an interval having all the occurrences, and grow it deeper to get longer 
   ;; strings. this was already done in radamsa, but i'm rewriting it here as i may have written 
   ;; that one while at work.

   ; a run of the same value?
   (define (run? bs)
      (if (null? bs)
         True
         (let loop ((a (car bs)) (bs (cdr bs)))
            (cond
               ((null? bs) True)
               ((eq? a (car bs)) (loop a (cdr bs)))
               (else False)))))

   ; bs -> (byte ..) | (offset ...), null x null if nothing of interest
   (define (frequent-subseq vec)
      (lets
         ((sufs (suffix-array vec))
          (end (vec-len vec))
          (read (λ (spos depth) (let ((p (+ (vec-ref sufs spos) depth))) (if (< p end) (vec-ref vec p) F)))))
         ; lead = (score . #(from to depth))
         (define (seek from to depth bs lead)
            ;(print (list 'seek from to depth bs lead))
            (if (< (- to from) 2) ; terminate when out of space or no repetition?
               lead
               (lets
                  ((nocc (- to from))
                   (nocc (if (run? bs) (div nocc (max 1 (length bs))) nocc))
                   (score (- (* depth (- nocc 1)) (* nocc 8))) ; area - estimated encoding size sans overlapping
                   (lead (if (> score (car lead)) (cons score (tuple from to depth)) lead))
                   (this (read from depth)))
                  (if this ;; not out of data yet
                     (lets
                        ((this-end ;; find end of this value
                           (bisect (λ (pos) (not (eq? this (read pos depth)))) from to)))
                        (if this-end ;; something follows?
                           (seek this-end to depth bs
                              (seek from this-end (+ depth 1) (cons this bs) lead))
                           (seek from to (+ depth 1) (cons this bs) lead))) ;; all the same
                     (seek (+ from 1) to depth bs lead)))))
         ;; min score = 2*2
         (let ((node (cdr (seek 0 (vec-len sufs) 0 null (cons 4 False)))))
            (if node
               (lets ((from to len node))
                  (values len (map (λ (sp) (vec-ref sufs sp)) (iota from 1 to))))
               (values 0 null)))))

   ;     (<rounds> byte ...) -> (<rounds+1> <len> byte0 .. byten <n> <pos0> .. <posn> byte ...)

   (define (base128 n)
      (if (< n 128)
         (list n)
         (cons (band 127 n) (base128 (>> n 7)))))

   ; push a representation of natural number n to byte list tl
   (define (natural n tl)
      (let loop ((ds (base128 n)) (tl tl) (first? True))
         (cond
            ((null? ds) tl)
            (first? (loop (cdr ds) (cons (car ds) tl) False))
            (else (loop (cdr ds) (cons (bor 128 (car ds)) tl) False)))))

   (define (intervals poss tl)
      (let loop ((last 0) (poss poss))
         (if (null? poss)
            tl
            (let ((this (car poss)))
               (natural (- this last) 
                  (loop this (cdr poss)))))))

   (define (cut-seqs lst poss shared)
      (define (walk lst poss pos pat)
         (cond
            ((null? lst) null)
            ((pair? pat)
               (walk (cdr lst) poss (+ pos 1) (cdr pat)))
            ((null? poss) lst)
            ((< (car poss) pos)
               (walk lst (cdr poss) pos pat))
            ((= (car poss) pos)
               (walk lst (cdr poss) pos shared))
            (else
               (cons (car lst)
                  (walk (cdr lst) poss (+ pos 1) pat)))))
      (walk lst poss 0 null))
      
   (define (move-to-front vec len poss)
      (list->vector
         (lets
            ((poss (sort < poss))
             (shared 
               (map (λ (p) (vec-ref vec p))
                  (iota (car poss) 1 (+ (car poss) len)))))
            (natural len
               (append shared
                  (intervals poss
                     (cut-seqs (vector->list vec) poss shared)))))))

   ; vec -> vec'
   (define (compress-data vec)
      ;; iterate a move-to-front of a large repeated substring while making progress
      (let loop ((rounds 0) (vec vec))
         (print (list 'round rounds 'length (vec-len vec)))
         (lets ((len poss (frequent-subseq vec)))
            (if (eq? len 0)
               (list->vector (cons rounds (vec->list vec)))
               (let ((vecp (move-to-front vec len poss)))
                  ;(show "move-to-front gave " vecp)
                  (if (< (vec-len vecp) (vec-len vec))
                     (loop (+ rounds 1) vecp)
                     (list->vector (cons rounds (vec->list vec)))))))))

   (let ((vec (vector 1 2 3 1 2 3 2 2 1 2 3)))
      (compress-data vec)
      (show " - data is " vec))

   (compress-data (file->vector "vm.c"))

   ; anything -> byte vector
   (define (compress thing)
      (list->vector (compress-data (list->vector (fasl-encode thing)))))


   ;;;
   ;;; Decompression
   ;;;

   ; compressed byte vector -> anything
   (define (decompress vec)
      (define (rounds bs)
         (if (= (car bs) 0)
            (fasl-decode (cdr bs))
            (error "decompression round not implemented: " bs)))
      (rounds (vec->list vec)))
  

)

