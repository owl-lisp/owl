;;;
;;; Vectors
;;;
; 
; vectors are one-dimensional data structures indexable by natural numbers, 
; having O(n log_256 n) access and memory use (effectively O(1)). They are 
; mainly intended to be used for static data requiring efficient (modulo 
; owl) iteration and random access.
;
; in owl, vectors are implemented as complete 256-ary trees. small vectors 
; fitting to one node of the tree are of raw or allocated type 11, meaning 
; they usually take 8+4n or 4+n bytes of memory, depending on whether the 
; values are normal descriptors or fixnums in the range 0-255.
;
; large vectors are 256-ary trees. each dispatch node in the tree handles 
; one byte of an index, and nodes starting from root each dispatch the 
; highest byte of an index. when only one byte is left, one reads the 
; reached leaf node, or the leaf node stored to the dispatch node.
;
; thus reading the vector in order corresponds to breadth-first walk 
; of the tree. notice that since no number > 0 has 0 as the highest 
; byte, the first dispatch position of the root is always free. this 
; position contains the size of the vector, so that it is accessable 
; in O(1) without space overhead or special case handling. leaf nodes 
; have the size as part of the normal owl object header.

;; order example using binary trees
;
;           (0 1)                 bits 0 and 1, only 1 can have children
;              |                  dispatch the top bit 
;            (2 3)                bits from top, 10 11, numbers ending here 2 and 3
;            /   \                dispatch top and second bit
;           /     \
;       (4 5)     (6 7)           bits from top, (100 101) (110 111)
;       /  |       |  \
;      /   |       |   \
; (9 8) (10 11) (12 13) (14 15)   etc
; 
; vectors use the same, but with 256-ary trees, which works well because 
; it is half of owl's fixnum base, so dispatching can be done easily without 
; shifting, and not too wide to make array mutations too bulky later.




,r "owl/lazy.l"

(define-module lib-vector

   (export
      vector              ; v0, .., vn → vector
      vector?             ; x → bool
      byte-vector?
      vec-len             ; v → n
      vec-ref             ; v x p → v[p] | error
      list->vector
      list->byte-vector   ; (byte ...) -> bvec | False
      vector->list
      vec->list
      vec-iter
      vec-iterr
      vec-fold
      vec-foldr
      vec-range           ; vec x start x end -> vec'
      vec-iter-range      ; vec x start x end -> ll
      vec-map             ; (val → val') x vec → vec'

      ; these assume a sorted vector (as used by pred) having matches in one continuous range
      vec-match-range         ; vec x val-pred -> lo x hi | False x False
      vec-match-range-between ; vec x pred x hi x lo -> lo x hi | False x False

      ;vec-equal?         ; v x v → bool
      ;vec-render         ; v x tail → tail'

      merge-chunks          ; exported for use in lib-io (may be moved later)
      leaf-data vec-leaf-of
      vec-leaves
      vec-cat             ;  vec x vec → vec
      vec-rev
      render
      )

   (import lib-lazy)

   (define (byte-vector? x) (eq? (fxband (type x)     #b100011111000) 2136))

   ;; internals
   ;
   ; vectors have major type 11
   ; 
   ; vector = 
   ;      raw 11, size 256       -> a vector leaf having only raw data (fixnums in the range 0-255, takes W + n bytes of memory)
   ;    alloc 11, variant 0,     -> wide leaf vector having any values (contains pointers, so takes W + Wn bytes of memory)
   ;    alloc 11, variant 1 (43) -> a dispatch node having [Leaf Disp0 Disp1 ...]

   ;;;
   ;;; Vector search
   ;;;

   ;; dispatch low 8 bits of a fixnum, returning the subnode
   (define (vec-dispatch-1 v n)
      (type-case v ;; <- could be removed to increase speed
         ((alloc 43) ; vector dispatch node with #[Leaf D0 ... D255]
            (lets ((n _ (fx+ (fxband n 255) 2))) ;; jump over header and leaf
               (ref v n)))
         (else
            (error "Bad vector node in dispatch-1: type " (type v)))))

   ; dispatch the high 8 bits of a fixnum, returning the subnode
   (define (vec-dispatch-2 v d) ; -> v'
      (type-case v ;; <- could be removed to increase speed
         ((alloc 43) ; vector dispatch node
            (lets 
               ((p _ (fx>> d 8))
                (p _ (fx+ p 2)))
               (ref v p)))
         ((alloc 11)
            (error "Leaf vector in dispatch-2: " v))
         (else
            (error "Bad vector node in dispatch-2: obj " v))))

   ; dispatch 8-bit parts (256-way tree)
   ; note, the highest one must know whether it must dispatch one or two bytes

   (define (vec-seek v ds)
      (lets ((d ds ds))
         (if (null? ds)
            (if (lesser? d #x100) ; just one byte at top digit?
               (vec-dispatch-1 v d)
               (vec-dispatch-1 (vec-dispatch-2 v d) d))
            (vec-dispatch-1 (vec-seek v ds) d))))

   ; vec x fixnum -> local value
   (define (vec-ref-digit v n)
      (type-case v
         ((raw 11)
            (refb v (fxband n 255)))
         ((alloc 43) 
            (vec-ref-digit (ref v 1) n)) ; read the leaf of the node
         ((alloc 11)
            (if (eq? n 255)
               (ref v 256)
               (lets ((n _ (fx+ (fxband n 255) 1)))
                  (ref v n))))
         (else
            (error "bad vector node in vec-ref-digit: type " (type v)))))


   ; find the node holding the last digit and read it
   (define (vec-ref-big v n)
      (vec-ref-digit 
         (vec-dispatch-2
            (vec-seek v (ncdr n)) 
            (ncar n))
         (ncar n)))

   ; vec x n -> vec[n] or fail 
   (define (vec-ref v n)
      (cond
         ((teq? n fix+)
            (cond
               ((teq? v (raw 11)) ; short path for raw byte vector access
                  (refb v n))
               ((lesser? n 256)
                  (vec-ref-digit v n))
               (else
                  (vec-ref-digit (vec-dispatch-2 v n) (fxband n 255)))))
         ((teq? n int+)
            (vec-ref-big v n))
         (else 
            (error "vec-ref: bad index: " n))))

   ;;; searching the leaves containing a pos 
   
   ;; todo: switch vec-ref to use vec-leaf-of for int+ indeces

   (define (vec-leaf-big v n)
      (vec-dispatch-2 (vec-seek v (ncdr n)) (ncar n)))

   (define (vec-leaf-of v n)
      (cond
         ((teq? n fix+)
            (cond
               ((teq? v (raw 11)) v)
               ((lesser? n 256) v)
               (else (vec-dispatch-2 v n))))
         ((teq? n int+)
            (vec-leaf-big v n))
         (else
            (error "vec-leaf-of: bad index: " n))))

   
   ;; others

   (define (vec-len vec)
      (type-case vec
         ((raw 11) (sizeb vec))
         ((alloc 11) (size vec))
         ((alloc 43) (ref vec 2)) ; root has [Leaf Size T1 .. Tn]
         (else (error "vec-len: not a vector: " vec))))



   ;;;
   ;;; Vector validity checking
   ;;;

   (define (vector-leaf-ok? vec)
      (type-case vec
         ((raw 11) True)
         ((alloc 11) True)
         (else 
            False)))

   (define (vector-fields-ok? vec p)
      (if (> p (size vec))
         True
         (let ((sub (ref vec p)))
            (and
               (type-case sub
                  ((raw 11) True)
                  ((alloc 11) True)
                  ((alloc 43)
                     (and
                        (vector-leaf-ok? (ref sub 1))
                        (vector-fields-ok? sub 2)))
                  (else 
                     (error " - bad fiedld " sub)
                     False))
               (vector-fields-ok? vec (+ p 1))))))

   (define (vector-nodes-ok? vec)
      (type-case vec
         ((raw 11) True)
         ((alloc 11) True)
         ((alloc 43) 
            (and (> (size vec) 2)
               (vector-leaf-ok? (ref vec 1)) ;; root leaf
               (number? (ref vec 2)) ;; size
               (vector-fields-ok? vec 3)))
         (else 
            False)))

   (define (valid-vector? vec)
      (and 
         (vector-nodes-ok? vec)))

   ;;;
   ;;; Vector construction
   ;;;

   ; note, a blank vector must use a raw one, since there are no such things as 0-tuples

   (define empty-vector 
      (raw null 11 False))

	(define (list->byte-vector bs)
		(raw bs 11 False))

   (define (make-leaf rvals n raw?)
      (if raw?
         ;; the leaf contains only fixnums 0-255, so make a compact leaf
        (list->byte-vector (reverse rvals)) ;; make node and reverse
        ;; the leaf contains other values, so need full 4/8-byte descriptors
        (listuple 11 n (reverse rvals))))

   (define (byte? val) 
      (and (teq? val fix+) (eq? val (fxband val 255))))

   ;; list -> list of leaf nodes
   (define (chunk-list lst out leaves n raw? len)
      (cond
         ((eq? n 256) ; flush out to leaves
            (let ((leaf (make-leaf out n raw?)))
               (chunk-list lst null (cons (make-leaf out n raw?) leaves) 0 True (+ len n))))
         ((null? lst) ; partial (last) leaf
            (if (null? out)
               (values (reverse leaves) len)
               (values (reverse (cons (make-leaf out n raw?) leaves)) (+ len n))))
         ((pair? lst)
            (if raw?
               (chunk-list (cdr lst) (cons (car lst) out) leaves (+ n 1) (byte? (car lst)) len)
               (chunk-list (cdr lst) (cons (car lst) out) leaves (+ n 1) False len)))
         (else (chunk-list (lst) out leaves n raw? len))))

   (define (grab l n)
      (let loop ((l l) (n n) (taken null))
         (cond
            ((null? l) (values (reverse taken) l))
            ((eq? n 0) (values (reverse taken) l))
            (else
               (loop (cdr l) (- n 1) (cons (car l) taken))))))
            
   (define (merge-each l s)
      (cond
         ((null? l) null)
         ((null? s) l)
         ((number? (car l))
            (cons (car l)
               (merge-each (cdr l) s)))
         (else
            (lets ((these s (grab s 256)))
               (cons
                  (listuple 43 (+ 1 (length these)) (cons (car l) these))
                  (merge-each (cdr l) s))))))

   (define (merger l n)
      (if (null? l)
         null
         (lets ((these l (grab l n)))
            (if (null? l)
               these
               (merge-each these (merger l (* n n)))))))

; start with power 1 and blank root
; grab power nodes from list -> these others
;   if others is null, return these
;   otherwise recurse on others -> others
;   for each of these
;     grab a max of 256 things from others -> below others
;     make a dispatch node for these and below
;     loop and return the list of these

   (define (cut-at lst pos out)
      (cond
         ((null? lst)
            (values (reverse out) null))
         ((eq? pos 0)
            (values (reverse out) lst))
         (else
            (cut-at (cdr lst) (- pos 1) (cons (car lst) out)))))

   (define (levels lst width)
      (lets ((here below (cut-at lst width null)))
         (if (null? below)
            (list here)
            (cons here (levels below (* width 256)))))) ; everything below the first level branches 256-ways

   (define (merge-levels lst)
      (foldr 
         (λ (this below)
            ;; this = list of leaves which will be as such or in dispatch nodes 
            ;;        on this level of the tree
            ;; below = possible list of nodes up to 256 of which will be attached 
            ;;         as subtrees to each leaf of this level, starting from left
            (let loop ((below below) (this this)) 
               (cond
                  ((null? below) this)
                  ((null? this)
                     (error "out of leaves before out of data: " (length below)))
                  ;((number? (car this)) ;; skip size field at roo
                  ;   (cons (car this) (loop below (cdr this))))
                  (else
                     (lets ((here below (cut-at below 256 null)))
                        ;; attach up to 256 subtrees to this leaf
                        (cons
                           (listuple 43 (+ 1 (length here)) (cons (car this) here))
                           (loop below (cdr this))))))))
         null (levels lst 255)))

   ; handle root here, since it is special in having 255 subtrees only (0-slot is empty and has size)
   (define (merge-chunks ll len)
      (cond
         ((null? ll)
            ;; no leaves, no data
            empty-vector)
         ((null? (cdr ll))
            ;; just one leaf, so it is also the vector
            (car ll))
         (else
            ;; the top node is special in that it has the size field
            ;; others can be computed easily recursively
            (lets
               ((low (car ll))                  ;; first leaf data, places 0-255
                (fields (cdr ll))    ;; fill in the length of the vector at dispatch position 0
                (subtrees (merge-levels fields))) ;; construct the subtrees
               (listuple 43 (+ 2 (length subtrees)) (ilist low len subtrees))))))

   
   (define (list->vector l)
      (if (null? l)
         empty-vector
         ;; leaves are chunked specially, so do that in a separate pass. also 
         ;; compute length to avoid possibly forcing a computation twice.
         (lets ((chunks len (chunk-list l null null 0 True 0)))
            ;; convert the list of leaf vectors to a tree
            (merge-chunks chunks len))))

   ;; deprecated
   ;(define bytes->vector list->vector)
   ;   ;(raw bs 11 False)

   (define (vector? x) ; == raw or a variant of major type 11?
      (cond
         ((teq? x (raw 11)) True)   ; leaf byte vector
         ((teq? x (alloc 11)) True) ; wide leaf
         ((teq? x (alloc 43)) True) ; root dispatch node
         (else False)))

   ;;;
   ;;; Vector iterators
   ;;;

   ;; iter - iterate forwards (leaves from left to right, tree breadth first left to right)

   ;(define (iter-raw-leaf v p e tl)
   ;   (if (eq? p e)
   ;      tl
   ;      (lets ((n _ (fx+ p 1)))
   ;         (cons (refb v p)
   ;            (iter-raw-leaf v n e tl)))))

   (define (iter-raw-leaf v p tl)
      (if (eq? p 0)
         (cons (refb v p) tl)
         (lets ((n _ (fx- p 1)))
            (iter-raw-leaf v n (cons (refb v p) tl)))))

   ;(define (iter-leaf v p e tl)
   ;   (if (eq? p e)
   ;      (cons (ref v p) tl)
   ;      (lets ((n _ (fx+ p 1)))
   ;         (cons (ref v p) (iter-leaf v n e tl)))))

   (define (iter-leaf v p tl)
      (if (eq? p 0)
         tl
         (lets ((n _ (fx- p 1)))
            (iter-leaf v n (cons (ref v p) tl)))))

   (define (iter-leaf-of v tl)
      (type-case v
         ((raw 11) 
            ;; only this ne can be empty
            (let ((s (sizeb v)))
               (if (eq? s 0)
                  tl
                  (iter-raw-leaf v (- s 1) tl))))
         ((alloc 11) (iter-leaf v (size v) tl))
         ((alloc 43) (iter-leaf-of (ref v 1) tl))
         (else tl))) ; size field -> number

   (define (vec-iter v)
      (let loop ((end (vec-len v)) (pos 0))
         (let ((this (vec-leaf-of v pos)))
            (iter-leaf-of this
               (λ () (let ((pos (+ pos 256))) (if (< pos end) (loop end pos) null)))))))

   (define (iter-leaf-range v p n t)
      (if (eq? n 0)
         t
         (pair (vec-ref v p)
            (iter-leaf-range v (+ p 1) (- n 1) t))))

   (define (iter-range-really v p n)
      (let ((start (band p #xff)))
         (cond
            ((eq? start 0)
               ;; read leaf from beginning
               (if (> n 255)
                  ;; iter a full leaf (usual suspect)
                  (iter-leaf-of (vec-leaf-of v p)
                     (λ () (iter-range-really v (+ p 256) (- n 256))))
                  ;; last leaf reached, iter prefix and stop
                  (iter-leaf-range (vec-leaf-of v p) 0 n null)))
            ((eq? n 0) null)
            ((lesser? n (- 256 start))
               ;; the whole range is in a part of this leaf
               (iter-leaf-range (vec-leaf-of v p) start n null))
            (else
               ;; this is the first leaf. iter a suffix of it.
               (lets
                  ((n-here (- 256 start))
                   (n-left (- n n-here)))
                  (iter-leaf-range (vec-leaf-of v p) start n-here
                     (λ () (iter-range-really v (+ p n-here) n-left))))))))

   (define (vec-iter-range v p e)
      (if (<= e (vec-len v))
         (cond
            ((< p e) 
               (iter-range-really v p (- e p)))
            ((= p e) null)
            (else (error "vec-iter-range: bad range " (cons p e))))
         (error "vec-iter-range: end outside of vector: " e)))

   ;; iterate back to front

   ;; todo: vec-iterr could also chunk whole leaves directly with fixnums like vec-iterr
   (define (iterr-raw-leaf v last tl)
      (if (eq? last 0) 
         tl
         (lets ((last (- last 1)))
            (cons (refb v last) 
               (λ () (iterr-raw-leaf v last tl))))))

   (define (iterr-leaf v p tl)
      (if (eq? p 1)
         (cons (ref v p) tl)
         (cons (ref v p) (λ () (iterr-leaf v (- p 1) tl)))))

   (define (iterr-any-leaf v tl)
      (type-case v
         ((raw 11) (iterr-raw-leaf v (sizeb v) tl))
         ((alloc 11) (iterr-leaf v (size v) tl))
         ((alloc 43) (iterr-any-leaf (ref v 1) tl))
         (else tl))) ; size field in root is a number → skip
   
   (define (vec-iterr-loop v p)
      (if (teq? p fix-) ; done
         null
         (iterr-any-leaf (vec-leaf-of v p)
            (λ () (vec-iterr-loop v (- p 256))))))

   (define (vec-iterr v)
      (lets 
         ((end (vec-len v))
          (last (band end #xff)))
         (cond
            ((eq? last 0) ; vec is empty or ends to a full leaf
               (if (eq? end 0) ; blank vector
                  null
                  (vec-iterr-loop v (- end 1)))) ; start from previous leaf
            (else 
               (vec-iterr-loop v (- end 1))))))

   ;; vector folds

   (define (vec-fold  op st vec) (lfold  op st (vec-iter  vec)))
   (define (vec-foldr op st vec) (lfoldr op st (vec-iterr vec)))

   ;; list conversions

   (define (vec->list vec) (vec-foldr cons null vec))

   (define vector->list vec->list)

   (define (leaf-data leaf)
      (if (teq? leaf (raw 11)) ;; a raw leaf with plain data
         leaf
         (ref leaf 1)))

   ;;;
   ;;; vector map
   ;;;

   ;; fixme: vec-map <- placeholder
   (define (vec-map fn vec)
       (list->vector (lmap fn (vec-iter vec))))

   ;(define (vec-map vec fn)
   ;   (type-case vec
   ;      ((raw 11) 
   ;         (lets
   ;            ((bs (iterr-raw-leaf v (sizeb v) null))
   ;             (bsp (map fn bs)))
   ;            (vals->leaf (map fn bs)))) ; <- this should be able to do (list->vector (map fn bs))
   ;      ((alloc 11) (iterr-leaf v (size v) tl))
   ;      ((alloc 43) (iterr-any-leaf (ref v 1) tl))
   ;      (else tl)))

   ;;;
   ;;; Vector ranges
   ;;;

   ;; fixme: proper vec-range not implemented 
   (define (vec-range-naive vec from to) ; O(m log n) 
      (list->vector
         (map (λ (p) (vec-ref vec p)) (iota from 1 to))))

   (define vec-range vec-range-naive)


   ;;;
   ;;; Vector leaf data stream (mainly useful for IO)
   ;;;

   ;; vec → a stream of leaves
   (define (vec-leaves vec)
      (let ((end (vec-len vec)))
         (let loop ((pos 0))
            (if (< pos end)
               (let ((data (leaf-data (vec-leaf-of vec pos))))
                  (pair data (loop (+ pos 256))))
               null))))

   ;; fixme: temporary vector append!
   (define (vec-cat a b)
      (list->vector
         (append
            (vector->list a)
            (vector->list b))))

   (define (vec-rev a)
      (list->vector
         (vec-iterr a)))

   ;;;
   ;;; Vector construction
   ;;;

	;; todo: start adding Vector-style constructors at some point
   (define-syntax vector
      (syntax-rules ()
         ((vector . things)
            (list->vector (list . things)))))

   (define render
      (λ (self obj tl)
         (cond
				((byte-vector? obj)
					(self self (cons "BVector" (vector->list obj)) tl))
				((vector? obj)
					(self self (cons "Vector" (vector->list obj)) tl))
            (else
               (render self obj tl)))))

)

