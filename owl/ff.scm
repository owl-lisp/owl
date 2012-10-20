;;;
;;; Finite functions (or red-black key-value maps)
;;;

;; fixme: ff unit tests went missing at some point. add with lib-compare vs naive alists.
;; fixme: ffc[ad]r are no longer needed as primitives

; Note: objects in owl are *ordered*. The gc was specifically
; designed to preserve order in order to improve locality and
; allow O(log n) maps (called ffs to avoid collision with
; the standard map function) of arbitrary objects.

(define-library (owl ff)

   (export 
      get         ; O(log2 n), ff x key x default-value -> value | default-value 
      put         ; O(log2 n), ff x key x value -> ff'
      del         ; O(log2 n), ff x key -> ff' 
      keys        ; O(n), ff → (key ...) 
      ff-update   ; O(log2 n), ff x key x value -> ff' | fail if key not in ff
      fupd        ; alias for ff-update 
                  ;    - no rebalancing, just walk to leaf and update value
      ff-union ff-diff ; TEMP
      ff-fold ff-foldr ; like list folds but (op st key val) -> st'
      ff-map      ; like list map but (op key val) -> val'
		ff-iter     ; ff -> ((key . value) ...) stream (in order)
      ff?
      ff-singleton? ; ff → bool (has just one key?)
      list->ff ff->list 
      ff->sexp
      ff-ok?
      empty
      empty?
      
      getf       ; (getf ff key) == (get ff key #false)

      )

   (import 
      (owl defmac)
      (owl list)
      (only (owl syscall) error)
      )

   (begin

      ; low 2 bits of ff type (as opposed to high 3 previously) have special meaning
      (define redness   #b10)
      (define rightness #b01)

      (define empty #empty)

      (define (empty? x) (eq? x empty))

      ;; shadowed below
      (define (black l k v r)
         (if (eq? l #empty)
            (if (eq? r #empty)
               (mkt type-ff k v)
               (mkt type-ff-r k v r))
            (if (eq? r #empty)
               (mkt type-ff k v l)
               (mkt type-ff k v l r))))

      ;; shadowed below
      (define (red l k v r)
         (if (eq? l #empty)
            (if (eq? r #empty)
               (mkt type-ff-red k v)
               (mkt type-ff-red-r k v r))
            (if (eq? r #empty)
               (mkt type-ff-red k v l)
               (mkt type-ff-red k v l r))))

      ;; vm versions
      (define black mkblack)
      (define red mkred)

      ;; local temporary helper because all branches are the wrong way around
      (define-syntax nonempty? 
         (syntax-rules ()
            ((nonempty? x) (not (eq? x #empty)))))

      ;; not red → black or #empty
      (define-syntax red?
         (syntax-rules ()
            ((red? node) (eq? redness (fxband (type node) redness))))) ;; false for black nodes and #empty
     
      ;; does a (non-empty) red or black node of size 3 have a right child? 2 never does and 4 always has
      (define-syntax right?
         (syntax-rules ()
            ((right? node) (eq? rightness (fxband (type node) rightness)))))

      ;; preserve structure, intended for debugging only
      (define (color ff)
         (if (red? ff) 'R 'B))

      (define (ff->sexp ff)
         (if (eq? ff #empty)
            ff
            (case (size ff)
               (2 (lets ((k v ff)) (list (color ff) k)))
               (3 (lets ((k v x ff)) 
                  (if (right? ff) 
                     (list (color ff) k '-> (ff->sexp x))
                     (list (color ff) (ff->sexp x) '<- k))))
               (4 (lets ((k v l r ff))
                  (list (color ff) (ff->sexp l) '<- k '-> (ff->sexp r))))
               (else
                  (list 'BAD 'NODE ff)))))

      (define (explode node)
         (case (size node)
            (2 (lets ((k v node)) (values #empty k v #empty)))
            (3 (lets ((k v x node))
                  (if (right? node)
                     (values #empty k v x)
                     (values x k v #empty))))
            (else
               (lets ((k v l r node))
                  (values l k v r)))))

      (define (red-red-violation? ff)
         (if (eq? ff #empty)
            #false
            (lets ((l k v r (explode ff)))
               (or
                  (and (red? ff) (or (red? l) (red? r)))
                  (red-red-violation? l)
                  (red-red-violation? r)))))
   
      ;; fixnum addition, math not defined yte
      (define (f+ a b)
         (lets ((c _ (fx+ a b))) c))

      ;; ff → nat | #false if difference spotted
      (define (black-depth ff)
         (if (eq? ff #empty)
            0
            (lets 
               ((l k v r (explode ff))
                (ld (black-depth l))
                (rd (black-depth r)))
               (if (and ld rd (eq? ld rd))
                  (f+ ld (if (red? ff) 0 1))
                  #false))))

      ;; are invariants in order
      (define (ff-ok? ff)
         (cond
            ((not (black-depth ff))
               ;(print "FF ERROR, black depths differ")
               #false)
            ((red-red-violation? ff)
               ;(print "FF ERROR, red-red violation")
               #false)
            (else 
               #true)))

      (define (ff? obj)
         (if (eq? obj #empty)
            #true
            (eq? 24 (fxband (type obj) #b1111100))))

      ;; bytecode above, vm primitive below
      (define-syntax with-ff
         (syntax-rules ()
            ;((with-ff (name l k v r) . rest)
            ;   (lets ((l k v r (explode name))) . rest))
            ((with-ff (name l k v r) . rest)
               (ff-bind name (lambda (l k v r) . rest)))
            ))

      ;; toggle redness, name of old prim
      (define-syntax ff-toggle
         (syntax-rules ()
            ((ff-toggle node)
               (cast node (fxbxor (type node) redness)))))

      ;; FIXME: misleading names!
      (define-syntax color-black (syntax-rules () ((color-black x) (ff-toggle x))))
      (define-syntax color-red   (syntax-rules () ((color-red x)   (ff-toggle x))))


      ;;;
      ;;; Balancing moves
      ;;;

      ; cases 1        2
      ;       Z        Z
      ;      / \      / \           (Y)
      ;    (Y)  d   (X)  d          / \
      ;    / \      / \      ->    X   Z
      ;  (X)  c    a  (Y)         / | | \
      ;  / \          / \        a  b c  d
      ; a   b        b   c
      ;
      ;   3         4
      ;   X         X
      ;  / \       / \
      ; a  (Z)    a  (Y)
      ;    / \       / \
      ;  (Y)  d     b  (Z)
      ;  / \           / \
      ; b   c         c   d

      (define (black-bleft left key val right)
         (if (red? left)
            (if (red? right)
               (red (color-black left) key val (color-black right))
               (with-ff (left ll lk lv lr)
                  (cond
                     ((red? ll)   ; case 1
                        (with-ff (ll a xk xv b)
                           (let ((yk lk) (yv lv) (c lr))
                              (red (black a xk xv b) yk yv (black c key val right)))))
                     ((red? lr) ; case 2
                        (with-ff (lr b yk yv c)
                           (let ((a ll) (xk lk) (xv lv))
                              (red (black a xk xv b) yk yv (black c key val right)))))
                     (else
                        (black left key val right)))))
            (black left key val right)))

      (define (black-bright left key val right)
         (if (red? right)
            (with-ff (right rl rk rv rr)
               (cond
                  ((red? rl) ; case 3
                     (with-ff (rl b yk yv c)
                        (let ((zk rk) (zv rv) (d rr))
                           (red 
                              (black left key val b)
                              yk yv
                              (black c zk zv d)))))
                  ((red? rr) ; case 4
                     (with-ff (rr c zk zv d)
                        (let ((b rl) (yk rk) (yv rv))
                           (red
                              (black left key val b)
                              yk yv
                              (black c zk zv d)))))
                  (else
                     (black left key val right))))
            (black left key val right)))
                  
      (define (putn node key val)
         (if (eq? node #empty)
            (red #empty key val #empty)
            (if (red? node)
               (with-ff (node left this this-val right)
                  (cond
                     ((lesser? key this)
                        (red (putn left key val) this this-val right))
                     ((eq? key this)
                        (red left key val right))
                     (else
                        (red left this this-val (putn right key val)))))
               (with-ff (node left this this-val right)
                  (cond
                     ((lesser? key this)
                        (black-bleft (putn left key val) this this-val right))
                     ((eq? key this)
                        (black left key val right))
                     (else
                        (black-bright left this this-val (putn right key val))))))))

      ;; bytecoded get
      (define (get ff key def)
         (if (eq? ff #empty)
            def
            (let ((this-k (ref ff 1)))
               (cond
                  ((eq? this-k key)
                     (ref ff 2))
                  ((lesser? key this-k)
                     ;; go left if possible
                     (case (size ff)
                        (4 (get (ref ff 3) key def))
                        (2 def)
                        (else 
                           (if (right? ff)
                              def
                              (get (ref ff 3) key def)))))
                  (else
                     ;; go right if possible
                     (case (size ff)
                        (4 (get (ref ff 4) key def))
                        (2 def)
                        (else 
                           (if (right? ff)
                              (get (ref ff 3) key def)
                              def))))))))

      ;; primitive get via vm
      ;(define (get ff key def) (ff key def))

      ;; ff key val -> ff', *key must be in ff*
      (define (ff-update ff key val)
         (if (eq? ff #empty)
            (error "fupd: not there: " key)
            (let ((this (ref ff 1)))
               (if (eq? key this)
                  (set ff 2 val) ;; key and value have fixed position
                  (case (size ff)
                     (2 (ff-update #empty key val)) ;; fail
                     (3 (set ff 3 (ff-update (ref ff 3) key val))) ;; must be here due to contract
                     (else
                        (if (lesser? key this)
                           (set ff 3 (ff-update (ref ff 3) key val))
                           (set ff 4 (ff-update (ref ff 4) key val)))))))))

      (define fupd ff-update)

      ;; TODO: benchmark fupd+ins, get->fupd\/ins vs put with real programs
      (define tag "foo")

      (define (put ff key val)
         (let ((res (ff key tag)))    ;; check if the key is already in ff
            (if (eq? res tag)         ;; not there → insert and rebalance
               (let ((ff (putn ff key val)))
                  (if (red? ff) (color-black ff) ff))
               (fupd ff key val))))   ;; path copy and update only


      ;;;
      ;;; FF Utilities
      ;;;

      ;; FIXME, use types directly later
      (define (ff-foldr op state tree)
         (if (eq? tree #empty)
            state
            (with-ff (tree l k v r)
               (if (nonempty? l)
                  (if (nonempty? r)
                     (lets
                        ((state (ff-foldr op state r))
                         (state (op state k v)))
                        (ff-foldr op state l))
                     (let ((state (op state k v)))
                        (ff-foldr op state l)))
                  (if (nonempty? r)
                     (let ((state (ff-foldr op state r)))
                        (op state k v))
                     (op state k v))))))

      ;; FIXME, as above
      (define (ff-fold op state tree)
         (if (nonempty? tree)
            (with-ff (tree l k v r)
               (if (nonempty? l)
                  (if (nonempty? r)
                     (lets
                        ((state (ff-fold op state l))
                         (state (op state k v)))
                        (ff-fold op state r))
                     (let
                        ((state (ff-fold op state l)))
                        (op state k v)))
                  (if (nonempty? r)
                     (let ((state (op state k v)))
                        (ff-fold op state r))
                     (op state k v))))
            state))
      
       ;; iterate key-value pairs in order
       (define (ff-iterate tree tl)
         (if (nonempty? tree)
            (with-ff (tree l k v r)
               (ff-iterate l
                  (cons (cons k v)
                     (λ () (ff-iterate r tl)))))
            tl))

       ;; iterate key-value pairs in reverse order
       (define (ff-iterrate tree tl)
         (if (nonempty? tree)
            (with-ff (tree l k v r)
               (ff-iterrate r
                  (cons (cons k v)
                     (λ () (ff-iterrate l tl)))))
            tl))

      (define (ff-iter  tree) (ff-iterate  tree null))
      (define (ff-iterr tree) (ff-iterrate tree null))

      ;; note: ff-map will switch argument order in the generic equivalent
      ;; fixme, also much faster if types are used directly
      (define (ff-map ff op)
         (if (eq? ff #empty)
            #empty
            (with-ff (ff l k v r)
               (if (red? ff)
                  (red   (ff-map l op) k (op k v) (ff-map r op))
                  (black (ff-map l op) k (op k v) (ff-map r op))))))

      ;; could benchmark if sort + grow from bottom is faster
      (define (list->ff lst)
         (fold
            (λ (ff node)
               (put ff (car node ) (cdr node)))
            #empty
            lst))

      (define (ff->list ff)
         (ff-foldr
            (λ (lst k v)
               (cons (cons k v) lst))
            null ff))


      ;;; 
      ;;; Deletion 
      ;;; 

      (define (ball-left left key val right)
         (cond
            ((red? left)
               (red (color-black left) key val right))
            ((red? right)
               (with-ff (right r zk zv c)
                  (with-ff (r a yk yv b)
                     (red
                        (black left key val a)
                        yk yv
                        (black-bright b zk zv (color-red c))))))
            (else
               (black-bright left key val (color-red right)))))

      (define (ball-right left key val right)
         (cond
            ((red? right)
               (red left key val (color-black right)))
            ((red? left)
               (with-ff (left a xk xv b)
                  (with-ff (b b yk yv c)
                     (red 
                        (black-bleft (color-red a) xk xv b)
                        yk yv
                        (black c key val right)))))
            (else
               (black-bleft (color-red left) key val right))))

      ;; converted old primops, could also test types and ref the right spot
      (define (ffcar node) (with-ff (node l k v r) l))
      (define (ffcdr node) (with-ff (node l k v r) r))

      (define (app left right)
         (cond
            ;;; if other branch is empty
            ((eq? left #empty) right)
            ((eq? right #empty) left)
            ;;; otherwise full nodes
            ((red? left)
               (if (red? right)
                  (let ((middle (app (ffcdr left) (ffcar right))))
                     (if (red? middle)
                        (with-ff (middle ml mk mv mr)
                           (with-ff (left ll lk lv lr)
                              (with-ff (right rl rk rv rr)
                                 (red
                                    (red ll lk lv ml)
                                    mk mv
                                    (red mr rk rv rr)))))
                        (with-ff (left a xk xv b)
                           (with-ff (right c yk yv d)
                              (red a xk xv 
                                 (red middle yk yv d))))))
                  (with-ff (left a xk xv b)
                     (red a xk xv (app b right)))))
            ((red? right)
               (with-ff (right rl rk rv rr)
                  (red (app left rl) rk rv rr)))
            (else
               ;;; both are black
               (let ((middle (app (ffcdr left) (ffcar right))))
                  (if (red? middle)
                     (with-ff (middle ml mk mv mr)
                        (with-ff (left ll lk lv lr)
                           (with-ff (right rl rk rv rr)
                              (red
                                 (black ll lk lv ml)
                                 mk mv
                                 (black mr rk rv rr)))))
                     (with-ff (left ll lk lv lr)
                        (with-ff (right rl rk rv rr)
                           (ball-left
                              ll lk lv
                              (black middle rk rv rr)))))))))

      (define (deln ff key)
         (if (nonempty? ff)
            (with-ff (ff left this-key val right)
               (cond
                  ((lesser? key this-key)
                     (let ((sub (deln left key)))
                        (cond
                           ((eq? sub left)   
                              ff)
                           ((red? left)
                              (red     sub this-key val right))
                           (else
                              (ball-left sub this-key val right)))))
                  ((eq? key this-key)
                     (app left right))
                  (else
                     (let ((sub (deln right key)))
                        (cond
                           ((eq? sub right)
                              ff)
                           ((red? right)
                              (red left this-key val sub))
                           (else
                              (ball-right left this-key val sub)))))))
            ff))


      (define (del ff key)
         (let ((ff (deln ff key)))
            (if (red? ff)
               (color-black ff)
               ff)))
         

      ;;;
      ;;; FIXME bad hacks
      ;;;

      ;; todo: placeholder ff-union
      (define (ff-union a b collide)
         (ff-fold
            (λ (a bk bv)
               (let ((av (get a bk #false)))
                  (if av ;; <- BUG, does not imply bk is not set
                     (put a bk (collide av bv))
                     (put a bk bv))))
            a b))

      ;; todo: placeholder ff-diff
      (define (ff-diff a b)
         (ff-fold (λ (a b _) (if (get a b #false) (del a b) a)) a b))

      ;; just one value? == is the root-node a black key-value pair
      (define (ff-singleton? ff)
         (eq? (size ff) 2))

      (define-syntax getf
         (syntax-rules ()
            ((getf ff key) (get ff key #false))))

      (define (keys ff)
         (ff-foldr (λ (out k v) (cons k out)) null ff))

))

;(import (owl ff))
;
;(print 
;   (get
;      (fold 
;         (λ (ff x) 
;            (if (not (ff-ok? ff))
;               (print "FF BAD " (ff->sexp ff)))
;            (put ff x (if (= x 42) 'correct (+ x 100))))
;         #empty
;         (iota 0 1 100))
;      42 'miss))
;
;
;(lets
;   ((rs (seed->rands (time-ms)))
;    (rs keys (random-permutation rs (iota 0 1 10)))
;    (pairs (map (λ (x) (cons x (+ x 100))) keys))
;    (ff (list->ff pairs))
;    (_ (print (ff->list ff)))
;    (rs keys (random-permutation rs keys))
;    (ff (fold (λ (ff x) (put ff x (+ x 200))) ff keys))
;    (rs keys (random-permutation rs keys)))
;   (for-each
;      (λ (key)
;         (let ((val (get ff key 'missing)))
;            (if (= val (+ key 200))
;               (print " ff[" key "] = " val)
;               (print " ff[" key "] = " val " <- WRONG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))))
;      keys)
;   (print (fold (λ (ff key) (del ff key)) ff keys)))
;
;
;
;
;
