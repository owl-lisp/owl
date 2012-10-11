(define-library (owl ff-ng)
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
      
      getf       ; (getf ff key) == (get ff key #false)
      )

   (import 
      (owl defmac)
      (owl io)
      (owl list))

   (begin

      ; low 2 bits of ff type (as opposed to high 3 previously) have special meaning
      (define redness   #b10)
      (define rightness #b01)

      (define (mkblack l k v r)
         (if (eq? l #empty)
            (if (eq? r #empty)
               (mkt type-ff k v)
               (mkt type-ff-r k v r))
            (if (eq? r #empty)
               (mkt type-ff k v l)
               (mkt type-ff k v l r))))

      (define (mkred l k v r)
         (if (eq? l #empty)
            (if (eq? r #empty)
               (mkt type-ff-red k v)
               (mkt type-ff-red-r k v r))
            (if (eq? r #empty)
               (mkt type-ff-red k v l)
               (mkt type-ff-red k v l r))))

      ;; local temporary helper because all branches are the wrong way around
      (define-syntax nonempty? 
         (syntax-rules ()
            ((nonempty? x) (not (eq? x #empty)))))

      ;; not red → black or #empty
      (define-syntax red?
         (syntax-rules ()
            ((red? node) (eq? redness (fxband (type node) redness))))) ;; false for black nodes and #empty
     
      ;; does a (non-empty) node of size 3 have a right child? 2 doesn't and 4 has.
      (define-syntax right?
         (syntax-rules ()
            ((right? node) (eq? rightness (fxband (type node) rightness)))))

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
      
      (define (ff? obj)
         (if (eq? obj #empty)
            #true
            (eq? 24 (fxband (type obj) #b1111100))))

      ;; old prim
      (define-syntax wiff
         (syntax-rules ()
            ((wiff (name l k v r) . rest)
               (lets ((l k v r (explode name))) . rest))))

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

      (define (mkblack-bleft left key val right)
         (if (red? left)
            (if (red? right)
               (mkred (color-black left) key val (color-black right))
               (wiff (left ll lk lv lr)
                  (cond
                     ((red? ll)   ; case 1
                        (wiff (ll a xk xv b)
                           (let ((yk lk) (yv lv) (c lr))
                              (mkred (mkblack a xk xv b) yk yv (mkblack c key val right)))))
                     ((red? lr) ; case 2
                        (wiff (lr b yk yv c)
                           (let ((a ll) (xk lk) (xv lv))
                              (mkred (mkblack a xk xv b) yk yv (mkblack c key val right)))))
                     (else
                        (mkblack left key val right)))))
            (mkblack left key val right)))

      (define (mkblack-bright left key val right)
         (if (red? right)
            (wiff (right rl rk rv rr)
               (cond
                  ((red? rl) ; case 3
                     (wiff (rl b yk yv c)
                        (let ((zk rk) (zv rv) (d rr))
                           (mkred 
                              (mkblack left key val b)
                              yk yv
                              (mkblack c zk zv d)))))
                  ((red? rr) ; case 4
                     (wiff (rr c zk zv d)
                        (let ((b rl) (yk rk) (yv rv))
                           (mkred
                              (mkblack left key val b)
                              yk yv
                              (mkblack c zk zv d)))))
                  (else
                     (mkblack left key val right))))
            (mkblack left key val right)))
                  
      (define (putn node key val)
         (if (eq? node #empty)
            (mkred #false key val #false)
            (if (red? node)
               (wiff (node left this this-val right)
                  (cond
                     ((lesser? key this)
                        (mkred (putn left key val) this this-val right))
                     ((eq? key this)
                        (mkred left key val right))
                     (else
                        (mkred left this this-val (putn right key val)))))
               (wiff (node left this this-val right)
                  (cond
                     ((lesser? key this)
                        (mkblack-bleft (putn left key val) this this-val right))
                     ((eq? key this)
                        (mkblack left key val right))
                     (else
                        (mkblack-bright left this this-val (putn right key val))))))))

      (define (get ff key def)
         (if ff
            (wiff (ff l k v r)
               (cond
                  ((eq? key k) v)
                  ((lesser? key k) (get l key def))
                  (else (get r key def))))
            def))

      ;(define (get ff key def) 
      ;   (ff key def))

      ;; FIXME, CHECK
      ;; ff key val -> ff' | #false
      (define (ff-update ff key val)
         (if (eq? ff #empty)
            #false
            (wiff (ff l k v r)
               (cond
                  ((lesser? key k)
                     (if (not (eq? l #empty))   ; #(k v l ...)
                        (set ff 3 (ff-update l key val))
                        #false))
                  ((eq? key k)
                     ; #(k v ...)
                     (set ff 2 val))
                  ((not (eq? l #empty))
                     (if (not (eq? r #empty))   ; #(k v l r)
                        (set ff 4 (ff-update r key val))
                        #false))
                  ((not (eq? r #empty))
                     (set ff 3 (ff-update r key val)))
                  (else #false)))))

      ;; TODO: benchmark fupd+ins vs put with real programs
      (define (put ff key val)
         (print (list 'put ff key val))
         (let ((ff (putn ff key val)))
            (print "made")
            (if (red? ff) (color-black ff) ff)))

      (define fupd ff-update)

      ;;;
      ;;; FF Utilities
      ;;;

      ;; FIXME, use types directly later
      (define (ff-foldr op state tree)
         (if (nonempty? tree)
            (wiff (tree l k v r)
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
                     (op state k v))))
            state))

      ;; FIXME, as above
      (define (ff-fold op state tree)
         (if (nonempty? tree)
            (wiff (tree l k v r)
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
            (wiff (tree l k v r)
               (ff-iterate l
                  (cons (cons k v)
                     (λ () (ff-iterate r tl)))))
            tl))

       ;; iterate key-value pairs in reverse order
       (define (ff-iterrate tree tl)
         (if (nonempty? tree)
            (wiff (tree l k v r)
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
            (wiff (ff l k v r)
               (if (red? ff)
                  (mkred   (ff-map l op) k (op k v) (ff-map r op))
                  (mkblack (ff-map l op) k (op k v) (ff-map r op))))))

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
               (mkred (color-black left) key val right))
            ((red? right)
               (wiff (right r zk zv c)
                  (wiff (r a yk yv b)
                     (mkred
                        (mkblack left key val a)
                        yk yv
                        (mkblack-bright b zk zv (color-red c))))))
            (else
               (mkblack-bright left key val (color-red right)))))

      (define (ball-right left key val right)
         (cond
            ((red? right)
               (mkred left key val (color-black right)))
            ((red? left)
               (wiff (left a xk xv b)
                  (wiff (b b yk yv c)
                     (mkred 
                        (mkblack-bleft (color-red a) xk xv b)
                        yk yv
                        (mkblack c key val right)))))
            (else
               (mkblack-bleft (color-red left) key val right))))

      ;; converted old primops, could also test types and ref the right spot
      (define (ffcar node) (wiff (node l k v r) l))
      (define (ffcdr node) (wiff (node l k v r) r))

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
                        (wiff (middle ml mk mv mr)
                           (wiff (left ll lk lv lr)
                              (wiff (right rl rk rv rr)
                                 (mkred
                                    (mkred ll lk lv ml)
                                    mk mv
                                    (mkred mr rk rv rr)))))
                        (wiff (left a xk xv b)
                           (wiff (right c yk yv d)
                              (mkred a xk xv 
                                 (mkred middle yk yv d))))))
                  (wiff (left a xk xv b)
                     (mkred a xk xv (app b right)))))
            ((red? right)
               (wiff (right rl rk rv rr)
                  (mkred (app left rl) rk rv rr)))
            (else
               ;;; both are black
               (let ((middle (app (ffcdr left) (ffcar right))))
                  (if (red? middle)
                     (wiff (middle ml mk mv mr)
                        (wiff (left ll lk lv lr)
                           (wiff (right rl rk rv rr)
                              (mkred
                                 (mkblack ll lk lv ml)
                                 mk mv
                                 (mkblack mr rk rv rr)))))
                     (wiff (left ll lk lv lr)
                        (wiff (right rl rk rv rr)
                           (ball-left
                              ll lk lv
                              (mkblack middle rk rv rr)))))))))

      (define (deln ff key)
         (if (nonempty? ff)
            (wiff (ff left this-key val right)
               (cond
                  ((lesser? key this-key)
                     (let ((sub (deln left key)))
                        (cond
                           ((eq? sub left)   
                              ff)
                           ((red? left)
                              (mkred     sub this-key val right))
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
                              (mkred left this-key val sub))
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

(print 
   (get
      (fold 
         (λ (ff x) 
            (print (list 'putting x 'to ff))
            (put ff x (if (= x 4) 'hit (+ x 100))))
         #empty
         (iota 0 1 10))
      4 'miss))
