;;;
;;; Finite functions (or red-black key-value maps)
;;;

;; fixme: ff unit tests went missing at some point. add with lib-compare vs naive alists.
;; fixme: ffc[ad]r are no longer needed as primitives

; Note: objects in owl are *ordered*. The gc was specifically
; designed to preserve order in order to improve locality and 
; allow O(log n) maps (called ffs to avoid collision with 
; the standard map function) of arbitrary objects. 

;; transitional association list based ff library which also uses #false for empty, but doesn't use any ff nodes
(define-library (owl ff-transitional)

   (export get put del keys ff-update fupd ff-union ff-diff ff-fold ff-foldr ff-map ff-iter ff? ff-singleton? list->ff ff->list empty-ff getf)

   (import
      (owl defmac)
      (owl list))

   (begin

      ;; O(n)!
      (define (get ff k d)
         (cond
            ((not ff) d)
            ((null? ff) d)
            (else
               (let ((this (caar ff)))
                  (cond
                     ((eq? this k) (cdar ff))
                     ((lesser? this k) (get (cdr ff) k d))
                     (else d))))))

      ;; O(n)!
      (define (put ff k v)
         (cond
            ((not ff)
               (put null k v))
            ((null? ff)
               (list (cons k v)))
            (else
               (let ((this (caar ff)))
                  (cond
                     ((eq? this k)
                        (cons (cons k v) (cdr ff)))
                     ((lesser? this k)
                        (cons (car ff)
                           (put (cdr ff) k v)))
                     (else
                        (cons (cons k v) ff)))))))

      (define (del ff k)
         (if ff
            (let ((new (remove (λ (x) (eq? (car x) k)) ff)))
               (if (null? new)
                  #false
                  new))
            ff))
     
      (define (keys ff) 
         (if ff (map car ff) null))

      (define ff-update put)
      (define fupd put)

      (define (ff-fold op state ff)
         (if ff
            (fold (λ (state node) (op state (car node) (cdr node))) state ff)
            state))

      (define (ff-foldr op state ff)
         (ff-fold op state (reverse ff)))

      (define (ff-union a b collide)
         (ff-fold
            (λ (a bk bv)
               (let ((av (get a bk #false)))
                  (if av
                     (put a bk (collide av bv))
                     (put a bk bv))))
            a b))

      (define (ff-diff a b)
         (ff-fold (λ (a bk bv) (del a bk)) a b))

      (define (getf ff k) (get ff k #false))

      (define empty-ff #false) ;; sigh

      (define ff->list (λ (x) x))

      (define (list->ff l) (fold (λ (ff n) (put ff (car n) (cdr n))) empty-ff l))

      (define (ff-singleton? ff)
         (and (pair? ff) (null? (cdr ff))))

      (define (ff? x)
         (or (not x) 
            (and (list? x) (all pair? x))))
      
      (define (ff-map ff op)
         (map (λ (node) (cons (car node) (op (car node) (cdr node)))) ff))

      (define ff-iter ff->list)
))



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
      empty-ff   ; could become Empty later
      
      getf       ; (getf ff key) == (get ff key #false)
      )

   (import 
      (owl defmac)
      (owl list))

   (begin

      ; Based on Okasaki's book and code from a functional pearl.
      ; 
      ; The ff nodes are actually compressed. A leaf node takes 
      ; 3 words of memory like a cons cell. A node with one 
      ; non-empty branch is 4 words and a full node is 5 words. 
      ; ff-bind fills the empty branches to keep the code 
      ; simple. Therefore a ff takes less memory and is 
      ; usually better than an association list.

      (define (ff? obj)
         (eq? (fxband (type obj) #b11111) 8)
         ;(eq? 64 (fxband (type-old obj) #b11111000))
         )

      (define empty-ff #false)   

      ;; todo: test how much slower it would be if this was on lisp side without vm support
      (define-syntax wiff
         (syntax-rules ()
            ((wiff (name l k v r) . rest)
               (ff-bind name
                  (lambda (l k v r) . rest)))))
               
      (define-syntax color-black
         (syntax-rules () ((color-black x) (ff-toggle x))))

      (define-syntax color-red
         (syntax-rules () ((color-red x) (ff-toggle x))))


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
         (if node
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
                        (mkblack-bright left this this-val (putn right key val))))))
            (mkred #false key val #false)))


      ;(define (put ff key val)
      ;   (if (get ff key #false)
      ;      (ff-update ff key val)
      ;      (let ((ff (putn ff key val)))
      ;         (if (red? ff)
      ;            (color-black ff)
      ;            ff))))

      ;(define ff-update fupd)

      ;(define (put ff key val)
      ;   (let ((new (fupd ff key val))) ; try vm-primitive first in case the key is already there (usually is)
      ;      (if new new 
      ;         (let ((ff (putn ff key val)))
      ;            (if (red? ff) (color-black ff) ff)))))

      ;;; plain bytecoded versions for testing ----------------------------------

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

      ;; ff key val -> ff' | #false
      (define (ff-update ff key val)
         (if ff
            (wiff (ff l k v r)
               (cond
                  ((lesser? key k)
                     (if l   ; #(k v l ...)
                        (set ff 3 (ff-update l key val))
                        #false))
                  ((eq? key k)
                     ; #(k v ...)
                     (set ff 2 val))
                  (l
                     (if r   ; #(k v l r)
                        (set ff 4 (ff-update r key val))
                        #false))
                  (r
                     (if r ; #(k v r)
                        (set ff 3 (ff-update r key val))
                        #false))
                  (else #false)))
            #false))

      ;; ff key val -> ff', update in place if there, otherwise insert and rebalance
      (define (put ff key val)
         (let ((ff (putn ff key val)))
            (if (red? ff) (color-black ff) ff)))


      (define fupd ff-update)

      ;;; -------------------------------------------------------------------

      (define (put-unbalanced node key val)
         (if node
            (wiff (node left ak av right)
               (cond   
                  ((lesser? key ak)
                     (mkblack (put-unbalanced left key val) ak av right))
                  ((eq? key ak)
                     (mkblack left key val right))
                  (else
                     (mkblack left ak av (put-unbalanced right key val)))))
            (mkblack #false key val #false)))
                  

      ;;;
      ;;; FF Utilities
      ;;;

      (define (ff-foldr op state tree)
         (if tree
            (wiff (tree l k v r)
               (if l 
                  (if r 
                     (lets
                        ((state (ff-foldr op state r))
                         (state (op state k v)))
                        (ff-foldr op state l))
                     (let ((state (op state k v)))
                        (ff-foldr op state l)))
                  (if r
                     (let ((state (ff-foldr op state r)))
                        (op state k v))
                     (op state k v))))
            state))

      (define (ff-fold op state tree)
         (if tree
            (wiff (tree l k v r)
               (if l
                  (if r
                     (lets
                        ((state (ff-fold op state l))
                         (state (op state k v)))
                        (ff-fold op state r))
                     (let
                        ((state (ff-fold op state l)))
                        (op state k v)))
                  (if r
                     (let ((state (op state k v)))
                        (ff-fold op state r))
                     (op state k v))))
            state))
      
       ;; iterate key-value pairs in order
       (define (ff-iterate tree tl)
         (if tree
            (wiff (tree l k v r)
               (ff-iterate l
                  (cons (cons k v)
                     (lambda ()
                        (ff-iterate r tl)))))
            tl))

       ;; iterate key-value pairs in reverse order
       (define (ff-iterrate tree tl)
         (if tree
            (wiff (tree l k v r)
               (ff-iterrate r
                  (cons (cons k v)
                     (lambda ()
                        (ff-iterrate l tl)))))
            tl))

      (define (ff-iter  tree) (ff-iterate  tree null))
      (define (ff-iterr tree) (ff-iterrate tree null))

      ;; note: ff-map will switch argument order in the generic equivalent
      (define (ff-map ff op)
         (if (not ff)
            #false
            (wiff (ff l k v r)
               (if (red? ff)
                  (mkred (ff-map l op) k (op k v) (ff-map r op))
                  (mkblack (ff-map l op) k (op k v) (ff-map r op))))))

      (define (list->ff lst)
         (fold
            (lambda (ff node)
               (put ff (car node ) (cdr node)))
            empty-ff
            lst))

      (define (ff->list ff)
         (ff-foldr
            (lambda (lst k v)
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
            ((not left) right)
            ((not right) left)
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
         (if ff
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
            #false))


      (define (del ff key)
         (let ((ff (deln ff key)))
            (if (red? ff)
               (color-black ff)
               ff)))
         

      ;;;
      ;;; FIXME bad hacks
      ;;;

      ;(define (del ff key)
      ;   (ff-fold
      ;      (lambda (new this val)
      ;         (if (eq? key this)
      ;            new
      ;            (put new this val)))
      ;      empty-ff ff))


      ;; todo: placeholder ff-union
      (define (ff-union a b collide)
         (ff-fold
            (lambda (a bk bv)
               (let ((av (get a bk #false)))
                  (if av ;; <- BUG, does not imply bk is not set
                     (put a bk (collide av bv))
                     (put a bk bv))))
            a b))

      ;; todo: placeholder ff-diff
      (define (ff-diff a b)
         (ff-fold (lambda (a b _) (if (get a b #false) (del a b) a)) a b))

      ;; just one value? == is the root-node a black key-value pair
      (define (ff-singleton? ff)
         (eq? type-ff-black-leaf (type ff)))

      (define-syntax getf
         (syntax-rules ()
            ((getf ff key) (get ff key #false))))

      (define (keys ff)
         (ff-foldr (λ (out k v) (cons k out)) null ff))
))
