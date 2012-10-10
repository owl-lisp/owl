;; ff library rewrite with new types and distinct #empty blank ff

(define-library (owl ff-ng)

   (export 
      empty?
      put     ;; O(n) temporarily!
      get     ;; O(n) temporarily!
   )

   (import
      (owl defmac)
      (owl io) (owl syscall) (owl random) (owl list-extra) (owl math) (owl time)
      (owl list)
      )

   (begin

      ;; unbalanced for now

      (define (empty? ff)
         (eq? ff #empty))

      ;; (valid nonempty) ff → left key val right
      (define (explode ff)
         (let ((s (size ff)))
            (cond
               ((eq? s 2)
                  (values #empty (ref ff 1) (ref ff 2) #empty))
               ((eq? s 4)
                  (lets ((k v l r ff))
                     (values l k v r)))
               (else
                  (if (eq? (type ff) type-ff-right) ;; size 3, only right branch
                     (values #empty (ref ff 1) (ref ff 2) (ref ff 3))
                     (values (ref ff 3) (ref ff 1) (ref ff 2) #empty))))))

      ;; for debugging
      (define (ff->sexp ff)
         (if (eq? ff #empty)
            ff
            (case (type ff)
               (type-ff
                  (if (eq? (size ff) 2)
                     (cons (ref ff 1) (ref ff 2)) ;; pair is key&value pair
                     (list 'ff (ff->sexp (ref ff 3)) (cons (ref ff 1) (ref ff 2)) (ff->sexp (ref ff 4)))))
               (type-ff-left
                  (if (eq? (size ff) 3)
                     (list 'ffl (ff->sexp (ref ff 3)) (cons (ref ff 1) (ref ff 2)))
                     (list 'ffl (ff->sexp (ref ff 3)) (cons (ref ff 1) (ref ff 2)) (ff->sexp (ref ff 4)))))
               (type-ff-right 
                  (if (eq? (size ff) 3)
                     (list 'ffr (cons (ref ff 1) (ref ff 2)) (ff->sexp (ref ff 3)))
                     (list 'ffr (ff->sexp (ref ff 3)) (cons (ref ff 1) (ref ff 2)) (ff->sexp (ref ff 4))))))))

      (define (node l k v r)
         (cond
            ((eq? l #empty)
               (if (eq? r #empty)
                  (mkt type-ff k v)
                  (mkt type-ff-right k v r)))
            ((eq? r #empty)
               (mkt type-ff-left k v l))
            (else
               (mkt type-ff k v l r))))

      (define (get ff k d)
         ;(print (list 'get (ff->sexp ff) 'key k 'default d))
         (if (eq? ff #empty)
            d
            (lets ((l this-k v r (explode ff)))
               (cond
                  ((lesser? k this-k) (get l k d))
                  ((eq? k this-k) v)
                  (else (get r k d))))))

      ;; ff k v → ff' | #false (if not already in ff)
      (define (fupd ff k v)
         (if (eq? ff #empty)
            #false
            (let ((this-k (ref ff 1)))
               (if (eq? this-k k)
                  (set ff 2 v)
                  (case (size ff)
                     (2 #false) ;; nowhere to go
                     (3 
                        (cond
                           ((lesser? k this-k)
                              (if (eq? (type ff) type-ff-left)
                                 (let ((sub (fupd (ref ff 3) k v)))
                                    (if sub
                                       (set ff 3 sub)
                                       #false))
                                 #false))
                           ((eq? (type ff) type-ff-right)
                              (let ((sub (fupd (ref ff 3) k v)))
                                 (if sub
                                    (set ff 3 sub)
                                    #false)))
                           (else #false)))
                     (else
                        (if (lesser? k this-k)
                           (let ((sub (fupd (ref ff 3) k v)))
                              (if sub (set ff 3 sub) #false))
                           (let ((sub (fupd (ref ff 4) k v)))
                              (if sub (set ff 4 sub) #false)))))))))

      ;; insert a new key to ff
      (define (ins ff k v)
         (print (list 'ins (ff->sexp ff) k v))
         (if (eq? ff #empty)
            (mkt type-ff k v) ;; balanced
            (let ((this-k (ref ff 1)))
               (case (type ff)
                  (type-ff ;; k v, k v l r
                     (if (lesser? k this-k) ;; insert left
                        (if (eq? (size ff) 2)
                           ;; leaf -> gain left leaf branch and lean left
                           (mkt type-ff-left this-k (ref ff 2) (mkt type-ff k v))
                           ;; full balanced node, tuck to left branch and lean there
                           (mkt type-ff-left this-k (ref ff 2)
                              (ins (ref ff 3) k v)
                              (ref ff 4)))
                        (if (eq? (size ff) 2)
                           ;; leaf -> gain right branch and lean right
                           (mkt type-ff-right this-k (ref ff 2) (mkt type-ff k v))
                           ;; full balanced node, tuck to right branch and lean there
                           (mkt type-ff-right this-k (ref ff 2)
                              (ref ff 3)
                              (ins (ref ff 4) k v)))))
                  (type-ff-left ; k v l, k v l+ r
                     (if (lesser? k this-k) 
                        ;; lean left + insert left → rotate
                        (if (eq? (size ff) 3) ;; 1k 1v (?k ?v) + 02 02 → hh
                           (lets 
                              ((l (ref ff 3))
                               (lk lv l)) ;; must be leaf due to invariant
                              (if (lesser? lk k) ;; child left will stay lefmost?
                                 (mkt type-ff k v l (mkt type-ff (ref ff 1) (ref ff 2)))
                                 (mkt type-ff lk lv (mkt type-ff k v) (mkt type-ff (ref ff 1) (ref ff 2)))))
                           ;; insert left to full left-leaning node
                           ;;  - insert to left branch, comes out weighting 2 more than right
                           ;;  - rotate once to regain balance
                           (lets 
                              ((l this-k this-v r ff)
                               (l (ins l k v))) ;; weight +2
                              (error "needed:" (list 'rot-right l this-k this-v r))))
                        ;; insert right, regain balance
                        (if (eq? (size ff) 3) ;; k v l → k v l r
                           (lets ((this-k this-v this-l ff))
                              (mkt type-ff this-k this-v this-l (mkt type-ff k v)))
                           (lets ((this-k this-v this-l this-r ff))
                              (mkt type-ff this-k this-v this-l (ins this-r k v))))))
                  (else ; ff-right → k v r, k v l r+
                     (cond
                        ((lesser? k this-k)
                           ;; lean right + insert left → regain balance
                           (if (eq? (size ff) 3)
                              (lets ((this-k this-v this-r ff))
                                 (mkt type-ff this-k this-v (mkt type-ff k v) this-r))
                              (lets ((this-k this-v this-l this-r ff))
                                 (mkt type-ff this-k this-v (ins this-l k v) this-r))))
                        ((eq? (size ff) 3) ;; one right, become a balanced triplet
                           ;; minor lean right + insert right → local balance
                           ;; k > this-k
                           (lets
                              ((this-k this-v r ff)
                               (rk rv r)) ;; leaf due to invariant
                              (if (lesser? k rk) ; this-k < k < rk → k is root
                                 (mkt type-ff k v (mkt type-ff this-k this-v) r)
                                 ; this-k < rk < k → rk is root
                                 (mkt type-ff rk rv (mkt type-ff this-k this-v) (mkt type-ff k v)))))
                        (else
                           ;; big lean right + insert right → rotate balance
                           (lets
                              ((this-k this-v l r ff)
                               (r (ins r k v))) ;; weight +2
                              (case (type r)
                                 (type-ff
                                    (begin
                                       (if (not (eq? (size r) 4)) (error "bad new sub: " r)) ;; DEBUG
                                       (lets ((rk rv rl rr r))
                                          (if (not (eq? (size rl) 2)) (error "bad new sub left: " r)) ;; DEBUG
                                          (lets
                                             ((rlk rlv rl))
                                              (mkt type-ff rlk rlv 
                                                (mkt type-ff-left this-k this-v l)
                                                (mkt type-ff-right rk rv rr))))))
                                 (else
                                    (error
                                       "rotate 2 got new sub " (ff->sexp r))))))))))))

      ;; update in-place without rebalance, or insert known new if it fails
      (define (put ff k v)
         (let ((ffp (fupd ff k v)))
            (if ffp ffp (ins ff k v))))

      (define (list->ff l)
         (fold
            (λ (ff pair) (put ff (car pair) (cdr pair)))
            #empty l))

      ;; DEPRECATED UNBALANCED
      (define (del ff k)
         (if (eq? ff #empty)
            ff
            (lets ((l this-k v r (explode ff)))
               (cond
                  ((lesser? k this-k)
                     ;; the key is not in this castle
                     (node (del l k) this-k v r))
                  ((eq? k this-k)
                     (cond
                        ;; trivial cases
                        ((eq? l #empty) r)
                        ((eq? r #empty) l)
                        (else 
                           ;; move left key&value up here and delete left node
                           (lets ((ll lk lv lr (explode l)))
                              (node (del ll lk) lk lv r)))))
                  (else
                     (node l this-k v (del r k)))))))

      (define (try rs n i)
         (if (> i 0)
            (lets
               ((rs size (rand rs n))
                (rs l (random-permutation rs (iota 0 1 size)))
                (l (map (λ (x) (cons x (+ x 100))) l))
                (ff (list->ff l)))
               (print l)
               (print (ff->sexp ff))
               (if 
                  (fold
                     (λ (ok? pair)
                        (let ((val (get ff (car pair) 'missing)))
                           (print " ff[" (car pair) "] = " val)
                           (and ok? (= val (cdr pair)))))
                     #true l)
                  (try rs n (- i 1))))
            (print "ALL OK")))

      (let ((seed 12124))
         (try (seed->rands seed) 5 10))
))
