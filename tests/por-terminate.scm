
(define (iter-ret n r)
   (if (= n 0)
      r
      (iter-ret (- n 1) r)))

(define (walk n)
   (iter-ret n 'ok))

(print "First terminate order: " 
   (por
      (walk -1)      ;; won't terminate
      (walk 1000)))

(print "Second terminate order: " 
   (por
      (walk 1000)
      (walk -1)))    ;; won't terminate

(print "Third nonterminate + fail: "
   (por
      (walk -1)              ;; nonterminating
      (iter-ret 100 #false)  ;; fails
      (iter-ret 1000 #false) ;; fails
      (iter-ret 2000 'ok)    ;; succeeds after failures
      (walk -1)))

;; walk down, but switch thread every time
(define (iter-ret-step n r)
   (if (= n 0)
      r
      (begin
         (set-ticker 0) ;; thread context switch in next function call (-)
         (iter-ret-step (- n 1) r))))

(print "Por termination order: "
   (por
      (walk -1)
      (iter-ret-step 101 'bad-a)
      (iter-ret-step 102 'bad-b)
      (iter-ret-step 100 'ok)
      (iter-ret-step 103 'bad-c)
      (walk -1)
      (iter-ret-step 104 'bad-d)))
