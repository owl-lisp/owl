;;;
;;; Measuring the costs of various operations
;;;

;; note, gc time not measured as if it was caused by the currently running thread

;; depth of thunk running tree adding one doubles running time and improves accuracy
(define depth 19) 

(define nruns (- (expt 2 depth) 1)) ;; number of times the thunk is evaled

(define runner
   (define (make depth)
      (if (eq? depth 1)
         (λ (thunk val) (thunk))
         (let ((sub (make (- depth 1))))
            (λ (thunk val) (sub thunk (sub thunk (thunk)))))))
   (make depth))

(define (run thunk) (runner thunk 32))

(define (time-tree t) 
   (lets 
      ((ss ms (clock))
       (val (run t))
       (ess ems (clock))
       (elapsed (- (+ (* ess 1000) ems) (+ (* ss 1000) ms))))
      elapsed))

(define (nums n)
   (lets 
      ((rs (seed->rands (time-ms))) 
       (rs ls (random-permutation rs (iota 0 1 n))))
      ls))

(define perm10 (nums 10))
(define perm20 (nums 20))
(define perm100 (nums 100))
(define perm200 (nums 200))
(define perm1000 (nums 1000))

(define vec10 (list->vector perm10))
(define vec20 (list->vector perm20))
(define vec100 (list->vector perm100))
(define vec200 (list->vector perm200))
(define vec1000 (list->vector perm1000))

(define ff10 (list->ff (zip cons perm10 perm10)))
(define ff100 (list->ff (zip cons perm100 perm100)))
(define blank-time ;; time of running a blank cycle (load immediate number + return)
   (fold min 100000000000
      (map (λ (x) (time-tree (λ () 42))) (iota 0 1 10))))

(if (< blank-time 1)
   (error "increase depth, blank time too short: " blank-time))

(show " => blank cycle takes " blank-time)

(define (time txt t)
   (lets 
      ((total (- (time-tree t) blank-time)) ;; how much to make <nruns> of these
       (ms-per-run (/ total nruns))
       (runs-per-s (floor (/ 1000 ms-per-run))))
      (print* (list txt ": " runs-per-s " runs/s"))))


;; return to sender
(define (echo)
   (let ((envelope (wait-mail)))
      (mail (ref envelope 1) envelope)
      (echo)))

;; lose it
(define (sink)
   (let ((envelope (wait-mail)))
      (sink)))

(fork-server 'sink sink)
(fork-server 'echo echo)

(define (k a b) a)
(define (i a) a)

(begin
   (print* (list "Repeating each opertion " nruns " times:"))
   (time " - if            " (λ () (if #t 42 43)))
   (time " - cons          " (λ () (cons 1 2)))
   (time " - vref 100[50]  " (λ () (vec-ref vec100 50)))
   (time " - vref 10[5]    " (λ () (vec-ref vec10 5)))
   (time " - ff14 del      " (λ () (del ff10 5)))
   (time " - ff100 del     " (λ () (del ff100 50)))
   (time " - callcc + exit " (λ () (call/cc (λ (ret) (ret ret)))))
   (time " - fixnum add    " (λ () (+ 1 2)))
   (time " - fixnum mul    " (λ () (* 1 2)))
   (time " - fixnum sub    " (λ () (- 1 2)))
   (time " - cons 10x      " (λ () (list 0 1 2 3 4 5 6 7 8 9)))
   (time " - ff get 10[5]  " (λ () (get ff10 5 #f)))
   (time " - vref 1000[500]" (λ () (vec-ref vec1000 500)))
   (time " - fixnum div    " (λ () (/ 2 1)))
   (time " - ff get 100[50]" (λ () (get ff100 50 #f)))
   (time " - 32bit add     " (λ () (+ #xabcd1234 #x1234abcd)))
   (time " - abcabcx s/a/  " (λ () (/a/ "abcabcx")))
   (time " - output null   " (λ () (mail stdout null)))
   (time " - map i 10 elt  " (λ () (map i perm10)))
   (time " - fold + 10 elt " (λ () (fold + 0 perm10)))
   (time " - fold k 10 vec " (λ () (fold k 0 vec10)))
   (time " - mail sink     " (λ () (mail 'sink #f)))
   (time " - 32bit mul     " (λ () (* #xabcd1234 #x1234abcd)))
   (time " - abcabxc s/c/  " (λ () (/c/ "abcabcx")))
   (time " - abcabxc s/cab/" (λ () (/cab/ "abcabcx")))
   (time " - iota 0 1 10   " (λ () (iota 0 1 10)))
   (time " - s/(...)\\1/    " (λ () (/(...)\1/ "abcabcx")))
   (time " - fold k 100 elt" (λ () (fold k 0 perm100)))
   (time " - fold k 200 elt" (λ () (fold k 0 perm200)))
   (time " - fold + 100 elt" (λ () (fold + 0 perm100)))
   (time " - fold + 200 elt" (λ () (fold + 0 perm200)))
   (time " - fold k 100 vec" (λ () (fold k 0 vec100)))
   (time " - fold k 200 vec" (λ () (fold k 0 vec200)))
   (time " - map i 100 elt " (λ () (map i perm100)))
   (time " - map i 200 elt " (λ () (map i perm200)))
   (time " - rev 100       " (λ () (reverse perm100)))
   (time " - sort 10       " (λ () (sort < perm10)))
   (time " - app 100 100   " (λ () (append perm100 perm100)))
   (time " - interact echo " (λ () (interact 'echo #f)))
   (time " - rev 200       " (λ () (reverse perm200)))
   (time " - fold + lst 50 " (λ () (fold + 0 (iota 0 1 50))))
   (time " - sort 20       " (λ () (sort < perm20)))
;   (time " - factor 100    " (λ () (factor 100)))
;   (time " - prime? 31337  " (λ () (prime? 31337)))
)











