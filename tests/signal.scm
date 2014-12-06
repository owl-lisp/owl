
(import 
   (prefix (owl sys) sys-))

(define (loop) (loop))

(define (fork-inf)
   (let ((r (sys-fork)))
      (if (number? r)
         r ;; return pid
         (loop)))) ;; get stuck

(define a (fork-inf))

(sys-kill a sys-sigkill)

(print sys-sigkill) ;; 9

(print (sys-wait a)) ;; (2 . 9)
