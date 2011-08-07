;;;
;;; Extra IO etc exposed via the sys-prim
;;;

;; Adding some extra system primops to see how much could be added while 
;; still keeping the generated .c code portable, win32 being the main 
;; reason for worry.

(define-module lib-sys
   (export
      dir-fold
      dir->list)

   ;;;
   ;;; Unsafe operations not to be exported
   ;;;

   ;; string → False | unsafe-dirptr
   (define (open-dir path)
      (let ((cs (c-string path)))
         (if (and cs (<= (string-length cs) #xffff))
            (sys-prim 11 cs F F)
            False)))

   ;; unsafe-dirfd → False | eof | bvec
   (define (read-dir obj)
      (sys-prim 12 obj F F))

   ;; _ → True
   (define (close-dir obj)
      (sys-prim 13 obj F F))


   ;;; 
   ;;; Safe derived operations
   ;;; 

   ;; dir elements are False or fake strings, which have the type of small raw ASCII 
   ;; strings, but may in fact contain anything the OS happens to allow in a file name.

   (define (dir-fold op st path)
      (let ((dfd (open-dir path)))
         (if dfd
            (let loop ((st st))
               (let ((val (read-dir dfd)))
                  (if (eof? val) 
                     st
                     (loop (op st val)))))
            st)))

   (define (dir->list path)
      (dir-fold (λ (seen this) (cons this seen)) null path))

)

