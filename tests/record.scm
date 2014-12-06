
(define-record-type pare (kons x y)
   pare?
   (x kar)
   (y kdr))

(define x (kons 'foo 'bar))

(if (record? x)
   (print x))

(print
   (let ((x (kons 'lemon 'curry)))
      (if (pare? x)
         (kdr x)
         'chili)))

(define-record-type trire (tri x y z)
  tri?
  (y tri2)
  (z tri3)
  (x tri1))

(let ((x (tri 11 22 33)))
   (print
      (list 
         (tri1 x)
         (tri2 x)
         (tri3 x))))



