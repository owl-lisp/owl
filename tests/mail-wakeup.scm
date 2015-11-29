

(define (doer)
   (let ((first (wait-mail)))
      (mail (ref first 1) 'hello-you)))

(print (interact doer 'hello-doer))
      
      
