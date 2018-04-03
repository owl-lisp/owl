(fork 'crasher
   (λ ()
      (wait-mail) ;; wait for a message before crashing
      (/ 1 0)))

(begin
   ;; link current thread to thread about to crash
   (link 'crasher)
   ;; trigger the crash
   (mail 'crasher 'itstime)
   ;; check that we get a crash
   (let ((envelope (wait-mail)))
      (print (ref envelope 1)
         " -> " (ref (ref envelope 2) 1))))

