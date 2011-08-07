;; check file io, assuming run via makefile

(lets
   ((port (open-output-file "tmp/test")))
   (mail port (render render "Hello, world!" null))
   (close-port port))

(lets
   ((vec (file->vector "tmp/test")))
   (print (list->string (vector->list vec))))

