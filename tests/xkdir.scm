
(import (owl sys))

(rmdir "tmp/new") ;; in case we have it already

(print "rmdir tmp/new -> " (rmdir "tmp/new")) ;; no

(print "mkdir tmp/new -> " (mkdir "tmp/new" 755)) ;; yes

(print "rmdir tmp/new -> " (rmdir "tmp/new")) ;; yes

(print "rmdir tmp/new -> " (rmdir "tmp/new")) ;; no
