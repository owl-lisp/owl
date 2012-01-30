
(print
   (list
      (equal? "\n" (list->string '(10)))
      (equal? "\x123;" (list->string (list #x123)))
      (equal? "**" "\x0000002a;\x2a;")))

(print "done")
