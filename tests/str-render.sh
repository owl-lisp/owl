#!/bin/sh

$@ -t '(write (list->string (iota 0 1 256)))' \
   | $@ -e '(equal? (iota 0 1 256) (string->list (read stdin)))'
