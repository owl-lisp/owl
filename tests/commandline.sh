#!/bin/sh

LISP="(import (scheme process-context)) (print (cdr (command-line)))"

$@ -e "$LISP" | grep "$LISP"

