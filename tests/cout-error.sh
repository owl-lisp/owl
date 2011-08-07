#!/bin/sh

LISP="tmp/cof-src-$$.scm"
C="tmp/cof-out-$$.c"
FASL="tmp/cof-out-$$.fasl"

echo '(lambda (args) (print (cons unbound args)))' > $LISP

test -f $LISP && echo made source file
$@ -o $C $LISP 2>/dev/null
test -f $C && echo "bad - made c file" || echo c ok

$@ -o $FASL $LISP 2>/dev/null
test -f $C && echo "bad - made fasl file" || echo fasl ok

rm $LISP
