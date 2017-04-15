#!/bin/sh

: ${CC:="gcc"}

LISP="tmp/foo-$$.scm"
C1="tmp/foo-C1-$$.c"
C2="tmp/foo-C2-$$.c"
C3="tmp/foo-C3-$$.c"
OBJ="tmp/foo-$$"

echo '(lambda (args) (print "kissa") 42)' > $LISP # <- halt with exit value 42

# check that all compile silently and produce equal code
$@ -x c -o $C1 $LISP
$@ -x c < $LISP > $C2 
$@ -x c -o $C3 < $LISP 

# check that the outputs are equal
diff $C1 $C2 || exit 1
diff $C1 $C3 || exit 2

# check that they work
$CC -o $OBJ $C1 2>/dev/null && ./$OBJ || echo $? | grep 42

rm $LISP $C1 $C2 $C3 $OBJ
