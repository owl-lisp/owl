#!/bin/sh

LISP="tmp/bare-$$.scm"
FASL="tmp/bare-$$.fasl"
C="tmp/bare-$$.c"
OBJ="tmp/bare-$$"

echo '(lambda (args) 42)' > $LISP # <- to be dumped as such, so program exits with return value 42

# check that all compile silently and produce equal code
$@ --run $LISP   # interpret and run
RINT=$?
echo "interpret: $RINT"

$@ --no-threads -o $FASL $LISP
$@ --load $FASL
RFASL=$?
echo "fasl: $RFASL"

$@ --no-threads -o $C $LISP
gcc -o $OBJ $C
$OBJ
RBIN=$?
echo "binary: $RBIN"

# don't remove files if there were differences in return values
test "$RBIN" = "$RFASL" && test "$RFASL" = "$RINT" || exit 1

rm $LISP $FASL $C $OBJ

