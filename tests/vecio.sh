#!/bin/sh

FILE="tmp/test-vecio-orig-$$.scm"
COPY="tmp/test-vecio-copy-$$.scm"
cat /dev/urandom | head -n 1000 > $FILE
$@ -e "(vector->file (file->vector \"$FILE\") \"$COPY\") \"done\""
diff $FILE $COPY
rm $FILE $COPY

