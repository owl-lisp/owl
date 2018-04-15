#!/bin/sh

# partial read vector io test
for string in "foo" "b" "ar"
do
   echo "(display \"$string\")" | $@ -q
   sleep 0.1 2>/dev/null
done | $@ -e '(file->vector "-")'
