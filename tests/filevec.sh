#!/bin/sh

# partial read vector io test
for string in "foo" "b" "ar"
do
   echo "(display \"$string\")" | $@ -q
   sleep 0.1
done | $@ -e '(file->vector "-")'

