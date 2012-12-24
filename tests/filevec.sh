#!/bin/sh

# partial read vector io test
(echo foo; sleep 0.1; echo -n "b"; sleep 0.1; echo ar) | $@ -e '(file->vector "-")'

