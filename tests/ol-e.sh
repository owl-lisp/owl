#!/bin/sh

# check that only 3 is printed to stdout

$@ -e '(+ 1 2)'

