#!/bin/bash

for FILE in owl/*.scm scheme/*.scm
do
   head -n 1 "$FILE" | grep -q "^;;; " || continue
   NAME=$(grep "^(define-library " $FILE | head -n 1 | sed -e 's/.define-library //')
   echo "## $NAME"
   echo ""
   grep "^;;;" "$FILE" | sed -re 's/;;; ?//' 
   echo ""
   echo
done
