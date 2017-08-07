#!/bin/sh

mkdir -p tmp

echo '(lambda (args) (if (equal? (cadr args) "slartibartfast") 0 1))' > tmp/exit.scm
$@ -o tmp/exit.fasl tmp/exit.scm

bin/vm tmp/exit.fasl 2>/dev/null
echo "vm error exit $? should be 126"

bin/vm tmp/exit.fasl foo
echo "normal nonzero exit $? should be 1"

bin/vm tmp/exit.fasl slartibartfast
echo "ok exit $? should be 0"

rm tmp/exit.scm tmp/exit.fasl
