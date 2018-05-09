#!/bin/sh

ECHO=$(which echo)

$@ -e "(import (owl sys)) (exec \"$ECHO\" (list \"$ECHO\" \"foo\")) (print \"fail\")"
