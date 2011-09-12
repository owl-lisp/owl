#!/bin/bash

OUTPUT=`$@ tests/hashbang.scm`
test "$OUTPUT" "=" 'Hello, world!' || echo "Bad hashbang output: $OUTPUT"
