#!/bin/sh

ME=$$

(/bin/echo -n '#!';
 for part in $@
 do
   /bin/echo -n "`pwd`/$part "
 done) | sed 's/ $//' > tmp/script-$ME

/bin/echo "" >> tmp/script-$ME

/bin/echo '

(print "ohai")

' >> tmp/script-$ME

chmod +x tmp/script-$ME

./tmp/script-$ME | grep "^ohai$" || exit 1

rm tmp/script-$ME

