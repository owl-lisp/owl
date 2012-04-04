#!/bin/sh

ME=$$

/bin/echo -n '#!'   > tmp/script-$ME
for part in $@
do
   /bin/echo -n "`pwd`"
   /bin/echo -n "/"
   /bin/echo -n $part
   /bin/echo -n " "
done >> tmp/script-$ME

/bin/echo "" >> tmp/script-$ME

/bin/echo '

(print "ohai")

' >> tmp/script-$ME

chmod +x tmp/script-$ME

./tmp/script-$ME | grep "^ohai$" || exit 1

rm tmp/script-$ME

