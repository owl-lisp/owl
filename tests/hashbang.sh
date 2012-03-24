#!/bin/sh

ME=$$

echo -n '#!'   > tmp/script-$ME
for part in $@
do
   echo -n "`pwd`"
   echo -n "/"
   echo -n $part
   echo -n " "
done >> tmp/script-$ME

echo "" >> tmp/script-$ME

echo '

(print "ohai")

' >> tmp/script-$ME

chmod +x tmp/script-$ME

./tmp/script-$ME | grep "^ohai$" || exit 1

rm tmp/script-$ME

