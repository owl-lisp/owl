#!/bin/sh

echo -n '#!'   > tmp/script
for part in $@
do
   echo -n "`pwd`"
   echo -n "/"
   echo -n $part
   echo -n " "
done >> tmp/script

echo "" >> tmp/script

echo '

(print "ohai")

' >> tmp/script

chmod +x tmp/script

./tmp/script
