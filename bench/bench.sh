#!/bin/bash

# TODO: should on startup check if the schemes are available and 
# automatically enable all found, unless given explicitly on command 
# line. 

test -f "bench.sh" || { echo "please run me in the bench/ directory"; exit 1; }

NRUNS=10        # adjust to compensate for level of supercomputerness
PAT="(42)"     # pattern all runs should have in output if executed correctly 
LOG=output     # stdout/err
PROG=input.l # file to run
TIME="/usr/bin/time -f %UX%M" # memory size reported by %M is 4x the correct (counts pages by accident?)
MAXTIME=200    # most of the tests are scaled to take 1-5 seconds per run for owl
MAXMEM=4194304 # 4GB 

function compute {
   CMD=$3
   RES=`$TIME $CMD < $PROG 2>&1 > $LOG`
   RES=`echo $RES | sed -e 's/X/ /' -re 's/.* ([0-9.]+ [0-9.]+)$/\1/'` # ignore exit values and stderr prints
   #echo $CMD
   #echo "------------------------------"
   #cat $LOG
   #echo "------------------------------"
   #echo "RES is $RES"
   #echo "------------------------------"
   grep -q $PAT $LOG && ../bin/ol -e '(list->string (foldr render null (list "   " (lref *args* 3) "s (" (div (string->integer (lref *args* 4)) 4096) "Mb) - " (lref *args* 5))))' $RES "$2" || echo "  $MAXTIME.00+ or fail - $2 - x_X"  
}

echo ",exit 0" > /tmp/s48-exit

ulimit -S -t $MAXTIME
ulimit -S -m $MAXMEM
ulimit -S -v $MAXMEM # limit also virtual memory

(echo "(begin ";
 for foo in $(seq 1 `expr $NRUNS - 1`); do echo "(test '())"; done;
 echo "(print (test '())) 0)") > start.txt


rm test.l input.l 2>/dev/null # temp files generated in benchmark

echo
echo "Test setup:"
echo " + host `uname -a`"
echo " + CPU `grep "model name" /proc/cpuinfo | tail -n 1`"
echo " + CPU `grep "bogomips" /proc/cpuinfo | tail -n 1`"
echo " + Schemes"
echo "   - `../bin/ol --version`"
echo "   - `gosh -V`"
echo "   - `scm --version | head -n 1`"
echo "   - Chibi Scheme 0.5.3"
echo "   - `/home/aki/opt/guile/bin/guile --version | head -n1`"
echo "   - `guile --version | head -n1` (legacy)"
echo "   - `racket --version | sed -e 's/Welcome to //'`"
echo "   - `echo "" | larceny | grep "Larceny v" | head -n 1`"
echo "   - Sigacheme 0.6.1"
echo "   - `echo "" | ikarus | grep Ikarus`"
echo "   - `echo "" | scheme48 | grep "Welcome" | sed -e 's/Welcome to //' -e 's/ (.*//'`"
echo "   - Chicken Scheme `csc -version | grep Version | sed -e 's/Version //'`"
echo "   - `echo "" | tinyscheme | head -n 1`"
echo

echo "Test parameters: $NRUNS runs per test, max time ${MAXTIME}s, max mem ${MAXMEM}KB"

for file in *.l
do
   echo
   echo "$file:"
   (

	cat r5rs.defs $file start.txt > $PROG
   compute $file "SigScheme" "sscm"

	cat r5rs.defs $file start.txt > $PROG
   cp $PROG /tmp/tinyscheme
   compute $file "TinyScheme" tinyscheme

	##
	## Owl
	##

   # owl using the tiny vm and running just about everything as bytecode 
   cat $file start.txt > $PROG
   compute $file "Owl (vm + ol.fasl)" "../bin/vm ../fasl/ol.fasl" 
   
	# compiled with light vm specialization. to be the default mode later on.
   cat $file start.txt > $PROG
   compute $file "Owl (ol)" "../bin/ol"

	# compare against old owls
   cat $file start.txt > $PROG
   compute $file "Owl (ol-0.1.5)" "ol-0.1.5"
   compute $file "Owl (ol-0.1.4)" "ol-0.1.4"
   compute $file "Owl (ol-0.1.3)" "ol-0.1.3"
   compute $file "Owl (ol-0.1.2)" "ol-0.1.2"
   compute $file "Owl (ol-0.1.1)" "ol-0.1.1"

   # only native as in via C
	# owl's bytecode2c compiler (making standalone binaries with custom vm instructions)
	cp $file $PROG
	echo "(lambda (args) " >> $PROG
	cat start.txt >> $PROG
	echo ")" >> $PROG
   cp $PROG /tmp/owl.l
	../bin/ol -O2 -o input.c $PROG
	gcc -O2 -o test input.c || echo "COMPILE FAILED"
   cp input.c /tmp
   cp test /tmp
   compute $file "Owl (ol -O2 -o foo.c + gcc -O2)" "./test" 

	##
	## VM based schemes
	##

   cat r5rs.defs $file start.txt > $PROG
   compute $file "Chibi Scheme" "chibi-scheme"

   cat r5rs.defs $file start.txt > $PROG
   compute $file "Gauche" gosh

	echo ",bench on"   | cat - r5rs.defs $file start.txt /tmp/s48-exit > $PROG
   compute $file "Scheme48" "scheme48"

	#echo -n " *       elk: " 
	#(grep -q "tags.*macro" $file && echo nomacro) || \
	#	cat r5rs.defs $file start.txt | $TIME elk > /dev/null 

	#echo -n " *      vscm: " 
	#cat r5rs.defs $file | sed -e "s/^,r.*//" | time vscm 2>&1 | grep user

	echo "(require 'macro-by-example)" | cat - r5rs.defs $file start.txt > $PROG
   compute $file "scm" "scm"

	cat r5rs.defs $file start.txt > $PROG
   compute $file "Guile" "/home/aki/opt/guile/bin/guile"

	echo "(use-syntax (ice-9 syncase))" | cat - r5rs.defs $file start.txt > $PROG
   compute $file "Guile (legacy)" "guile"
   
#	#echo -n " * femtolisp: " # no call/cc, values, bignums or r5rs macros
#	#(grep -q "tags.*bignum" $file && echo nobig) || \
#	#(grep -q "tags.*cont" $file && echo nocont) || \
#	#(grep -q "tags.*values" $file && echo novals) || \
#	#(grep -q "tags.*macro" $file && echo nomacro) || \
#	#(echo "(define display print)" > /tmp/file.l; cat r5rs.defs $file | grep -v call-with-current-continuation | grep -v define-syntax >> /tmp/file.l; $TIME flisp < /tmp/file.l > /tmp/out 2>/tmp/out-time; grep -q "compile error" /tmp/out-time && echo error || cat /tmp/out-time)



	##
	## JIT compilers
	##

	cat r5rs.defs $file start.txt > $PROG
   #compute $file "MzScheme 4.2.1" "mzscheme"
   compute $file "Racket" "racket"

	##
	## Native code compilers
	##

	#cat r5rs.defs $file start.txt  > $PROG
   #compute $file "petite chez scheme" "/home/aki/opt/petite/bin/a6le/scheme -b /home/aki/opt/petite/boot/a6le/petite.boot"

   # not yet on this machine
	cat r5rs.defs $file start.txt | grep -v NOIKARUS > $PROG
   compute $file "Ikarus" ikarus

	cat r5rs.defs $file start.txt > $PROG
   compute $file "Larceny" larceny

	cat r5rs.defs $file start.txt > $PROG
   rm foo &>/dev/null
	csc -O5 -R numbers -o foo $PROG 2>/dev/null
   cp $PROG /tmp/chicken.l
   compute $file "Chicken Scheme (csc -O5)" ./foo

   ## compile test.l to test.com with usual integrations and run
   ## MIT Scheme is one of the fastest, but I couldn't get it to run on my 64-bit machine 
   #rm test.* &> /dev/null
	#echo "(declare (usual-integrations))" | cat - r5rs.defs $file start.txt > test.l
   #echo "" | mit-scheme --eval '(cf "test.l")' &> /dev/null
   #echo "" > $PROG
   #compute $file "MIT Scheme" "mit-scheme --load test.com"

   ) | sort -n 
done | tee benchmark-output.txt

