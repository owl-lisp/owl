#!/bin/bash

# TODO: should on startup check if the schemes are available and 
# automatically enable all found, unless given explicitly on command 
# line. 

NRUNS=1        # adjust to compensate for level of supercomputerness
PAT="(42)"     # pattern all runs should have in output if executed correctly 
LOG=output     # stdout/err
PROG=input.l # file to run
TIME="/usr/bin/time -f %UX%M" # memory size reported by %M is 4x the correct (counts pages by accident?)
MAXTIME=100   # most of the tests are scaled to take 1-5 seconds per run for owl
MAXMEM=409600

function compute {
   CMD=$3
   RES=`$TIME $CMD < $PROG 2>&1 > $LOG`
   RES=`echo $RES | sed -e 's/X/ /'`
   #echo "------------------------------"
   #cat $LOG
   #echo "------------------------------"
   #echo "RES is $RES"
   grep -q $PAT $LOG && ../bin/ol -e '(list->string (foldr (lambda (val tl) (render render val tl)) null (list "   " (lref *args* 3) "s (" (div (string->integer (lref *args* 4)) 4096) "Mb) - " (lref *args* 5))))' $RES "$2" || echo "  $MAXTIME.00+ or fail - $2 - x_X"  
}

echo ",exit 0" > /tmp/s48-exit

ulimit -S -t $MAXTIME
ulimit -S -m $MAXMEM

# amm, no seq? :)
echo "(begin (print (car (list " > start.txt
yes "(test '()) " | dd bs=12 count=$NRUNS >> start.txt 2>/dev/null
echo "))) 0)" >> start.txt


rm test.l input.l 2>/dev/null # temp files generated in benchmark

echo
echo "Machine:"
echo " - host `uname -a`"
echo " - cpu `grep "model name" /proc/cpuinfo | tail -n 1`"
echo " - cpu `grep "bogomips" /proc/cpuinfo | tail -n 1`"
echo
echo "Test parameters: $NRUNS runs per test, max time $MAXTIME, max mem $MAXMEM"

for file in *.l
do
   echo
   echo "$file:"
   (

	##
	## VM based schemes
	##

   # owl using the tiny vm and running just about everything as bytecode 
   cat $file start.txt > $PROG
   compute $file "Owl (vm + ol.fasl)" "../bin/vm ../fasl/ol.fasl" 
   
	# compiled with light vm specialization. to be the default mode later on.
   cat $file start.txt > $PROG
   compute $file "Owl (ol)" "../bin/ol"

   cat r5rs.defs $file start.txt > $PROG
   compute $file "Chibi Scheme 0.4" "chibi-scheme"

   cat r5rs.defs $file start.txt > $PROG
   compute $file "Gauche 2.9" gosh

	echo ",bench on"   | cat - r5rs.defs $file start.txt /tmp/s48-exit > $PROG
   compute $file "Scheme48 1.8" "scheme48"

	#echo -n " *       elk: " 
	#(grep -q "tags.*macro" $file && echo nomacro) || \
	#	cat r5rs.defs $file start.txt | $TIME elk > /dev/null 

	#echo -n " *      vscm: " 
	#cat r5rs.defs $file | sed -e "s/^,r.*//" | time vscm 2>&1 | grep user

	echo "(require 'macro-by-example)" | cat - r5rs.defs $file start.txt > $PROG
   compute $file "scm" "scm"

	cat r5rs.defs $file start.txt > $PROG
   compute $file "Guile 2.0.2" "/home/aki/opt/guile-2.0.2/bin/guile"

	echo "(use-syntax (ice-9 syncase))" | cat - r5rs.defs $file start.txt > $PROG
   compute $file "Guile 1.8" "guile"
   
	#cat r5rs.defs $file start.txt > $PROG
   #compute $file "guile 2.0" "guile-2.0"

#	#echo -n " * femtolisp: " # no call/cc, values, bignums or r5rs macros
#	#(grep -q "tags.*bignum" $file && echo nobig) || \
#	#(grep -q "tags.*cont" $file && echo nocont) || \
#	#(grep -q "tags.*values" $file && echo novals) || \
#	#(grep -q "tags.*macro" $file && echo nomacro) || \
#	#(echo "(define display print)" > /tmp/file.l; cat r5rs.defs $file | grep -v call-with-current-continuation | grep -v define-syntax >> /tmp/file.l; $TIME flisp < /tmp/file.l > /tmp/out 2>/tmp/out-time; grep -q "compile error" /tmp/out-time && echo error || cat /tmp/out-time)
#
   
	cat r5rs.defs $file start.txt > $PROG
   compute $file "TinyScheme 1.37" tinyscheme



	##
	## JIT compilers
	##

	cat r5rs.defs $file start.txt > $PROG
   compute $file "MzScheme 4.2.1" "mzscheme"

	##
	## Native code compilers
	##

	cat r5rs.defs $file start.txt  > $PROG
   compute $file "petite chez scheme" "/home/aki/opt/petite/bin/i3le/scheme -b /home/aki/opt/petite/boot/i3le/petite.boot"

   # not yet on this machine
	cat r5rs.defs $file start.txt | grep -v NOIKARUS > $PROG
   compute $file "Ikarus 0.0.3" ikarus

	cat r5rs.defs $file start.txt > $PROG
   compute $file "Larceny 0.97 (IA32)" larceny

   # only native as in via C
	# owl's bytecode2c compiler (making standalone binaries with custom vm instructions)
	cp $file $PROG
	echo "(lambda (args) " >> $PROG
	cat start.txt >> $PROG
	echo ")" >> $PROG
	../bin/ol --native -o input.c $PROG
	gcc -O2 -o test input.c
   compute $file "Owl (ol --native -o foo.c + gcc -O2)" "./test" 

   # use same source, but compile only a fixed set of functions to C
	#../bin/ol --usual-suspects -o input.c $PROG
	#gcc -O2 -o test input.c
   #compute $file "Owl (ol --usual-suspects -o foo.c + gcc -O2)" "./test" 

	cat r5rs.defs $file start.txt > $PROG
   rm foo &>/dev/null
	csc -O5 -R numbers -o foo $PROG 2>/dev/null
   cp $PROG /tmp/chicken.l
   compute $file "Chicken Scheme 4.5.0 (csc -O5)" ./foo

   ## compile test.l to test.com with usual integrations and run
   ## MIT Scheme is one of the fastest, but I couldn't get it to run on my 64-bit machine 
   rm test.* &> /dev/null
	echo "(declare (usual-integrations))" | cat - r5rs.defs $file start.txt > test.l
   echo "" | mit-scheme --eval '(cf "test.l")' &> /dev/null
   echo "" > $PROG
   compute $file "MIT Scheme" "mit-scheme --load test.com"

   ) | sort -n 
done | tee benchmark-output.txt

