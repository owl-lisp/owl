DESTDIR=
PREFIX=/usr
BINDIR=/bin
INSTALL=install
TIME=/usr/bin/time -p

CFLAGS=-Wall -O2 -fomit-frame-pointer
#CC=gcc

# owl needs just a single binary
owl: bin/ol 

## fasl (plain bytecode) image boostrap

fasl/boot.fasl: fasl/init.fasl
	# start bootstrapping with the bundled init.fasl image
	cp fasl/init.fasl fasl/boot.fasl

fasl/ol.fasl: bin/vm fasl/boot.fasl owl/*.l
	# selfcompile boot.fasl until a fixed point is reached
	$(TIME) bin/vm fasl/boot.fasl --run owl/ol.l -s none -o fasl/bootp.fasl
	ls -la fasl/bootp.fasl
	# check that the new image passes tests
	tests/run bin/vm fasl/bootp.fasl
	# copy new image to ol.fasl if it is a fixed point, otherwise recompile
	diff -q fasl/boot.fasl fasl/bootp.fasl && cp fasl/bootp.fasl fasl/ol.fasl || cp fasl/bootp.fasl fasl/boot.fasl && make fasl/ol.fasl
	

## building just the virtual machine to run fasl images

bin/vm: c/vm.c
	$(CC) $(CFLAGS) -o bin/vm c/vm.c

c/vm.c: c/ovm.c
	# make a vm without a bundled heap
	echo "unsigned char *heap = 0;" > c/vm.c
	cat c/ovm.c >> c/vm.c


## building standalone image out of the fixed point fasl image

c/ol.c: fasl/ol.fasl
	# compile the repl using the fixed point image 
	bin/vm fasl/ol.fasl --run owl/ol.l -s some -o c/ol.c

bin/ol: c/ol.c
	# compile the real owl repl binary
	$(CC) $(CFLAGS) -o bin/olp c/ol.c
	tests/run bin/olp
	test -f bin/ol && mv bin/ol bin/ol-old || true
	mv bin/olp bin/ol


## running unit tests manually

fasltest: bin/vm
	tests/run bin/vm fasl/ol.fasl

test: bin/vm
	tests/run bin/ol


## MinGW builds for win32 

# these should work on Debian-likes after $ sudo apt-get install mingw32 wine1.2

bin/vm.exe: c/vm.c
	test -x `which i586-mingw32msvc-gcc`
	i586-mingw32msvc-gcc $(CFLAGS) -o bin/vm.exe c/vm.c -lwsock32

bin/ol.exe: c/ol.c
	test -x `which i586-mingw32msvc-gcc`
	i586-mingw32msvc-gcc $(CFLAGS) -o bin/ol.exe c/ol.c -lwsock32
	# tests/run.sh wine bin/ol.exe <- stdio does not work in wine :(


## data 

owl/unicode-char-folds.l:
	echo "(define char-folds '(" > owl/unicode-char-folds.l 
	curl http://www.unicode.org/Public/6.0.0/ucd/CaseFolding.txt | grep "[0-9A-F]* [SFC]; " | sed -re 's/ #.*//' -e 's/( [SFC])?;//g' -e 's/^/ /' -e 's/ / #x/g' -e 's/ /(/' -e 's/$$/)/' | tr "[A-F]" "[a-f]" >> owl/unicode-char-folds.l 
	echo "))" >> owl/unicode-char-folds.l

## meta

doc/ol.1.gz: doc/ol.1
	cat doc/ol.1 | gzip -9 > doc/ol.1.gz

install: bin/ol bin/vm doc/ol.1.gz
	-mkdir -p $(DESTDIR)$(PREFIX)/bin
	-mkdir -p $(DESTDIR)$(PREFIX)/share/man/man1
	$(INSTALL) -m 755 bin/ol $(DESTDIR)$(PREFIX)/bin/ol
	$(INSTALL) -m 755 bin/vm $(DESTDIR)$(PREFIX)/bin/owl-vm
	$(INSTALL) -m 644 doc/ol.1.gz $(DESTDIR)$(PREFIX)/share/man/man1/ol.1.gz

uninstall:
	-rm $(DESTDIR)$(PREFIX)/bin/ol
	-rm $(DESTDIR)$(PREFIX)/bin/owl-vm
	-rm $(DESTDIR)$(PREFIX)/share/man/man1/ol.1.gz

clean:
	-rm fasl/boot.fasl fasl/bootp.fasl fasl/ol.fasl
	-rm c/vm.c c/ol.c
	-rm bin/ol bin/vm

# make a standalone binary against dietlibc for relase
standalone:
	-rm bin/ol
	make CFLAGS="-O2 -DNO_SECCOMP" CC="diet gcc" bin/ol

fasl-update: fasl/ol.fasl
	cp fasl/ol.fasl fasl/init.fasl
	# some paranoia
	gpg -b fasl/init.fasl

todo: bin/vm 
	bin/vm fasl/ol.fasl -n owl/*.l | less

.PHONY: install uninstall todo test fasltest owl standalone

