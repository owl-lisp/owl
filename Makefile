DESTDIR=
PREFIX=/usr
BINDIR=/bin

CFLAGS=-Wall -O2 -fomit-frame-pointer
CC=gcc

## compile the repl/compiler by default

owl: bin/ol 


## building just the virtual machine to run fasl images

bin/vm: c/vm.c
	$(CC) $(CFLAGS) -o bin/vm c/vm.c

c/vm.c: c/ovm.c
	echo "unsigned char *heap = 0;" > c/vm.c
	cat c/ovm.c >> c/vm.c;


## building the standalone read-eval-print loop and compiler

c/ol.c: .fixedpoint
	bin/vm fasl/ol.fasl --run owl/ol.l -- -s some -o c/ol.c

bin/ol: c/ol.c
	$(CC) $(CFLAGS) -o bin/new-ol c/ol.c
	tests/run bin/new-ol
	test -f bin/ol && mv bin/ol bin/old-ol || true
	mv bin/new-ol bin/ol


## rebuilding the repl fasl image with itself

fasl/ol.fasl: bin/vm owl/*.l
	bin/vm fasl/ol.fasl --run owl/ol.l -- -s none -o fasl/ol-new.fasl
	tests/run bin/vm fasl/ol-new.fasl
	cp fasl/ol.fasl fasl/ol-old.fasl
	mv fasl/ol-new.fasl fasl/ol.fasl

## rebuilding the repl fasl image until a fixed point is reached

.fixedpoint: fasl/ol.fasl 
	test -d tmp || mkdir tmp
	cp fasl/ol.fasl tmp
	touch owl/ol.l
	make fasl/ol.fasl
	diff -q tmp/ol.fasl fasl/ol.fasl && touch .fixedpoint || make .fixedpoint

stable: .fixedpoint

## running unit tests manually against vm+fasl/ol

fasltest: bin/vm
	tests/run bin/vm fasl/ol.fasl

test: bin/vm
	tests/run bin/ol


## MinGW builds for win32 

# these should work on Debian-likes after $ sudo apt-get install mingw32 wine1.2

bin/vm.exe: vm.c
	test -x `which i586-mingw32msvc-gcc`
	i586-mingw32msvc-gcc $(CFLAGS) -o bin/vm.exe vm.c -lwsock32

bin/ol.exe: ol.c
	test -x `which i586-mingw32msvc-gcc`
	i586-mingw32msvc-gcc $(CFLAGS) -o bin/ol.exe ol.c -lwsock32
	# tests/run.sh wine bin/ol.exe <- stdio does not work in wine :(


## meta

install: bin/ol
	test -d $(DESTDIR)$(PREFIX)/bin || mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp bin/ol $(DESTDIR)$(PREFIX)/bin
	test -d $(DESTDIR)$(PREFIX)/share/man/man1 || mkdir -p $(DESTDIR)$(PREFIX)/share/man/man1
	cat doc/ol.1 | gzip -9 > $(DESTDIR)$(PREFIX)/share/man/man1/ol.1.gz

uninstall:
	rm $(DESTDIR)$(PREFIX)/bin/ol || echo "no ol"
	rm $(DESTDIR)$(PREFIX)/share/man/man1/ol.1.gz || echo "no manpage"

todo: bin/vm 
	bin/vm fasl/ol.fasl -n owl/*.l | less

.PHONY: install uninstall todo test fasltest stable owl

