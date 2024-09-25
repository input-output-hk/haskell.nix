test.o: test.c
	$(CC) -o test.o -c test.c

test.a: test.o
	$(AR) -rs test-clib.a test.o

test.dll: test.o
	$(CC) -o test-clib-0.dll -shared test.o -Wl,--out-implib=test-clib.dll.a

.PHONY: install
install: test.a test.dll
	mkdir $(out)/bin
	mkdir $(out)/lib
	mv test-clib-0.dll $(out)/bin
	mv test-clib.dll.a $(out)/bin
	mv test-clib.a $(out)/lib

