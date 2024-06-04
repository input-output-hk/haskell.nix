test.o: test.c
	$(CC) -o test.o -c test.c

test.a: test.o
	$(AR) -rs test-clib.a test.o

test.dll: test.o
	$(CC) -o test-clib.dll -shared test.o

.PHONY: install
install: test.a test.dll
	mkdir $(out)/bin
	mkdir $(out)/lib
	mv test-clib.dll $(out)/bin
	mv test-clib.a $(out)/lib

