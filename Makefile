NASM=/usr/local/bin/nasm
CC=/usr/local/bin/gcc-6
CFLAGS= -g -fomit-frame-pointer -Wall -pedantic

build: clean stst

stst: startup.c stst.o
	$(CC) $(CFLAGS) -o stst startup.c stst.o

stst.o: stst.s
	$(NASM) -g -f macho64 stst.s -o stst.o

clean:
	rm -f stst.o stst

.PHONY: build clean
.SILENT: build clean stst stst.o
