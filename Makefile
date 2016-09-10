NASM=/usr/local/bin/nasm
GCC=/usr/local/bin/gcc-6

build: clean stst

stst: startup.c stst.o
	$(GCC) -o stst startup.c stst.o

stst.o: stst.s
	$(NASM) -f macho64 stst.s -o stst.o

clean:
	rm -f stst.o stst

.PHONY: build clean
.SILENT: build clean stst stst.o
