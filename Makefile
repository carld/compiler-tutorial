PLATFORM=$(shell uname -s)
ARCH=$(shell uname -m)

SCHEME=petite

ENTRY=scheme_entry

ifeq ($(PLATFORM),Linux)
  FORMAT=elf64
  NASM=nasm
  CC=gcc
endif

# On Darwin this assumes nasm and gcc installed with brew:
#  - brew update
#  - brew install --HEAD michaelballantyne/homebrew-chez/chez-scheme
#  - brew install nasm
#  - brew unlink gcc
#  - brew install gcc
ifeq ($(PLATFORM),Darwin)
  FORMAT=macho64
  NASM=/usr/local/bin/nasm
  CC=/usr/local/bin/gcc-6
  NASM_PREFIX= --prefix _
endif

CFLAGS= -m64 -g -fomit-frame-pointer -Wall -pedantic
NFLAGS= -g $(NASM_PREFIX)

build: clean stst

stst: stst.o startup.c
	$(CC) $(CFLAGS) -o stst stst.o startup.c

stst.o: stst.s
	$(NASM) $(NFLAGS) -f $(FORMAT) stst.s -o stst.o

clean:
	rm -f stst.o stst

test:
	ENTRY=$(ENTRY) $(SCHEME) --script auto.scm

.PHONY: build clean test
.SILENT: build clean test stst stst.o
