#include <stdio.h>
#include <sys/types.h>
#define _GNU_SOURCE  /* MAP_ANONYMOUS is not POSIX */
#include <sys/mman.h>
#include <unistd.h>
#define bool_f      0x2F
#define bool_t      0x6F
#define bool_mask   0xBF

#define fx_mask     0x03
#define fx_tag      0x00
#define fx_shift       2

#define char_mask   0xFF
#define char_tag    0x0F
#define char_shift     8

#define nil_tag     0x3F

const char *ascii_table[0x80] = {
 "nul",       "soh",    "stx",     "etx",   "eot", "enq",    "ack",  "bel",
 "backspace", "tab",    "newline", "vt",    "np",  "return", "so",   "si",
 "dle",       "dc1",    "dc2",     "dc3",   "dc4", "nak",    "syn",  "etb",
 "can",       "em",     "sub",     "esc",   "fs",  "gs",     "rs",   "us",
 "space"
};
typedef unsigned int ptr;
static void print_ptr(ptr x) {
  if ((x & fx_mask) == fx_tag) {
    printf("%d", ((int)x) >> fx_shift);
  } else if (x == bool_f) {
    printf("#f");
  } else if (x == bool_t) {
    printf("#t");
  } else if (x == nil_tag) {
    printf("()");
  } else if ((x & char_mask) == char_tag) {
    int c = (int)(x >> char_shift);
    if (0 <= c && c <= 0x20) {
      printf("#\\%s", ascii_table[c]);
    } else {
      printf("#\\%c", c);
    }
  } else {
    printf("#<unknown 0x%08x>", x);
  }
  printf("\n");
}

static char * allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char * p = mmap(0, aligned_size + 2 * page, 
                  PROT_READ | PROT_WRITE,
                  MAP_ANONYMOUS | MAP_PRIVATE,
                  0, 0);
  if (p == MAP_FAILED) {
    perror("mmap");
  }
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) {
    perror("mprotect");
  }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) {
    perror("mprotect");
  }
  return (p + page);
}

static void deallocate_protected_space(char *p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) {
    perror("munmap");
  }
}

extern int scheme_entry(char *);

int main(int argc, char *argv[]) {
  int stack_size = (32768 * 4096); /* holds 16K cells */
  char * stack_top = allocate_protected_space(stack_size);
  char * stack_base = stack_top + stack_size;
  /*
  register char *r8 asm ("r8") = stack_top;
  register char *r9 asm ("r9") = stack_base;
  */
  print_ptr(scheme_entry(stack_base));
  deallocate_protected_space(stack_top, stack_size);
  return 0;
}
