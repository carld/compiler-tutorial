#include <stdio.h>
#include <sys/types.h>
#define _GNU_SOURCE  /* MAP_ANONYMOUS is not POSIX */
#include <sys/mman.h>
#include <unistd.h>
#define bool_f      0x2F
#define bool_t      0x6F
#define bool_mask   0xBF

#define fx_mask     0x03 /* 00000011 */
#define fx_tag      0x00
#define fx_shift       2

#define char_mask   0xFF
#define char_tag    0x0F
#define char_shift     8

#define nil_tag     0x3F

#define obj_mask    0x07 /* 00000111 */
#define pair_tag    0x01 /* 00000001 */
#define closure_tag 0x02 /* 00000010 */
#define symbol_tag  0x03 /* 00000011 */
#define string_tag  0x06 /* 00000110 */
#define vector_tag  0x05 /* 00000101 */

#define car(pair)  (* (ptr *) (pair-1))
#define cdr(pair)  (* (ptr *) (pair+7))

typedef struct {
  void *rax; /*   0   scratch   */
  void *rbx; /*   8   preserve  */
  void *rcx; /*  16   scratch   */
  void *rdx; /*  24   scratch   */
  void *rdi; /*  32   scratch   */
  void *rsi; /*  40   scratch   */
  void *rsp; /*  48   preserve  */
  void *rbp; /*  56   preserve  */
  void *r8;  /*  64   scratch   */
  void *r9;  /*  72   scratch   */
  void *r10; /*  80   scratch   */
  void *r11; /*  88   scratch   */
  void *r12; /*  96   preserve  */
  void *r13; /* 104   preserve  */
  void *r14; /* 112   preserve  */
  void *r15; /* 120   preserve  */
} context;

const char *ascii_table[0x80] = {
  "nul",       "soh",    "stx",     "etx",   "eot", "enq",    "ack",  "bel",
  "backspace", "tab",    "newline", "vt",    "np",  "return", "so",   "si",
  "dle",       "dc1",    "dc2",     "dc3",   "dc4", "nak",    "syn",  "etb",
  "can",       "em",     "sub",     "esc",   "fs",  "gs",     "rs",   "us",
  "space"
};
typedef unsigned long ptr;
static void print_ptr(ptr x, int depth, int from_car) {
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
  } else if ((x & obj_mask) == pair_tag) {
    ptr _car = car(x);
    ptr _cdr = cdr(x);
    if (from_car) printf("(");
    print_ptr(_car, depth+1, 1);
    if (_cdr != nil_tag) {
      if ((_cdr & obj_mask) != pair_tag) { /* dotted pair */
        printf(" . ");
        print_ptr(_cdr, depth+1, 0);
        printf(")");
      } else if ((_cdr & obj_mask) == pair_tag) { /* list */
        printf(" ");
        print_ptr(_cdr, depth+1, 0);
      } else {
        printf("ERROR"); /* badly formed expression? */
      }
    } else { /* end of list */
      printf(")");
    }
  } else if ((x & obj_mask) == vector_tag) {
    ptr *v = (ptr *) (x & ~obj_mask); /* untag vector pointer */
    ptr length = v[0]; /* first quad word is length in bytes of vector */
    int i;
    length = length >> 3; /* divide by 8 to get number of objects in vector */
    printf("#(");
    for(i = 0; i < length; i++) {
      print_ptr(v[1+i], depth+1, 0);
      if (i < (length-1)) printf(" ");
    }
    printf(")");
  } else if ((x & obj_mask) == string_tag ) {
    ptr *v = (ptr *) (x & ~obj_mask); /* untag pointer */
    ptr length = v[0]; /* first quad word is length in bytes */
    int i;
    length = length >> 3; /* divide by 8 to get number of chars in string */
    printf("\"");
    for(i = 0; i < length; i++) {
      int c = (int)(v[1+i] >> char_shift);
      if (c=='"') printf("\\\"");
      else if (c=='\\') printf("\\\\");
      else printf("%c", c);
    }
    printf("\"");
  } else {
    printf("#<unknown 0x%08lx>", x);
  }

  if (depth == 0) printf("\n");
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

extern long scheme_entry(context *, char *, char *);

int main(int argc, char *argv[]) {
  int stack_size = (16 * 4096); /* holds 16K cells */
  int heap_size = (16 * 4096);
  char * stack_top = allocate_protected_space(stack_size);
  char * stack_base = stack_top + stack_size;
  char * heap_base = allocate_protected_space(heap_size);
  context ctxt;
  print_ptr(scheme_entry(&ctxt, stack_base, heap_base), 0, 1);
  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap_base, heap_size);
  return 0;
}
