/* This is the runtime that hosts natively compiled scheme code.
 * It knows how to call the entry point for the compiled scheme code,
 * and how to print the resulting scheme expression.
 */

#include <stdio.h>       /* printf, perror */
#include <stdlib.h>      /* exit */
#include <sys/types.h>   /* mmap */
#define _GNU_SOURCE      /* MAP_ANONYMOUS is not POSIX */
#include <sys/mman.h>    /* mmap, mprotect */
#include <unistd.h>      /* getpagesize */

/* Scheme expressions are encoded in machine words, 64 bit "quad words".
 * The lower bits of each word are used to encode type information. */
/* Boolean type */
#define bool_f      0x2F /* 00101111 */
#define bool_t      0x6F /* 01101111 */
#define bool_mask   0xBF /* 10111111 */
/* Fixnum type */
#define fx_mask     0x03 /* 00000011 */
#define fx_tag      0x00 /* 00000000 */
#define fx_shift       2
/* Char type */
#define char_mask   0xFF /* 11111111 */
#define char_tag    0x0F /* 00001111 */
#define char_shift     8
/* Special nil object */
#define nil_tag     0x3F /* 00111111 */
/* Other types */
#define obj_mask    0x07 /* 00000111 */
#define pair_tag    0x01 /* 00000001 */
#define closure_tag 0x02 /* 00000010 */
#define symbol_tag  0x03 /* 00000011 */
#define string_tag  0x06 /* 00000110 */
#define vector_tag  0x05 /* 00000101 */

#define car(pair)  (* (ptr *) (pair-1))
#define cdr(pair)  (* (ptr *) (pair+7))

/* Context is a structure for storing register values between procedure calls.
 * See System V AMD64 ABI for details on registers and convention.
 * The first six registers, rdi, rsi, rdx, rcx, r8, r9 are used for passing
 * arguments to functions.
 * The registers rbx, rsp, rbp, r12, r13, r14, r15 must be preserved by the
 * called function, the callee.
 * A functions return value is stored in rax and rdx.
 */
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

/* Friendly names for non-printable ASCII codes */
const char *ascii_table[0x40] = {
  "nul",       "soh",    "stx",     "etx",   "eot", "enq",    "ack",  "bel",
  "backspace", "tab",    "newline", "vt",    "np",  "return", "so",   "si",
  "dle",       "dc1",    "dc2",     "dc3",   "dc4", "nak",    "syn",  "etb",
  "can",       "em",     "sub",     "esc",   "fs",  "gs",     "rs",   "us",
  "space"
};

/* A scheme expression is represented by a 8-byte machine word on x86_64 */
typedef unsigned long ptr;

/* Print a scheme expression using the LISP parenthesized notation.
 *   x         Scheme expression, in machine word tagged form
 *   depth     Current depth when recursing through nested pairs/lists
 *   is_head   Non-zero when printing the car of a pair/list, indicates nested pair
 */
static void print_ptr(ptr x, int depth, int is_head) {
  if ((x & fx_mask) == fx_tag) {
    /* Print the fixnum value */
    printf("%d", ((int)x) >> fx_shift);
  } else if (x == bool_f) {
    /* Print false */
    printf("#f");
  } else if (x == bool_t) {
    /* Print true */
    printf("#t");
  } else if (x == nil_tag) {
    /* Print nil, the empty list */
    printf("()");
  } else if ((x & char_mask) == char_tag) {
    /* Print a single character, or friendly name for non-printable values */
    int c = (int)(x >> char_shift);
    if (0 <= c && c <= 0x20) {
      printf("#\\%s", ascii_table[c]);
    } else {
      printf("#\\%c", c);
    }
  } else if ((x & obj_mask) == pair_tag) {
    /* Print dotted pair or list. */
    ptr _car = car(x);
    ptr _cdr = cdr(x);

    /* If this is the first pair in a nested pair or list, open parenthesis. */
    if (is_head) printf("(");

    /* Print the expression stored in the head of pair. */
    print_ptr(_car, depth+1, 1);

    if (_cdr != nil_tag) {

      if ((_cdr & obj_mask) != pair_tag) {
        /* Print the rest of dotted pair. */
        printf(" . ");
        print_ptr(_cdr, depth+1, 0);
        printf(")");
      } else if ((_cdr & obj_mask) == pair_tag) {
        /* Print a space followed by the rest of the list. */
        printf(" ");
        print_ptr(_cdr, depth+1, 0);
      } else {
        printf("ERROR"); /* badly formed expression? */
      }
    } else {
      /* The end of list, print a closing parenthesis. */
      printf(")");
    }
  } else if ((x & obj_mask) == vector_tag) {
    /* Print a vector. Print each object in the vector. */
    ptr *v = (ptr *) (x & ~obj_mask); /* untag vector pointer */
    ptr length = v[0]; /* first quad word is length in bytes of vector, untagged */
    int i;
    length = length >> 3; /* divide by 8 to get number of objects in vector */
    printf("#(");
    for(i = 0; i < length; i++) {
      print_ptr(v[1+i], depth+1, 0);
      if (i < (length-1)) printf(" ");
    }
    printf(")");
  } else if ((x & obj_mask) == string_tag ) {
    /* Print a string. */
    ptr *v = (ptr *) (x & ~obj_mask); /* untag pointer */
    ptr length = v[0]; /* first quad word is length in bytes, untagged */
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
    /* The type tag is unrecognised. */
    printf("#<unknown 0x%08lx>", x);
  }

  /* If at the outer most depth when nested, finish printing with a newline. */
  if (depth == 0) printf("\n");
}

/* Allocate the specified amount in bytes of memory.
 * This also reserves extra space, two pages - the page size specified by the
 * Operation System. One above and one below the usable memory.
 * These two pages are marked for protection so that accidently writing beyond
 * the returned memory will cause an immediate fault - this is to avoid
 * situations where the program runs past it's allocated space and carries on
 * unaware.
 */
static char * allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char * p = mmap(0, aligned_size + 2 * page,
                  PROT_READ | PROT_WRITE,
                  MAP_ANONYMOUS | MAP_PRIVATE,
                  0, 0);
  if (p == MAP_FAILED) {
    perror("mmap"); exit(1);
  }
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) {
    perror("mprotect"); exit(1);
  }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) {
    perror("mprotect"); exit(1);
  }
  return (p + page);
}

/* Free the allocated memory. */
static void deallocate_protected_space(char *p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) {
    perror("munmap"); exit(1);
  }
}

/* scheme_entry is exported by the compiled scheme code. */
extern long scheme_entry(context *, char *, char *);

/* The execution entry point for a C program. */
int main(int argc, char *argv[]) {

  int stack_size = (16 * 4096); /* Holds 8K cells (each cell is 8 bytes). */
  int heap_size  = (16 * 4096);

  /* By convention the stack grows downwards in memory, that is, pushing 
   * something on the stack decreases the stack index.
   * The base of the stack is in fact the top of the allocated space, and
   * the top of the stack is the bottom of the allocated space.
   */
  char * stack_top  = allocate_protected_space(stack_size);
  char * stack_base = stack_top + stack_size;

  /* The heap grows upwards in memory, towards the stack. */
  char * heap_base  = allocate_protected_space(heap_size);

  /* Storage for the registers to preserve state between function calls. */
  context ctxt;

  /* Temporary storage for the result of executing the compiled scheme code. */
  ptr expr = nil_tag;

  /* Run the compiled scheme code.
   *
   * The System V (Five) AMD64 calling convention means that the arguments to
   * scheme_entry are passed in order on the registers:
   *   rdi (ctxt), rsi (stack_base), rdx (heap_base)
   */
  expr = scheme_entry(&ctxt, stack_base, heap_base);

  /* Display the result from running the compiled scheme code. */
  print_ptr(expr, 0 /* depth */,  (expr & obj_mask) == pair_tag  /* is_head */ );

  /* Free the allocated stack and heap memory. */
  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap_base, heap_size);

  return 0;
}
