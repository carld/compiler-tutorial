#include <stdio.h>
#include <ctype.h>
#define bool_f      0x2F
#define bool_t      0x6F
#define bool_mask   0xBF //?

#define fx_mask     0x03
#define fx_tag      0x00
#define fx_shift       2

#define char_mask   0xFF
#define char_tag    0x0F
#define char_shift     8

#define nil_tag     0x3F

const char *ascii_table[0x7F] = {
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
    char c = (char)(x >> char_shift);
    if (iscntrl(c) || isspace(c)) {
      printf("#\\%s", ascii_table[c]);
    } else {
      printf("#\\%c", c);
    }
  } else {
    printf("#<unknown 0x%08x>", x);
  }
  printf("\n");
}
extern int scheme_entry();
int main(int argc, char *argv[]) {
  print_ptr(scheme_entry());
  return 0;
}
