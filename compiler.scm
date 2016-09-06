
(define (compile-program x)
  (unless (integer? x) (error ---))
  (emit "global _scheme_entry")
  (emit "section .text")
  (emit "_scheme_entry:")
  (emit "  mov rax, ~s" x)
  (emit "  ret")

  )

(load "tests-driver.scm")
(load "tests-1.1-req.scm")


