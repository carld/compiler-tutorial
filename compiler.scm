
(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")

(define fxshift    2)
(define fxmask  #x03)
(define bool_f  #x2F)
(define bool_t  #x6F)
(define wordsize   4)
(define charmask  #xFF)
(define chartag   #b00001111) ; 0x0F
(define charshift  8)
(define niltag    #b00111111)
(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))
(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))
(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))
(define (immediate-rep x)
  (cond
    [(fixnum? x) (ash x fxshift)]
    [(boolean? x) (if (equal? x #t) bool_t bool_f)]
    [(char? x)   (logor (ash (char->integer x) charshift) chartag)]
    [(null? x)   niltag]
    [else (errorf 'immediate-rep "can't represent ~s" x)]
    ))

(define (emit-program x)
  (unless (immediate? x) (error 'emit-program "can't compile: " x))
  (emit "section .text")
  (emit "global _scheme_entry")
  (emit "_scheme_entry:")
  (emit "  mov rax, ~s" (immediate-rep x))
  (emit "  ret"))

