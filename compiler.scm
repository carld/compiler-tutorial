
(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.4-req.scm")
(load "tests-1.5-req.scm")
(load "tests-1.6-req.scm")

(define fxshift    2)
(define fxmask  #x03)
(define fxtag   #x00)
(define wordsize   8)
(define boolmask        #b10111111)
(define booltag           #b101111)
(define bool-f  #x2F) ; #b00101111
(define bool-t  #x6F) ; #b01101111
(define bool-bit   6)
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
(define (variable? x)
  (symbol? x))

(define (immediate-rep x)
  (cond
    [(fixnum? x) (ash x fxshift)]
    [(boolean? x) (if (equal? x #t) bool-t bool-f)]
    [(char? x)   (logor (ash (char->integer x) charshift) chartag)]
    [(null? x)   niltag]
    [else (errorf 'immediate-rep "no immediate representation for ~s" x)]
    ))

(define-syntax define-primitive
  (syntax-rules ()
                [(_ (prim-name si env arg* ...) b b* ...)
                 (begin
                   (putprop 'prim-name '*is-prim* #t)
                   (putprop 'prim-name '*arg-count*
                            (length '(arg* ...)))
                   (putprop 'prim-name '*emitter*
                            (lambda (si env arg* ...) b  b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (if? expr) ; test body-when-true body-when-false)
  (and (list? expr)
       (equal? (car expr) 'if)))

(define (and? expr)
  (and (list? expr)
       (equal? (car expr) 'and)))

(define (or? expr)
  (and (list? expr)
       (equal? (car expr) 'or)))

(define (let? expr)
  (and (list? expr)
       (equal? (car expr) 'let)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'primitive-emitter "missing emitter for" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (equal? (length args) (getprop prim '*arg-count*)))

(define (emit-primcall si env expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

(define (emit-immediate expr)
  (emit "  mov rax, ~s" (immediate-rep expr)))

(define (let-bindings expr)
  (cadr expr))

(define (let-body expr)
  (caddr expr))

(define (empty? x)
  (and (list? x)
       (= 0 (length x))))

(define (first x)
  (car x))

(define (rest x)
  (cdr x))

(define (rhs x)
  (cadr x))

(define (lhs x)
  (car x))

(define (emit-stack-save si)
  (emit "  mov [rsp + ~s], eax" si))

(define (next-stack-index si)
  (- si wordsize))

(define (extend-env var si env)
  (cons (cons var si) env))

(define (emit-let si env expr)
  (define (process-let bindings si new-env)
    (cond
      [(empty? bindings)
       (emit-expr si new-env (let-body expr))]
      [else
        (let ([b (first bindings)])
          (emit-expr si env (rhs b))
          (emit-stack-save si)
          (process-let (rest bindings)
                       (next-stack-index si)
                       (extend-env (lhs b) si new-env)))]))
  (process-let (let-bindings expr) si env))

(define (lookup v e)
  (cdr (assoc v e)))

(define (emit-stack-load si)
  (emit "  mov rax, [rsp + ~s]" si))

(define (emit-variable-ref env var)
  (cond
    [(lookup var env) => emit-stack-load]
    [else (error 'emit-variable-ref "could not find variable ref." var)]))

(define (if-test expr)
  (cadr expr))

(define (if-conseq expr)
  (caddr expr))

(define (if-altern expr)
  (cadddr expr))

(define (emit-if si env expr)
  (let ([alt-label (unique-label)]
        [end-label (unique-label)])
    (emit-expr si env (if-test expr))
    (emit "  cmp al, ~s" bool-f)
    (emit "  je ~a" alt-label)
    (emit-expr si env (if-conseq expr))
    (emit "  jmp ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr si env (if-altern expr))
    (emit "~a:" end-label)))

; (and a b ...)
; (if a (if b #t #f) #f)
(define (transform-and expr)
  (let conseq ([i (cdr expr)])
    (if (null? i)
      #t
      `(if ,(car i) ,(conseq (cdr i)) #f))))

; (or a b ...)
; (if a #t (if b #t #f) #f)
(define (transform-or expr)
  (let altern ([i (cdr expr)])
    (if (null? i)
      #f
      `(if ,(car i) #t ,(altern (cdr i))))))

(define (emit-expr si env expr)
  (cond
    [(immediate? expr) (emit-immediate expr)]
    [(variable? expr)  (emit-variable-ref env expr)]
    [(if? expr)        (emit-if si env expr)]
    [(and? expr)       (emit-if si env (transform-and expr))]
    [(or? expr)        (emit-if si env (transform-or expr))]
    [(let? expr)       (emit-let si env expr)]
    [(primcall? expr)  (emit-primcall si env expr)]
    [else (error 'emit-expr "type not supported" expr)]))

(define (emit-program expr)
  (emit-function-header "L_scheme_entry")
  (emit-expr (- wordsize) '() expr)
  (emit "  ret")
  (emit-function-header "_scheme_entry")
  (emit "  mov rcx, rsp")  ; save the C stack pointer
  (emit "  mov [rsp + 8], rsp") ; stack base argument?
  (emit "  call L_scheme_entry")  ; push instruction pointer on stack and jmp
  (emit "  mov rsp, rcx")  ; restore the C stack pointer
  (emit "  ret"))

(define (emit-function-header name)
  (emit "global ~a" name)
  (emit "~a:" name))

; The primitive fxadd1 takes one argument, which must evaluate to a fixnum, and
; returns that value incremented by 1. The implemen- tation of fxadd1 should
; first emit the code for evaluating the argument. Evaluating that code at
; runtime would place the value of the argument at the return-value register
; %eax. The value placed in %eax should therefore be incremented and the new
; computed value should be placed back in %eax. Remember though that all the
; fixnums in our system are shifted to the left by two. So, a fxadd1 instruction
; translates to an instruction that increments %eax by 4.
(define-primitive ($fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "  add rax, ~s" (immediate-rep 1)))  ; add x, y   x ← x + y

(define-primitive ($fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "  sub rax, ~s" (immediate-rep 1)))

(define-primitive ($fixnum->char si env arg)
  (emit-expr si env arg)                            ; mov rax, arg
  (emit "  shl rax, ~s" (- charshift fxshift))  ; shift left 8 - 2 = 6 bits
  (emit "  or  rax, ~s" chartag))               ; or 00001111

; The implementation of the primitive char->fixnum should evaluate its argument,
; which must evaluate to a character, then convert the value to the appropriate
; fixnum. Since we defined the tag for characters to be 00001111b and the tag
; for fixnums to be 00b, it suffices to shift the character value to the right
; by six bits to obtain the fixnum value. The primitive fixnum->char should
; shift the fixnum value to the left, then tag the result with the character
; tag. Tagging a value is performed using the instruction orl.
(define-primitive ($char->fixnum si env arg)
  (emit-expr si env arg)  ; mov rax, arg
  (emit "  shr rax, ~s" (- charshift fxshift))
  (emit "  and rax, ~s" (lognot fxmask)))

; Implementing predicates such as fixnum? is not as simple. First, after the
; argument to fixnum? is evaluated, the lower two bits of the result must be
; extracted and compared to the fixnum tag 00b. If the comparison succeeds, we
; return the true value, otherwise we return the false value. Extracting the
; lower bits using the fixnum mask is done using the bitwise-and instructions
; and/andl2. The result is compared with the fixnum tag using the cmp/cmpl
; instruction. The Intel-386 architecture provides many instructions for
; conditionally setting the lower half of a register by either a 1 or a 0
; depedning on the relation of the objects involved in the comparison. One such
; instruction is sete which sets the argument register to 1 if the two compared
; numbers were equal and to 0 otherwise. A small glitch here is that the sete
; instruction only sets a 16-bit register. To work around this problem, we use
; the movzbl instruction that sign-extends the lower half of the register to the
; upper half. Since both 0 and 1 have 0 as their sign bit, the result of the
; extension is that the upper bits will be all zeros. Finally, the result of the
; comparison is shifted to the left by an appropriate number of bits and or’ed
; with the false value 00101111b to obtain either the false value or the true
; value 01101111b.
(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "  and al, ~s" fxmask)
  (emit "  cmp al, ~s" fxtag)
  (emit "  sete al")  ; set equal: set to 1 otherwise 0 on condition (ZF=0)
  (emit "  movsx rax, al")
  (emit "  sal al, ~s" bool-bit)
  (emit "  or  al, ~s" bool-f))

(define (emit-true-when-equal)
  (emit "  sete al")  ; set equal: set to 1 otherwise 0 on condition (ZF=0)
  (emit "  movsx rax, al")
  (emit "  sal al, ~s" bool-bit)
  (emit "  or  al, ~s" bool-f))

(define-primitive ($fxzero? si env arg)
  (emit-expr si env arg)
  (emit "  cmp rax, 0")
  (emit-true-when-equal))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit "  cmp al, ~s" niltag)
  (emit-true-when-equal))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit "  and rax, ~s" boolmask)
  (emit "  cmp rax, ~s" bool-f)
  (emit-true-when-equal))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "  and rax, ~s" charmask)
  (emit "  cmp rax, ~s" chartag)
  (emit-true-when-equal))

; The primitive not takes any kind of value and returns #t if the object is #f,
; otherwise it returns #f.
;  (emit "  xor rax, ~s" #b01000000) ; not so simple
(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit "  cmp rax, ~s" bool-f)
  (emit-true-when-equal))

(define-primitive ($fxlognot si env arg)
  (emit-expr si env arg)
  (emit "  shr rax, ~s" fxshift)
  (emit "  not rax")
  (emit "  shl rax, ~s" fxshift))

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        L))))

(define-primitive (fx+ si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "  mov [rsp + ~s], rax" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "  add rax, [rsp + ~s]" si))

; Not sure why subtracting signed numbers don't need to be
; right shifted (untagged) here.
; Note the arguments go on the stack in reverse order
(define-primitive (fx- si env arg1 arg2)
  (emit-expr si env arg2)   ; rax <- arg1
  (emit "  mov [rsp + ~s], rax" si) ; push?
  (emit-expr (- si wordsize) env arg1)
  (emit "  sub rax, [rsp + ~s]" si))

(define-primitive (fx* si env arg1 arg2)
   (emit-expr si env arg2)   ; eval arg and put in rax
   (emit "  mov [rsp + ~s], rax" si)
   (emit-expr (- si wordsize) env arg1) ; eval arg and put in rax
   (emit "  mov [rsp + ~s], rax" (- si wordsize))
   (emit "  mov eax, [rsp + ~s]" si)
   (emit "  mov ebx, [rsp + ~s]" (- si wordsize))
   (emit "  imul ebx") ; eax * ebx
   (emit "  mov ebx, 4")
   (emit "  idiv ebx")) ; eax / ebx

(define-primitive (fxlogor si env arg1 arg2)
   (emit-expr si env arg2)
   (emit "  mov [rsp + ~s], rax" si)
   (emit-expr (- si wordsize) env arg1)
   (emit "  mov [rsp + ~s], rax" (- si wordsize))
   (emit "  mov rax, [rsp + ~s]" si)
   (emit "  mov rbx, [rsp + ~s]" (- si wordsize))
   (emit "  or rax, rbx"))

(define-primitive (fxlognot si env arg1)
  (emit-expr si env arg1)
  (emit "  shr rax, ~s" fxshift)
  (emit "  not rax")
  (emit "  shl rax, ~s" fxshift))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "  mov [rsp + ~s], rax" si)
  (emit-expr (- si wordsize) env arg1)
  (emit "  mov [rsp + ~s], rax" (- si wordsize))
  (emit "  mov rax, [rsp + ~s]" si)
  (emit "  mov rbx, [rsp + ~s]" (- si wordsize))
  (emit "  and rax, rbx"))

(define-primitive (fx= si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "  mov [rsp + ~s], rax" si)
  (emit-expr (- si wordsize) env arg1)
  (emit "  mov [rsp + ~s], rax" (- si wordsize))
  (emit "  mov rax, [rsp + ~s]" si)
  (emit "  mov rbx, [rsp + ~s]" (- si wordsize))
  (emit "  cmp rax, rbx")
  (emit-true-when-equal))

(define-primitive (fx< si env arg1 arg2)
  (let ([true-label (unique-label)]
        [end-label (unique-label)])
  (emit-expr si env arg2)
  (emit "  mov [rsp + ~s], rax" si)
  (emit-expr (- si wordsize) env arg1)
  (emit "  mov [rsp + ~s], rax" (- si wordsize))
  (emit "  mov rbx, [rsp + ~s]" si)
  (emit "  mov rax, [rsp + ~s]" (- si wordsize))
  (emit "  cmp rax, rbx")
  (emit "  setl al")
  (emit "  movsx rax, al")
  (emit "  sal al, ~s" bool-bit)
  (emit "  or  al, ~s" bool-f)))

(define-primitive (fx<= si env arg1 arg2)
  (let ([true-label (unique-label)]
        [end-label (unique-label)])
  (emit-expr si env arg2)
  (emit "  mov [rsp + ~s], rax" si)
  (emit-expr (- si wordsize) env arg1)
  (emit "  mov [rsp + ~s], rax" (- si wordsize))
  (emit "  mov rbx, [rsp + ~s]" si)
  (emit "  mov rax, [rsp + ~s]" (- si wordsize))
  (emit "  cmp rax, rbx")
  (emit "  setle al")
  (emit "  movsx rax, al")
  (emit "  sal al, ~s" bool-bit)
  (emit "  or  al, ~s" bool-f)))

(define-primitive (fx> si env arg1 arg2)
  (let ([true-label (unique-label)]
        [end-label (unique-label)])
  (emit-expr si env arg2)
  (emit "  mov [rsp + ~s], rax" si)
  (emit-expr (- si wordsize) env arg1)
  (emit "  mov [rsp + ~s], rax" (- si wordsize))
  (emit "  mov rbx, [rsp + ~s]" si)
  (emit "  mov rax, [rsp + ~s]" (- si wordsize))
  (emit "  cmp rax, rbx")
  (emit "  setg al")
  (emit "  movsx rax, al")
  (emit "  sal al, ~s" bool-bit)
  (emit "  or  al, ~s" bool-f)))

(define-primitive (fx>= si env arg1 arg2)
  (let ([true-label (unique-label)]
        [end-label (unique-label)])
  (emit-expr si env arg2)
  (emit "  mov [rsp + ~s], rax" si)
  (emit-expr (- si wordsize) env arg1)
  (emit "  mov [rsp + ~s], rax" (- si wordsize))
  (emit "  mov rbx, [rsp + ~s]" si)
  (emit "  mov rax, [rsp + ~s]" (- si wordsize))
  (emit "  cmp rax, rbx")
  (emit "  setge al")
  (emit "  movsx rax, al")
  (emit "  sal al, ~s" bool-bit)
  (emit "  or  al, ~s" bool-f)))

