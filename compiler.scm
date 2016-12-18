; Scheme compiler for x86_64

(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.4-req.scm")
(load "tests-1.5-req.scm")
(load "tests-1.6-req.scm")
(load "tests-1.7-req.scm")
(load "tests-1.8-req.scm")
(load "tests-1.9-req.scm")

; The lower bits of a 64 bit machine word contains a Scheme type tag
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
(define objshift  3)
(define objmask   #b00000111)
(define pairtag   #b00000001)
(define clotag    #b00000010)
(define symtag    #b00000011)
(define vectag    #b00000101)
(define strtag    #b00000110)

(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))
(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))
(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))
(define (variable? x)
  (symbol? x))

; Encode an immediate value in a machine word with a type tag
(define (immediate-rep x)
  (cond
    [(fixnum? x) (ash x fxshift)]
    [(boolean? x) (if (equal? x #t) bool-t bool-f)]
    [(char? x)   (logor (ash (char->integer x) charshift) chartag)]
    [(null? x)   niltag]
    [else (errorf 'immediate-rep "no immediate representation for ~s" x)]))

; Declare a global symbol with properties describing it as a code generator
; for a primitive operation.
(define-syntax define-primitive
  (syntax-rules ()
                [(_ (prim-name si env arg* ...) b b* ...)
                 (begin
                   (putprop 'prim-name '*is-prim* #t)
                   (putprop 'prim-name '*arg-count*
                            (length '(arg* ...)))
                   (putprop 'prim-name '*emitter*
                            (lambda (si env arg* ...) b b* ...)))]))

; This macro is the same as define-primitive, apart from the *emitter* which
; has no (lambda ... ), this was to allow declaring a lambda-case *emitter*.
(define-syntax define-variadic-primitive
  (syntax-rules ()
                [(_ (prim-name) b b* ...)
                 (begin
                   (putprop 'prim-name '*is-prim* #t)
                   (putprop 'prim-name '*arg-count* #f)
                   (putprop 'prim-name '*emitter*
                            b b* ...))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (list-starts-with-any? expr val)
  (and (list? expr) (< 0 (length expr)) (memq (car expr) val)))

; Defines procedures that check for a specific symbol at the head of the list.
(define-syntax define-list-head-predicate
  (syntax-rules ()
                [(_ (predicate sym* ...))
                    (define (predicate expr)
                      (list-starts-with-any? expr (list sym* ...)))]))

(define-list-head-predicate (if? 'if))
(define-list-head-predicate (and? 'and))
(define-list-head-predicate (or? 'or))
(define-list-head-predicate (or? 'or))
(define-list-head-predicate (let? 'let 'let*))
(define-list-head-predicate (letrec? 'letrec ))
(define-list-head-predicate (begin? 'begin))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'primitive-emitter "missing emitter for" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (equal? (length args) (getprop prim '*arg-count*)))

; Emits assembly for a primitive.
(define (emit-primcall si env expr tail?)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)
    (if tail? (emit "  ret"))))

; Generate assembly that places an immediate value in the return register, rax.
; If this code is in tail position, return to the caller.
(define (emit-immediate expr tail?)
  (emit "  mov rax, ~s;  immediate" (immediate-rep expr))
  (if tail? (emit "  ret")))

(define let-bindings cadr)
(define let-body cddr)

(define (empty? x)
  (and (list? x) (= 0 (length x))))

(define first car)
(define rest cdr)
(define rhs cadr)
(define lhs car)

; Store the current expression held in rax, on the stack at the given index.
(define (emit-stack-save si)
  (emit "  mov [rsp + ~s], rax; emit-stack-save" si))

; Get the next stack index, a machine word (8 bytes) below the stack index argument.
(define (next-stack-index si)
  (- si wordsize))

; Push the variable name and it's stack index onto the environment.
(define (extend-env var si env)
  (cons (cons var si) env))

; Emit a let or let* expression.
; This emits all of the bindings first, storing them on the stack before
; emitting the let body.
(define (emit-let si env expr tail?)
  (define (process-let bindings si new-env)
    (cond
      [(empty? bindings)
      ; If a let expression is in tail position, then the body of the let is in
      ; tail position.
          (emit-expr si new-env (cons 'begin (let-body expr))  tail?)]
      [else
        ; Emit assembly for evaluating the next let binding
        (let ([b (first bindings)])
          (emit-expr si (if (equal? (car expr) 'let*) new-env env) (rhs b) #f)
          (emit-stack-save si)
          (process-let (rest bindings)
                       (next-stack-index si)
                       (extend-env (lhs b) si new-env)))]))
  (process-let (let-bindings expr) si env))

; Returns the value from an association list, or false.
(define (lookup var alist)
  (let ((val (assoc var alist)))
    (if (pair? val) (cdr val) #f)))

; Emit assembly for loading value from stack into rax register.
(define (emit-stack-load si tail?)
  (emit "  mov rax, [rsp + ~s]; load from stack" si)
  (if tail? (emit "  ret")))

; Look up the expression in the environment and emit code that loads the
; value from the stack using the index.
(define (emit-variable-ref env expr tail?)
  (let ([si (lookup expr env)])
    (cond
      [si  (emit-stack-load si tail?)]
      [else (error 'emit-variable-ref "could not find variable" var)])))

(define if-test cadr)
(define if-conseq caddr)
(define if-altern cadddr)

; Generate the assembly code for an if expression.
(define (emit-if si env expr tail?)
  (let ([alt-label (unique-label)]
        [end-label (unique-label)])
    (emit-expr si env (if-test expr) #f) ; never in tail position
    (emit "  cmp al, ~s;  false?" bool-f)
    (emit "  je ~a;   jump to else" alt-label)
    (emit-expr si env (if-conseq expr) tail?)
    (unless tail? (emit "  jmp ~a;  jump to end" end-label))
    (emit "~a:" alt-label)
    (emit-expr si env (if-altern expr) tail?)
    (unless tail? (emit "~a:" end-label))))

; Transform a cond expression into a nested if expression.
(define (transform-cond expr)
  (let next-cond ([rem (cdr expr)])
    (unless (null? rem)
      `(if ,(caar rem) ,(cadar rem)
          ,(next-cond (cdr rem))))))

; Transform an and expression into a nested if expression.
; (and a b ...)
; (if a (if b #t #f) #f)
(define (transform-and expr)
  (let conseq ([i (cdr expr)])
    (if (null? i)
      #t
      `(if ,(car i) ,(conseq (cdr i)) #f))))

; Transform an or expression into a nested if expression.
; (or a b ...)
; (if a #t (if b #t #f) #f)
(define (transform-or expr)
  (let altern ([i (cdr expr)])
    (if (null? i)
      #f
      `(if ,(car i) #t ,(altern (cdr i))))))

; Generate a unique label for each var in the list
(define (unique-labels lvars)
  (map (lambda (lvar)
         (format "~a_~a" (unique-label) lvar)) lvars))

(define letrec-bindings let-bindings)
(define letrec-body let-body)

; Create an initial association list
(define (make-initial-env lvars labels)
  (map cons lvars labels))

; Generate the assembly code prelude for the compiled scheme expression.
(define (emit-scheme-entry expr env)
  (emit-function-header "L_scheme_entry" )
  (emit-expr (- wordsize) env expr #f)
  (emit "  ret"))

; Emit a letrec expression
; for now letrec is only at the top of the stack?
(define (emit-letrec expr)
  (let* ([bindings (letrec-bindings expr)]
         [lvars (map lhs bindings)]
         [lambdas (map rhs bindings)]
         [labels (unique-labels lvars)]
         [env (make-initial-env lvars labels)])
    (for-each (emit-lambda env) lambdas labels)
    (emit-scheme-entry (cons 'begin (letrec-body expr)) env)))

(define lambda-formals cadr)
(define lambda-body caddr)

; Emit code that evaluates arguments passed to a lambda and then emits code
; that evaluates the lambda body.
(define (emit-lambda env)
  (lambda (expr label)
    (emit-function-header label)
    (let ([fmls (lambda-formals expr)]
          [body (lambda-body expr)])   ; The body of a procedure is in tail position.
      (let f ([fmls fmls] [si (- wordsize)] [env env])
        (cond
          [(empty? fmls)  ; emit expression
              (emit-expr si env body 'tail-position)]
          [else  ; move stack index downwards to accomodate argument,
                 ; and add stack index to environment
            (f (rest fmls)
               (next-stack-index si)
               (extend-env (first fmls) si env))])))))

; Emit code that adds (or subtracts) the size of a machine word to or from the
; stack index.
(define (emit-adjust-base si)
  (cond
    [(> 0 si)  (emit "  sub rsp, ~s; adjust base" (- si))]
    [(< 0 si)  (emit "  add rsp, ~s; adjust base" si)]))

(define call-target car)
(define call-args cdr)

; Emit assembly for a procedure call
(define (emit-call label tail?)
  (if tail?
    (emit "  jmp ~a; tail call" label)
    (emit "  call ~a" label)))

; Emit evaluation of arguments and call a procedure
(define (emit-app si env expr tail?)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr si env (first args) #f)
      (emit-stack-save si)
      (emit-arguments (next-stack-index si) (rest args))))
  ; moves arguments on stack adjacent to rsp, overwriting any local variables.
  (define (emit-move offset si args)
    (unless (empty? args)
      (emit "  mov rax, [rsp + ~s]" si)
      (emit "  mov [rsp + ~s], rax; move arg ~s" (- si offset) (car args))
      (emit-move offset (next-stack-index si) (rest args))))
  (if tail?
    (begin
      (emit-arguments si (call-args expr)) ; evaluates args
      (if (< si (- wordsize)) ; if the stack index is below the return address
        (emit-move (- si (- wordsize)) si (call-args expr))) ;collapse frame
      (emit-call (lookup (call-target expr) env) 'tail-position))
    (begin
      (emit-arguments (- si wordsize) (call-args expr))
      (emit-adjust-base (+ si wordsize))
      (emit-call (lookup (call-target expr) env) #f)
      (emit-adjust-base (- (+ si wordsize))))))

; Determine apply, either when the expression starts with app, or the 
; expression starts with a symbol that is in the environment.
; Note :- revisit this when implementing closures?
(define (app? expr env)
  (cond
    [(list-starts-with-any? expr '(app)) #t]
    [(lookup (car expr) env)       #t]
    [else #f]))

; Remove the app symbol from the head of a list.
(define (chomp-app expr)
  (cond
    [(list-starts-with-any? expr '(app))  (cdr expr)]
    [else expr]))

; Loop through each expression following begin and emit the code for each.
(define (emit-begin si env expr tail?)
  (for-each (lambda(e)
              (emit-expr si env e tail?)) (cdr expr)))

; Emit assembly code based on the form of the given expression.
(define (emit-expr si env expr tail?)
  (cond
    [(immediate? expr) (emit-immediate expr tail?)]
    [(variable? expr)  (emit-variable-ref env expr tail?)] ; gets si from env
    [(if? expr)        (emit-if si env expr tail?)]
    [(and? expr)       (emit-if si env (transform-and expr) tail?)]
    [(or? expr)        (emit-if si env (transform-or expr) tail?)]
    [(let? expr)       (emit-let si env expr tail?)]
    [(begin? expr)     (emit-begin si env expr tail?)]
    [(primcall? expr)  (emit-primcall si env expr tail?)]
    [(app? expr env)   (emit-app si env (chomp-app expr) tail?)] ; primitives shadow environment?
    [else (error 'emit-expr "error in expression" expr)]))

; Emit the entry point for the compiled scheme code.
; The emitted code preserves registers according to the System V ABI.
(define (emit-program expr)
  (if (letrec? expr)
    (emit-letrec expr)
    (emit-scheme-entry expr '()))
  (emit-function-header (or (getenv "ENTRY") "_scheme_entry")) ;"scheme_entry")
  ; parameters in rdi, rsi, rdx, rcx, r8, r9, then stack right to left
  ; preserve registers rbx, rsp, rbp, r12, r13, r14, r15
  (emit "  mov rcx, rdi; store context pointer in rdx") ; allocated context argument
  (emit "  mov [rcx + 8], rbx")
  (emit "  mov [rcx + 48], rsp")
  (emit "  mov [rcx + 56], rbp")
  (emit "  mov [rcx + 96], r12")
  (emit "  mov [rcx + 104], r13")
  (emit "  mov [rcx + 112], r14")
  (emit "  mov [rcx + 120], r15")
  (emit "  mov rsp, rsi") ; allocated stack base argument, calling convention puts it in rdi ...
  (emit "  mov rbp, rdx") ; allocated heap base argument, 
  (emit "  call L_scheme_entry")  ; push rip to rsp and jmp
  (emit "  mov rbx, [rcx + 8]")
  (emit "  mov rsp, [rcx + 48]")
  (emit "  mov rbp, [rcx + 56]")
  (emit "  mov r12, [rcx + 96]")
  (emit "  mov r13, [rcx + 104]")
  (emit "  mov r14, [rcx + 112]")
  (emit "  mov r15, [rcx + 120]")
  (emit "  ret")) ; pop rsp and jump

; Export and declare a label in the emitted assembly code.
(define (emit-function-header name)
  (emit "global ~a" name)
  (emit "~a:" name))

; Defines procedures that emit primitives of one argument.
(define-syntax define-primitive-unary
  (syntax-rules ()
               [(_ (name) b ...)
                (define-primitive (name si env arg)
                  (emit-expr si env arg #f)
                  b ... )]))

; Add one to the fixnum value held in the rax register.
(define-primitive-unary (fxadd1)
  (emit "  add rax, ~s" (immediate-rep 1)))  ; add x, y   x ← x + y

; Subtract one from the fixnum value held in the rax register.
(define-primitive-unary (fxsub1)
  (emit "  sub rax, ~s" (immediate-rep 1)))

; Convert the fixnum to a tagged character.
(define-primitive-unary (fixnum->char)
  (emit "  shl rax, ~s" (- charshift fxshift))  ; shift left 8 - 2 = 6 bits
  (emit "  or  rax, ~s" chartag))               ; or 00001111

; Convert the character to a tagged fixnum.
(define-primitive-unary (char->fixnum)
  (emit "  shr rax, ~s" (- charshift fxshift))
  (emit "  and rax, ~s" (lognot fxmask)))

; Pairs are tagged using the first bit, 0x01,
; subtracting one, results in a pointer to the car, implicitly untagging.
(define-primitive (car si env arg)
  (emit "  mov rax, [ rax - 1 ]; car"))

; Pairs are tagged using the first bit, 0x01,
; adding 7 results in a pointer to the cdr, implicity untagging.
(define-primitive (cdr si env arg)
  (emit "  mov rax, [ rax + 7 ]; cdr"))

; Sets the car of a pair.
(define-primitive (set-car! si env arg1 arg2)
  (emit-exprs-load si env arg1 arg2 'rax 'rbx)
  (emit "  mov [ rax - 1 ], rbx"))  ; untag (pairtag is 1), and set car

; Sets the cdr of a pair.
(define-primitive (set-cdr! si env arg1 arg2)
  (emit-exprs-load si env arg1 arg2 'rax 'rbx)
  (emit "  mov [ rax + 7 ], rbx"))  ; untag and offset, and set cdr

; Emits code that will allocate a new pair on the heap,
; and move the heap pointer, stored in register rbp.
(define-primitive (cons si env arg1 arg2)
  (emit-expr si env arg1 #f)
  (emit "  mov [ rbp + 0 ], rax")
  (emit-expr si env arg2 #f)
  (emit "  mov [ rbp + 8 ], rax")
  (emit "  mov rax, rbp")
  (emit "  or  rax, ~s" pairtag)
  (emit "  add rbp, 16"))

; Emits code that will set the al (low 8 bits of rax) register to 1 based on
; the given operator argument and the flags state after a cmp
(define (emit-true-using set-byte-on-condition)
  (emit "  ~s al" set-byte-on-condition)  ; set equal: set to 1 otherwise 0 on condition (ZF=0)
  (emit "  movsx rax, al")
  (emit "  sal al, ~s" bool-bit)
  (emit "  or  al, ~s" bool-f))

; Defines a procedure that determines whether it's argument is of a specific type
; and leaves true in the rax register if so.
(define-syntax define-primitive-predicate
  (syntax-rules ()
    [(_ (name tag-or-value) b ...)
     (define-primitive (name si env arg)
        (emit-expr si env arg #f)
        b ...
        (emit "  cmp rax, ~s" tag-or-value)
        (emit-true-using 'sete))]
    [(_ (name tag-or-value mask))
     (define-primitive-predicate (name tag-or-value)
        (emit "  and rax, ~s" mask))]))

(define-primitive-predicate (fxzero? 0))
(define-primitive-predicate (fixnum? fxtag fxmask))
(define-primitive-predicate (pair? pairtag objmask))
(define-primitive-predicate (null? niltag))
(define-primitive-predicate (boolean? bool-f boolmask))
(define-primitive-predicate (char? chartag charmask))
(define-primitive-predicate (vector? vectag objmask))
(define-primitive-predicate (string? strtag objmask))

; The primitive not takes any kind of value and returns #t if the object is #f,
; otherwise it returns #f.
(define-primitive (not si env arg)
  (emit-expr si env arg #f)
  (emit "  cmp rax, ~s" bool-f)
  (emit-true-using 'sete))

; Returns a string containing a unique label and increments an internal counter.
(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        L))))

; Emits code that adds it's two fixnum expressions and leaves the result in 
; the rax register.
(define-primitive (fx+ si env arg1 arg2)
  (emit-expr si env arg1 #f)
  (emit "  mov [rsp + ~s], rax;  put on stack" si)
  (emit-expr (next-stack-index si) env arg2 #f)
  (emit "  add rax, [rsp + ~s];  add stack and rax" si))

; Emits code that adds two fixnums and puts the result in the rax register.
(define-primitive (fx- si env arg1 arg2)
  (emit-expr si env arg2 #f)   ; rax <- arg1
  (emit "  mov [rsp + ~s], rax" si)
  (emit-expr (next-stack-index si) env arg1 #f)
  (emit "  sub rax, [rsp + ~s]" si))

; Emits code that multiplies two fixnums and puts the result in the rax
; register.
(define-primitive (fx* si env arg1 arg2)
  (emit-exprs-load si env arg1 arg2 'rax 'rbx)
  (emit "  imul ebx") ; eax * ebx
  (emit "  mov ebx, 4")
  (emit "  idiv ebx")) ; eax / ebx

; Emits code that evaluates two expressions, saves them to the stack from the
; given stack index, and also stores the results in the two register arguments.
(define (emit-exprs-load si env arg1 arg2 register1 register2)
  (emit-expr si env arg1 #f)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env arg2 #f)
  (emit-stack-save (next-stack-index si))
  (emit "  mov ~s, [rsp + ~s]" register1 si) ; (emit-stack-load si)
  (emit "  mov ~s, [rsp + ~s]" register2 (next-stack-index si))) ; (emit-stack-load (- si wordsize))

; Emits code that performs a logical or on it's two arguments.
(define-primitive (fxlogor si env arg1 arg2)
  (emit-exprs-load si env arg1 arg2 'rax 'rbx)
  (emit "  or rax, rbx"))

; Emits code that performs a logical not on it's fixnum argument and leaves 
; the result in the rax register.
(define-primitive-unary (fxlognot)
  (emit "  shr rax, ~s" fxshift)
  (emit "  not rax")
  (emit "  shl rax, ~s" fxshift))

; Emits code that does a logical and on it's fixnum arguments and leaves the
; result in the rax register.
(define-primitive (fxlogand si env arg1 arg2)
  (emit-exprs-load si env arg1 arg2 'rax 'rbx)
  (emit "  and rax, rbx"))

; Defines a procedure that will evaluate it's two arguments using the provided
; comparison operator.
(define-syntax define-primitive-compare
  (syntax-rules ()
                [(_ (prim-name operator-instruction))
                  (define-primitive (prim-name si env arg1 arg2)
                                    (emit-exprs-load si env arg1 arg2 'rax 'rbx)
                                    (emit "  cmp rax, rbx" )
                                    (emit-true-using operator-instruction))]))

(define-primitive-compare (fx= 'sete))
(define-primitive-compare (fx< 'setl))
(define-primitive-compare (fx<= 'setle))
(define-primitive-compare (fx> 'setg))
(define-primitive-compare (fx>= 'setge))
(define-primitive-compare (eq? 'sete))
(define-primitive-compare (char= 'sete))

(define-variadic-primitive (make-vector)
  (case-lambda
    [(si env len) (apply (primitive-emitter 'make-vector) si env (list len #f))]
    [(si env len val)
        (let ([label (unique-label)])
          (emit-expr si env len #f)
          (emit "  sar rax, ~s" fxshift) ; untag length
          (emit "  sal rax, 3") ; multiply by 8
          (emit "  mov [ rbp ], rax")   ; set vector length
          (emit-expr si env val #f)
          (emit "  mov rbx, rax") ;
          (emit "  mov rdi, 8; offset")
          (emit "~a:" label)
          (emit "  mov [ rbp + rdi ], rbx")
          (emit "  add rdi, 8")
          (emit "  cmp rdi, [ rbp ]")
          (emit "  jle ~a" label)
          (emit "  mov rax, rbp")
          (emit "  or  rax, ~s" vectag)
          (emit "  add rbp, rdi"))]))

(define-variadic-primitive (make-string)
  (case-lambda
    [(si env len) (apply (primitive-emitter 'make-string) si env (list len #f))]
    [(si env len val)
        (let ([label (unique-label)])
          (emit-expr si env len #f)
          (emit "  sar rax, ~s" fxshift) ; untag length
          (emit "  sal rax, 3") ; multiply by 8
          (emit "  mov [ rbp ], rax")   ; set length
          (emit-expr si env val #f)
          (emit "  mov rbx, rax") ;
          (emit "  mov rdi, 8; offset")
          (emit "~a:" label)
          (emit "  mov [ rbp + rdi ], rbx")
          (emit "  add rdi, 8")
          (emit "  cmp rdi, [ rbp ]")
          (emit "  jle ~a" label)
          (emit "  mov rax, rbp")
          (emit "  or  rax, ~s" strtag)
          (emit "  add rbp, rdi"))]))

(define-primitive (vector-length si env arg)
  (emit-expr si env arg #f)
  ; assuming rax is actually a vector
  (emit "  sar rax, ~s" objshift) ;untag
  (emit "  sal rax, ~s" objshift) ;untag
  (emit "  mov rax, [rax]")
  (emit "  sar rax, 3") ; divide by 8
  (emit "  sal rax, ~s" fxshift)
  (emit "  or  rax, ~s" fxtag))

(define-primitive (string-length si env arg)
  (emit-expr si env arg #f)
  ; assuming rax is actually a string
  (emit "  sar rax, ~s" objshift) ;untag
  (emit "  sal rax, ~s" objshift) ;untag
  (emit "  mov rax, [rax]")
  (emit "  sar rax, 3") ; divide by 8
  (emit "  sal rax, ~s" fxshift)
  (emit "  or  rax, ~s" fxtag))

(define-primitive (vector-set! si env v index value)
  (emit-expr si env value #f)
  (emit "  mov rdx, rax")
  (emit-expr si env index #f)
  (emit "  mov rbx, rax")
  (emit "  sar rbx, ~s" fxshift); untag index
  (emit "  sal rbx, 3"); multiply index by 8
  (emit "  add rbx, 8"); offset index past length
  (emit-expr si env v #f)
  (emit "  sar rax, ~s" objshift) ;untag vector
  (emit "  sal rax, ~s" objshift) ;untag vector
  (emit "  mov [ rax + rbx ], rdx"))

(define-primitive (string-set! si env v index value)
  (emit-expr si env value #f)
  (emit "  mov rdx, rax")
  (emit-expr si env index #f)
  (emit "  mov rbx, rax")
  (emit "  sar rbx, ~s" fxshift); untag index
  (emit "  sal rbx, 3"); multiply index by 8
  (emit "  add rbx, 8"); offset index past length
  (emit-expr si env v #f)
  (emit "  sar rax, ~s" objshift) ;untag
  (emit "  sal rax, ~s" objshift) ;untag
  (emit "  mov [ rax + rbx ], rdx"))

(define-primitive (vector-ref si env v index)
  (emit-exprs-load si env v index 'rbx 'rdx)
  (emit "  sar rbx, ~s" objshift) ;untag vector
  (emit "  sal rbx, ~s" objshift) ;untag vector
  (emit "  sar rdx, ~s" fxshift); untag index
  (emit "  sal rdx, 3"); multiply index by 8
  (emit "  add rdx, 8"); offset index past length
  (emit "  mov rax, [ rbx + rdx ]"))

(define-primitive (string-ref si env v index)
  (emit-exprs-load si env v index 'rbx 'rdx)
  (emit "  sar rbx, ~s" objshift) ;untag
  (emit "  sal rbx, ~s" objshift) ;untag
  (emit "  sar rdx, ~s" fxshift); untag index
  (emit "  sal rdx, 3"); multiply index by 8
  (emit "  add rdx, 8"); offset index past length
  (emit "  mov rax, [ rbx + rdx ]"))

