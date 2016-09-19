(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.4-req.scm")
(load "tests-1.5-req.scm")
(load "tests-1.6-req.scm")
(load "tests-1.7-req.scm")
(load "tests-1.8-req.scm")

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
    [else (errorf 'immediate-rep "no immediate representation for ~s" x)]))

(define-syntax define-primitive
  (syntax-rules ()
                [(_ (prim-name si env arg* ...) b b* ...)
                 (begin
                   (putprop 'prim-name '*is-prim* #t)
                   (putprop 'prim-name '*arg-count*
                            (length '(arg* ...)))
                   (putprop 'prim-name '*emitter*
                            (lambda (si env arg* ...) b b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (list-starts-with-any? expr val)
  (and (list? expr) (< 0 (length expr)) (memq (car expr) val)))

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

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'primitive-emitter "missing emitter for" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (equal? (length args) (getprop prim '*arg-count*)))

(define (emit-primcall si env expr tail?)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)
    (if tail? (emit "  ret"))))

(define (emit-immediate expr tail?)
  (emit "  mov rax, ~s;  immediate" (immediate-rep expr))
  (if tail? (emit "  ret")))

(define let-bindings cadr)
(define let-body caddr)

(define (empty? x)
  (and (list? x) (= 0 (length x))))

(define first car)
(define rest cdr)
(define rhs cadr)
(define lhs car)

(define (emit-stack-save si)
  (emit "  mov [rsp + ~s], rax; emit-stack-save" si))

(define (next-stack-index si)
  (- si wordsize))

(define (extend-env var si env)
  (cons (cons var si) env))

; todo: implement let*
(define (emit-let si env expr tail?)
  (define (process-let bindings si new-env)
    (cond
      [(empty? bindings)
      ; If a let expression is in tail position, then the body of the let is in
      ; tail position.
        (emit-expr si new-env (let-body expr) tail?)]
      [else
        (let ([b (first bindings)])
          (emit-expr si env (rhs b) #f)
          (emit-stack-save si)
          (process-let (rest bindings)
                       (next-stack-index si)
                       (extend-env (lhs b) si new-env)))]))
  (process-let (let-bindings expr) si env))

(define (lookup var alist)
  (let ((val (assoc var alist)))
    (if (pair? val) (cdr val) #f)))

(define (emit-stack-load si tail?)
  (emit "  mov rax, [rsp + ~s]; load from stack" si)
  (if tail? (emit "  ret")))

(define (emit-variable-ref env expr tail?)
  (let ([si (lookup expr env)])
    (cond
      [si  (emit-stack-load si tail?)]
      [else (error 'emit-variable-ref "could not find variable" var)])))

(define if-test cadr)
(define if-conseq caddr)
(define if-altern cadddr)

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

(define (unique-labels lvars)
  (map (lambda (lvar)
         (format "~a_~a" (unique-label) lvar)) lvars))

(define letrec-bindings let-bindings)
(define letrec-body let-body)

(define (make-initial-env lvars labels)
  (map cons lvars labels))

(define (emit-scheme-entry expr env)
  (emit-function-header "L_scheme_entry" )
  (emit-expr (- wordsize) env expr #f)
  (emit "  ret"))

; for now letrec is only at the top of the stack?
(define (emit-letrec expr)
  (let* ([bindings (letrec-bindings expr)]
         [lvars (map lhs bindings)]
         [lambdas (map rhs bindings)]
         [labels (unique-labels lvars)]
         [env (make-initial-env lvars labels)])
    (for-each (emit-lambda env) lambdas labels)
    (emit-scheme-entry (letrec-body expr) env)))

(define lambda-formals cadr)
(define lambda-body caddr)

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

(define (emit-adjust-base si)
  (cond
    [(> 0 si)  (emit "  sub rsp, ~s; adjust base" (- si))]
    [(< 0 si)  (emit "  add rsp, ~s; adjust base" si)]))

(define call-target car)
(define call-args cdr)

(define (emit-call label tail?)
  (if tail?
    (emit "  jmp ~a; tail call" label)
    (emit "  call ~a" label)))

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

(define (app? expr env)
  (cond
    [(list-starts-with-any? expr '(app)) #t]
    [(lookup (car expr) env)       #t]
    [else #f]))

(define (chomp-app expr)
  (cond
    [(list-starts-with-any? expr '(app))  (cdr expr)]
    [else expr]))

(define (emit-expr si env expr tail?)
  (cond
    [(immediate? expr) (emit-immediate expr tail?)]
    [(variable? expr)  (emit-variable-ref env expr tail?)] ; gets si from env
    [(if? expr)        (emit-if si env expr tail?)]
    [(and? expr)       (emit-if si env (transform-and expr) tail?)]
    [(or? expr)        (emit-if si env (transform-or expr) tail?)]
    [(let? expr)       (emit-let si env expr tail?)]
    [(primcall? expr)  (emit-primcall si env expr tail?)]
    [(app? expr env)   (emit-app si env (chomp-app expr) tail?)] ; primitives shadow environment?
    [else (error 'emit-expr "error in expression" expr)]))

(define (emit-program expr)
  (if (letrec? expr)
    (emit-letrec expr)
    (emit-scheme-entry expr '()))
  (emit-function-header (getenv "ENTRY")) ;"scheme_entry")
  (emit "  mov rcx, rsp")  ; save the C stack pointer
  (emit "  mov rsp, rdi") ; allocated stack base argument, calling convention puts it in rdi ...
  (emit "  call L_scheme_entry")  ; push rip to rsp and jmp
  (emit "  mov rsp, rcx")  ; restore the C stack pointer
  (emit "  ret")) ; pop rsp and jump

(define (emit-function-header name)
  (emit "global ~a" name)
  (emit "~a:" name))

(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg #f)
  (emit "  add rax, ~s" (immediate-rep 1)))  ; add x, y   x ← x + y

(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg #f)
  (emit "  sub rax, ~s" (immediate-rep 1)))

(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg #f)                            ; mov rax, arg
  (emit "  shl rax, ~s" (- charshift fxshift))  ; shift left 8 - 2 = 6 bits
  (emit "  or  rax, ~s" chartag))               ; or 00001111

(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg #f)  ; mov rax, arg
  (emit "  shr rax, ~s" (- charshift fxshift))
  (emit "  and rax, ~s" (lognot fxmask)))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg #f)
  (emit "  and al, ~s" fxmask)
  (emit "  cmp al, ~s" fxtag)
  (emit-true-using 'sete))

(define (emit-true-using set-byte-on-condition)
  (emit "  ~s al" set-byte-on-condition)  ; set equal: set to 1 otherwise 0 on condition (ZF=0)
  (emit "  movsx rax, al")
  (emit "  sal al, ~s" bool-bit)
  (emit "  or  al, ~s" bool-f))

(define-primitive (fxzero? si env arg)
  (emit-expr si env arg #f)
  (emit "  cmp rax, 0")
  (emit-true-using 'sete))

(define-primitive (null? si env arg)
  (emit-expr si env arg #f)
  (emit "  cmp al, ~s" niltag)
  (emit-true-using 'sete))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg #f)
  (emit "  and rax, ~s" boolmask)
  (emit "  cmp rax, ~s" bool-f)
  (emit-true-using 'sete))

(define-primitive (char? si env arg)
  (emit-expr si env arg #f)
  (emit "  and rax, ~s" charmask)
  (emit "  cmp rax, ~s" chartag)
  (emit-true-using 'sete))

; The primitive not takes any kind of value and returns #t if the object is #f,
; otherwise it returns #f.
(define-primitive (not si env arg)
  (emit-expr si env arg #f)
  (emit "  cmp rax, ~s" bool-f)
  (emit-true-using 'sete))

(define-primitive (fxlognot si env arg)
  (emit-expr si env arg #f)
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
  (emit-expr si env arg1 #f)
  (emit "  mov [rsp + ~s], rax;  put on stack" si)
  (emit-expr (- si wordsize) env arg2 #f)
  (emit "  add rax, [rsp + ~s];  add stack and rax" si))

(define-primitive (fx- si env arg1 arg2)
  (emit-expr si env arg2 #f)   ; rax <- arg1
  (emit "  mov [rsp + ~s], rax" si)
  (emit-expr (- si wordsize) env arg1 #f)
  (emit "  sub rax, [rsp + ~s]" si))

(define-primitive (fx* si env arg1 arg2)
  (emit-exprs-load si env arg1 arg2 'rax 'rbx)
  (emit "  imul ebx") ; eax * ebx
  (emit "  mov ebx, 4")
  (emit "  idiv ebx")) ; eax / ebx

(define (emit-exprs-load si env arg1 arg2 register1 register2)
  (emit-expr si env arg1 #f)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env arg2 #f)
  (emit-stack-save (next-stack-index si))
  (emit "  mov ~s, [rsp + ~s]" register1 si) ; (emit-stack-load si)
  (emit "  mov ~s, [rsp + ~s]" register2 (next-stack-index si))) ; (emit-stack-load (- si wordsize))

(define-primitive (fxlogor si env arg1 arg2)
  (emit-exprs-load si env arg1 arg2 'rax 'rbx)
  (emit "  or rax, rbx"))

(define-primitive (fxlognot si env arg1)
  (emit-expr si env arg1 #f)
  (emit "  shr rax, ~s" fxshift)
  (emit "  not rax")
  (emit "  shl rax, ~s" fxshift))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-exprs-load si env arg1 arg2 'rax 'rbx)
  (emit "  and rax, rbx"))

(define-syntax define-binary-primitive
  (syntax-rules ()
                [(_ (prim-name operator-instruction))
                  (define-primitive (prim-name si env arg1 arg2)
                                    (emit-exprs-load si env arg1 arg2 'rax 'rbx)
                                    (emit "  cmp rax, rbx" )
                                    (emit-true-using operator-instruction))]))

(define-binary-primitive (fx= 'sete))
(define-binary-primitive (fx< 'setl))
(define-binary-primitive (fx<= 'setle))
(define-binary-primitive (fx> 'setg))
(define-binary-primitive (fx>= 'setge))

