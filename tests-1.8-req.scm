
(add-tests-with-string-output "procedures"
  [(letrec () 12) => "12\n"]
  [(letrec () (let ([x 5]) (fx+ x x))) => "10\n"]
  [(letrec ([f (lambda () 5)]) 7) => "7\n"]
  [(letrec ([f (lambda () 5)]) (let ([x 12]) x)) => "12\n"]
  [(letrec ([f (lambda () 5)]) (f)) => "5\n"]
  [(letrec ([f (lambda () 5)]) (let ([x (f)]) x)) => "5\n"]
  [(letrec ([f (lambda () 5)]) (fx+ (f) 6)) => "11\n"]
  [(letrec ([f (lambda () 5)]) (fx- 20 (f))) => "15\n"]
  [(letrec ([f (lambda () 5)]) (fx+ (f) (f))) => "10\n"]
  [(letrec ([f (lambda () (fx+ 5 7))]
            [g (lambda () 13)]) 
    (fx+ (f) (g))) => "25\n"]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f 13)) => "25\n"]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f (f 10))) => "34\n"]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f (f (f 0)))) => "36\n"]
  [(letrec ([f (lambda (x y) (fx+ x y))] 
            [g (lambda (x) (fx+ x 12))])
    (f 16 (f (g 0) (fx+ 1 (g 0))))) => "41\n"]
  [(letrec ([f (lambda (x) (g x x))]
            [g (lambda (x y) (fx+ x y))])
     (f 12)) => "24\n"]
  [(letrec ([f (lambda (x) 
                 (if (fxzero? x)
                     1
                     (fx* x (f (fxsub1 x)))))])
      (f 5)) => "120\n"]
  [(letrec ([e (lambda (x) (if (fxzero? x) #t (o (fxsub1 x))))]
            [o (lambda (x) (if (fxzero? x) #f (e (fxsub1 x))))])
     (e 25)) => "#f\n"]
)

(add-tests-with-string-output "deeply nested procedures"
  [(letrec ([sum (lambda (n ac)
                   (if (fxzero? n)
                        ac
                        (app sum (fxsub1 n) (fx+ n ac))))])
    (app sum 10000 0)) => "50005000\n"]
  [(letrec ([e (lambda (x) (if (fxzero? x) #t (app o (fxsub1 x))))]
            [o (lambda (x) (if (fxzero? x) #f (app e (fxsub1 x))))])
     (app e 5000000)) => "#t\n"]
  [(letrec ((fib (lambda (n)
                   (if (or (fx= 1 n) (fx= 2 n)) 1
                       (fx+ (fib (fx- n 1)) (fib (fx- n 2))))))) (fib 20)) => "6765\n"]
)
