;;verify-scheme, takes our subset of scheme consisting of mainly letrecs, effects,registers,frame-vars, labels and lambda expressions, and goes through a series of tests that targets a certain machine.These tests could change depending on the target of our compiler.  Goes from LverifyScheme to LverifyScheme.
;;

(library (Compiler verify-scheme)
     (export verify-scheme parse-LverifyScheme)
     (import
      (chezscheme)
      (source-grammar)
      (Framework nanopass)
      (Framework helpers))
(define-parser parse-LverifyScheme LverifyScheme)

(define-pass verify-scheme : LverifyScheme (x) -> LverifyScheme ()
  (definitions 
    (define same-label? 
      (lambda (env) 
        (cond
          [(null? env) #t]
          [(member (car env) (cdr env)) #f]
          [else (same-label? (cdr env))])))
    (define s-ls 
      (lambda (ls)
        (same-label? (map extract-suffix ls))))
    (define lookup
      (lambda (x env)
        (member x env))))
  
  (Prog : Prog (x) -> Prog ()
        [(letrec ([,l* ,[le*]] ...) ,bd)
         (unless (s-ls l*) (error who "Duplicate label-suffix" l*))
         (Body bd l*)
         x])

  ;(LambdaExpr : LambdaExpr (x env) -> LambdaExpr ()
   ;           [(lambda () ,bd)
    ;          (Body bd env)
     ;          x])
         
  (Body : Body (x env) -> Body ()
        [(locate ([,uv* ,[locrf*]] ...) ,tl)
         (Tail tl (append uv* env))
         (unless (s-ls uv*) (error who "Duplicate unique-var suffix" uv*))
         x]) 

  (Pred : Pred (x) -> Pred ()
        [(true) x]
        [(false) x]
        [(,relop ,triv1 ,triv2)
         (unless (or (not (frame-var? triv1)) (not (frame-var? triv2))) (error who "Cannot both be frames" triv1 triv2))
         (if (and (integer? triv1) (not (frame-var? triv1))) (unless (int32? triv1) (error who "Must be in certain range" triv1 triv2)))
         (if (and (integer? triv2) (not (frame-var? triv2))) (unless (int32? triv2) (error who "Must be in certain range" triv1 triv2))) x])

  (Tail : Tail (x env) -> Tail ()
        [(,triv) 
         (unless (or (register? triv) (label? triv) (frame-var? triv)) (error who "Invalid Triv in Tail position" triv))
         (if (label? triv) (unless (lookup triv env) (error who "unbound label")))]
        [(begin ,[ef*] ... ,tl)
         (Tail tl env)
         x]
        [(if ,[pred] ,tl1 ,tl2)
         (Tail tl1 env)
         (Tail tl2 env)
         x])

  (Effect : Effect (x) -> Effect ()
          ;[(if ,pred ,ef0 ,ef1)
           ;(Effect ef0)
           ;(Effect ef1)
           ;x]
          ;[(begin ,ef* ... ,ef)
          ; (Effect ef*)
          ; (Effect ef)
          ; x]
          [(set! ,v ,triv)
           (if (and (frame-var? v) (frame-var? triv))
               (error who "Cannot both be frames" v triv))
           (if (label? triv) (unless (register? v)(error who "Must be register" v)))
           (if (or (int32? triv) (int64? triv))(unless (or (int32? triv) (and (register? v) (int64? triv)))(error who "Needs to be in certain range" triv)))
           x]
          [(set! ,v (,op ,triv1 ,triv2)) 
           (unless (eqv? v triv1) (error who "Must be equal" v triv1))
           (if (label? triv1)(error who "labels cannot be used in binops" triv1)
               (if (label? triv2) (error who "labels cannot be used in binops" triv2)))
           (if (and (frame-var? triv1) (frame-var? triv2)) (error who "Cannot both be frame-vars" triv1 triv2))
           (if (eqv? op `*)(unless (or (uvar? v) (register? v)) (error who "Must be register2" v)))
           (if (eqv? op `sra)(unless (and (<= 0 triv2) (>= 63 triv2))(error who "Must be in certain range" triv2)))
           (unless (or (register? triv1) (frame-var? triv1) (label? triv1) (uvar? triv1))
             (unless (int32? triv1)
               (error who "out of range" triv1)))
           (unless (or (register? triv2) (frame-var? triv2) (label? triv2) (uvar? triv2))
             (unless (int32? triv2)
               (error who "out of range" triv2)))
           x])

  ;(Var : Var (x) -> Var ()
   ;    [,r (unless (or (register? r) (frame-var? r)) (error who "Invalid register" r))
   ;        x])
  ))