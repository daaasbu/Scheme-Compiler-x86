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
        (member x env)))
         (define walk-symbol
           (lambda (sym ls)
             (letrec ((walk
                      (lambda (y t)
                        (cond
                         [(null? t) y]
                         [(pair? t) (let ([c1 (caar t)] [c2 (cdar t)])
                                      (if (eqv? y c1)
                                          (if (or (register? c2) (frame-var? c2)) c2 (walk c2 ls))
                                          (walk y (cdr t))))]))))
               (walk sym ls)))))
  
  (Prog : Prog (x) -> Prog ()
        [(letrec ([,l* ,[le*]] ...) ,bd)
         (unless (s-ls l*) (error who "Duplicate label-suffix"))
         `(letrec ([,l* ,(map (lambda (x) (LambdaExpr x l*)) le*)] ...) ,(Body bd l*))])
  (LambdaExpr : LambdaExpr (x env) -> LambdaExpr ()
              [(lambda () ,bd) `(lambda () ,(Body bd env))])
       
  (Body : Body (x env) -> Body ()
        [(locate ([,uv* ,locrf*] ...) ,tl)
         (unless (s-ls uv*) (error who "Duplicate unique-var suffix" uv*))
         `(locate ([,uv* ,locrf*] ...) ,(Tail tl (append uv* env) (map cons uv* locrf*)))]) 

  (Pred : Pred (x env env2) -> Pred ()
        [(true) x]
        [(false) x]
        [(,relop ,triv1 ,triv2)
         (if (or (label? triv1) (uvar? triv1))
             (unless (lookup triv1 env) (error who "label not in env")))
         (if (or (label? triv2) (uvar? triv2))
             (unless (lookup triv2 env) (error who "label not in env")))
         (unless (or (not (frame-var? triv1)) (not (frame-var? triv2))) (error who "Cannot both be frames" triv1 triv2))
         (if (integer? triv1) (unless (int32? triv1) (error who "Must be in certain range" triv1 triv2)))
         (if (integer? triv2) (unless (int32? triv2) (error who "Must be in certain range" triv1 triv2)))
         (if (and (integer? triv1) (register? triv2)) (error who "int32 must be the first arg"))
         (if (and (uvar? triv2) (uvar? triv1) (equal? (walk-symbol triv1 env2) (walk-symbol triv2 env2))) (error who "cannot both be frames")) x]
        [(begin ,ef* ... ,pred) `(begin ,(map (lambda (x) (Effect x env env2)) ef*) ... ,(Pred pred env env2))]
        [(if ,pred1 ,pred2 ,pred3) (Pred pred1 env env2) (Pred pred2 env env2) (Pred pred3 env env2) x])

  (Tail : Tail (x env env2) -> Tail ()
        [(,triv) 
         (unless (or (register? triv) (label? triv) (frame-var? triv)) (error who "Invalid Triv in Tail position" triv))
         (if (or (uvar? triv) (label? triv)) (unless (lookup triv env) (error who "unbound label"))) x]
        [(begin ,ef* ... ,tl)
          `(begin ,(map (lambda (x) (Effect x env env2)) ef*) ... ,(Tail tl env env2))]
        [(if ,pred ,tl1 ,tl2)
         (Pred pred env env2) (Tail tl1 env env2) (Tail tl2 env env2) x])

  (Effect : Effect (x env env2) -> Effect ()
          [(set! ,v ,triv)
           (if (uvar? v) (unless (lookup v env) (error who "uvar not in env")))
           (if (uvar? triv) (unless (lookup triv env) (error who "uvar not in env")))
           (if (and (frame-var? v) (frame-var? triv))
               (error who "Cannot both be frames" v triv))
           (if (label? triv) (unless (register? v)(error who "Must be register" v)))
           (if (or (int32? triv) (int64? triv))(unless (or (int32? triv) (and (register? v) (int64? triv)))(error who "Needs to be in certain range" triv)))
           x]
          [(set! ,v (,op ,triv1 ,triv2))
           (if (uvar? v) (unless (lookup v env) (error who "uvar not in env")))
           (if (uvar? triv1) (unless (lookup triv1 env) (error who "uvar not in env")))
           (if (uvar? triv2) (unless (lookup triv2 env) (error who "uvar not in env")))
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
           x]
          [(if ,pred ,ef1 ,ef2) (Pred pred env env2) (Effect ef1 env env2) (Effect ef2 env env2) x]
          [(begin ,ef* ... ,ef1) `(begin ,(map (lambda (x) (Effect x env env2)) ef*) ... ,(Effect ef1 env env2))]
          [(nop) x])))