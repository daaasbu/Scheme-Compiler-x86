;;verify-scheme, takes our subset of scheme consisting of mainly letrecs, effects,registers,frame-vars, labels and lambda expressions, and goes through a series of tests that targets a certain machine.These tests could change depending on the target of our compiler.  Goes from LverifyScheme to LverifyScheme.
;;
(library (Compiler verify-scheme)
         (export verify-scheme parse-LverifyScheme)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define binop?
           (lambda (x)
             (if (memq x '(+ - * logand logor sra)) #t #f)))
         (define lookup
           (lambda (x env)
             (member x env)))
         (define duplicate-labels?
           (lambda (env)
             (cond
              [(null? env) #t]
              [(member (car env) (cdr env)) #f]
              [else (duplicate-labels? (cdr env))])))
         (define suffix-list
           (lambda (ls)
             (duplicate-labels? (map extract-suffix ls))))

         (define-parser parse-LverifyScheme LverifyScheme)

         (define-pass verify-scheme : LverifyScheme (x) -> LverifyScheme ()
           (Prog : Prog (x) -> Prog ()
                 [(letrec ([,l* ,[le*]] ...) ,bd)
                  (unless (suffix-list l*) (error who "Duplicate Labels"))
                   `(letrec ([,l* ,(map (lambda (x) (LambdaExpr x l*)) le*)] ...) ,(Body bd l*))])
           (LambdaExpr : LambdaExpr (x env) -> LambdaExpr ()
                       [(lambda () ,bd)  `(lambda () ,(Body bd env))])
           (Body : Body (x env) -> Body ()
                 [(locals (,uv* ...) ,tl)
                  (unless (suffix-list uv*) (error who "Duplicate uvar's"))
                  `(locals (,uv* ...) ,(Tail tl (append uv* env) uv*))])
           (Tail : Tail (x env env2) -> Tail ()
                 [(begin ,ef* ... ,tl1)   `(begin ,(map (lambda (x) (Effect x env env2)) ef*) ... ,(Tail tl1 env env2))]
                 [(,triv ,locrf* ...)
;                  (unless (or (register? triv) (label? triv) (frame-var? triv))
;                      (error who "triv must be a label or variable" triv))
                  (if (or (uvar? triv) (label? triv))
                      (unless (lookup triv env) (error who "unbound label"))) x]
                 [(if ,pred ,tl1 ,tl2) (Pred pred env env2) (Tail tl1 env env2) (Tail tl2 env env2) x])
           (Pred : Pred (x env env2) -> Pred ()
                 [(true) x]
                 [(false) x]
                 [(,relop ,triv1 ,triv2)
                  (if (or (label? triv1) (uvar? triv1))
                      (unless (lookup triv1 env) (error who "not in env")))
                  (if (or (label? triv2) (uvar? triv2))
                      (unless (lookup triv2 env) (error who "not in env")))
;                  (unless (or (not (frame-var? triv1)) (not (frame-var? triv2)))
;                          (error who "Two frame vars while using a relop"))
;                  (if (integer? triv1)
;                      (unless (int32? triv1) (error who "must use a 32 bit integer")))
;                  (if (integer? triv2)
;                      (unless (int32? triv2) (error who "must use a 32 bit integer")))
;                  (if (and (integer? triv1) (register? triv2))
;                      (error who "int32 must be the first arg to cmpq"))
                  x]
                 [(if ,pred1 ,pred2 ,pred3) (Pred pred1 env env2) (Pred pred2 env env2) (Pred pred3 env env2) x]
                 [(begin ,ef* ... ,pred) `(begin ,(map (lambda (x) (Effect x env env2)) ef*) ... ,(Pred pred env env2))])
           (Effect : Effect (x env env2) -> Effect ()
                   [(set! ,v ,triv)
                    (if (uvar? v)  (unless (lookup v env) (error who "not in env")))
                    (if (uvar? triv)  (unless (lookup triv env) (error who "not in env")))
;                    (if (and (frame-var? v) (frame-var? triv))
;                        (error who "Can't set! a frame variable to a frame variable" v triv))
;                    (if (label? triv)
;                        (unless (or (register? v) (label? v) (uvar? v)) ;;;;;changed this in a4 -> or label?/uvar?
;                                (error who "If triv is a label, then var must be a register" v triv)))
;                    (if (or (int32? triv) (int64? triv))
;                        (unless (or (int32? triv) (and (register? v) (int64? triv)))
;                                (error who "Triv not valid" triv)))
                    x]
                   [(set! ,v (,op ,triv1 ,triv2))
                    (if (uvar? v)  (unless (lookup v env) (error who "not in env")))
                    (if (uvar? triv1)  (unless (lookup triv1 env) (error who "not in env")))
                    (if (uvar? triv2)  (unless (lookup triv2 env) (error who "not in env")))
;                    (unless (eqv? v triv1)
;                            (error who "LHS must match" v triv1))
;                    (if (label? triv1)
;                        (error who "Triv1 must be a label" triv1)
;                        (if (label? triv2)
;                            (error who "Triv2 must be a label" triv2)))
;                    (if (and (frame-var? triv1) (frame-var? triv2))
;                        (error who "Can't have two frame variables" triv1 triv2))
;                    (if (eqv? op `*)
;                        (unless (or (register? v) (uvar? v))
;                                (error who "When using * the variable must be a register" v)))
                    (if (eqv? op `sra)
                        (unless (and (<= 0 triv2) (>= 63 triv2))
                                (error who "sra out of bounds" triv2)))
 ;                   (unless (or (register? triv1) (frame-var? triv1) (label? triv1) (uvar? triv1))
 ;                           (unless (int32? triv1)
 ;                                   (error who "int triv1 is out of bounds")))
 ;                   (unless (or (register? triv2) (frame-var? triv2) (label? triv2) (uvar? triv2))
 ;                           (unless (int32? triv2)
 ;                                   (error who "int triv2 is out of bounds")))
                    x]
                   [(if ,pred ,ef1 ,ef2) (Pred pred env env2) (Effect ef1 env env2) (Effect ef2 env env2) x]
                   [(begin ,ef* ... ,ef1) `(begin ,(map (lambda (x) (Effect x env env2)) ef*) ... ,(Effect ef1 env env2))]
                   [(nop) x]))
) 