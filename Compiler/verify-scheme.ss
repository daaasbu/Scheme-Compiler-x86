(library (Compiler verify-scheme)
         (export verify-scheme parse-LverifyScheme)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

(define-parser parse-LverifyScheme LverifyScheme)




         (define binop?
           (lambda (x)
             (if (memq x '(+ - * logand logor sra)) #t #f)))
         (define rule1
           (lambda (x y)
             (eq? x y)))
         (define rule2
           (lambda (x y)
             (and (not (label? x)) (not (label? y)))))
         (define rule3/4
           (lambda (x y)
             (or (not (frame-var? x)) (not (frame-var? y)))))
         (define rule5
           (lambda (x y)
             (when (label? x)
                   (register? y))))
         (define rule6
           (lambda (x y)
             (when (int64? y)
                   (or (int32? y) (and (register? x) (int64? y))))))
         (define rule7
           (lambda (x y)
             (and (eq? x '*) (register? x))))
         (define rule8
           (lambda (x y)
             (and (eq? x 'sra) (int64? y))))
         (define rule9
           (lambda (x)
             (not (int64? x))))
         (define binop32bit
           (lambda (x y)
             (and (not (eq? x 'sra)) (int32? y))))

         
         (define-pass verify-scheme : LverifyScheme (x) -> LverifyScheme ()
           (Prog : Prog (x) -> Prog ()
                 [(letrec ([,l* ,[le*]] ...) ,[tl]) x])
           (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
                       [(lambda () ,[tl]) x])
           (Tail : Tail (x) -> Tail ()
                 [(begin ,[ef] ,[ef*] ... ,[tl1]) x]
                 [(,triv)
                  (unless (or (register? triv) (label? triv) (frame-var? triv))
                      (error who "triv must be a label or variable" triv))])
           (Effect : Effect (x) -> Effect ()
                   [(set! ,v ,triv)
                    (rule3/4 v triv)
                    (rule5 v triv)
                    (rule6 v triv)]
                   [(set! ,v (,op ,triv1 ,triv2))
                    (rule1 v triv1)
                    (rule2 triv1 triv2)
                    (rule3/4 triv1 triv2)
                    (rule7 op v)
                    (rule8 op triv2)
                    (binop32bit op triv2)])))