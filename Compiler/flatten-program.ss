(library (Compiler flatten-program)
         (export flatten-program parse-LflattenProgram )
         (import
          (source-grammar)
          (chezscheme)
          (Framework nanopass)
          (Framework helpers))

(define-parser parse-LflattenProgram LflattenProgram)
         
(define-pass flatten-program : LexposeFrameVar (x) -> LflattenProgram ()
  (Prog : Prog (x) -> Prog ()
        [(letrec ([,l* ,le*] ...) ,tl)
         `(code ,(append (Tail tl) (apply append (map (lambda (x y) (cons x (LambdaExpr y))) l* le*))) ...)]
         
        ; (let ([codeblocks (apply append (map cons l* c**))])
         ;  `(code ,(append c* codeblocks) ...))]
        [else (error who "No matching cases in Prog")])
  (LambdaExpr : LambdaExpr (x) -> * (c*)
              [(lambda () ,tl) (Tail tl)]
               ;(let ([tl (Tail tl)])
               ;  tl)]
              [else (error who "No matching cases in LambdExpr")])
  (Tail : Tail (x) -> * (c*)
        [(,triv) (in-context Code (list `(jump ,triv)))]
        [(begin ,[* c**] ... ,[* c*])
         (append (apply append c**) c*)]
        [else (error who "No matching cases in Tail")])
  (Effect : Effect (x) -> * (*c)
          [(set! ,[v] ,[rhs]) (in-context Code (list `(set! ,v ,rhs)))])))