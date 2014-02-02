;;flatten-program, goes from LexposeFrameVar to LflattenProgram. Replaces begin with the keyword code, breaks up nested begins, places the tails of letrecs
;;at the front of the program, and uses map to place each label with its block of effects.

(library (Compiler flatten-program)
         (export flatten-program parse-LflattenProgram )
         (import
          (source-grammar)
          (chezscheme)
          (Framework nanopass)
          (Framework helpers))

(define-parser parse-LflattenProgram LflattenProgram)
         
(define-pass flatten-program : LexposeBasicBlocks (x) -> LflattenProgram ()
  (Prog : Prog (x) -> Prog ()
        [(letrec ([,l* ,le*] ...) ,tl)
         `(code ,(append (Tail tl) (apply append (map (lambda (x y) (cons x (LambdaExpr y))) l* le*))) ...)]
        [else (error who "Error")]
        )
  (LambdaExpr : LambdaExpr (x) -> * (c*)
              [(lambda () ,tl) (Tail tl)]
              [else (error who "Error")])
  (Tail : Tail (x) -> * (c*)
        [(,triv) (in-context Code (list `(jump ,triv)))]
        [(begin ,[* c**] ... ,[* c*])
         (append (apply append c**) c*)]
        [else (error who "Error")])
  (Effect : Effect (x) -> * (*c)
          [(set! ,[locrf] ,[rhs]) (in-context Code (list `(set! ,locrf ,rhs)))]
          [else (error who "Error")])))