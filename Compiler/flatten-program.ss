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
         `(code ,(append (Tail tl) (apply append (map (lambda (x y) (cons x (LambdaExpr y))) l* le*))) ...)])
  (LambdaExpr : LambdaExpr (x) -> Code ()
              [(lambda () ,[c*]) c*])
  (Tail : Tail (x) -> Code ()
        [(,triv) (list `(jump ,triv))]
        [(begin ,[c**] ... ,[c*]) (append c** c*)]
        [(if (,relop ,triv0 ,triv1) (,l0) (,l1)) (in-context Code (list `(if (,relop ,triv0 ,triv1) (jump ,l0)) `(jump ,l1)))])
  (Effect : Effect (x) -> Code ()
          [(set! ,[locrf] ,[rhs]) `(set! ,locrf ,rhs)])))