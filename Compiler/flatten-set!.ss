(library (Compiler flatten-set!)
         (export flatten-set!)
         (import (chezscheme)
                 (source-grammar)
                 (Framework nanopass)
                 (Framework helpers))
;;This pass goes from LremoveComplexOpera* to LflattenSet!, its main purpose is to push set!'s inside of begins and ifs.

(define-pass flatten-set! : LremoveComplexOpera* (x) -> LflattenSet! ()
  (Effect : Effect (x) -> Effect ()
          [(set! ,uv ,val) 
           (Value val uv)]
	  [(call ,[triv] ,[triv*] ...) `(call ,triv ,triv* ...)])

  (Value : Value (v uv) -> Effect ()
         [(prim ,op ,triv0 ,triv1) 
          `(set! ,uv (,op ,triv0 ,triv1))]
         [(if ,[pred] ,val0 ,val1) 
          `(if ,pred ,(Value val0 uv) ,(Value val1 uv))]
         [(begin ,[ef*] ... ,val) 
          `(begin ,ef* ... ,(Value val uv))]
         [,triv `(set! ,uv ,v)]
	 [(call ,[triv] ,[triv*] ...) `(set! ,uv (call ,triv ,triv* ...))]
	 [(alloc ,triv) `(set! ,uv (alloc ,triv))]
	 [(mref ,triv0 ,triv1) `(set! ,uv (mref ,triv0 ,triv1))]
  )
)
)
