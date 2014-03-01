(library (Compiler flatten-set!)
         (export flatten-set! parse-LflattenSet!)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LflattenSet! LflattenSet!)

         (define-pass flatten-set! : LremoveComplexOpera* (x) -> LflattenSet! ()
	   (definitions 

	     (define resolve-set!
	       (lambda (uv val)
		 (nanopass-case (LremoveComplexOpera* Value) val
				[(begin ,ef* ... ,val) `(begin ,ef* ... (set! ,uv ,val))]
				[(if ,pred ,val0 ,val1) `(if ,pred (set! ,uv ,val0) (set! ,uv ,val1))]))))

           (Effect : Effect (x) -> Effect ()
		   [(nop) `(nop)]
                   [(set! ,uv ,val) (resolve-set! uv val)])
	   (Value : Value (x) -> * ()
		  [,triv `,triv]
		  [(if ,pred ,val0 ,val1) (let ((x (Value val0)) (y (Value val1)))
						(in-context Effect `(if ,pred ,x ,y)))]
		  [(prim ,op ,[triv0] ,[triv1]) (in-context Tail `(prim ,op ,triv0 ,triv1))]
		  [(begin ,[ef*] ... ,val) (let ((x (Value val))) (in-context Effect `(begin ,ef* ... ,x)))])))

	      
