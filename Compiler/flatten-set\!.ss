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
				[(begin ,ef* ... ,val) `(begin ,ef* ... (set! uv val))]
				[(if ,pred ,val0 ,val1) `(if ,pred (set! ,uv ,val0) (set! ,uv ,val1))]))))

           (Effect : Effect (x env ) -> Effect ()
		   [(nop) `(nop)]
                   [(set! ,uv ,[val]) `,(resolve-set! uv val)])
	   (Value : Value (x env) -> * ()
		  [,triv `,triv]
		  [(if ,pred ,[val0] ,[val1]) (with-output-language LflattenSet! Effect `(if ,pred ,val0 ,val1))]
		  [(prim ,op ,[val0] ,[val1]) (with-output-language lflattenSet! Tail `(prim ,op ,val0 ,val1))]
		  [(begin ,[ef*] ... ,[val]) (with-output-language LflattenSet! Effect `(begin ,ef* ... ,val))])))

	      
