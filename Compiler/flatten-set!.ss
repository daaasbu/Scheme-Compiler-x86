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
	     (with-output-language (LflattenSet! Effect)
	     (define resolve-set!
	       (lambda (uv val)
		 (nanopass-case (LremoveComplexOpera* Value) val
				[(begin ,ef* ... ,val) `(begin ,(map (lambda (x) (Effect x)) ef*) ... (set! ,uv ,(Value val)))]
				[(if ,pred ,val0 ,val1) `(if ,(Pred pred) (set! ,uv ,(Value val0)) (set! ,uv ,(Value val1)))]
				[,triv `(set! ,uv ,triv)]
				[(prim ,op ,triv0 ,triv1) `(set! ,uv (,op ,triv0 ,triv1))])))))

	   (Pred : Pred (x) -> Pred ())
           (Effect : Effect (x) -> Effect ()
		   [(nop) `(nop)]
                   [(set! ,uv ,val) (resolve-set! uv val)])
	   (Value : Value (x) -> * ()
		  [,triv `,triv]
		  [(if ,[pred] ,val0 ,val1) (let ((x (Value val0)) (y (Value val1)))
						(in-context Tail `(if ,pred ,x ,y)))]
		  [(prim ,op ,[triv0] ,[triv1]) (in-context Rhs `(,op ,triv0 ,triv1))]
		  [(begin ,[ef*] ... ,val) (let ((x (Value val))) (in-context Effect `(begin ,ef* ... ,x)))])))

	      
