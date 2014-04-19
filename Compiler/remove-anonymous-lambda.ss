(library (Compiler remove-anonymous-lambda)
	 (export remove-anonymous-lambda)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass remove-anonymous-lambda : LconvertAssignments (orig) -> LremoveAnonymousLambda ()
#|
	   (LambdaExpr : LambdaExpr (x) -> * ()
		       [(lambda (,uv* ...) ,[expr]) (printf "what about here? \n") 
			(let ([l (unique-label 'ANON)])
			  (in-context Expr `(letrec ([,l (lambda (,uv* ...) ,expr)]) ,l)))])
|#
	   (Expr : Expr (x) -> Expr ()
		 [(letrec ([,uv0* (lambda (,uv1* ...) ,[expr0])] ...) ,[expr1])
		  `(letrec ([,uv0* (lambda (,uv1* ...) ,expr0)] ...) ,expr1)]
		 [(lambda (,uv* ...) ,[expr])
		  (let ([l (unique-name 'ANON)])
		    `(letrec ([,l (lambda (,uv* ...) ,expr)]) ,l))])
	   (Expr orig)))