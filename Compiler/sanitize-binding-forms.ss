(library (Compiler sanitize-binding-forms)
	 (export sanitize-binding-forms)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass sanitize-binding-forms : LverifyScheme (orig) -> LverifyScheme ()

	   (definitions 
	     (define remove-lambdas
	       (lambda (uv* expr*)
		 (define extract
		   (lambda (uv* expr* binding* le* return-null* return-expr*)
		     (if (null? uv*) 
			 (values (reverse binding*) (reverse le*) (reverse return-null*) (reverse return-expr*))
			 (nanopass-case (LverifyScheme Expr) (car expr*)
					[,le (extract (cdr uv*) 
						      (cdr expr*) 
						      (cons (car uv*) binding*) 
						      (cons le le*)
						      return-null*
						      return-expr*)]
					[else (extract 
					       (cdr uv*)
					       (cdr expr*)
					       binding* 
					       le*
					       (cons (car uv*) return-null*)
					       (cons (car expr*) return-expr*))]))))
		 (extract uv* expr* '() '() '() '())))

	     )
#|
	   (LambdaExpr : LambdaExpr (x) -> * ()
		       [(lambda (,uv* ...) ,[expr]) (printf "what about here? \n") 
			(let ([l (unique-label 'ANON)])
			  (in-context Expr `(letrec ([,l (lambda (,uv* ...) ,expr)]) ,l)))])

|#
	   (Expr : Expr (x) -> Expr ()
		 [(letrec () ,[expr]) `,expr]
		 [(let () ,[expr]) `,expr]
		 [(let [(,uv* ,expr*) ...] ,expr) (let-values ([(binding* le* uv* expr*) (remove-lambdas uv* expr*)])
						   
						    (if (null? le*)
							
							`(let ([,uv* ,expr*] ...) ,expr)
							`(letrec ([,binding* ,le*] ...)
							   (let ([,uv* ,expr*] ...) ,expr))))])



	    (Expr orig))
)