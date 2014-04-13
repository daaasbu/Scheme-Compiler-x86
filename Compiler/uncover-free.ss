(library (Compiler uncover-free)
	 (export uncover-free)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))
	 
	 (define-pass uncover-free : LverifyScheme (orig) -> LuncoverFree ()
	   (definitions

	     )

	   (LambdaExpr : LambdaExpr (x) -> LambdaExpr (found)
		       [(lambda (,uv* ...) ,expr) (let-values ([(expr found*) (Expr expr)])
;						    (printf "found*: ~a \n free*: ~a \n" found* (difference found* uv*))
						    (let ([free* (difference found* uv*)])

						      (values `(lambda (,uv* ...) (free (,free* ...) ,expr)) free*)))])


			(Expr : Expr (x) -> Expr (found)
			      [,uv (values uv (list uv))]
 			      [(let ((,uv* ,[expr* -> expr* found*]) ...) ,[expr -> expr found]) 
			       (values `(let ((,uv* ,expr*) ...) ,expr) (difference (union found (apply union found*)) uv*))]
			      [(letrec ((,uv* ,[le* -> le* found*]) ...) ,[expr -> expr found])
			       (values `(letrec ([,uv* ,le*] ...) ,expr) (difference (union found (apply union found*)) uv*))]
			      [(if ,[expr0 -> expr0 found0] ,[expr1 -> expr1 found1] ,[expr2 -> expr2 found2])
			       (values `(if ,expr0 ,expr1 ,expr2) (union found0 found1 found2))]
			      [(quote ,i) (values `(quote ,i) '())]
			      [(begin ,[expr* -> expr* found*] ... ,[expr -> expr found])
			       (values `(begin ,expr* ... ,expr) (union (apply union found*) found))]
			      [(,prim ,[expr* -> expr* found*] ...)
			       (values `(,prim ,expr* ...) (apply union found*))]
			      [(call ,[expr -> expr found] ,[expr* -> expr* found*] ...)
		;	       (printf "found*: ~a \n found: ~a \n" found* found)
			       (values `(call ,expr ,expr* ...) (union (apply union found*) found))]
			      [else (error who "You forgot something")])
			(let-values ([(expr found) (Expr orig)])
			  (if (not (null? found))
			      (error who "You made a mistake")
			      expr))))
	   
