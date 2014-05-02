(library (Compiler uncover-assigned)
	 (export uncover-assigned)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass uncover-assigned : LconvertComplexDatum (orig) -> LuncoverAssigned  ()
	   (definitions

	     )

		 
			(Expr : Expr (x) -> Expr (found)
			      [,uv (values uv '())]
			      [(lambda (,uv* ...) ,[expr -> expr found])
			       (let* ((asgn* (intersection found  uv*))
				      (diff* (difference found uv*)))
				 (values `(lambda (,uv* ...) (assigned (,asgn* ...) ,expr)) diff*))]
			       
			      [(set! ,uv ,[expr -> expr found]) (values `(set! ,uv ,expr) (set-cons uv found))]
 			      [(let ((,uv* ,[expr* -> expr* found*]) ...) ,[expr -> expr found]) 
			       (let* ((found* (union found (apply union found*)))
				      (asgn* (intersection found*  uv*))
				      (diff* (difference found* uv*)))
				 (values `(let ((,uv* ,expr*) ...) (assigned (,asgn* ...) ,expr)) diff*))]
			      [(letrec ((,uv* ,[expr* -> expr* found*]) ...) ,[expr -> expr found])
			        (let* ((found* (union found (apply union found*)))
				      (asgn* (intersection found*  uv*))
				      (diff* (difference found* uv*)))
				  (values `(letrec ([,uv* ,expr*] ...) (assigned (,asgn* ...) ,expr)) diff*))]
			      [(if ,[expr0 -> expr0 found0] ,[expr1 -> expr1 found1] ,[expr2 -> expr2 found2])
			       (values `(if ,expr0 ,expr1 ,expr2) (union found0 found1 found2))]
			      [(quote ,i) (values `(quote ,i) '())]
			      [(begin ,[expr* -> expr* found*] ... ,[expr -> expr found])
			       (values `(begin ,expr* ... ,expr) (union (apply union found*) found))]
			      [(,prim ,[expr* -> expr* found*] ...)
			       (values `(,prim ,expr* ...) (apply union found*))]
			      [(call ,[expr -> expr found] ,[expr* -> expr* found*] ...)
		;	      
			       (values `(call ,expr ,expr* ...) (union (apply union found*) found))]
			      [else (error who "You forgot something")])
			(let-values ([(expr found) (Expr orig)])
			  (if (not (null? found))
			      (error who "You made a mistake")
			      expr))))
	   

