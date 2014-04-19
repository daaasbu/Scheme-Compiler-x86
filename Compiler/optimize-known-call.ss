(library (Compiler optimize-known-call)
	 (export optimize-known-call)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass optimize-known-call : LconvertClosures (orig) -> LconvertClosures ()

	   (definitions

	     (define uv->label
	       (lambda (sym)
		 (let* ((name (extract-root sym))
			(num (extract-suffix sym))
			(str (string-append name "$" num))
			(l (string->symbol str)))
		   l)))

	     )
	   #|
	   (find-vars : Expr (x bound*) -> Expr ()
		      [,l `,l]
		      [(letrec ([,l0* (lambda (,uv0* ...) (bind-free (,uv2 ,uv2* ...) ,expr0))] ...) (closures ((,uv1* ,l1* ,uv** ...) ...) ,expr1))
		       (let ((bound1* (append uv1* l1* uv**)))
			 `(letrec ([,l0* (lambda (,uv0* ...) ,(find-vars expr0 bound1*))] ...) (closures ((,uv1* ,l1* ,uv** ...) ...) ,(find-vars expr1 bound1*))))]
		      [,uv `,uv]
		      [(let ([,uv* ,[find-vars : expr* bound* -> expr*]] ...) ,[find-vars : expr bound* -> expr]) `(let ([,uv* ,expr*] ...) ,expr)]
		      [(quote ,i) `(quote ,i)]
		      [(if ,[find-vars : expr0 bound* -> expr0] ,[find-vars : expr1 bound* -> expr1] ,[find-vars : expr2 bound* -> expr2]) `(if ,expr0 ,expr1 ,expr2)]
		      [(begin ,[find-vars : expr* bound* -> expr*] ... ,[find-vars : expr bound* -> expr]) `(begin ,expr* ... ,expr)]
		      [(,prim ,[find-vars : expr* bound* -> expr*] ...) `(prim ,expr* ...) ]
		      [(call ,expr ,[find-vars : expr* bound* -> expr*] ...) (if (and (uvar? expr) (memq expr bound*)) 
										 `(call ,(uv->label expr) ,expr*)
										 `(call ,expr ,expr* ...))])

			|#					  


	   (Expr : Expr (x bound*) -> Expr ()
		  [(letrec ([,l0* (lambda (,uv0* ...) (bind-free (,uv2 ,uv2* ...) ,expr0*))] ...) 
		     (closures ((,uv1* ,l1* ,uv** ...) ...) ,expr1)) 
		    `(letrec ([,l0* (lambda (,uv0* ...) (bind-free (,uv2 ,uv2* ...)  ,(map (lambda (x) (Expr x (append uv1* bound*))) expr0*)))] ...)
		       (closures ((,uv1* ,l1* ,uv** ...) ...) ,(Expr expr1 (append uv1* bound*))))]
		  [(call ,uv ,expr* ...) (if (memq uv bound*)
					       `(call ,(uv->label uv) ,(map (lambda (x) (Expr x bound*)) expr*) ...)
					       `(call ,uv ,(map (lambda (x) (Expr x bound*)) expr*) ...))])
	   
	   
				
	   (Expr orig '())
		 )
	       )