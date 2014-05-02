(library (Compiler purify-letrec)
	 (export purify-letrec)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass purify-letrec : LuncoverAssigned (x) -> LpurifyLetrec  ()
	   (definitions

	     )

	   (Expr : Expr (x) -> Expr ()
		 #;[(letrec ([,uv0* ,[expr*]] ...) (assigned () ,[expr]))
		 `(letrec ([,uv0* ,expr*] ...) ,expr)]
		 [(letrec ([,uv0* ,[expr*]] ...) (assigned (,uv1* ...) ,[expr]))
		  (if (null? uv0*) 
		      expr
		      (let* ((tmp* (map (lambda (x) (unique-name 'TMP)) uv0*))
			     (expr2* (map (lambda (uv tmp) `(set! ,uv ,tmp)) uv0* tmp*))
			     (expr1* (reverse (cdr (reverse expr2*))))
			     (expr1 (car (last-pair expr2*)))
			     (void* (map (lambda (x) `(void)) uv0*)))
			`(let ((,uv0* ,void*) ...)
			   (assigned (,uv0* ...)
				     (begin
				       (let ([,tmp* ,expr*] ...)
					 (assigned ()
						   (begin ,expr1* ... ,expr1)))
				       ,expr)))))])
		 )
	   )