(library (Compiler convert-assignments)
	 (export convert-assignments)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass convert-assignments : LpurifyLetrec (orig) -> LconvertAssignments  ()
	   (definitions

	   

	     )
	   #;(AssignedBody : AssignedBody (x) -> Expr ()
			 [(assigned (,uv* ...) ,[expr uv* -> expr])
			  (let* ((expr* (map (lambda (x) (let ((expr1* (list (unique-name 'TMP) `(void))))
							       `(cons ,expr1* ...))) uv*)))
			    `(let ([,uv* ,expr*] ...) ,expr))])
				
			 
			 
		       
	   (Expr : Expr (x asgn*) -> Expr ()
		 [(lambda (,uv0* ...) (assigned (,uv1* ...) ,[expr1 (append asgn* uv1*) -> expr1]))
		  (let* ((tmp* (map (lambda (x) (unique-name (string->symbol (extract-root x)))) uv1*))
			 (expr1* (map (lambda (tmp) (let ((expr-new* (list tmp `(void))))
						       `(cons ,expr-new* ...)))  tmp*))
			 (a-list (map (lambda (x y) (cons x y)) uv1* tmp*))
			 (uv0* (map (lambda (x) (cond
						 [(assq x a-list) => cdr]
						 [else x])) uv0*)))
		    `(lambda (,uv0* ...) (let ([,uv1* ,expr1*] ...) ,expr1)))]

		  
		 [(let ([,uv0* ,[expr0* asgn* -> expr0*]] ...) (assigned (,uv1* ...) ,[expr (append asgn* uv1*) -> expr1]))
		  (let* ((tmp* (map (lambda (x) (unique-name (string->symbol (extract-root x)))) uv1*))
			 (expr1* (map (lambda (tmp) (let ((expr-new* (list tmp `(void))))
						       `(cons ,expr-new* ...)))  tmp*))
			 (a-list (map (lambda (x y) (cons x y)) uv1* tmp*))
			 (uv0* (map (lambda (x) (cond
						 [(assq x a-list) => cdr]
						 [else x])) uv0*)))

		  `(let ([,uv0* ,expr0*] ...) (let ([,uv1* ,expr1*] ...) ,expr1)))]
		 [,uv (if (memv uv asgn*) (let ((expr* (list uv))) `(car ,expr* ...)) `,uv)]
		 [(set! ,uv ,[expr asgn* -> expr]) (let ((expr* (list uv expr))) `(set-car! ,expr* ...))]
		 
		)

	   (let ((expr (Expr orig '())))
	     expr)
)
)