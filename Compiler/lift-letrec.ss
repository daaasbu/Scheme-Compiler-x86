(library (Compiler lift-letrec)
	 (export lift-letrec)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass lift-letrec : LintroduceProcedurePrimitives (x) -> LliftLetrec ()
	   (definitions
	     (define bindings '())
	     )

	   (Expr : Expr (x) -> Expr ()
	      [(letrec ([,l* ,[le*]] ...) ,[expr]) 
	       (begin
		 (set! bindings (append (map cons l* le*) bindings))
		 `,expr)])

	   (Expr2 : Expr (x) -> Prog ()
	      [(letrec ([,l* ,[le*]] ...) ,[expr]) (begin
						     (set! bindings (append (map cons l* le*) bindings))
						     (let ((l* (map (lambda (x) (car x)) bindings))
							   (le* (map (lambda (x) (cdr x)) bindings)))
						       `(letrec ([,l* ,le*] ...) ,expr)))]
	      [,l (let ((l* (map (lambda (x) (car x)) bindings))
			(le* (map (lambda (x) (cdr x)) bindings)))
		    `(letrec ([,l* ,le*] ...) ,l))]
	      [,uv  (let ((l* (map (lambda (x) (car x)) bindings))
			  (le* (map (lambda (x) (cdr x)) bindings)))
		      `(letrec ([,l* ,le*] ...) ,uv))] 
	      [(quote ,i) (let ((l* (map (lambda (x) (car x)) bindings))
				(le* (map (lambda (x) (cdr x)) bindings)))
			    `(letrec ([,l* ,le*] ...) (quote ,i)))]
	      [(if ,[expr0] ,[expr1] ,[expr2]) (let ((l* (map (lambda (x) (car x)) bindings))
						       (le* (map (lambda (x) (cdr x)) bindings)))
						   `(letrec ([,l* ,le*] ...) (if ,expr0 ,expr1 ,expr2)))]
	      [(begin ,[expr*] ... ,[expr]) (let ((l* (map (lambda (x) (car x)) bindings))
						       (le* (map (lambda (x) (cdr x)) bindings)))
						   `(letrec ([,l* ,le*] ...) (begin ,expr* ... ,expr)))]
	      [(let ([,uv* ,[expr*]] ...) ,[expr]) (let ((l* (map (lambda (x) (car x)) bindings))
							 (le* (map (lambda (x) (cdr x)) bindings)))
						     `(letrec ([,l* ,le*] ...) (let ([,uv* ,expr*] ...) ,expr)))]
	      [(,prim ,[expr*] ...) (let ((l* (map (lambda (x) (car x)) bindings))
					  (le* (map (lambda (x) (cdr x)) bindings)))
				      `(letrec ([,l* ,le*] ...) (,prim ,expr* ...)))]
	      [(call ,[expr] ,[expr*] ...) (let ((l* (map (lambda (x) (car x)) bindings))
						 (le* (map (lambda (x) (cdr x)) bindings)))
					     `(letrec ([,l* ,le*] ...) (call ,expr ,expr* ...)))])))