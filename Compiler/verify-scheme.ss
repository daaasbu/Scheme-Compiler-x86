(library (Compiler verify-scheme)
	 (export verify-scheme)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass verify-scheme : LverifyScheme (x) -> LverifyScheme ()
	   (definitions
	     (define lookup
	       (lambda (x envr)
		 (member x envr)))

	     (define duplicate-labels?
	       (lambda (env)
		 (cond
		  [(null? env) #t]
		  [(member (car env) (cdr env)) #f]
		  [else (duplicate-labels? (cdr env))])))

	     (define suffix-list
	       (lambda (ls)
		 (duplicate-labels? (map extract-suffix ls))))

	     (define map^
	       (lambda (proc ls)
		 (cond
		  ((null? ls) '())
		  (else (let ((x (proc (car ls))))
			  (cons x (map^ proc (cdr ls))))))))

	     (define uv-list '()))

	   (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
		       [(lambda (,uv* ...) ,expr)
			(begin
			  (set! uv-list (append uv* uv-list))
			  (unless (suffix-list uv-list) (error who "Duplicate suffixes"))
			  `(lambda (,uv* ...) ,(Expr expr)))])

	   (Expr : Expr (x) -> Expr ()
		 [,uv (unless (lookup uv uv-list) (error who "unbound variable")) x]
		 [(quote ,i) (if (integer? i)
				 (begin (unless (fixnum-range? i) (error who "fixnum not in range" i)) x)
				 x)]
		 [(if ,expr0 ,expr1 ,expr2) `(if ,(Expr expr0) ,(Expr expr1) ,(Expr expr2))]
		 [(begin ,expr* ... ,expr) `(begin ,(map^ Expr expr*) ... ,(Expr expr))]
		 [(let ([,uv* ,expr*] ...) ,expr) (begin
						    (set! uv-list (append uv* uv-list))
						    (unless (suffix-list uv-list) (error who "Duplicate suffixes"))
						    `(let ([,uv* ,(map^ Expr expr*)] ...) ,(Expr expr)))]
		 [(letrec ([,uv* ,le*] ...) ,expr) (begin
						     (set! uv-list (append uv* uv-list))
						     (unless (suffix-list uv*) (error who "Duplicate Labels"))
						     `(letrec ([,uv* ,(map^ LambdaExpr le*)] ...) ,(Expr expr)))]
		 [(,prim ,expr* ...)
		  (begin
		    (let ((len (if (list? expr*) (length expr*) 1))
			  (expr* (map^ Expr expr*))
			  (x `(,prim ,expr* ...)))
		      (case prim
			[(set-car! set-cdr! < <= = >= > eq? + - * cons vector-ref)
			 (unless (= 2 len) (error who "Incorrect argument count" prim)) x]
			[(procedure? boolean? fixnum? null? pair? vector? car cdr make-vector vector-length)
			 (unless (= 1 len) (error who "Incorrect argument count" prim)) x]
			[(void) (unless (= 0 len) (error who "Incorrect argument count" prim)) x]
			[(set-car! set-cdr!) (unless (= 2 len) (error who "Incorrect argument count" prim)) x]
			[(vector-set!) (unless (= 3 len) (error who "Incorrect argument count" prim)) x]
			[else (error who "didnt match this case ~a" prim)])))]
		 [(call ,expr ,expr* ...) `(call ,(Expr expr) ,(map^ Expr expr*) ...)])))