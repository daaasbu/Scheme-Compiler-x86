(library (Compiler parse-scheme)
	 (export parse-scheme)
	 (import
	  (chezscheme)
	  (Framework match)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass parse-scheme : * (x) -> LparseScheme ()
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
#|
	     (define-syntax and^
	       (syntax-rules ()
		 ((and^) '#t)
		 ((and^ e1) (with-output-language (LparseScheme Expr) e1))
		 ((and^ e1 e2 e3 ...) (begin (printf "sdgdsfg") (with-output-language (LparseScheme Expr) `(if ,e1 ,(and^ e2 e3 ...) '#f))))))

	     (define-syntax or^
	       (syntax-rules ()
		 ((or^) '#f)
		 ((or^ e1) (with-output-language (LparseScheme Expr) e1))
		 ((or^ e1 e2 e3 ...) (let ((t (unique-name 'TMP))) 
				      (with-outputlanguage (LparseScheme Expr) `(let ((,t ,e1)) (if ,t ,t ,(or^ e2 e3 ...))))))))
|#



	     (define and^
	       (lambda (e env)
		 (cond
		  ((null? e) '#t)
		  ((null? (cdr e)) (parse-data (car e) env))
		  (else (with-output-language (LparseScheme Expr) `(if ,(parse-data (car e) env) ,(and^ (cdr e) env) '#f))))))


	     (define or^
	       (lambda (e env)
		 (cond
		  ((null? e) '#f)
		  ((null? (cdr e)) (parse-data (car e) env))
		  (else (let ((t (unique-name 'TMP)))  (with-output-language (LparseScheme Expr) `(let ((,t ,(parse-data (car e) env))) (if ,t ,t ,(or^ (cdr e) env)))))))))



	     (define duplicate?
	       (lambda (ls)
		 (cond
		  [(null? ls) #f]
		  [(memv (car ls) (cdr ls)) #t]
		  [else (duplicate? (cdr ls))])))


			 
	     (define uv-list '())
	     (define proc3* '(vector-set! procedure-set!))
	     (define proc2* '(+ - * cons = <= < >= > eq? set-cdr! set-car! make-procedure vector-ref procedure-ref))
	     (define proc1* '(car cdr make-vector vector-length procedure-code boolean? fixnum? null? pair? vector? procedure?))
	     
	     (define parse-data
	       (lambda (expr env)
		 (match expr
			[,d (guard (or (number? d) (boolean? d))) (unless (or (integer? d) (boolean? d)) (error who "not proper datum")) (with-output-language (LparseScheme Expr) `(quote ,d))]
			[(quote ,d)  (guard (and (not (assq 'quote env)) (or (symbol? expr) (vector? expr) (list? expr)))) (with-output-language (LparseScheme Expr) `(quote ,d))]
			[,d (guard (symbol? d)) (unless (assq d env) (error who "unbound variable" d)) (cadr (assq d env))]
			[(not ,e) (guard (not (assq 'not env))) (with-output-language (LparseScheme Expr) `(if ,(parse-data e env) (quote #f) (quote #t)))]
			[(and ,e1 ...) (guard (not (assq 'and env)))  (and^ e1 env)]
			[(or ,e1 ...) (guard (not (assq 'or env))) (or^ e1 env)]
			[(set! ,uv ,e)  (guard (not (assq 'set! env))) (unless (assq uv env) (error who "unbound variable" uv)) (with-output-language (LparseScheme Expr) `(set! ,(cadr (assq uv env)) ,(parse-data e env)))]
			[(,op ,e1 ,e2 ,e3) (guard (and (not (assq op env)) (memv op proc3*))) (with-output-language (LparseScheme Expr) `(,op ,(parse-data e1 env) ,(parse-data e2 env) ,(parse-data e3 env)))]
			[(,op ,e1 ,e2) (guard (and (not (assq op env)) (memv op proc2*))) (with-output-language (LparseScheme Expr) `(,op ,(parse-data e1 env) ,(parse-data e2 env)))]
			[(,op ,e) (guard (and (not (assq op env)) (memv op proc1*)))  (with-output-language (LparseScheme Expr) `(,op ,(parse-data e env)))]
			[(void) (guard (not (assq 'void env)))  (with-output-language (LparseScheme Expr) `(void))]
			[(begin ,e* ... ,e1) (guard (not (assq 'begin env))) (with-output-language (LparseScheme Expr) `(begin ,(map (lambda (e) (parse-data e env)) e*) ... ,(parse-data e1 env)))]
			[(if ,e1 ,e2 ,e3) (guard (not (assq 'if env))) (with-output-language (LparseScheme Expr) `(if ,(parse-data e1 env) ,(parse-data e2 env) ,(parse-data e3 env)))]
			[(if ,e1 ,e2) (guard (not (assq 'if env))) (with-output-language (LparseScheme Expr) `(if ,(parse-data e1 env) ,(parse-data e2 env) (void)))]
			[(let ([,uv* ,e0*] ...) ,e) (guard (not (assq 'let env))) (unless (not (duplicate? uv*)) (error who "duplicate bindings"))
			 (let* ((a-list (map (lambda (uv) (list uv (unique-name uv))) uv*))
				(uv* (map (lambda (x) (cadr x)) a-list))
				(env-new (filter (lambda (x) (not (assq (car x) a-list))) env))
				(env-new (append a-list env-new)))
			   (with-output-language (LparseScheme Expr) `(let ([,uv* ,(map (lambda (e) (parse-data e env)) e0*)] ...) ,(parse-data e env-new))))] 
			[(let ([,uv* ,e0*] ...) ,e1* ... ,e1) (guard (not (assq 'let env)))
			 (unless (not (duplicate? uv*)) (error who "duplicate bindings"))
			  (let* ((a-list (map (lambda (uv) (list uv (unique-name uv))) uv*))
				 (uv* (map (lambda (x) (cadr x)) a-list))
				 (env-new (filter (lambda (x) (not (assq (car x) a-list))) env))
				 (env-new (append a-list env-new)))
			    (let ((bgn (with-output-language (LparseScheme Expr) `(begin ,(map (lambda (e1) (parse-data e1 env-new)) e1*) ... ,(parse-data e1 env-new)))))
			      (with-output-language (LparseScheme Expr) `(let ([,uv* ,(map (lambda (e) (parse-data e env)) e0*)] ...) ,bgn))))]
			[(lambda (,uv* ...) ,e) (guard (not (assq 'lambda env))) (unless (not (duplicate? uv*)) (error who "duplicate bindings"))
			 (let* ((a-list (map (lambda (uv) (list uv (unique-name uv))) uv*))
				(uv* (map (lambda (x) (cadr x)) a-list))
				(env-new (filter (lambda (x) (not (assq (car x) a-list))) env))
				(env-new (append a-list env-new)))
			   (with-output-language (LparseScheme Expr) `(lambda (,uv* ...) ,(parse-data e env-new))))]
			[(lambda (,uv* ...) ,e* ... ,e) (guard (not (assq 'lambda env))) (unless (not (duplicate? uv*)) (error who "duplicate bindings"))
			 (let* ((a-list (map (lambda (uv) (list uv (unique-name uv))) uv*))
				(uv* (map (lambda (x) (cadr x)) a-list))
				(env-new (filter (lambda (x) (not (assq (car x) a-list))) env))
				(env-new (append a-list env-new)))
			   (let ((bgn (with-output-language (LparseScheme Expr) `(begin ,(map (lambda (e) (parse-data e env-new)) e*) ... ,(parse-data e env-new)))))
			     (with-output-language (LparseScheme Expr) `(lambda (,uv* ...) ,bgn))))]
			[(letrec ([,uv* ,e*] ...) ,e) (guard (not (assq 'letrec env))) (unless (not (duplicate? uv*)) (error who "duplicate bindings"))
			 (let* ((a-list (map (lambda (uv) (list uv (unique-name uv))) uv*))
				(uv* (map (lambda (x) (cadr x)) a-list))
				(env-new (filter (lambda (x) (not (assq (car x) a-list))) env))
				(env-new (append a-list env-new)))
			   (with-output-language (LparseScheme Expr) `(letrec ([,uv* ,(map (lambda (e) (parse-data e env-new)) e*)] ...) ,(parse-data e env-new))))]
			[(letrec ([,uv* ,e*] ...) ,e1* ... ,e1) (guard (not (assq 'letrec env))) (unless (not (duplicate? uv*)) (error who "duplicate bindings"))
			 (let* ((a-list (map (lambda (uv) (list uv (unique-name uv))) uv*))
				(uv* (map (lambda (x) (cadr x)) a-list))
				(env-new (filter (lambda (x) (not (assq (car x) a-list))) env))
				(env-new (append a-list env-new)))
			   (let ((bgn (with-output-language (LparseScheme Expr) `(begin ,(map (lambda (e) (parse-data e env-new)) e1*) ... ,(parse-data e1 env-new)))))
			     (with-output-language (LparseScheme Expr) `(letrec ([,uv* ,(map (lambda (e) (parse-data e env-new)) e*)] ...) ,bgn))))]
			;[(,expr ,expr* ...) (guard (and (not (memv expr env)) (memv expr (append (list 'set! 'cons 'let 'quote 'letrec 'lambda 'begin 'if)  proc1* proc2* proc3*)))) (error who "incorrect argument count")]
			[(,expr ,expr* ...)
					     (with-output-language (LparseScheme Expr) `(call ,(cond
												[(assq expr env) => cadr]
											       
												[else (parse-data expr env)]) ,(map (lambda (expr) (parse-data expr env)) expr*) ...))]
			[,x (error who "invalid expression" expr)]

			))))
	   (parse-data x '())


	   ;(Expr : * (x) -> Expr ())
		 
	;	 [,d `(quote ,d)])
	   #;(Expr : Expr (x) -> Expr ()
		 [(lambda (,uv* ...) ,expr)
		  (begin
		    (set! uv-list (append uv* uv-list))
		    (unless (suffix-list uv-list) (error who "Duplicate suffixes"))
		    `(lambda (,uv* ...) ,(Expr expr)))]
		 [,uv (unless (lookup uv uv-list) (error who "unbound variable")) x]
		 [(quote ,d) (if (integer? d)
				 (begin (unless (fixnum-range? d) (error who "fixnum not in range" d)) x)
				 x)]
		 [(if ,expr0 ,expr1 ,expr2) `(if ,(Expr expr0) ,(Expr expr1) ,(Expr expr2))]
		 [(begin ,expr* ... ,expr) `(begin ,(map^ Expr expr*) ... ,(Expr expr))]
		 [(let ([,uv* ,expr*] ...) ,expr) (begin
						    (set! uv-list (append uv* uv-list))
						    (unless (suffix-list uv-list) (error who "Duplicate suffixes"))
						    `(let ([,uv* ,(map^ Expr expr*)] ...) ,(Expr expr)))]
		 [(letrec ([,uv* ,expr*] ...) ,expr) (begin
						     (set! uv-list (append uv* uv-list))
						     (unless (suffix-list uv*) (error who "Duplicate Labels"))
						     `(letrec ([,uv* ,(map^ Expr expr*)] ...) ,(Expr expr)))]
		 [(set! ,uv ,expr) (unless (lookup uv uv-list) (error who "unbound variable" uv)) `(set! ,uv ,(Expr expr))]
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