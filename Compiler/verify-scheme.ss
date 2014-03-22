(library (Compiler verify-scheme)
	 (export verify-scheme)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 ;; Midterm TODO: Complete this pass definition.

	 (define-pass verify-scheme : LverifyScheme (x) -> LverifyScheme ()
	   (definitions
	     (define lookup
	       (lambda (x env)
		 (member x env)))

	     (define duplicate-labels?
	       (lambda (env)
		 (cond
		  [(null? env) #t]
		  [(member (car env) (cdr env)) #f]
		  [else (duplicate-labels? (cdr env))])))

	     (define suffix-list
	       (lambda (ls)
		 (duplicate-labels? (map extract-suffix ls))))

	     
	     (define uv-list '())
	     )

	   (Prog : Prog (x) -> Prog ()
		 [(letrec ([,l* ,[le* l* -> le*]] ...) ,[val l* -> val]) 
		  (unless (suffix-list l*) (error who "Duplicate Labels"))
		  x])
	   
	   (LambdaExpr : LambdaExpr (x env) -> LambdaExpr ()
		       [(lambda (,uv* ...) ,[val (append uv* env) -> val]) (begin (set! uv-list (append uv* uv-list)) (unless (suffix-list uv-list) (error who "Duplicate suffixes")))])

	   (Value : Value (x env) -> Value ()
		  [(let ([,uv* ,[val* (append uv* env) -> val*]] ...) ,[val (append uv* env) -> val])
		   (begin (set! uv-list (append uv* uv-list)) (unless (suffix-list uv-list) (error who "Duplicate suffixes")))
		   x]

		  #;[(vprim ,op ,[val0 env -> val0] ,[val1 env -> val1])
 		   (if (eqv? op `sra)
		       (unless (and (integer? val1) (<= 0 val1) (>= 63 val1))
			       (error who "sra out of bounds/not a number" val1))) x]
		  [,uv (unless (and (not (lookup (extract-suffix uv) (map (lambda (x) (extract-suffix x)) uv-list))) 
				   (lookup uv env)) (error who "unbound var" uv)) x]
		 [,l (unless (lookup l env) (error who "unbound label" l)) x]
		 [(,vprim ,[val* env -> val*] ...)
		  (begin

		    (let ((len (if (list? val*) (length val*) 1)))
		    (case vprim
		      [(+ - * cons vector-ref) (unless (= 2 len) (error who "Incorrect argument count" vprim))]
		      [(car cdr make-vector vector-length) (unless (= 1 len) (error who "Incorrect argument count" vprim))]
		      [(void) (unless (= 0 len) (error who "Incorrect argument count" vprim))])
			     x))]
		 [(quote ,i) (if (integer? i) (unless (fixnum-range? i) (error who "fixnum not in range" i)) x)]
		 #;[else (printf "expression: ~a \n" x) ])
	   
	   (Effect : Effect (x env) -> Effect ()
		   [(let ([,uv* ,[val* (append uv* env) -> val*]] ...) ,[ef (append uv* env) -> ef])
		    (begin (set! uv-list (append uv* uv-list)) (unless (suffix-list uv-list) (error who "Duplicate suffixes")))
		    x]
		   [(,eprim ,[val* env -> val*] ...)
		  (begin
		    (let ((len (if (list? val*) (length val*) 1)))
		    (case eprim
		      [(set-car! set-cdr!) (unless (= 2 len) (error who "Incorrect argument count" eprim))]
		      [(vector-set!) (unless (= 3 len) (error who "Incorrect argument count" eprim))]))
		  				     x)])
	   
	   (Pred : Pred (x env) -> Pred ()
		 [(let ([,uv* ,[val* (append uv* env) -> val*]] ...) ,[pred (append uv* env) -> pred])
		  (begin (set! uv-list (append uv* uv-list)) (unless (suffix-list uv-list) (error who "Duplicate suffixes")))
		  x]

		 [(,pprim ,[val* env -> val*] ...)
		  (begin
		    (let ((len (if (list? val*) (length val*) 1)))
		    (case pprim
		      [(< <= > >= = eq?) (unless (= 2 len) (error who "Incorrect argument count" pprim))]
		      [(fixnum? boolean? null? pair? vector?) (unless (= 1 len) (error who "Incorrect argument count" pprim))]))
		   
				     x)])
	   
		 ))


