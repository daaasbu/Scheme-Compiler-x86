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
		 [(letrec ([,l* ,[le* l* -> le*]] ...) ,[tl l* -> tl]) 
		  (unless (suffix-list l*) (error who "Duplicate Labels"))
		  x])
	   
	   (LambdaExpr : LambdaExpr (x env) -> LambdaExpr ()
		       [(lambda (,uv* ...) ,[tl (append uv* env) -> tl]) (begin (set! uv-list (append uv* uv-list)) (unless (suffix-list uv*) (error who "Duplicate suffixes")))])

	   (Value : Value (x env) -> Value ()
		  [(let ([,uv* ,[val* (append uv* env) -> val*]] ...) ,[val (append uv* env) -> val])
		   (begin (set! uv-list (append uv* uv-list)) (unless (suffix-list uv*) (error who "Duplicate suffixes")))
		   x]

		  [(prim ,op ,[val0 env -> val0] ,[val1 env -> val1])
 		   (if (eqv? op `sra)
		       (unless (and (integer? val1) (<= 0 val1) (>= 63 val1))
			       (error who "sra out of bounds/not a number" val1))) x])
	   
	   (Effect : Effect (x env) -> Effect ()
		   [(let ([,uv* ,[val* (append uv* env) -> val*]] ...) ,[ef (append uv* env) -> ef])
		    (begin (set! uv-list (append uv* uv-list)) (unless (suffix-list uv*) (error who "Duplicate suffixes")))
		    x])
	   
	   (Pred : Pred (x env) -> Pred ()
		 [(let ([,uv* ,[val* (append uv* env) -> val*]] ...) ,[pred (append uv* env) -> pred])
		  (begin (set! uv-list (append uv* uv-list)) (unless (suffix-list uv*) (error who "Duplicate suffixes")))
		  x])

	   (Tail : Tail (x env) -> Tail () 
		 [(let ([,uv* ,[val* (append uv* env) -> val*]] ...) ,[tl (append uv* env) -> tl])
		  (begin (set! uv-list (append uv* uv-list)) (unless (suffix-list uv*) (error who "Duplicate suffixes")))
		  x])
	   (Triv : Triv (x env) -> Triv ()
		 [,uv (unless (and (not (lookup (extract-suffix uv) (map (lambda (x) (extract-suffix x)) uv-list))) 
				   (lookup uv env)) (error who "unbound var" uv)) x]
		 [,l (unless (lookup l env) (error who "unbound label" l)) x])))

