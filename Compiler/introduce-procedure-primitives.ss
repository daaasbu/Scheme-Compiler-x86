(library (Compiler introduce-procedure-primitives)
         (export introduce-procedure-primitives)
         (import
          (chezscheme)
          (source-grammar)
          (Framework helpers)
          (Framework nanopass))

         (define-pass introduce-procedure-primitives : LconvertClosures (orig) -> LintroduceProcedurePrimitives ()
           (definitions
	     (define make-procedure-set!-block
	       (lambda (cp uv uv* size free*)
		 (define bundle
		   (lambda (cp uv uv* size count free*)
		     (cond
		      [(= size count) '()]
		      [else (if (memq (car uv*) free*) 
				(let ([cell (with-output-language (LintroduceProcedurePrimitives Expr) 
								  `(procedure-set! ,uv (quote ,count) (procedure-ref ,cp (quote ,count))))])
				
				  (cons cell (bundle cp uv (cdr uv*) size (add1 count) free*)))
				(let ([cell (with-output-language (LintroduceProcedurePrimitives Expr) 
								  `(procedure-set! ,uv (quote ,count) ,(car uv*)))])
				  (cons cell (bundle cp uv (cdr uv*) size (add1 count) free*))))])))
				
		 (bundle cp uv uv* size 0 free*)))
	
	     (define get-index
	       (lambda (uv free*)
		 (define help
		   (lambda (uv free* count)
		     (cond
		      [(null? free*) #f] 
		      [(equal? uv (car free*)) count]
		      [else (help uv (cdr free*) (add1 count))])))
		 (help uv free* 0)))
	     )

	   (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
			[(lambda (,uv0* ...) (bind-free (,uv ,uv1* ...) ,expr))
			 `(lambda (,uv0* ...) ,(Expr expr uv uv1*))]
			[else (Expr x '() '())]
			)

	   (ClosureBody : ClosureBody (x cp free*) -> Expr ()
			[(closures ((,uv* ,l1* ,uv** ...) ...) ,expr)
			 (let* ([size* (map length uv**)]
			 [block* (apply append 
					(map (lambda (uv l uv* size) (make-procedure-set!-block cp uv uv* size free*)) 
					     uv* l1* uv** size*))]
			 [mp* (map (lambda (l size) `(make-procedure ,l (quote ,size))) l1* size*)])
			 `(let ([,uv* ,mp*] ...) (begin ,block* ... ,(Expr expr cp free*))))]
			[else (error who "fuck nanopass")])
			 
	   
	   (Expr : Expr (x cp free*) -> Expr ()
		 [(letrec ([,l0* ,le*] ...) ,clbd)
		  `(letrec ([,l0* ,(map (lambda (le) (LambdaExpr le)) le*)] ...) ,(ClosureBody clbd cp free*))]	 
		 [,uv (let ([index (get-index uv free*)])
			(if index 
			    `(procedure-ref ,cp (quote ,index))
			    uv))]
		 [(quote ,i) `(quote ,i)]
		 [(let ([,uv* ,expr* ] ...) ,expr) 
		  `(let ([,uv* ,(map (lambda (expr) (Expr expr cp free*)) expr*)] ...) ,(Expr expr cp free*))]
		 [(begin ,expr* ... ,expr)
		  `(begin ,(map (lambda (expr) (Expr expr cp free*)) expr*) ... ,(Expr expr cp free*))]
		 [(,prim ,expr* ...) `(,prim ,(map (lambda (expr) (Expr expr cp free*)) expr*) ...)]
		 [(call ,expr ,expr* ...)
		
		      `(call (procedure-code ,(Expr expr cp free*)) ,(map (lambda (expr) (Expr expr cp free*)) expr*) ...)]
		 [(if ,expr0 ,expr1 ,expr2)
		  `(if ,(Expr expr0 cp free*) ,(Expr expr1 cp free*) ,(Expr expr2 cp free*))] 
		 [else (error who "You forgot something")]
		 )

	   (begin #;(printf "orig: ~a \n \n" orig) (LambdaExpr orig))
	   )
	 )