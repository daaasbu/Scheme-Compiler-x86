(library (Compiler normalize-context)
	 (export normalize-context)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass normalize-context : LliftLetrec (x) -> LnormalizeContext ()
	   (definitions
	     (define effect-prim?
	       (lambda (x)
		 (if (memq x '(set-car! set-cdr! vector-set! procedure-set!)) #t #f)))
	     (define value-prim?
	       (lambda (x)
		 (if (memq x '(+ - * car cdr cons make-vector vector-length vector-ref void make-procedure procedure-code procedure-ref)) #t #f)))
	     (define pred-prim?
	       (lambda (x)
		 (if (memq x '(< <= = >= > boolean? eq? fixnum? null? pair? vector? procedure?))
		     #t #f)))     
	     
	     (define-syntax make-if
	       (syntax-rules ()
		 [(make-if p e* ...) (with-output-language (LnormalizeContext Pred) `(if (eq? (,p ,e* ...) #f) (false) (true)))])) 
	     

	     )
	   (Prog : Prog (x) -> Prog ()
		 [(letrec ([,l* ,[le*]] ...) ,expr) `(letrec ([,l* ,le*] ...) ,(Expr expr 'Value))])

	   (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
		       [(lambda (,uv* ...) ,expr) `(lambda (,uv* ...) ,(Expr expr 'Value))])




	   (Expr : Expr (x type) -> * ()
		 [,l  (case type
			((Value) (in-context Value `,l))
			((Pred) (in-context Pred `(true)));;MAY BE POSSIBLE SOURCE OF FUTURE ERROR   
			((Effect) (in-context Effect `(nop))))] 
		 [,uv  (case type
			 ((Value) (in-context Value `,uv))
			 ((Pred) (in-context Pred `(true)))   
			 ((Effect) (in-context Effect `(nop))))]    
		 [(quote ,i) (case type
			       ((Value) (in-context Value `(quote ,i)))
			       ((Pred) (in-context Pred (if (equal? i #f) `(false)  `(true))))   
			       ((Effect) (in-context Effect `(nop))))]   
		 [(if ,expr0 ,expr1 ,expr2) (case type
					      ((Value)  (in-context Value `(if ,(Expr expr0 'Pred) ,(Expr expr1 type) ,(Expr expr2 type))))
					      ((Pred)  (in-context Pred `(if ,(Expr expr0 'Pred) ,(Expr expr1 type) ,(Expr expr2 type))))
					      ((Effect)  (in-context Effect `(if ,(Expr expr0 'Pred) ,(Expr expr1 type) ,(Expr expr2 type)))))]
		 [(begin ,expr* ... ,expr) (case type
					     ((Value)  (in-context Value `(begin ,(map (lambda (x) (Expr x 'Effect)) expr*) ... ,(Expr expr type))))
					     ((Pred)  (in-context Pred `(begin ,(map (lambda (x) (Expr x 'Effect)) expr*) ... ,(Expr expr type))))
					     ((Effect)  (in-context Effect `(begin ,(map (lambda (x) (Expr x 'Effect)) expr*) ... ,(Expr expr type)))))]
		 [(let ([,uv* ,expr*] ...) ,expr) (case type
						    ((Value)  (in-context Value `(let ([,uv* ,(map (lambda (x) (Expr x 'Value)) expr*)] ...) ,(Expr expr type))))
						    ((Pred)  (in-context Pred `(let ([,uv* ,(map (lambda (x) (Expr x 'Value)) expr*)] ...) ,(Expr expr type))))
						    ((Effect)  (in-context Effect `(let ([,uv* ,(map (lambda (x) (Expr x 'Value)) expr*)] ...) ,(Expr expr type)))))] 
		 [(,prim ,expr* ...) (case type 
				       ((Value) (cond
						 ((pred-prim? prim) (let ((pprim prim)) (in-context Value `(if (,pprim ,(map (lambda (x) (Expr x type)) expr*) ...) '#t '#f))));;May need to make #t and #f
						 ((effect-prim? prim) (let* ((eprim prim)
									     (ef (in-context Effect `(,eprim ,(map (lambda (x) (Expr x type)) expr*) ...)))
									     (ef* (list ef))
									     (val (in-context Value `(void))))
									(in-context Value `(begin ,ef* ... ,val))))
						 ((value-prim? prim) (let ((vprim prim)) (in-context Value `(,vprim ,(map (lambda (x) (Expr x type)) expr*) ...))))))
				       
				       ((Effect) (cond
						  ((pred-prim? prim) (let* ((ef* (map (lambda (ef) (Expr ef 'Effect)) expr*))
									    (ef (car (last-pair ef*)))
									    (ef* (reverse (cdr (reverse ef*)))))
								       (in-context Effect `(begin ,ef* ... ,ef))))
						  ((effect-prim? prim) (let ((eprim prim)) (in-context Effect `(,eprim ,(map (lambda (x) (Expr x 'Value)) expr*) ...))))
						  ((value-prim? prim) (case prim
									((void) (in-context Effect `(nop))) 
									(else (let* ((ef* (map (lambda (ef) (Expr ef 'Effect)) expr*))
										     (ef (car (last-pair ef*)))
										     (ef* (reverse (cdr (reverse ef*)))))
										(in-context Effect `(begin ,ef* ... ,ef))))))))
				       ((Pred) (cond
						((pred-prim? prim) (let ((pprim prim)) (in-context Pred `(,pprim ,(map (lambda (x) (Expr x 'Value)) expr*) ...))))
						((effect-prim? prim) (let* ((eprim prim)
									    (ef (in-context Effect `(,eprim ,(map (lambda (x) (Expr x 'Value))  expr*)  ...)))
									    (ef* (list ef)))
								       (in-context Pred `(begin ,ef* ... (true)))))  
						((value-prim? prim) (let ((vprim prim)) (in-context Pred `(if (eq? (,vprim ,(map (lambda (x) (Expr x 'Value))  expr*) ...) '#f) (false) (true))))))))]
		 
		 [(call ,expr ,expr* ...) (case type 
					    ((Value) (in-context Value `(call ,(Expr expr 'Value) ,(map (lambda (x) (Expr x 'Value)) expr*) ...)))
					    ((Effect) (in-context Effect `(call ,(Expr expr 'Value) ,(map (lambda (x) (Expr x 'Value)) expr*) ...)))
					    (else (display "Your hypothesis is wrong")))])))

