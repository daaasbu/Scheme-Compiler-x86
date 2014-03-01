(library (Compiler remove-complex-opera*)
	 (export remove-complex-opera* parse-LremoveComplexOpera*)
	 (import 
	  (chezscheme)
	  (source-grammar)
	  (Framework nanopass)
	  (Framework helpers))

	 (define-parser parse-LremoveComplexOpera* LremoveComplexOpera*)
;;This pass goes from LverifyScheme to LremoveComplexOpera*. Its purpose is to unnest prims and calls, and replace all nested operations by using set!'s to uvariables.
;;It replaces all complex operations in call with uvars, and it unnests the complex calls, and set!'s them to uvars.
	 (define-pass remove-complex-opera* : LverifyScheme (x) -> LremoveComplexOpera* ()
	   (definitions
	     (define VAR-ls '())

	     (define make-var
	       (lambda ()
		 (let ((v (unique-name 'VAR)))
		       (begin (set! VAR-ls (cons v VAR-ls)) v))))
	     (define simple?
	       (lambda (exp)
 		 (or (integer? exp) (uvar? exp))))


	    

	     (define Value*
	       (lambda (val*)
		 (with-output-language (LremoveComplexOpera* Effect)
		 (if (null? val*) '()
		    (let* ((first (Value (car val*)))
			    (rest (cdr val*)))
		       (if (or (uvar? first) (number? first))
			   (begin (set! local-var-ls (cons first local-var-ls)) (Value* rest))
		     (let* ((VAR (make-var))
			    (set `(set! ,VAR , first))) (begin  (set! local-var-ls (cons (with-output-language (LremoveComplexOpera* Triv) VAR) local-var-ls)) (cons set (Value* rest))))))))))




	     (define Value**
	       (lambda (val*)
		 (with-output-language (LremoveComplexOpera* Effect)
		 (define Value*-h
		   (lambda (val* acc local-var-ls)
		     (if (null? val*) (values (reverse acc) local-var-ls)
			 (let* ((first (Value (car val*)))
				(rest (cdr val*)))
			   (if (or (uvar? first) (number? first))
			       (Value*-h rest acc (cons first local-var-ls))
			       (let* ((VAR (make-var))
				      (ef `(set! ,VAR ,first))
				      (v (with-output-language (LremoveComplexOpera* Triv) VAR)))
				 (Value*-h rest (cons ef acc) (cons v local-var-ls))))))))
		 (Value*-h val* '() '()))))
				 

		      
			 
		      
 
	     (define nested?
	       (lambda (exp)
		 (nanopass-case (LremoveComplexOpera* Value) exp
				[(prim ,op ,triv0 ,triv1) #t]
				[,triv #t]
				[else #f])))

	     (define local-var-ls '())

	     (define initialize 
	       (lambda () 
		 (set! local-var-ls '())))

	     (define simple-ls
	       (lambda (ls)
		 (map simple? ls)))
    
	     (define map^
	       (lambda (proc ls)
		 (cond
		  ((null? ls) ls)
		  (else (let ((output (proc (car ls)))) (cons output (map^ proc (cdr ls))))))))
	     )
#|
	   (Prog : Prog (x) -> Prog ()
		 [(letrec ([,l* ,[le*]] ...) (locals (,uv* ...) (call ,val ,val* ...))) (begin (display "LOOP-Prog") `(letrec ([,l* ,le*] ... ) (locals (,uv* ...) (call ,val ,val* ...))))])


	   (LambdaExpr : LambdaExpr(x) -> LambdaExpr ()
		       [(lambda (,uv* ...) ,[bd]) (begin (display "LOOP-LE") `(lambda (,uv* ...) ,bd))])
|#
           (Body : Body (x) -> Body ()    
		 [(locals (,uv* ...) ,tl) (begin (set! VAR-ls '()) (let ((t (Tail tl)))  `(locals (,(append uv* VAR-ls) ...) ,t)))])

           (Tail : Tail (x) -> Tail ()
	    
		 [,triv `,triv]
		 [(prim ,op ,[val0] ,[val1]) (cond
					      [(and (simple? val1) (simple? val0)) `(prim ,op ,val0 ,val1)]
					      [(simple? val0)  (let ((VAR (make-var)))
								 `(begin (set! ,VAR ,val1) (prim ,op ,val0 ,VAR)))]
					      [(simple? val1)  (let ((VAR (make-var)))
								 `(begin (set! ,VAR ,val0) (prim ,op ,VAR ,val1)))]
					      
					      [else (let ((VAR0 (make-var)) (VAR1 (make-var))) `(begin (set! ,VAR0 ,val0) (set! ,VAR1 ,val1) (prim ,op ,VAR0 ,VAR1)))])]
		 
		 [(call ,[val] ,val* ...) (if (not (member #f (simple-ls val*))) 
					      `(call ,val ,(map^ Value val*) ...)
					      (let-values (((output local-var-ls) (Value** val*)))
						`(begin ,output ... (call ,val ,(reverse local-var-ls) ...))))]  ;changed
		 [(if ,[pred] ,[tl1] ,[tl2]) `(if ,pred ,tl1 ,tl2)]
		 [(begin ,[ef*] ... ,[tl1]) `(begin ,ef* ... ,tl1)])
           (Pred : Pred (x) -> Pred ()
                 [(true) `(true)]
                 [(false) `(false)]
                 [(prim ,relop ,[val0] ,[val1])  (cond 
					       [(and (simple? val1) (simple? val0)) `(,relop ,val0 ,val1)]
					       [(simple? val0)  (let ((VAR (make-var)))
								 `(begin (set! ,VAR ,val1) (,relop ,val0 ,VAR)))]
					       [(simple? val1)  (let ((VAR (make-var)))
								 `(begin (set! ,VAR ,val0) (,relop ,VAR ,val1)))]
					       
					       [else (let ((VAR0 (make-var)) (VAR1 (make-var))) `(begin (set! ,VAR0 ,val0) (set! ,VAR1 ,val1) (,relop ,VAR0 ,VAR1)))])]

                 [(if ,[pred1] ,[pred2] ,[pred3]) `(if ,pred1 ,pred2 ,pred3)]
                 [(begin ,[ef*] ... ,[pred]) `(begin ,ef* ... ,pred)])

           (Effect : Effect (x) -> Effect ()
                   [(set! ,uv ,val) `(set! ,uv ,(Value val))] 
		   [(if ,[pred0] ,[ef0] ,[ef1]) `(if ,pred0 ,ef0 ,ef1)]
		   [(begin ,[ef*] ... ,[ef]) `(begin ,ef* ... ,ef)]
		   [(nop) `(nop)]
		    [(call ,[val] ,val* ...) (if (not (member #f (simple-ls val*))) 
					      `(call ,val ,(map^ Value val*) ...)
					      (let-values (((output local-var-ls) (Value** val*)))
						`(begin ,output ... (call ,val ,(reverse local-var-ls) ...))))]
		   #;[(call ,[val] ,val* ...) (begin (initialize) (if (not (member #f (simple-ls val*))) 
								    `(call ,val ,(map^ Value val*) ...) 
								    (let* ((output (Value* val*)))
								      `(begin ,output ... (call ,val ,(reverse local-var-ls) ...)))))])

	   (Value : Value (x) -> Value ()
		  [,triv (Triv triv)]
		  [(if ,[pred] ,[val0] ,[val1])
		   `(if ,pred ,val0 ,val1)]
	#|	   (cond 
		    [(and (nested? val1) (nested? val0)) `(if ,pred ,val0 ,val1)]
		    [(nested? val0) (let ((VAR (make-var)))
					      `(if ,pred ,val0 ,val1)))]
				      
		    [(nested? val1) (let ((VAR (make-var)))
					    `(begin (set! ,VAR ,val0) (if ,pred ,VAR ,val1)))]
		    [else (let ((VAR0 (make-var)) (VAR1 (make-var))) `(begin (set! ,VAR0 ,val0) (set! ,VAR1 ,val1) (if ,pred ,VAR0 ,VAR1)))])]
		   |#
		   [(prim ,op ,[val0] ,[val1])
		   (cond 
		    [(and (simple? val1) (simple? val0)) `(prim ,op ,val0 ,val1)]
		    [(simple? val0) (let ((VAR (make-var)))
				       `(begin (set! ,VAR ,val1) (prim ,op ,val0 ,VAR)))]
		    [(simple? val1) (let ((VAR (make-var)))
				      `(begin (set! ,VAR ,val0) (prim ,op ,VAR ,val1)))]
		    
		    [else (let ((VAR0 (make-var))
 				(VAR1 (make-var)))
			    `(begin  (set! ,VAR0 ,val0) (set! ,VAR1 ,val1) (prim ,op ,VAR0 ,VAR1)))])]

		  [(begin ,[ef*] ... ,[val]) `(begin ,ef* ... ,val)]
		  
		  [(call ,[val] ,val* ...) (if (not (member #f (simple-ls val*))) 
					       `(call ,val ,(map^ Value val*) ...)
					       (let-values (((output local-var-ls) (Value** val*)))
						 `(begin ,output ... (call ,val ,(reverse local-var-ls) ...))))]
		  #;[(call ,[val] ,val* ...) (begin (initialize) (if (not (member #f (simple-ls val*))) 
								   `(call ,val ,(map Value val*) ...) 
								   (let* ((output (Value* val*)))
								     `(begin ,output ... (call ,val ,(reverse local-var-ls) ...)))))]
		  [else (error who "mistake in Value")])

	   (Triv : Triv (x) -> Triv () 
		 [,i `,i]
		 [,l `,l]
		 [,uv `,uv])))





