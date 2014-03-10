(library (Compiler remove-complex-opera*)
	 (export remove-complex-opera* parse-LremoveComplexOpera*)
	 (import 
	  (chezscheme)
	  (source-grammar)
	  (Framework nanopass)
	  (Framework helpers))

	 (define-parser parse-LremoveComplexOpera* LremoveComplexOpera*)
;;This pass goes from LverifyScheme to LremoveComplexOpera*. Its purpose is to unnest prims and calls, and replace all not-nested? operations by using set!'s to uvariables.
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
 		 (or (label? exp) (integer? exp) (uvar? exp))))


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
	     
	     (define local-var-ls '())
	     )

           (Body : Body (x) -> Body ()    
		 [(locals (,uv* ...) ,tl) (begin (set! VAR-ls '()) (let ((t (Tail tl)))  `(locals (,(append uv* VAR-ls) ...) ,t)))]
		  [else (error who "Error in Body")])

           (Tail : Tail (x) -> Tail ()
	    
		 [,triv `,triv]
		 [(mref ,val0 ,val1) (cond 
				      ((and (simple? val0) (simple? val1)) `(mref ,val0 ,val1))
				      ((simple? val0) 
					   (let* ((VAR1 (make-var)) 
	      					  (ef1 (in-context Effect `(set! ,VAR1 ,(Value val1))))
						  (ef* (list ef1)))
					     `(begin ,ef* ... (mref ,val0 ,VAR1))))
				      ((simple? val1) 
					   (let* ((VAR0 (make-var)) 
	      					  (ef0 (in-context Effect `(set! ,VAR0 ,(Value val0))))
						  (ef* (list ef0)))
					     `(begin ,ef* ... (mref ,VAR0 ,val1))))		      
				      (else
				       (let* ((VAR0 (make-var)) 
					      (VAR1 (make-var)) 
					      (ef0 (in-context Effect `(set! ,VAR0 ,(Value val0)))) 
					      (ef1 (in-context Effect `(set! ,VAR1 ,(Value val1))))
					      (ef* (list ef0 ef1)))
					 `(begin ,ef* ... (mref ,VAR0 ,VAR1)))))] ;;Checks out
		 [(alloc ,val0) (if (simple? val0)
				     `(alloc ,val0)
				     (let* ((VAR (make-var)) 
					    (ef (in-context Effect `(set! ,VAR ,(Value val0))))) ;;Checks out 
				    `(begin ,ef (alloc ,VAR))))]
		 [(prim ,op ,[val0] ,[val1]) (cond
					      [(and (simple? val1) (simple? val0)) `(prim ,op ,val0 ,val1)]
					      [(simple? val0)  (let ((VAR (make-var)))
								 `(begin (set! ,VAR ,val1) (prim ,op ,val0 ,VAR)))]
					      [(simple? val1)  (let ((VAR (make-var)))
								 `(begin (set! ,VAR ,val0) (prim ,op ,VAR ,val1)))]
					      
					      [else (let ((VAR0 (make-var)) (VAR1 (make-var))) `(begin (set! ,VAR0 ,val0) (set! ,VAR1 ,val1) (prim ,op ,VAR0 ,VAR1)))])]
		 
		 [(call ,val ,val* ...) (cond
					   ((and (simple? val) (not (member #f (simple-ls val*))))
					    `(call ,val ,(map^ Value val*) ...))
					    ((not (member #f (simple-ls val*)))
						   (let* ((VAR (make-var))
							  (ef (in-context Effect `(set! ,VAR ,(Value val)))))
						     `(begin ,ef (call ,VAR ,(map^ Value val*) ...))))
					    ((simple? val)
					     (let-values (((output local-var-ls) (Value** val*)))
					       `(begin ,output ... (call ,val ,(reverse local-var-ls) ...))))
					    (else 
						(let-values (((output local-var-ls) (Value** val*)))
						   (let* ((VAR (make-var)) 
							  (ef (in-context Effect `(set! ,VAR ,val)))
							  (ef* (append (list ef) output))) ;;THIS MIGHT HURT US LATER.
						     `(begin ,ef* ... (call ,VAR ,(reverse local-var-ls) ...))))))] ;changed
		 [(if ,[pred] ,[tl1] ,[tl2]) `(if ,pred ,tl1 ,tl2)]
		 [(begin ,[ef*] ... ,[tl1]) `(begin ,ef* ... ,tl1)]
		  [else (error who "Error in Tail")])

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
		   [(mset! ,val0 ,val1 ,val2)
		    (cond
		     ((and (simple? val0) (simple? val1) (simple? val2)) `(mset! ,val0 ,val1 ,val2))
		     ((and (simple? val0) (simple? val1)) (let*
							      ((VAR2 (make-var)) 
							       (ef2 `(set! ,VAR2 ,(Value val2)))
							       (ef* (list ef2)))
							    `(begin ,ef* ... (mset! ,val0 ,val1 ,VAR2))))
		     ((and (simple? val0) (simple? val2))  (let*
							      ((VAR1 (make-var)) 
							       (ef1 `(set! ,VAR1 ,(Value val1)))
							       (ef* (list ef1)))
							    `(begin ,ef* ... (mset! ,val0 ,VAR1 ,val2))))
		     ((and (simple? val1) (simple? val2))  (let*
							      ((VAR0 (make-var)) 
							       (ef0 `(set! ,VAR0 ,(Value val0)))
							       (ef* (list ef0)))
							    `(begin ,ef* ... (mset! ,VAR0 ,val1 ,val2))))
		     ((simple? val0)  (let*
					  ((VAR1 (make-var))
					   (VAR2 (make-var))
					   (ef1 `(set! ,VAR1 ,(Value val1)))
					   (ef2 `(set! ,VAR2 ,(Value val2)))
					   (ef* (list ef1 ef2)))
					`(begin ,ef* ... (mset! ,val0 ,VAR1 ,VAR2))))
		     ((simple? val1) (let*
					  ((VAR0 (make-var))
					   (VAR2 (make-var))
					   (ef0 `(set! ,VAR0 ,(Value val0)))
					   (ef2 `(set! ,VAR2 ,(Value val2)))
					   (ef* (list ef0 ef2)))
					`(begin ,ef* ... (mset! ,VAR0 ,val1 ,VAR2))))
		     ((simple? val2) (let*
					  ((VAR1 (make-var))
					   (VAR0 (make-var))
					   (ef1 `(set! ,VAR1 ,(Value val1)))
					   (ef0 `(set! ,VAR0 ,(Value val0)))
					   (ef* (list ef0 ef1)))
					`(begin ,ef* ... (mset! ,VAR0 ,VAR1 ,val2))))

		    (else 
		     (let* ((VAR0 (make-var)) 
			    (VAR1 (make-var))
			    (VAR2 (make-var))
			    (ef0 (in-context Effect `(set! ,VAR0 ,(Value val0)))) 
			    (ef1 (in-context Effect `(set! ,VAR1 ,(Value val1))))
			    (ef2 (in-context Effect `(set! ,VAR2 ,(Value val2))))
			    (ef* (list ef0 ef1 ef2)))
		      `(begin ,ef* ... (mset! ,VAR0 ,VAR1 ,VAR2)))))]
		   [(if ,[pred0] ,[ef0] ,[ef1]) `(if ,pred0 ,ef0 ,ef1)]
		   [(begin ,[ef*] ... ,[ef]) `(begin ,ef* ... ,ef)]
		   [(nop) `(nop)]
		   [(call ,val ,val* ...) (cond
					   ((and (simple? val) (not (member #f (simple-ls val*))))
					    `(call ,val ,(map^ Value val*) ...))
					    ((not (member #f (simple-ls val*)))
						   (let* ((VAR (make-var))
							  (ef (in-context Effect `(set! ,VAR ,(Value val)))))
						     `(begin ,ef (call ,VAR ,(map^ Value val*) ...))))
					    ((simple? val)
					     (let-values (((output local-var-ls) (Value** val*)))
					       `(begin ,output ... (call ,val ,(reverse local-var-ls) ...))))
					    (else 
						(let-values (((output local-var-ls) (Value** val*)))
						   (let* ((VAR (make-var)) 
							  (ef (in-context Effect `(set! ,VAR ,(Value val))))
							  (ef* (append (list ef) output))) ;;THIS MIGHT HURT US LATER.
						     `(begin ,ef* ... (call ,VAR ,(reverse local-var-ls) ...))))))]
		   [else (error who "Error in Effect")]
		  
		  )

	   (Value : Value (x) -> Value ()
		  [,triv (Triv triv)]
		  [(if ,[pred] ,[val0] ,[val1])
		   `(if ,pred ,val0 ,val1)]
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
		  
		  [(call ,val ,val* ...) (cond
					   ((and (simple? val) (not (member #f (simple-ls val*))))
					    `(call ,val ,(map^ Value val*) ...))
					    ((not (member #f (simple-ls val*)))
					     (let* ((VAR (make-var))
						    (ef (in-context Effect `(set! ,VAR ,(Value val)))))
					       `(begin ,ef (call ,VAR ,(map^ Value val*) ...))))
					    ((simple? val)
					     (let-values (((output local-var-ls) (Value** val*)))
					       `(begin ,output ... (call ,val ,(reverse local-var-ls) ...))))
					    (else 
						(let-values (((output local-var-ls) (Value** val*)))
						   (let* ((VAR (make-var)) 
							  (ef (in-context Effect `(set! ,VAR ,(Value val))))
							  (ef* (append (list ef) output))) ;;THIS MIGHT HURT US LATER.
						     `(begin ,ef* ... (call ,VAR ,(reverse local-var-ls) ...))))))]
		  [(alloc ,val0) (if (simple? val0)
				     `(alloc ,val0)
				     (let* ((VAR (make-var)) 
					    (ef (in-context Effect `(set! ,VAR ,(Value val0))))) 
				    `(begin ,ef (alloc ,VAR))))]
		  [(mref ,val0 ,val1) (cond 
				      ((and (simple? val0) (simple? val1)) `(mref ,val0 ,val1))
				      ((simple? val0) 
					   (let* ((VAR1 (make-var)) 
	      					  (ef1 (in-context Effect `(set! ,VAR1 ,(Value val1))))
						  (ef* (list ef1)))
					     `(begin ,ef* ... (mref ,val0 ,VAR1))))
				      ((simple? val1) 
					   (let* ((VAR0 (make-var)) 
	      					  (ef0 (in-context Effect `(set! ,VAR0 ,(Value val0))))
						  (ef* (list ef0)))
					     `(begin ,ef* ... (mref ,VAR0 ,val1))))		      
				      (else
				       (let* ((VAR0 (make-var)) 
					      (VAR1 (make-var)) 
					      (ef0 (in-context Effect `(set! ,VAR0 ,(Value val0)))) 
					      (ef1 (in-context Effect `(set! ,VAR1 ,(Value val1))))
					      (ef* (list ef0 ef1)))
					 `(begin ,ef* ... (mref ,VAR0 ,VAR1)))))]
		  [else (error who "mistake in Value")])

	   (Triv : Triv (x) -> Triv () 
		 [,i `,i]
		 [,l `,l]
		 [,uv `,uv])))





