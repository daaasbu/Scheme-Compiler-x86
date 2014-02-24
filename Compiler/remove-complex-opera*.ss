(library (Compiler remove-complex-opera*)
	 (export remove-complex-opera* parse-LremoveComplexOpera*)
	 (import 
	  (chezscheme)
	  (source-grammar)
	  (Framework nanopass)
	  (Framework helpers))

	 (define-parser parse-LremoveComplexOpera* LremoveComplexOpera*)

	 (define-pass remove-complex-opera* : LverifyScheme (x) -> LremoveComplexOpera* ()
	   (definitions
	     (define VAR-ls '())

	     (define make-var
	       (lambda ()
		 (let ((v (unique-name 'VAR)))
		       (begin (set! VAR-ls (cons v VAR-ls)) #;(display VAR-ls) v))))
	     #|
	     (define traverse*
	       (lambda (prcr ls*)
		 (map (lambda (x) (prcr x)) ls*)))
|#
	     (define simple?
	       (lambda (exp)
		 (or (integer? exp) (uvar? exp))))



	     (define Value*
	       (lambda (val*)
		 (with-output-language (LremoveComplexOpera* Effect)
		 (if (null? val*) '()
		     (let* ((first (Value (car val*)))
			    (rest (cdr val*)) 
			    (VAR (make-var))
			    (set `(set! ,VAR , first)))
		      (begin  (set! local-var-ls (cons (with-output-language (LremoveComplexOpera* Triv) VAR) local-var-ls)) (cons set (Value* rest))))))))
		      
 
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
    
	     )
#|
	   (Prog : Prog (x) -> Prog ()
		 [(letrec ([,l* ,[le*]] ...) (locals (,uv* ...) (call ,val ,val* ...))) (begin (display "LOOP-Prog") `(letrec ([,l* ,le*] ... ) (locals (,uv* ...) (call ,val ,val* ...))))])


	   (LambdaExpr : LambdaExpr(x) -> LambdaExpr ()
		       [(lambda (,uv* ...) ,[bd]) (begin (display "LOOP-LE") `(lambda (,uv* ...) ,bd))])
|#
           (Body : Body (x) -> Body ()
                 [(locals (,uv* ...) ,[tl]) `(locals (,(append uv* VAR-ls) ...) ,tl)])

           (Tail : Tail (x) -> Tail ()
	    
		 [,triv `,triv]
		 [(prim ,op ,[val0] ,[val1]) (cond
					      [(and (simple? val1) (simple? val0)) `(prim ,op ,val0 ,val1)]
					      [(simple? val0)  (let ((VAR (make-var)))
								 `(begin (set! ,VAR ,val1) (prim ,op ,val0 ,VAR)))]
					      [(simple? val1)  (let ((VAR (make-var)))
								 `(begin (set! ,VAR ,val0) (prim ,op ,VAR ,val1)))]
					      
					      [else (let ((VAR0 (make-var)) (VAR1 (make-var))) `(begin (set! ,VAR0 ,val0) (set! ,VAR1 ,val1) (prim ,op ,VAR0 ,VAR1)))])]
		 
		 [(call ,[val] ,val* ...) (begin (initialize) (if (not (member #f (simple-ls val*))) 
								  `(call ,val ,(map Value val*) ...) 
								  (let* ((output (Value* val*)))
								
								   `(begin ,output ... (call ,val ,(reverse local-var-ls) ...)))))]  ;changed
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
		    [(nop) `(nop)])

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
		  [else (error who "blah")])

	   (Triv : Triv (x) -> Triv () 
		 [,i `,i]
		 [,l `,l]
		 [,uv `,uv])))
