;;verify-scheme, takes our subset of scheme consisting of mainly letrecs, effects,registers,frame-vars, labels and lambda expressions, and goes through a series of tests that targets a certain machine.These tests could change depending on the target of our compiler.  Goes from LverifyScheme to LverifyScheme.
;;
(library (Compiler verify-uil)
         (export verify-uil)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         
	 
	 ;;Simply checks if input values follow our grammar and language restrictions.
         (define-pass verify-uil : LremoveLet (x) -> LremoveLet ()
	   (definitions
	     
	     ;;pred to determine a binop
	     (define binop?
	       (lambda (x)
		 (if (memq x '(+ - * logand logor sra)) #t #f)))

	     ;;simple lookup function to see if a variable is stored in our env.
	     (define lookup
	       (lambda (x env)
		 (member x env)))

	     ;;recurses over env to see if there are any duplicate labels in it.
	     (define duplicate-labels?
	       (lambda (env)
		 (cond
		  [(null? env) #t]
		  [(member (car env) (cdr env)) #f]
		  [else (duplicate-labels? (cdr env))])))

	     ;;checks to see if there are unique suffixes.
	     (define suffix-list
	       (lambda (ls)
		 (duplicate-labels? (map extract-suffix ls))))

	     (define label-ls '())



	     )

           (Prog : Prog (x) -> Prog ()
                 [(letrec ([,l* ,[le*]] ...) ,bd)
		  (set! label-ls (append l* label-ls))
		  ;;applies suffix-list to our list of labels to check for duplicates
                  (unless (suffix-list l*) (error who "Duplicate Labels"))
		  `(letrec ([,l* ,(map (lambda (x) (LambdaExpr x l*)) le*)] ...) ,(Body bd l*))])
           (LambdaExpr : LambdaExpr (x env) -> LambdaExpr ()
                       [(lambda (,uv* ...) ,bd)  `(lambda (,uv* ...) ,(Body bd (append uv* env)))])
           (Body : Body (x env) -> Body ()
                 [(locals (,uv* ...) ,tl)
		  ;;checks for duplicate suffixes in uvar list
                  (unless (suffix-list uv*) (error who "Duplicate uvar's"))
                  `(locals (,uv* ...) ,(Tail tl (append uv* env)))])
           (Tail : Tail (x env ) -> Tail ()
                 [(begin ,ef* ... ,tl1)   `(begin ,(map (lambda (x) (Effect x env )) ef*) ... ,(Tail tl1 env ))]
                 [,triv (Triv triv env) x]
                 [(if ,pred ,tl1 ,tl2) (Pred pred env ) (Tail tl1 env ) (Tail tl2 env ) x]
		 [(call ,val ,val* ...) (Value val env) (map (lambda (x) (Value x env)) val*) x]
		 [(prim ,op ,val0 ,val1) x])
           (Pred : Pred (x env ) -> Pred ()
                 [(true) x]
                 [(false) x]
                 [(prim ,relop ,val0 ,val1) (Value val0 env) (Value val1 env)
                  x]
                 [(if ,pred1 ,pred2 ,pred3) (Pred pred1 env) (Pred pred2 env) (Pred pred3 env) x]
                 [(begin ,ef* ... ,pred) `(begin ,(map (lambda (x) (Effect x env )) ef*) ... ,(Pred pred env ))])
           (Effect : Effect (x env ) -> Effect ()
                   [(set! ,uv ,val)
                    (if (uvar? uv)  (unless (lookup uv env) (error who "not in env")))
		    (Value val env) x]
		   [(if ,pred0 ,ef0 ,ef1) (Pred pred0 env) (Effect ef0 env) (Effect ef1 env)  x]
		   [(begin ,ef* ... ,ef) (map (lambda (x) (Effect x env)) ef*) (Effect ef env)  x]
		   [(nop) x]
		   [(call ,[val] ,[val*] ...) `(call ,val ,val* ...)]) 
	   (Value : Value (x env) -> Value ()
		  [,triv (Triv triv env) x]
		  [(call ,[val] ,[val*] ...) `(call ,val ,val* ...)]
		  [(if ,pred ,val0 ,val1) (Pred pred env) (Value val0 env) (Value val1 env) x]
		  [(prim ,op ,val0 ,val1)
		   (if  (eqv? op `sra);;checks sra machine constraint impose from x86 arch.
                        (unless (and  (integer? val1) (<= 0 val1) (>= 63 val1))
                                (error who "sra out of bounds/not a number" val1))) (Value val0 env) (Value val1 env) x]
		  [(begin ,ef* ... ,val) (map (lambda (x) (Effect x env)) ef*) (Value val env) x])
	   (Triv : Triv (x env) ->  Triv ()
		 [,uv (unless (lookup uv env) (error who "uvar not in env")) x]
		 [,i i]
		 [,l (unless (lookup l label-ls) (error who "label not in env")) x])))


