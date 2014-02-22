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
		       (begin (set! VAR-ls (cons v VAR-ls)) v))))
	     
	     (define traverse*
	       (lambda (prcr ls*)
		 (map (lambda (x) (prcr x)) ls*)))

	     (define simple?
	       (lambda (exp)
		 (or (integer? exp) (uvar? exp))))



				
	     
	     )

	   (Prog : Prog (x) -> Prog ()
		 [(letrec ([,l* ,[le*]] ...) (locals (,uv* ...) (call ,val ,val* ...))) `(letrec ([,l* ,le*] ... ) (locals (,uv* ...) (call ,val ,val* ...)))])

           (Body : Body (x) -> Body ()
                 [(locals (,uv* ...) ,[tl]) (display "locals: ") (display (append VAR-ls uv*)) (newline) `(locals (,(append VAR-ls uv*) ...) ,tl)])
           (Tail : Tail (x) -> Tail ()
		 [,triv `,triv]
		 [(prim ,op ,[val0] ,[val1]) `(prim ,op ,val0 ,val1) ]
		 [(call ,[val] ,[val*] ...) (display "variables: ") (display (cons val VAR-ls)) (newline)  `(call ,val ,VAR-ls ...)]
		 [(if ,[pred] ,[tl1] ,[tl2]) `(if ,pred ,tl1 ,tl2)]
		 [(begin ,ef* ... ,[tl1]) `(begin ,(traverse* Effect ef*) ... ,tl1)])
           (Pred : Pred (x) -> Pred ()
                 [(true) `(true)]
                 [(false) `(true)]
                 [(prim ,relop ,[val0] ,[val1]) `(,relop ,val0 ,val1) ]
                 [(if ,[pred1] ,[pred2] ,[pred3]) `(if ,pred1 ,pred2 ,pred3) ]
                 [(begin ,ef* ... ,[pred]) `(begin ,(traverse* Effect ef*) ,pred)])
           (Effect : Effect (x) -> Effect ()
                   [(set! ,uv ,[val]) `(set! ,uv ,val)] 
		   [(if ,[pred0] ,[ef0] ,[ef1]) `(if ,pred0 ,ef0 ,ef1)]
		   [(begin ,ef* ... ,[ef]) `(begin ,(traverse* Effect ef*) ... ,ef)]
		    [(nop) `(nop)])
	   (Value : Value (x) -> * ()
		  [,triv `,triv]
		  [(if ,[pred] ,[val0] ,[val1]) (in-context Effect `(begin (set! ,(make-var) (if ,pred ,val0 ,val1))))]
		  [(prim ,op ,val0 ,val1)
		   (cond 
		    [(and (simple? val1) (simple? val0)) `(prim ,op ,val0 ,val1)]
		    [(simple? val0) (let ((VAR (make-var))
					  (flat (Value val1)))
				     (in-context Value `(begin (set! ,VAR ,flat) ... (prim ,op ,val0 ,VAR))))]
		    [(simple? val1) (let ((VAR (make-var))
					  (flat (Value val0)))
				      (in-context Value `(begin (set! ,VAR ,flat) ... (prim ,op ,VAR ,val1))))]

		    [else (let ((VAR0 (make-var))
				(VAR1 (make-var))
				(flat0 (Value val0))
				(flat1 (Value val1)))
				     (in-context Value `(begin  (set! ,VAR0 ,flat0) (set! ,VAR1 ,flat1) (prim ,op ,VAR0 ,VAR1))))])]

		  [(begin ,ef* ... ,[val]) (in-context Value `(begin ,(traverse* Effect ef*) ... ,val))])))
