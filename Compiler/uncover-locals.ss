(library (Compiler uncover-locals)
	 (export uncover-locals)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 ;; Midterm TODO: Complete this pass definition.

	 (define-pass uncover-locals : LverifyScheme (x) -> LuncoverLocals ()
	   ;; Use a mutable accumulator strategy here.  Don't forget to initialize it to the empty list:
	   (definitions 
	     (define locals-accum '())
	     )
	   (Prog : Prog (x) -> Prog ()
		 [(letrec ([,l* ,[le*]] ...) ,tl) (begin (set! locals-accum '()) 
							 (let ((tl (Tail tl)))
							   `(letrec ([,l* ,le*] ...) (locals (,locals-accum ...) ,tl))))])

	   (Value : Value (x) -> Value ()
		  [(let ([,uv* ,[val*]] ...) ,[val])
		   (begin
		     (set! locals-accum (append locals-accum uv*))
		     `(let ([,uv* ,val*] ...) ,val))])
	   (Effect : Effect (x) -> Effect ()
		   [(let ([,uv* ,[val*]] ...) ,[ef])
		    (begin
		      (set! locals-accum (append uv* locals-accum))
		      `(let ([,uv* ,val*] ...) ,ef))])
	   
	   (Pred : Pred (x) -> Pred ()
		 [(let ([,uv* ,[val*]] ...) ,[pred])
		  (begin
		    (set! locals-accum (append uv* locals-accum))
		    `(let ([,uv* ,val*] ...) ,pred))])

	   (Tail : Tail (x) -> Tail () 
		 [(let ([,uv* ,[val*]] ...) ,[tl])
		  (begin
		    (set! locals-accum (append uv* locals-accum))
		    `(let ([,uv* ,val*] ...) ,tl))])

	   (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
		       ;; We're giving away a little boilerplate code here to speed things up!:
		       [(lambda (,uv* ...) ,tl)
			(begin
			  (set! locals-accum '())
			  (let ([tl (Tail tl)])
			    `(lambda (,uv* ...) 
			       (locals (,locals-accum ...) ,tl))))])
	   ))
