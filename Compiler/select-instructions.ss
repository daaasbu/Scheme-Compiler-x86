;;verify-scheme, takes our subset of scheme consisting of mainly letrecs, effects,registers,frame-vars, labels and lambda expressions, and goes through a series of tests that targets a certain machine.These tests could change depending on the target of our compiler.  Goes from LverifyScheme to LverifyScheme.
;;
(library (Compiler select-instrutions)
         (export select-instructions parse-LselectInstructions)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))
	 (definitions
	   (define UNSP* '())

	   (define commutative?
	     (lambda (binop)
	       (memq binop '(+ * logand logor))))

	   (define BINOP2?
	     (lambda (binop)
	       (memq binop '(- + logor logand))))

	   (define int64/label?
	     (lambda (triv)
	       (or (label? triv) (int64? triv))))

	   (define uvar/reg?
	     (lambda (triv)
	       (or (uvar? triv) (register? triv))))
	   
	   (define update-UNSP*!
	     (lambda (UNSP)
	       (set! UNSP* (cons UNSP UNSP*))))

	   (define new-UNSP
	     (lambda ()
	       (let ([u (unique-name UNSP)])
		 (begin (update-UNSP*! u) u))))
	   
	   (define flip-relop
	     (lambda (relop)
	       [(eqv? relop '<) '>]
	       [(eqv? relop '>) '<]
	       [(eqv? relop '<=) '>=]
	       [(eqv? relop '>=) '<=]
	       [else '=]))
	   
;;Takes a variable, a binop, a unspillable, and a triv, and outputs the form (begin (set! u Triv) (set! Var (binop Var u)))   
	   (define X1
	     (lambda (var binop UNSP triv2)
	       (let ([UNSP (new-UNSP)]) `(begin (set! ,UNSP ,triv2) (set! ,var (binop ,var ,UNSP))))))
	   
	   (define X2
	     (lambda (var binop UNSP triv2)
	        (let ([UNSP (new-UNSP)]) `(begin (set! ,UNSP ,var) ,(BINOP2 ,UNSP ,binop ,UNSP ,triv2) (set! ,var ,UNSP)))))

	   (define MOVE
	     (lambda (var triv)
	       (cond
		[(and (frame-var? var) (or (frame-var? triv) (int64/label? triv))) (let ([UNSP (new-UNSP)]) `(begin (set! ,UNSP ,triv) (set! ,var UNSP)))]
		[else `(set! ,var ,triv)])))
	   
	   (define BINOP1
	     (lambda (var binop triv1 triv2)
	       (cond
		[(eqv? var triv1) (BINOP2 var binop triv1 triv2)]
		[(and (commutative? binop) (eqv? var triv2)) (BINOP2 var binop triv2 triv1)]
		[else (let ([UNSP (new-UNSP)]) `(begin (set! ,UNSP ,triv1) ,(BINOP2 ,var ,binop ,UNSP ,triv2) (set! ,var ,UNSP))))))

	     ;;BINOP2 in (set! Var (binop Triv1 Triv2))		     
	     (define BINOP2
	       (lambda (var binop triv1 triv2)
		 [(and (BINOP2? binop) 
		       (or (and (uvar/reg? var) (int64/label? triv2)) 
			   (and (frame-var? var) (frame-var? triv2)) 
			   (and (frame-var? var) (int64/label? triv2))))
		  (let ([UNSP (new-UNSP)]) (X1 var binop UNSP triv2))]
		 
		 [(eqv? '* binop) (if (and (uvar/reg? var) (int64/label? triv2)) 
				      (let ([UNSP (new-UNSP)]) (X1 var binop UNSP triv2))
				      (let ([UNSP (new-UNSP)]) (X2 var binop UNSP triv2)))]
		 [else `(set! ,var (,binop ,triv1 ,triv2))]))

	     (define X4
	       (lambda (relop triv1 triv2)
		 `(,(flip-relop relop) ,triv2 ,triv1)))
	   
	     (define X5
	       (lambda (UNSP relop triv1 triv2)
		 `(begin (set! ,UNSP ,triv1) (relop ,UNSP ,triv2))))
	     
	     (define X6
	       (lambda (UNSP relop triv1 triv2)
		 `(begin (set! ,UNSP ,triv2) (,(flip-relop relop) ,UNSP ,triv1))))

	     
	     (define X7
	       (lambda (UNSP1 UNSP2 relop triv1 triv2)
		 `(begin (set! ,UNSP1 ,triv1) (set! ,UNSP2 ,triv2) (relop ,UNSP1 ,UNSP2))))
	     
	     (define RELOP1
	       (lambda (relop triv1 triv2)
		 (cond
#;X4		  [(and (int32? triv1) (or (uvar/reg? triv2) (frame-var? triv2))) 
		   (X4 relop triv1 triv2)]
#;X5              [(or (and (frame-var? triv1) (frame-var? triv2)) (and (int32? triv1) (int32? triv2)) (and (int64/label? triv1) (or (uvar/reg? triv2) (frame-var triv2) (int32? triv2))))
		   (X5 (new-UNSP) relop triv1 triv2)]
#;X6              [(or (and (uvar/reg? triv1) (int64/label? triv2)) (and (frame-var? triv1) (int64/label? triv2)) (and (int32? triv1) (int64/label? triv2))) 
		   (X6 (new-UNSP) relop triv1 triv2)]
#;X7              [(and (int64/label? triv1) (int64/label? triv2)) (X7 (new-UNSP) (new-UNSP) relop triv1 triv2)]
		  [else `(,relop ,triv1 ,triv2) ])))
#|
	     (define X8
	       (lambda (relop triv1 triv2)
		 

	     (define RELOP2
	       (lambda (relop triv1 triv2)
		 (cond 
		  [(or (and (frame-var? triv1) (frame-var triv2)) (and (frame-var? triv1) (int64/label? triv2)) (and (uvar/reg? triv1) (int64/label? triv2))) 
		   (X8 relop triv1 triv2)]
		  [else `(,relop ,triv1 ,triv2)])))

|#		     
)  












	     )
		       (define-parser parse-verifyScheme (x) -> LverifyScheme ()
#|
			 (Prog : Prog (x) -> Prog ()
			       [(letrec ([,l* ,[le*]] ...) ,bd)
				`(letrec ([,l* ,(map (lambda (x) (LambdaExpr x l*)) le*)] ...) ,(Body bd l*))])
|#
#|			 (LambdaExpr : LambdaExpr (x env) -> LambdaExpr ()
			             [(lambda () ,bd)  `(lambda () ,(Body bd env))])
|#
			 (Body : Body (x) -> Body ()
			       [(locals (,uv1* ...) 
						(ulocals (,uv2* ...) 
							 (locate ([,uv3* ,locrf*] ...)
								 (frame-conflict ,cfgraph ,[tl])))) 
				`(locals (,uv1*)
					 (ulocals (,uv1* ... UNSF*)
						  (locate ([,uv3* ,locrf*] ...)
							  (frame-conflict ,cfgraph ,tl))))]

			       [(locate ((,uv** ,locrf*) ...) ,tl) `(locate ((,uv** ,locrf*) ...) ,tl)])
			 #|
			 (Ubody : Ubody (x) -> Ubody ()
				[(ulocals (,uv* ...) ,lbd) ])

			 (Lbody : Lbody (x) -> Lbody ()
				[(locate ((,uv* ,locrf*) ...) ,fbd)])

			 (FrameBody : FrameBody (x) -> FrameBody ()
				    [(frame-conflict ,cfgraph ,tl) ])
			 |#
#|
			 (Tail : Tail (x) -> Tail ()
			       [(begin ,ef* ... ,[tl1)   ]
			       [(,triv ,locrf* ...)]
			       [(if ,pred ,tl1 ,tl2) ]

|#
			 (Pred : Pred (x) -> Pred ()
			  ;     [(true) x]
			  ;     [(false) x]
			       [(,relop ,triv1 ,triv2) `,(RELOP1 relop triv1 triv2)]
				;[(if ,pred1 ,pred2 ,pred3) ]
				;[(begin ,ef* ... ,pred) `(begin ,(map (lambda (x) (Effect x)) ef*) ... ,(Pred pred env env2))]
			       )

			       (Effect : Effect (x env env2) -> Effect ()
				       [(set! ,v ,triv) `,(MOVE v triv)]
					[(set! ,v (,op ,triv1 ,triv2)) `,(BINOP1 v op triv1 triv2) ]
				;	[(if ,pred ,ef1 ,ef2) (Pred pred env env2) (Effect ef1 env env2) (Effect ef2 env env2) x]
				;	[(begin ,ef* ... ,ef1) `(begin ,(map (lambda (x) (Effect x)) ef*) ... ,(Effect ef1))]
				;	[(nop) x])
			       )
			 ) 