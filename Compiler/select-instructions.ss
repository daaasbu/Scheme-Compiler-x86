;;verify-scheme, takes our subset of scheme consisting of mainly letrecs, effects,registers,frame-vars, labels and lambda expressions, and goes through a series of tests that targets a certain machine.These tests could change depending on the target of our compiler.  Goes from LverifyScheme to LverifyScheme.
;;
(library (Compiler select-instructions)
         (export select-instructions #;parse-LassignNewFrame)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

	 ;(define-parser parse-LassignNewFrame LassignNewFrame)
	 
         (define-pass select-instructions : LassignNewFrame (x) -> LassignNewFrame ()


	   (definitions

	     (define unspillable?
	       (lambda (u)
		 (equal? (extract-root u) "UNSP")))

	     (define UNSP* '())

	     (define commutative?
	       (lambda (binop)
		 (memq binop '(+ * logand logor))))

	     (define BINOP2?
	       (lambda (binop)
		 (memq binop '(- + logor logand))))

	     (define int64/label?
	       (lambda (triv)
		 (or (label? triv) (and (not (int32? triv)) (int64? triv)))))

	     (define uvar/reg?
	       (lambda (triv)
		 (or (uvar? triv) (register? triv))))
	     
	     (define update-UNSP*!

	       (lambda (UNSP)

		 (set! UNSP* (cons UNSP UNSP*))))

	     (define new-UNSP
	       (lambda ()
		 (let* ([u (unique-name 'UNSP)])
		   (begin (update-UNSP*! u) u))))
	     
	     (define flip-relop
	       (lambda (relop)
		 (cond
		  [(eqv? relop '<) '>]
		  [(eqv? relop '>) '<]
		  [(eqv? relop '<=) '>=]
		  [(eqv? relop '>=) '<=]
		  [else '=])))

	     (define converter
	       (with-output-language (LassignNewFrame Effect)
				     (lambda (ef)
				       `,ef)))

	     
	     ;;Takes a variable, a binop, a unspillable, and a triv, and outputs the form (begin (set! u Triv) (set! Var (binop Var u)))   
	     (define X1
	       (with-output-language (LassignNewFrame Effect)
	       (lambda (var binop UNSP triv2)
		 (let* ((store1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP ,triv2)))
		       (store2 (with-output-language (LassignNewFrame Effect) `(set! ,var (,binop ,var ,UNSP))))
		       (combine (list store1))) #;maybeheretoo
	        `(begin ,combine ... ,store2)))))

	     


	     (define MOVE
	       (with-output-language (LassignNewFrame Effect)
	       (lambda (var triv)
		 (cond
		  [(and (frame-var? var) (or (frame-var? triv) (int64/label? triv))) 
		   (let* ([UNSP (new-UNSP)]
			  [store1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP ,triv))]
			  [store2 (with-output-language (LassignNewFrame Effect) `(set! ,var ,UNSP))])
		     `(begin ,(list store1) ... ,store2))]
		  [else `(set! ,var ,triv)]))))
	     
	     (define BINOP1
	       (lambda (var binop triv1 triv2)
		 (cond
		  [(eqv? var triv1) (BINOP2 var binop triv1 triv2)]
		  [(and (commutative? binop) (eqv? var triv2)) (BINOP2 var binop triv2 triv1)]
		  [else (let* ([UNSP (new-UNSP)]
			      [store1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP ,triv1))]
			      [store2 #;(with-output-language (LassignNewFrame Effect) `(set! ,UNSP (,binop ,UNSP ,triv2))) (BINOP2 UNSP binop UNSP triv2)]
			      #;possiblechange	      [store3 (with-output-language (LassignNewFrame Effect) `(set! ,var ,UNSP))]
			      [combine (list store1 store2)])
			  (BINOP1-h combine store3))])))
	 
	     (define BINOP1-h
	        (with-output-language (LassignNewFrame Effect)
				      (lambda (combine store3)
					`(begin ,combine ... ,store3))))


	     (define X2
	       (with-output-language (LassignNewFrame Effect)
				     (lambda (var binop UNSP triv2)
				       (let* ((store1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP ,var)))
					      (store2 #;(with-output-language (LassignNewFrame Effect) `(set! ,UNSP (,binop ,UNSP ,triv2))) (BINOP2 UNSP binop UNSP triv2))
					      (store3 (with-output-language (LassignNewFrame Effect) `(set! ,var ,UNSP)))
					      (combine (list store1 store2))) #;heretoo
 					 `(begin ,combine ... ,store3)))))

	     ;;BINOP2 in (set! Var (binop Triv1 Triv2))		     
	     (define BINOP2
	       (lambda (var binop triv1 triv2)
		 (cond
		  [(and (BINOP2? binop) 
			(or (and (uvar/reg? var) (int64/label? triv2)) 
			    (and (frame-var? var) (frame-var? triv2)) 
			    (and (frame-var? var) (int64/label? triv2))))
		   
		       (let* ([UNSP (new-UNSP)]) 
			 (X1 var binop UNSP triv2))]
		  
		  [(and (eqv? '* binop) (and (uvar/reg? var) (int64/label? triv2)))
		  
		       (let* ([UNSP (new-UNSP)]) 
			 (X1 var binop UNSP triv2))]

		  [(and (eqv? '* binop) (and (frame-var? var) (or (uvar/reg? triv2) (frame-var? triv2) (int32? triv2) (int64/label? triv2))))  ;;added more checks
		
		       (let* ([UNSP (new-UNSP)])    
			 (X2 var binop UNSP triv2))]
		  [else 
		   (BINOP2-h var binop triv1 triv2)])))

	     (define BINOP2-h
	       (with-output-language (LassignNewFrame Effect)
				     (lambda (var binop triv1 triv2)
				       `(set! ,var (,binop ,triv1 ,triv2)))))

	     (define X4
	       (with-output-language (LassignNewFrame Pred)
				     (lambda (relop triv1 triv2)
				       `(,(flip-relop relop) ,triv2 ,triv1))))
	     
	     (define X5
	       (with-output-language (LassignNewFrame Pred)
	       (lambda (UNSP relop triv1 triv2)
		 (let* ((store1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP ,triv1)))
		       (store2 (with-output-language (LassignNewFrame Pred) `(,relop ,UNSP ,triv2)))
		       (combine (list store1)))
		 `(begin ,combine ... ,store2))))) #;maybeheretoo
	     
	     (define X6
	       (with-output-language (LassignNewFrame Pred)
	       (lambda (UNSP relop triv1 triv2)
		 (let* ((store1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP ,triv2)))
			(store2 (with-output-language (LassignNewFrame Pred) `(,(flip-relop relop) ,UNSP ,triv1))))
		 `(begin ,(list store1) ... ,store2)))))

	     
	     (define X7
	       (with-output-language (LassignNewFrame Pred)
	       (lambda (UNSP1 UNSP2 relop triv1 triv2)
		 (let* ((store1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP1 ,triv1)))
			(store2 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP2 ,triv2)))
			(store3 (with-output-language (LassignNewFrame Pred) `(,relop ,UNSP1 ,UNSP2)))
			(combine (list store1 store2)))
		 `(begin ,combine ... ,store3)))))
	     
	     (define RELOP1
	       (lambda (relop triv1 triv2)
		 (cond
 #;X4		  [(and (int32? triv1) (or (uvar/reg? triv2) (frame-var? triv2))) 
		   (X4 relop triv1 triv2)];good on tests, and X6
#;X5              [(or (and (frame-var? triv1) (frame-var? triv2)) (and (int32? triv1) (int32? triv2)) (and (int64/label? triv1) (or (uvar/reg? triv2) (frame-var? triv2) (int32? triv2))))
		   (X5 (new-UNSP) relop triv1 triv2)] ;good on tests, and X5
#;X6              [(or (and (uvar/reg? triv1) (int64/label? triv2)) (and (frame-var? triv1) (int64/label? triv2)) (and (int32? triv1) (int64/label? triv2))) 
		   (X6 (new-UNSP) relop triv1 triv2)];good on tests,
#;X7              [(and (int64/label? triv1) (int64/label? triv2)) 
		   (X7 (new-UNSP) (new-UNSP) relop triv1 triv2)]
		  [else (RELOP1-h relop triv1 triv2)])))
	    
	   (define RELOP1-h
	     (with-output-language (LassignNewFrame Pred)
	       (lambda (relop triv1 triv2)
		 `(,relop ,triv1 ,triv2)))))
	     
	   
	   (Body : Body (x) -> Body ()
		 [(locals (,uv1* ...) 
			  (ulocals (,uv2* ...) 
				   (locate ([,uv3* ,locrf*] ...)
					   (frame-conflict ,cfgraph ,tl))))
		  (begin (set! UNSP* '())
			 (let ((tl (Tail tl)))
		  `(locals (,uv1* ...)
			   (ulocals (,(append UNSP* uv2*) ...)
				    (locate ([,uv3* ,locrf*] ...)
					    (frame-conflict ,cfgraph ,tl))))))]

		 [(locate ((,uv** ,locrf*) ...) ,[tl]) `(locate ((,uv** ,locrf*) ...) ,tl)])
	   
	   (Tail : Tail (x) -> Tail ()
		 [(begin ,ef* ... ,[tl1])  `(begin ,(map Effect ef*) ... ,tl1)]
		 [(,triv ,locrf* ...) `(,triv ,locrf* ...) ]
		 [(if ,[pred] ,[tl1] ,[tl2]) `(if ,pred ,tl1 ,tl2) ])

	   
	   (Pred : Pred (x) -> Pred ()
		 [(true) `(true)]
		 [(false) `(false)]
                 [(,relop ,triv1 ,triv2) (RELOP1 relop triv1 triv2)]
                 [(if ,[pred1] ,[pred2] ,[pred3]) `(if ,pred1 ,pred2 ,pred3) ]
                 [(begin ,ef* ... ,[pred]) `(begin ,(map (lambda (x) (Effect x)) ef*) ... ,pred)]
		 )

	   (Effect : Effect (x) -> Effect ()
		   [(set! ,v ,triv)  (MOVE v triv)]
		   [(set! ,v (,op ,triv1 ,triv2))   (BINOP1 v op triv1 triv2) ]
		   [(if ,[pred] ,[ef1] ,[ef2]) `(if ,pred ,ef1 ,ef2) ]
		   [(begin ,ef* ... ,[ef1]) `(begin ,(map (lambda (x) (Effect x)) ef*) ... ,ef1)]
		   [(return-point ,l ,[tl]) `(return-point ,l ,tl)]
		   [(nop) `(nop)]
		   [else (error who "blah")]
		   )
	   )
)
	 