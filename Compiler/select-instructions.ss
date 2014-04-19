
(library (Compiler select-instructions)
         (export select-instructions)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))


;;This pass goes from LANF to LANF, and its main purpose is to modify our code to insure that it fits the machine constraints of x86.
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
		       (combine (list store1)))
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
			      [store2 (BINOP2 UNSP binop UNSP triv2)]
			      [store3 (with-output-language (LassignNewFrame Effect) `(set! ,var ,UNSP))]
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
					      (store2  (BINOP2 UNSP binop UNSP triv2))
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

		  [(and (eqv? '* binop) (and (frame-var? var) (or (uvar/reg? triv2) (frame-var? triv2) (int32? triv2) (int64/label? triv2))))  		
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
		 `(begin ,combine ... ,store2))))) 
	     
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
 		  [(and (int32? triv1) (or (uvar/reg? triv2) (frame-var? triv2))) 
		   (X4 relop triv1 triv2)]
		  [(or (and (frame-var? triv1) (frame-var? triv2)) (and (int32? triv1) (int32? triv2)) (and (int64/label? triv1) (or (uvar/reg? triv2) (frame-var? triv2) (int32? triv2))))
		   (X5 (new-UNSP) relop triv1 triv2)] 
		  [(or (and (uvar/reg? triv1) (int64/label? triv2)) (and (frame-var? triv1) (int64/label? triv2)) (and (int32? triv1) (int64/label? triv2))) 
		   (X6 (new-UNSP) relop triv1 triv2)]
		  [(and (int64/label? triv1) (int64/label? triv2)) 
		   (X7 (new-UNSP) (new-UNSP) relop triv1 triv2)]
		  [else (RELOP1-h relop triv1 triv2)])))
	    
	   (define RELOP1-h
	     (with-output-language (LassignNewFrame Pred)
	       (lambda (relop triv1 triv2)
		 `(,relop ,triv1 ,triv2))))
;;Interesting. If triv0 or triv1 is an integer, then you must assign it to a register. I wonder if it because we are doing
;;make-disp or make-index wrong somewhere else down the line. 
	   (define MREF
	     (lambda (v triv0 triv1)
	       (cond
		((and (or (uvar? v) (register? v)) (or (uvar? triv0) (register? triv0)) (or  (uvar? triv1) (register? triv1)))
		 (with-output-language (LassignNewFrame Effect) `(set! ,v (mref ,triv0 ,triv1))))
		((and (or (uvar? v) (register? v)) (or (uvar? triv0) (register? triv0)))
		 (let*
		     ((UNSP1 (new-UNSP))
		      	(ef1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP1 ,triv1)))
			(ef* (list ef1))
			(ef (with-output-language (LassignNewFrame Effect) `(set! ,v (mref ,triv0 ,UNSP1)))))
		   (with-output-language (LassignNewFrame Effect) `(begin ,ef* ... ,ef))))
		((and (or (uvar? v) (register? v)) (or (uvar? triv1) (register? triv1)))
		 (let*
		     ((UNSP0 (new-UNSP))
		      	(ef0 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP0 ,triv0)))
			(ef* (list ef0))
			(ef (with-output-language (LassignNewFrame Effect) `(set! ,v (mref ,UNSP0 ,triv1)))))
		   (with-output-language (LassignNewFrame Effect) `(begin ,ef* ... ,ef))))
		 
		 
	       ;MAYBE COULD HAVE MORE TESTS AND MORE PRECISE SITUATIONS, but idk.
		;possibly inefficient if there are more situations when you dont need to spill.
		(else
		 (let* ((UNSP0 (new-UNSP))
			(UNSP1 (new-UNSP))
			(UNSP2 (new-UNSP))
			(UNSP3 (new-UNSP))
			(ef0 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP0 ,triv0)))
			(ef1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP1 ,triv1)))
			(ef2 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP2 (mref ,UNSP0 ,UNSP1))))
			(ef3 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP3 ,UNSP2)))
			(ef (with-output-language (LassignNewFrame Effect) `(set! ,v ,UNSP3)))
			(ef* (list ef0 ef1 ef2 ef3)))
		   (with-output-language (LassignNewFrame Effect) `(begin ,ef* ... ,ef)))))))


	  ;IF YOU EVER EVER HAVE AN ERROR IN SELECT INSTRS. SOMEWHERE DOWN THE ROAD, SWITCH OUT THE COMMENTED LINES, YOU DID THIS BAS
	  ;ON TRIAL AND ERROR, SO THERE MAY HAVE JUST NOT BEEN A TEST CASE WHEN THIS WAS RELEVANT.
	  ;IF THAT DID NOT WORK, THEN COMMENT OUT EVERYTHING BUT ELSE LINE. SHOULD ENSURE ALL PASSES BUT VERY INEFFCIENT.
	   (define MSET!
	     (lambda (triv1 triv2 triv3)
	       (cond
		((and (or (uvar? triv1) (register? triv1)) (or (int32? triv2) (uvar? triv2) (register? triv2)) (or (int32? triv3) (uvar? triv3) (register? triv3)))
		 (with-output-language (LassignNewFrame Effect) `(mset! ,triv1 ,triv2 ,triv3)))
		((and (or (uvar? triv1) (register? triv1)) (or (int32? triv3) (uvar? triv3) (register? triv3)))
		 (with-output-language (LassignNewFrame Effect) `(mset! ,triv1 ,triv2 ,triv3)))

		((and (or (uvar? triv1) (register? triv1)) (or (int32? triv2) (uvar? triv2) (register? triv2)))
		 (let* ((UNSP3 (new-UNSP))
			(ef3 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP3 ,triv3)))
			(ef* (list ef3))
			(ef (with-output-language (LassignNewFrame Effect) `(mset! ,triv1 ,triv2 ,UNSP3))))
		   (with-output-language (LassignNewFrame Effect) `(begin ,ef* ... ,ef))))

		((and (or (int32? triv2) (uvar? triv2) (register? triv2))  (or (int32? triv3) (uvar? triv3) (register? triv3)))
		 (let* ((UNSP1 (new-UNSP))
			(ef1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP1 ,triv1)))
			(ef* (list ef1))
			(ef (with-output-language (LassignNewFrame Effect) `(mset! ,UNSP1 ,triv2 ,triv3))))
		   (with-output-language (LassignNewFrame Effect) `(begin ,ef* ... ,ef))))
		((or (uvar? triv1) (register? triv1))
		 (let* ((UNSP2 (new-UNSP))
			(UNSP3 (new-UNSP))
			(ef2 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP2 ,triv2)))
			(ef3 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP3 ,triv3)))
			(ef (with-output-language (LassignNewFrame Effect) `(mset! ,triv1 ,UNSP2 ,triv3)))
			(ef* (list ef2))
			(ef* (list ef2 ef3)))
		   (with-output-language (LassignNewFrame Effect) `(begin ,ef* ... ,ef))))
		((or (int32? triv2) (uvar? triv2) (register? triv2))
		 (let* ((UNSP1 (new-UNSP))
			(UNSP3 (new-UNSP))
			(ef1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP1 ,triv1)))
			(ef3 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP3 ,triv3)))
			(ef (with-output-language (LassignNewFrame Effect) `(mset! ,UNSP1 ,triv2 ,UNSP3)))
			(ef* (list ef1 ef3)))
		   (with-output-language (LassignNewFrame Effect) `(begin ,ef* ... ,ef))))
		((or (int32? triv3) (uvar? triv3) (register? triv3))
		 (let* ((UNSP1 (new-UNSP))
			(UNSP2 (new-UNSP))
			(ef1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP1 ,triv1)))
			(ef2 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP2 ,triv2)))
			(ef (with-output-language (LassignNewFrame Effect) `(mset! ,UNSP1 ,UNSP2 ,triv3)))
			(ef* (list ef1 ef2)))
		   (with-output-language (LassignNewFrame Effect) `(begin ,ef* ... ,ef))))		
	       (else 
		(let* ((UNSP1 (new-UNSP))
		       (UNSP2 (new-UNSP))
		       (UNSP3 (new-UNSP))
		       (UNSP4 (new-UNSP))
		       (ef1 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP1 ,triv1)))
		       (ef2 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP2 ,triv2)))
		       (ef3 (with-output-language (LassignNewFrame Effect) `(set! ,UNSP3 ,triv3)))
		       (ef4 (with-output-language (LassignNewFrame Effect) `(mset! ,UNSP1 ,UNSP2 ,UNSP3)))
		       (ef* (list ef1 ef2 ef3)))
		 (with-output-language (LassignNewFrame Effect) `(begin ,ef* ... ,ef4))))))))
		     
	 

	   
	      
	     
	   
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
		 [(if ,[pred] ,[tl1] ,[tl2]) `(if ,pred ,tl1 ,tl2) ]

		 [(alloc ,triv0) `(alloc ,triv0)] 
		 [(mref ,triv0 ,triv1) `(mref ,triv0 ,triv1)]) 

	   
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

		   [(set! ,v (mref ,triv0 ,triv1)) (MREF v triv0 triv1)] 
		   [(mset! ,triv0 ,triv1 ,triv2)  (MSET! triv0 triv1 triv2)]

		   [(if ,[pred] ,[ef1] ,[ef2]) `(if ,pred ,ef1 ,ef2) ]
		   [(begin ,ef* ... ,[ef1]) `(begin ,(map (lambda (x) (Effect x)) ef*) ... ,ef1)]
		   [(return-point ,l ,[tl]) `(return-point ,l ,tl)]
		   [(nop) `(nop)]
		  
		   )
	   )
)
	 