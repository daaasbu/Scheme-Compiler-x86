(library (Compiler specify-representation)
	 (export specify-representation)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 ;; Midterm TODO: Complete this pass definition.

	 (define-pass specify-representation : LverifyScheme (x) -> LspecifyRepresentation ()

	   (Prog: Prog (x) -> Prog ()
		  [(letrec ([,l* ,le*] ...) ,val) `(letrec ([,l* ,le*] ...) ,(Val->Tail val))])

	   (Val->Tail: Value (x) -> Tail ()
		       [])

	   (Value : Value (x) -> Value ()
		 [(,vprim ,[val*] ...) (printf "vprim: ~a \n" vprim) (let ((first (car val*))
						      (second (cadr val*)))
					 (case vprim
					   ((+)  `(prim + ,first ,second))
					   ((-) `(prim + ,first ,second))
					   ((*) 
						(cond
						 [(or (and (integer? first) (integer? second))
						       (integer? first)
						       (integer? second))
						   `(prim * ,first ,second)]
						 [else `(prim * ,first ,(sra second shift-fixnum))]))))]
					      
		 [(quote ,i)(display "do I get here") (case i
			       ((#t) $true)
			       ((#f) $false)
			       ((()) $nil)
			       ((void) $void)
			       (else (ash i shift-fixnum)))])
	   
	   (Effect : Effect (x) -> Effect ()
		  #; [(,eprim ,[val*] ...) ])
		 
	   (Pred : Pred (x) -> Pred ()
		 [(,pprim ,[val*] ...) (case pprim
					 ((= < <= > >=) `(prim ,pprim ,(car val*) ,(cadr val*)))
					 ((null?) `(prim = ,(car val*) ,$nil))
					 ((boolean?) `(prim = (prim logand ,(car val*) ,mask-boolean) ,tag-boolean))
					 ((fixnum?) `(prim = (prim logand ,(car val*) ,mask-fixnum) ,tag-fixnum))
					 ((vector?) `(prim = (prim logand ,(car val*) ,mask-vector) ,tag-vector))
					 ((pair?) `(prim = (prim logand ,(car val*) ,mask-pair) ,tag-pair))
					 ((eq?) `(prim = ,(car val*) ,(cadr val*))))])
	   
		 ))


		  
