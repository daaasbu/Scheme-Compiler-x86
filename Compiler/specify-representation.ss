(library (Compiler specify-representation)
	 (export specify-representation)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass specify-representation : LnormalizeContext (x) -> LspecifyRepresentation ()

	   (Value2 : Value (x) -> Value ()
		   [(,vprim ,[val*] ...) 
		    (let ((first (if (null? val*) '() (car val*))))
		      (case vprim
			((void) $void)
			((+) (let ((second (cadr val*))) `(prim + ,first ,second)))
			((-) (let ((second (cadr val*))) `(prim - ,first ,second)))
			((*) (let ((second (cadr val*)))
			       (cond
				[(and (integer? first) (integer? second)) `(prim * ,(sra second shift-fixnum) ,first)]
				[(integer? first) `(prim * ,second ,(sra first shift-fixnum))]
				[(integer? second) `(prim * ,first ,(sra second shift-fixnum))]
				[else `(prim * ,first (prim sra ,second ,shift-fixnum))]))) 
			((car) `(mref ,first ,(- disp-car tag-pair)))
			((cdr) `(mref ,first ,(- disp-cdr tag-pair)))
			((cons) (let ([x (unique-name 'TMP)]
				      [y (unique-name 'TMP)]
				      [z (unique-name 'TMP)]
				      [second (cadr val*)])
				  `(let ([,x ,first] [,y ,second])
				     (let ([,z (prim + (alloc ,size-pair) ,tag-pair)])
				       (begin
					 (mset! ,z ,(- disp-car tag-pair) ,x)
					 (mset! ,z ,(- disp-cdr tag-pair) ,y)
					 ,z)))))
			((make-vector) (cond
					((integer? first) (let ([tmp (unique-name 'TMP)])
							    `(let ([,tmp (prim + (alloc ,(+ disp-vector-data first)) ,tag-vector)])
							       (begin
								 (mset! ,tmp ,(- disp-vector-length tag-vector) ,first)
								 ,tmp))))
					(else (let ([tmp1 (unique-name 'TMP)]
						    [tmp2 (unique-name 'TMP)]) 
						`(let ([,tmp1 ,first])
						   (let ([,tmp2 (prim + (alloc (prim + ,disp-vector-data ,tmp1)) ,tag-vector)])
						     (begin
						       (mset! ,tmp2 ,(- disp-vector-length tag-vector) ,tmp1)
						       ,tmp2)))))))
			((vector-length) `(mref ,first ,(- disp-vector-length tag-vector)))
			((vector-ref) (let ((second (cadr val*))) 
					(cond
					 ((integer? second)  `(mref ,first ,(+ (- disp-vector-data tag-vector) second)))
					 (else  `(mref ,first (prim + ,(- disp-vector-data tag-vector) ,second))))))
			
			((make-procedure)   (let ([tmp (unique-name 'TMP)])
	;				      (printf "disp-proc: ~a \n cadr: ~a \n" disp-procedure-data (cadr val*))
					      `(let ([,tmp (prim + (alloc ,(* disp-procedure-data (add1 (cadr val*)))) ,tag-procedure)])
						 (begin
						   (mset! ,tmp ,(- disp-procedure-code tag-procedure) ,first)
						   ,tmp))))
			
			
		       ((procedure-ref) (let ((second (cadr val*)))
					  `(mref ,first ,(+ (- disp-procedure-data tag-procedure) second))))
		       ((procedure-code) `(mref ,first ,(- disp-procedure-code tag-procedure)))
			))]
		   [(quote ,i) (case i
				 ((#t) $true)
				 ((#f) $false)
				 ((()) $nil)
				 ((void) $void)
				 (else (ash i shift-fixnum)))]
		   [,l (in-context Value l)]
		   [,uv (in-context Value uv)]
		   [(if ,[pred] ,val0 ,val1) `(if ,pred ,(Value2 val0) ,(Value2 val1))]
		   [(begin ,[ef*] ... ,val) `(begin ,ef* ... ,(Value2 val))]
		   [(let ([,uv* ,val*] ...) ,val) `(let ([,uv* ,(map Value2 val*)] ...) ,(Value2 val))]
		   [(call ,val ,val* ...) `(call ,(Value2 val) ,(map Value2 val*) ...)])
	   
					;val->tail processor to transform val into tail
					;effect-prim? (set-car! set-cdr! vector-set!)
					;value-prim? (+ - * car cdr cons make-vector vector-length vector-ref void)
					;pred-prim? (< <= = >= > boolean? eq? fixnum? null? pair? vector?)
	   (Value : Value (x) -> Tail ()
		  [(,vprim ,[val*] ...) 
		   (let ((first (if (null? val*) '() (car val*))))
		     (case vprim
		       ((void) $void)
		       ((+) (let ((second (cadr val*))) `(prim + ,first ,second)))
		       ((-) (let ((second (cadr val*))) `(prim - ,first ,second)))
		       ((*)
			(let ((second (cadr val*)))
			  (cond
			   [(and (integer? first) (integer? second)) `(prim * ,(sra second shift-fixnum) ,first)]
			   [(integer? first) `(prim * ,second ,(sra first shift-fixnum))]
			   [(integer? second) `(prim * ,first ,(sra second shift-fixnum))]
			   [else `(prim * ,first (prim sra ,second ,shift-fixnum))])))
		       ((car) `(mref ,first ,(- disp-car tag-pair)))
		       ((cdr) `(mref ,first ,(- disp-cdr tag-pair)))
		       ((cons) (let ([x (unique-name 'TMP)]
				     [y (unique-name 'TMP)]
				     [z (unique-name 'TMP)]
				     [second (cadr val*)])
				 `(let ([,x ,first] [,y ,second])
				    (let ([,z (prim + (alloc ,size-pair) ,tag-pair)])
				      (begin
					(mset! ,z ,(- disp-car tag-pair) ,x)
					(mset! ,z ,(- disp-cdr tag-pair) ,y)
					,z)))))
		       ((make-vector) (cond
				       ((integer? first) (let ([tmp (unique-name 'TMP)])
							   `(let ([,tmp (prim + (alloc ,(+ disp-vector-data first)) ,tag-vector)])
							      (begin
								(mset! ,tmp ,(- disp-vector-length tag-vector) ,first)
								,tmp))))
				       (else (let ([tmp1 (unique-name 'TMP)]
						   [tmp2 (unique-name 'TMP)]) 
					       `(let ([,tmp1 ,first])
						  (let ([,tmp2 (prim + (alloc (prim + ,disp-vector-data ,tmp1)) ,tag-vector)])
						    (begin
						      (mset! ,tmp2 ,(- disp-vector-length tag-vector) ,tmp1)
						      ,tmp2)))))))
		       ((vector-length) `(mref ,first ,(- disp-vector-length tag-vector)))
		       ((vector-ref) (let ((second (cadr val*)))
				       (cond
					((integer? second) `(mref ,first ,(+ (- disp-vector-data tag-vector) second)))
					(else `(mref ,first (prim + ,(- disp-vector-data tag-vector) ,second))))))		
		       ((make-procedure)   (let ([tmp (unique-name 'TMP)])
					     `(let ([,tmp (prim + (alloc ,(* disp-procedure-data (add1 (cadr val*)))) ,tag-procedure)])
						(begin
						  (mset! ,tmp ,(- disp-procedure-code tag-procedure) ,first)
						  ,tmp))))

		       ((procedure-ref) (let ((second (cadr val*)))
					  `(mref ,first ,(+ (- disp-procedure-data tag-procedure) second))))

		       ((procedure-code) `(mref ,first ,(- disp-procedure-code tag-procedure)))
		       
		       ))]

		  [(quote ,i) (case i
				((#t) $true)
				((#f) $false)
				((()) $nil)
				((void) $void)
				(else (ash i shift-fixnum)))]
		  [,l `,l]
		  [,uv `,uv]
		  [(if ,[pred] ,[val0 -> tl0] ,[val1 -> tl1]) `(if ,pred ,tl0 ,tl1)]
		  [(begin ,[ef*] ... ,[val -> tl]) `(begin ,ef* ... ,tl)]
		  [(let ([,uv* ,[val* -> val*]] ...) ,[val -> tl]) `(let ([,uv* ,val*] ...) ,tl)]
		  [(call ,val ,val* ...) `(call ,(Value2 val) ,(map Value2 val*) ...)])
	   
	   (Effect : Effect (x) -> Effect ()
		   [(,eprim ,val* ...) (case eprim
					 ((procedure-set!) (let ([first (Value2 (car val*))]
								 [second (Value2 (cadr val*))]
								 [third (Value2 (caddr val*))])
							     `(mset! ,first ,(+ (- disp-procedure-data tag-procedure) second) ,third)))  ;;change here
							 
					 ((set-car!) `(mset! ,(Value2 (car val*)) ,(- disp-car tag-pair) ,(Value2 (cadr val*))))
					 ((set-cdr!) `(mset! ,(Value2 (car val*)) ,(- disp-cdr tag-pair) ,(Value2 (cadr val*))))
					 ((vector-set!) (let ([first (Value2 (car val*))]
							      [second (Value2 (cadr val*))]
							      [third (Value2 (caddr val*))])
							  (cond
							   ((integer? second) 
							    `(mset! ,first ,(+ (- disp-vector-data tag-vector) second) ,third))
							   (else 
							    `(mset! ,first (prim + ,(- disp-vector-data tag-vector) ,second) ,third)))))
					 )]
		   [(nop) `(nop)]
		   [(if ,[pred] ,[ef1] ,[ef2]) `(if ,pred ,ef1 ,ef2)]
		   [(begin ,[ef*] ... ,[ef]) `(begin ,ef* ... ,ef)]
		   [(let ([,uv* ,val*] ...) ,[ef]) `(let ([,uv* ,(map Value2 val*)] ...) ,ef)]
		   [(call ,val ,val* ...) `(call ,(Value2 val) ,(map Value2 val*) ...)])
	   
	   (Pred : Pred (x) -> Pred ()
		 [(,pprim ,[val*] ...) (case pprim
					 ((= < <= > >=) `(prim ,pprim ,(car val*) ,(cadr val*)))
					 ((null?) `(prim = ,(car val*) ,$nil))
					 ((boolean?) `(prim = (prim logand ,(car val*) ,mask-boolean) ,tag-boolean))
					 ((fixnum?) `(prim = (prim logand ,(car val*) ,mask-fixnum) ,tag-fixnum))
					 ((vector?) `(prim = (prim logand ,(car val*) ,mask-vector) ,tag-vector))
					 ((pair?) `(prim = (prim logand ,(car val*) ,mask-pair) ,tag-pair))
					 ((eq?) `(prim = ,(car val*) ,(cadr val*)))
					 ((procedure?) `(prim = (prim logand ,(car val*) ,mask-procedure) ,tag-procedure)))])))
