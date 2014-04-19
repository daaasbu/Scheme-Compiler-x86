(library (Compiler convert-complex-datum)
	 (export convert-complex-datum)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))
	 (define-parser parse-LverifyScheme LverifyScheme)

	 (define-pass convert-complex-datum : LverifyScheme (x) -> LconvertComplexDatum ()
	   (definitions

	     (define vector?^
	       (lambda (d)
		 (vector? `,d)))

	     (define list?^
	       (lambda (ls)
		 (and (not (number? ls)) (not (null? `(,ls))) (list? `(,ls)))))

 (define deconstruct-list
   (lambda (d)
     (define help
       (lambda (ls)
	 (cond
	  [(immediate? ls) (with-output-language (LconvertComplexDatum Expr) `(quote ,ls))]
	  [(null? ls) (with-output-language (LconvertComplexDatum Expr) `(quote ()))]
	  [(vector? (car ls)) (let* ((tmp-new (unique-name 'TMP))
				     (expr (deconstruct-vector (car ls) tmp-new))
				     (expr* (list (with-output-language (LconvertComplexDatum Expr) `,(help (cdr ls)))))
				     (expr* (cons expr expr*)))
				(with-output-language (LconvertComplexDatum Expr) `(cons ,expr* ...)))]
	  [(pair? (car ls)) (let* ((expr (with-output-language (LconvertComplexDatum Expr) `,(help (car ls))))
				   (expr* (list (with-output-language (LconvertComplexDatum Expr) `,(help (cdr ls)))))
				   (expr* (cons expr expr*)))
			      (with-output-language (LconvertComplexDatum Expr) `(cons ,expr* ...)))]
	  [else (let* ((expr (with-output-language (LconvertComplexDatum Expr) `(quote ,(car ls))))
		       (expr* (list (with-output-language (LconvertComplexDatum Expr) `,(help (cdr ls)))))
		       (expr* (cons expr expr*)))
		    (with-output-language (LconvertComplexDatum Expr) `(cons ,expr* ...)))])))
     (help `,d)))

 (define deconstruct-vector
   (lambda (d tmp)
     (define help
       (lambda (vec)
	 (let ((size (vector-length vec)))
	   (let loop ((i 0))
	     (cond
	      [(= i size) '()]
	      [else (let ((value (vector-ref vec i)))
		      (cond
		       [(pair? value) (let* ((ind (with-output-language (LconvertComplexDatum Expr) `(quote ,i)))
					     (deconstruct (deconstruct-list value))
					     (expr* (with-output-language (LconvertComplexDatum Expr) (list tmp ind deconstruct)))
					     (expr (with-output-language (LconvertComplexDatum Expr) `(vector-set! ,expr* ...))))
					(cons expr (loop (add1 i))))]
		       [(vector? value) (let* ((tmp-new (unique-name 'TMP))
					       (ind (with-output-language (LconvertComplexDatum Expr) `(quote ,i)))
					       (deconstruct (deconstruct-vector value tmp-new))
					       (expr* (with-output-language (LconvertComplexDatum Expr) (list tmp ind deconstruct)))
					       (expr (with-output-language (LconvertComplexDatum Expr) `(vector-set! ,expr* ...)))
					       (expr (with-output-language (LconvertComplexDatum Expr) `(vector-set! ,expr* ...))))
					  (cons expr (loop (add1 i))))]
		       [else (let* ((imm (with-output-language (LconvertComplexDatum Expr) `(quote ,value)))
				    (ind (with-output-language (LconvertComplexDatum Expr) `(quote ,i)))
				    (expr* (with-output-language (LconvertComplexDatum Expr) (list tmp ind imm)))
				    (expr (with-output-language (LconvertComplexDatum Expr) `(vector-set! ,expr* ...))))
					      (cons expr (loop (add1 i))))]))])))))
     (let* ((expr1* (help `,d))
	    (vec `,d)
	    (expr0* (list (with-output-language (LconvertComplexDatum Expr) `(quote ,(vector-length vec)))))
	    (expr0 (with-output-language (LconvertComplexDatum Expr) `(make-vector ,expr0* ...))))
	   (with-output-language (LconvertComplexDatum Expr) `(let ([,tmp ,expr0])
								(begin ,expr1* ... ,tmp))))))

	     )

	   (Expr : Expr (x) -> Expr ()
		 [(quote ,d) (guard (immediate? d))  (let ((i d)) `(quote ,i))]
		 [(quote ,d) (guard (vector?^ d))  (let ((tmp (unique-name 'TMP))) `,(deconstruct-vector d tmp))]
		 [(quote ,d) (let ((ls  `,(deconstruct-list d))) ls)]
	
		  ) 
		 
							     
)
)