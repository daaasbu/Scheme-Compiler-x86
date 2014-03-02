(library (Compiler impose-calling-conventions)
         (export impose-calling-conventions parse-LimposeCallingConventions)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LimposeCallingConventions LimposeCallingConventions)

	 (define-pass impose-calling-conventions : LflattenSet! (x) -> LimposeCallingConventions ()
	   (definitions

	     (define Frame* '())

	     (define make-rp
	       (lambda ()
		 (unique-name 'RP)))

	     (define make-nfv
	       (lambda ()
		 (unique-name 'NFV)))
	     
	     (define make-rp-label
	       (lambda ()
		 (unique-label 'RPL)))

	     

	     (define make-effect-block1
	       (with-output-language (LimposeCallingConventions Effect)
				     (lambda (uv* pr*)
				       (define make-block-helper
					 (lambda (uv* pr* acc input* nfv*)
					   (cond
					    [(null? uv*) (values acc input* (reverse nfv*))]
					    [(null? pr*) (let ((nfv (make-nfv)) (first (car uv*))) (make-block-helper (cdr uv*) pr* (cons `(set! ,nfv ,first) acc) (cons nfv input*) (cons nfv nfv*)))]
					    [else (make-block-helper (cdr uv*) (cdr pr*) (cons `(set! ,(car pr*) ,(car uv*)) acc) (cons (car pr*) input*) nfv*)])))
				       (make-block-helper uv* pr* '() '() '()))))
	   

	     

	     (define make-set!-block
	       (with-output-language (LimposeCallingConventions Effect)
				     (lambda (uv* pr* n rp)
				       (cond
					[(null? uv*) '()]
					[(null? pr*) (let ((fv (index->frame-var n)) (first (car uv*))) (cons `(set! ,first ,fv) (make-set!-block (cdr uv*) pr* (add1 n) rp)))]
					[else (cons `(set! ,(car uv*) ,(car pr*)) (make-set!-block (cdr uv*) (cdr pr*) n rp))]))))

	     (define make-call-block
	       (with-output-language (LimposeCallingConventions Effect)
				     (lambda (uv* pr* n rp)
				       (define make-call-block-helper
					 (lambda (uv* pr* n rp acc input*)
					   (cond
					    [(null? uv*) (values (reverse acc) input*)]
					    [(null? pr*) (let ((fv (index->frame-var n)) (first (car uv*))) (make-call-block-helper (cdr uv*) pr* (add1 n) rp (cons `(set! ,fv ,first) acc) (cons fv input*)))]
					    [else  (make-call-block-helper (cdr uv*) (cdr pr*) n rp (cons `(set! ,(car pr*) ,(car uv*)) acc) (cons (car pr*) input*))])))
				       (make-call-block-helper uv* pr* n rp '() '()))))
	     )



	   (Prog : Prog (x) -> Prog ()
		 [(letrec ([,l* ,le*] ...) (locals (,uv* ...) ,tl)) (let ((rp (make-rp))
									  (le* (map (lambda (x) (LambdaExpr x (make-rp))) le*)))
								      (begin (set! Frame* '())
									     (let ((tl (Tail tl rp)))
									  `(letrec ([,l* ,le*] ...) (locals (,uv* ... ,rp) (new-frames ((,Frame* ...) ...) (begin (set! ,rp ,return-address-register) ,tl)))))))])


	   (LambdaExpr : LambdaExpr (x rp) -> LambdaExpr ()
		       [(lambda (,uv0* ...) (locals (,uv1* ...) ,tl))
			(begin (set! Frame* '()) (let* ((tl (Tail tl rp)) (Fr* (apply append Frame*))) `(lambda () (locals (,rp ,uv0* ... ,uv1* ... ,Fr* ...) (new-frames ((,Frame* ...) ...) (begin ,(cons `(set! ,rp ,return-address-register) (make-set!-block uv0* parameter-registers 0 rp)) ... ,tl))))))])


	   (Tail : Tail (x rp) -> Tail ()
		 [,triv `(begin (set! ,return-value-register ,triv) (,rp ,frame-pointer-register ,return-value-register)) ]
		  [(call ,triv0 ,triv* ...)(let* ((set (with-output-language (LimposeCallingConventions Effect) `(set! ,return-address-register ,rp)))
						  (ef (list set)))
					    (let-values (((ef* input*) (make-call-block triv* parameter-registers 0 rp))) 
					    `(begin ,(append (reverse ef*)  ef)  ... (,triv0 ,return-address-register ,frame-pointer-register ,input* ...))))]
		 [(prim ,op ,triv0 ,triv1) `(begin (set! ,return-value-register (,op ,triv0 ,triv1)) (,rp ,frame-pointer-register ,return-value-register))]
		 [(if ,[pred0] ,tl0 ,tl1) `(if ,pred0 ,(Tail tl0 rp) ,(Tail tl1 rp))]
		 [(begin ,[ef*] ... ,tl) `(begin ,ef* ... ,(Tail tl rp))])

	   (Pred : Pred (x) -> Pred ()
		 [(true) `(true)]
		 [(false) `(false)]
		 [(,relop ,triv0 ,triv1) `(,relop ,triv0 ,triv1)]
		 [(if ,[pred0] ,[pred1] ,[pred2]) `(if ,pred0 ,pred1 ,pred2)]
		 [(begin ,[ef*] ... ,[pred]) `(begin ,ef* ... ,pred)])

	   (Effect : Effect (x) -> Effect ()
		   [(set! ,uv ,rhs)  (Rhs rhs uv)]
;		   [(set! ,uv (,op ,triv0 ,triv1))  `(set ,uv (,op ,triv0 ,triv1))]
		   #;[(set! ,uv (call ,triv ,triv* ...)) 
		      (let-values (((ef-first input* nfv*) (make-effect-block1 triv* parameter-registers)))
			(let* ((RPL (make-rp-label))
			       (return `(set! ,return-address-register ,RPL))
			       (calls (in-context Tail `(,triv ,frame-pointer-register ,return-address-register ,input* ...)))
			       (ef* (append ef-first (list return)))
			       (tl (in-context Tail `(begin ,ef* ... ,calls)))
			       (block  `(return-point ,RPL ,tl))
			       (ef `(set! ,uv ,return-value-register)))
		
			  (begin (set! Frame* (cons nfv* Frame*)) 
				 `(begin ,block ,ef))))]
				
				 
		   [(call ,triv ,triv* ...) (let-values (((ef* input* nfv*) (make-effect-block1 triv* parameter-registers)))
					     (begin (set! Frame* (cons nfv* Frame*)) (let ((RPL (make-rp-label))) `(return-point ,RPL (begin ,(append ef* (list `(set! ,return-address-register ,RPL))) ... (,triv ,frame-pointer-register ,return-address-register ,input* ...))))))]
		   [(if ,[pred] ,[ef0] ,[ef1]) `(if ,pred ,ef0 ,ef1)]
		   [(begin ,[ef*] ... ,[ef]) `(begin ,ef* ... ,ef)]
		   [(nop) `(nop)]
		   [else (error who "FUCK")])

	   (Rhs : Rhs (x uv) -> * ()
		[,triv (in-context Effect `(set! ,uv ,triv))]
		[(,op ,triv0 ,triv1) (in-context Effect `(set! ,uv (,op ,triv0 ,triv1)))]
		[(call ,triv ,triv* ...) (let-values (((ef-first input* nfv*) (make-effect-block1 triv* parameter-registers)))
					   (let* ((RPL (make-rp-label))
						  (return (in-context Effect `(set! ,return-address-register ,RPL)))
						  (calls (in-context Tail `(,triv ,frame-pointer-register ,return-address-register ,input* ...)))
						  (ef* (append ef-first (list return)))
						  (tl (in-context Tail `(begin ,ef* ... ,calls)))
						  (block  (in-context Effect `(return-point ,RPL ,tl)))
						  (ef (in-context Effect `(set! ,uv ,return-value-register))))
					     (begin (set! Frame* (cons nfv* Frame*))
						    (in-context Effect `(begin ,block ,ef)))))]
		[else (error who "FUCK")])

	  

	   

		   )
	   )
