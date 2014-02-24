9(library (Compiler impose-calling-conventions)
         (export impose-calling-conventions parse-LimposeCallingConventions)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LimposeCallingConventions LimposeCallingConventions)
; Helpers:
; parameter-registers, frame-pointer-register, return-value-register, and return-address-register
         (define-pass impose-calling-conventions : LflattenSet! (x) -> LimposeCallingConventions ()
	   (definitions

#|	     
	     (define rp (void))

	     (define rp2 (void))

	     (define make-rp2
	       (lambda ()
		 (begin (set! rp2 (unique-name 'RP)) rp2)))

	     (define make-rp
	       (lambda ()
		 (begin (set! rp (unique-name 'RP)) rp)))
|#

	     (define make-rp
	       (lambda ()
		 (unique-name 'RP)))

	     (define make-set!-block
	       (lambda (uv* pr* n rp)
		 (cond
		  [(null? uv*) '()]
		  [(null? pr*) (let ((fv (index->frame-var n)) (first (car uv*))) (cons (with-output-language (LimposeCallingConventions Effect) `(set! ,first ,fv)) (make-set!-block (cdr uv*) pr* (add1 n) rp)))]
		  [else (cons (with-output-language (LimposeCallingConventions Effect) `(set! ,(car uv*) ,(car pr*))) (make-set!-block (cdr uv*) (cdr pr*) n rp))])))

	     (define input* '())

	     (define make-call-block
	       (lambda (triv* pr* n triv*-len pr-len rp)
		 (let ((fv-num (- triv*-len pr-len)))
		   (cond
		    [(null? triv*) (list (with-output-language (LimposeCallingConventions Effect) `(set! ,return-address-register ,rp)))]
		    [(and (> fv-num 0) (< n fv-num))  (begin (set! input* (cons (index->frame-var n) input*)) (cons (with-output-language (LimposeCallingConventions Effect) `(set! ,(index->frame-var n) ,(car triv*))) (make-call-block (cdr triv*) pr* (add1 n) triv*-len pr-len rp)))]
		    [else  (begin (set! input* (cons (car pr*) input*)) (cons (with-output-language (LimposeCallingConventions Effect) `(set! ,(car pr*) ,(car triv*))) (make-call-block (cdr triv*) (cdr pr*) n triv*-len pr-len rp)))]))))
      
		 

	     )
	  (Prog : Prog (x) -> Prog ()
		[(letrec ([,l* ,le*] ...) (locals (,uv* ...) ,tl)) (let ((rp (make-rp))) (begin  `(letrec ([,l* ,(map (lambda (x) (LambdaExpr x (make-rp))) le*)] ...) (locals (,uv* ... ,rp) (begin (set! ,rp ,return-address-register) ,(Tail tl rp))))))])


	  (LambdaExpr : LambdaExpr (x rp) -> LambdaExpr ()
		      [(lambda (,uv0* ...) (locals (,uv1* ...) ,tl)) 
		       `(lambda () (locals (,uv1* ... ,rp ,uv0* ...) (begin ,(cons `(set! ,rp ,return-address-register) (make-set!-block uv0* parameter-registers 0 rp)) ... ,(Tail tl rp))))])


	  (Tail : Tail (x rp) -> Tail ()
		[,triv  `(begin (set! ,return-value-register ,triv) (,rp ,frame-pointer-register ,return-value-register)) ]
		[(call ,triv0 ,triv* ...)  `(begin ,(make-call-block triv* parameter-registers 0 (length triv*) (length parameter-registers) rp) ... (,triv0 ,return-address-register ,frame-pointer-register ,input* ...))]
		[(prim ,op ,[triv0] ,[triv1]) `(begin (set! ,return-value-register (,op ,triv0 ,triv1)) (,rp ,frame-pointer-register ,return-value-register)) ]
		[(if ,[pred0] ,tl0 ,tl1) `(if ,pred0 ,(Tail tl0 rp) ,(Tail tl1 rp))]
		[(begin ,[ef*] ... ,tl) `(begin ,ef* ... ,(Tail tl rp))])

	  (Effect : Effect (x) -> Effect ()

		  [(set! ,uv ,[rhs]) `(set! ,uv ,rhs) ]


) 
))


	 

