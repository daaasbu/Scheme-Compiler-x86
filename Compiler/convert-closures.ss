(library (Compiler convert-closures)
	 (export convert-closures)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass convert-closures : LuncoverFree (x) -> LconvertClosures ()
	   (definitions

	     (define make-cp
	       (lambda ()
		 (unique-name 'CP)))

	     ;;MAKE AS MANY CP'S AS THE NUMBER PASSED IN AND RETURNS THEM IN A LIST
	     (define mult-cp
	       (lambda (n)
		 (cond
		  ((zero? n) '())
		  (else (cons (make-cp) (mult-cp (sub1 n)))))))

	     ;;FLATTEN'S AN ARBITRARILY NESTED LIST
	     (define flatten
	       (lambda (ls)
		 (cond
		  ((null? ls) '())
		  ((list? (car ls)) (append (flatten (car ls)) (flatten (cdr ls))))
		  (else (cons (car ls) (flatten (cdr ls))))))))

	   (LambdaExpr : LambdaExpr (x) -> * (expr uv**)
		       [(lambda (,uv* ...) (free (,uv** ...) ,expr))
			(let ((cp (make-cp)))
			  (values (in-context LambdaExpr `(lambda (,cp ,uv* ...) (bind-free (,cp ,uv** ...) ,(Expr expr)))) uv**))]
		       [else (error who "FUCK NANOPASS AND IT'S STUPID FUCKING QUARKS!!!")])

	   (Expr : Expr (x) -> Expr ()
		 [(letrec ((,uv* ,le*) ...) ,expr)

		  (let* ((a-list (map (lambda (x) (let-values (((le free*) (LambdaExpr x)))
							      (list le free*))) le*))
			 (le* (map car a-list))
			 (uv** (map cadr a-list))
			 (l* (map unique-label uv*)))
		    ;(printf "le*: ~a \n uv**: ~a \n" le* uv**)
		    `(letrec ((,l* ,le*) ...)
		       (closures ((,uv* ,l* ,uv** ...) ...) ,(Expr expr))))]

		 [(call ,uv ,[expr*] ...) `(call ,uv ,uv ,expr* ...)]

		 [(call ,[expr] ,[expr*] ...) (let ((tmp (unique-name 'TMP)))
						`(let ((,tmp ,expr))
						   (call ,tmp ,tmp ,expr* ...)))])))