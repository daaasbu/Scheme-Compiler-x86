(library (Compiler optimize-direct-call)
	 (export optimize-direct-call)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 (define-pass optimize-direct-call : LverifyScheme (orig) -> LverifyScheme ()


	   (Expr : Expr (x) -> Expr ()

		 [(call (lambda (,uv* ...) ,[expr]) ,[expr*] ...) `(let ([,uv* ,expr*] ...) ,expr)])

	   (Expr orig)))