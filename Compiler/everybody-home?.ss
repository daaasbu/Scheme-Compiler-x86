
(library (Compiler everybody-home?)
         (export everybody-home? #;parse-LverifyScheme)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))



(define-pass everybody-home? : LassignRegisters (x) -> * ()
  (home? : Body (x) -> * ()
	 [(locals (,uv* ...) ,ubd) #f]
	 [(locate ([,uv* ,locrf*] ...) ,tl) #t])
  (Prog : Prog (x) -> * ()
	[(letrec ([,l* (lambda () ,bd*)] ...) ,bd)
	 (andmap home? (cons bd bd*))]
	[else (error who "EBH?")])))