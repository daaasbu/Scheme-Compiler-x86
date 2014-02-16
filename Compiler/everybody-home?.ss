(define-pass everybody-home? : LassignRegisters (x) -> * ()
  (home? : Body (x) -> * ()
	 [(locals (,uv* ...) ,ubd) #f]
	 [(locate ([,uv* ,locrf*] ...) ,tl) #t])
  (Prog : Prog (x) -> * ()
	[(letrec ([,l* (lambda () ,bd*)] ...) ,bd)
	 (andmap home? (cons bd bd*))]
	[else (error who "Invalid program shape ~s" x)]))