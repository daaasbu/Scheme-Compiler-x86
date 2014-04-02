 
(library (Compiler optimize-jumps)
         (export optimize-jumps)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

	 (define-parser parse-LexposeBasicBlocks LexposeBasicBlocks)

         (define-pass optimize-jumps : LexposeBasicBlocks (x) -> LexposeBasicBlocks ()
	   (definitions
	       (define walk-symbol
		 (lambda (x s)
		   (letrec ((walk
			     (lambda (y t)
			       (cond
				[(null? t) y]
				[(pair? t) (let ([check1 (caar t)] [check2 (cdar t)])
					     (if (eqv? y check1)
						 (if (or (register? check2) (frame-var? check2)) check2 (walk check2 s))
						 (walk y (cdr t))))]))))
		     (walk x s))))


	     (define alist '()))
           
	   (Prog1 : Prog (x) -> Prog ()
	      [(letrec ([,l* ,le*] ...) ,tl) 
	       (map (lambda (le pair) (LambdaExpr le pair)) le* l*) 
	        (Prog2 x alist)])

	   (Prog2 : Prog (x alist) -> Prog ()
		  [(letrec ([,l* ,le*] ...) ,tl) (let* ((le* (map (lambda (le) (LambdaExpr2 le alist)) le*))
							(block* (map (lambda (l le) (if (null? le) '() (cons l le))) l* le*))
							(block* (remq '() block*))
							(l* (map car block*))
							(le* (map cdr block*)))
						   `(letrec ([,l* ,le*] ...) ,(Tail tl alist)))])

   
	   (Tail : Tail (x alist) -> Tail ()
		 [(if (,relop ,triv0 ,triv1) (,l0) (,l1)) (let* ((c-label (walk-symbol l0 alist))
								 (a-label (walk-symbol l1 alist)))
							    `(if (,relop ,triv0 ,triv1) (,c-label) (,a-label)))])
	   
	   (Effect : Effect (x alist) -> Effect ())
	   
	   (Loc : Loc (x alist) -> Loc ())

	   (LambdaExpr2 : LambdaExpr (x alist) -> LambdaExpr ()
			[(lambda () (,l)) `()])


	   (Triv : Triv (x alist) -> Triv ()
		 [,l  (walk-symbol l alist)])

	   (LambdaExpr : LambdaExpr (x pair) -> LambdaExpr ()
	      [(lambda () (,l)) (set! alist (cons (cons pair l) alist))])))