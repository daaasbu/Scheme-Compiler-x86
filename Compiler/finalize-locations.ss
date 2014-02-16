;;Finalize Locations. Goes from LverifyScheme to LfinalizeLocations. This pass removes the locate form. Every uvariable is replaced with the location it
;;refers to. Also gets rid of unncessary set!'s and replace them with (nop).



(library (Compiler finalize-locations)
         (export finalize-locations parse-LfinalizeLocations)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LfinalizeLocations LfinalizeLocations)

         (define-pass finalize-locations : LdiscardCallLive (x) -> LfinalizeLocations ()
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


             (define map*
               (lambda (proc ls)
                 (cond
		  ((null? ls) '())
		  (else (let ((cell (proc (car ls)))) (cons cell (map* proc (cdr ls))))))))

             (define Ef*
               (lambda (ef*)
                 (reverse (map* Effect (reverse ef*)))))

	    (define remove-frames
	      (lambda (env)
		(filter (lambda (x) (register? (cdr x))) env))) 
	    
	     (define reg-assocs '())
	     
	     
	


)

      #|     (Prog : Prog (x) -> Prog ()
                 [(letrec ([,l* ,le*] ...) ,bd) `(letrec ([,l* ,(map (lambda (x) (LambdaExpr x)) le*)] ...) ,(Body bd '()))])
           (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
                       [(lambda () ,bd) `(lambda () ,(Body bd '()))]) |#
           (Body : Body (x) -> Tail ()
                 [(locate ([,uv* ,r*] ...) ,tl) (Tail tl (map cons uv* r*))]
                 [else (error who "something went wrong body" x)])
           (Tail : Tail (x env) -> Tail ()
                 [(begin ,ef* ... ,tl1) `(begin ,(map (lambda (x) (Effect x env)) ef*) ... ,(Tail tl1 env))]
                 [(,triv) `(,(Triv triv env))]
                 [(if ,pred ,tl1 ,tl2) `(if ,(Pred pred env) ,(Tail tl1 env) ,(Tail tl2 env))]
                 [else (error who "something went wrong tail" x env)])
           (Pred : Pred (x env) -> Pred ()
                 [(true) `(true)]
                 [(false) `(false)]
                 [(,relop ,triv1 ,triv2) `(,relop ,(Triv triv1 env) ,(Triv triv2 env))]
                 [(if ,pred1 ,pred2 ,pred3) `(if ,(Pred pred1 env) ,(Pred pred2 env) ,(Pred pred3 env))]
                 [(begin ,ef* ... ,pred) `(begin ,(map (lambda (x) (Effect x env)) ef*) ... ,(Pred pred env))]
                 [else (error who "something went wrong pred" x env)])
           (Effect : Effect (x env) -> Effect ()
                   [(set! ,v ,triv) (if
				     (equal? (walk-symbol v env) (walk-symbol triv env))  `(nop)
				     `(set! ,(Var v env) ,(Triv triv env)))];changed here and 2 lines above
                   [(set! ,v (,op ,triv1 ,triv2)) `(set! ,(Var v env) (,op ,(Triv triv1 env) ,(Triv triv2 env)))]
                   [(if ,pred ,ef1 ,ef2) `(if ,(Pred pred env) ,(Effect ef1 env) ,(Effect ef2 env))]
                   [(begin ,ef* ... ,ef1) `(begin ,(map (lambda (x) (Effect x env)) ef*) ... ,(Effect ef1 env))]
                   [(nop) `(nop)]
                   [else (error who "something went wrong effect" x env)])
           (Triv : Triv (x env) -> Triv ()
                 [,v `,(Var v env)]
                 [,i `,i]
                 [,l `,l]
                 [else (error who "something went wrong triv" x env)])
           (Var : Var (x env) -> Loc ()
                [,uv `,(walk-symbol uv env)]
                [,locrf `,locrf]
                [else (error who "something went wrong var" x env)]))
) ;End Library