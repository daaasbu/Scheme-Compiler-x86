
(library (Compiler uncover-register-conflict)
         (export uncover-register-conflict parse-LuncoverRegisterConflict)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LuncoverRegisterConflict LuncoverRegisterConflict)

         (define-pass uncover-register-conflict : LselectInstructions (x) -> LuncoverRegisterConflict ()
           (definitions

             (define init-conflict-table
               (lambda (ls)
                 (map list ls)))

             (define conflict-table '())
             (define live-ls '())
             (define blah '())

             (define update-table
               (lambda (var cf* cf-ls)
                 (let* ([found (assq var cf-ls)]
                        [rest (cdr found)]
                        [conflicts (union cf* rest)])
                   
                   (begin
                     (set-cdr! found conflicts)
                    
                     (for-each (lambda (v)
                                 
                                 (if (uvar? v)
                                     (let* ([check (list var)]
                                            [found1 (assq v cf-ls)]
                                            [rest1 (cdr found1)]
                                            [conflicts1 (union check rest1)])
                                       (set-cdr! found1 conflicts1))))
                                 
                                 
                                 cf*) cf-ls))))


            #| (define update-table-reg
                 (lambda (reg cf* c-table)
                 (let* ([found (assq var cf-ls)]
                        [rest (cdr found)]
                        [conflicts (set-cons reg rest)])
                   (for-each (lambda (v)
                               (if (uvar? v)
              |#
             (define update-table-reg
               (lambda (reg live* c-table)
                 (map (lambda (x) (if (member (car x) live*)
                                      (cons (car x) (set-cons reg (cdr x)))
                                      x)) c-table)))
                   
             (define remove-frame-var
               (lambda (ls)
                 (cond
                   ((null? ls) '())
                   ((or (label? (car ls)) (frame-var? (car ls))) (remove-frame-var (cdr ls)))
                   (else (set-cons (car ls) (remove-frame-var (cdr ls)))))))

             (define map*
               (lambda (proc ls)
                 (cond
                   ((null? ls) '())
                   (else (cons (proc (car ls)) (map* proc (cdr ls)))))))

             (define Ef*
               (lambda (ef*)
                 (reverse (map* Effect (reverse ef*)))))


             )

           (Prog : Prog (x) -> Prog ()
                 [(letrec ([,l* ,[le*]] ...) ,[bd]) `(letrec ([,l* ,le*] ...) ,bd)]
                 [else (error who "something went wrong - Prog")])
           (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
                       [(lambda () ,[bd]) `(lambda () ,bd)]
                       [else (error who "something went wrong - LambdaExpr")])
	   
			 (Body : Body (x) -> Body ()
			       [(locals (,uv1* ...) 
						(ulocals (,uv2* ...) 
							 (locate ([,uv3* ,locrf*] ...)
								 (frame-conflict ,cfgraph ,tl))))
				(begin
                                              (set! conflict-table (init-conflict-table uv*))
                                              (let ([a (Tail tl)])
				`(locals (,uv1* ...)
					 (ulocals (,uv1* ... )
						  (locate ([,uv3* ,locrf*] ...)
							  (frame-conflict ,cfgraph
									  (register-conflict ,conflict-table ,a)))))))]

			       [(locate ((,uv** ,locrf*) ...) ,tl) `(locate ((,uv** ,locrf*) ...) ,tl)])

           (Body : Body (x) -> Body ()
                 [(locals (,uv* ...) ,tl) (begin
                                              (set! conflict-table (init-conflict-table uv*))
                                              (let ([a (Tail tl)])
                                               ;(newline) (display "conflict table: ") (display conflict-table) (newline)
                                                
                                              `(locals (,uv* ...) (register-conflict ,conflict-table ,a))))]
                 [else (error who "something went wrong - Body")])
           (Tail : Tail (x) -> Tail ()
                 [(,triv ,locrf* ...) (begin
                                        
                                        (set! live-ls (remove-frame-var (set-cons triv locrf*)))
                                        
                                        (in-context Tail `(,triv ,locrf* ...)))]
                 [(begin ,ef* ... ,tl) (begin
                                         (let* ([a (Tail tl)]
                                                [b (Ef* ef*)])
                                         `(begin ,b ... ,a)))]
                 [(if ,pred ,tl0 ,tl1) (begin
                                         (let* ([a (Tail tl1)]
                                                [b (Tail tl0)]
                                                [c (Pred pred)])
                                         `(if ,c ,b ,a)))]
                 [else (error who "something went wrong - Tail")])
           (Pred : Pred (x) -> Pred ()
                 [(true) `(true)]
                 [(false) `(false)]
                 [(,relop ,triv0 ,triv1) (begin
                                           (if (or (register? triv0) (uvar? triv0))
                                               (set! live-ls (set-cons triv0 live-ls)))
                                           (if (or (register? triv1) (uvar? triv1))
                                               (set! live-ls (set-cons triv1 live-ls)))
                                           `(,relop ,triv0 ,triv1))]
                 [(if ,pred0 ,pred1 ,pred2) (begin
                                              (let* ([a (Pred pred2)]
                                                     [b (Pred pred1)]
                                                     [c (Pred pred0)])
                                              `(if ,c ,b ,a)))]
                 [(begin ,ef* ... ,pred) (begin
                                           (let* ([a (Pred pred)]
                                                  [b (Ef* ef*)])
                                           `(begin ,b ... ,a)))]
                 [else (error who "something went wrong - Pred")])
           (Effect : Effect (x) -> Effect ()
                   [(nop) `(nop)]
                   [(set! ,v ,triv) (begin
                                      (set! live-ls (remove v live-ls))
                                      (cond
                                        ((and (uvar? v) (or (register? triv) (uvar? triv)))
                                         (set! conflict-table (update-table v (remove triv live-ls) conflict-table)))
                                        ((uvar? v)
                                         (set! conflict-table (update-table v live-ls conflict-table))
                                   )
                                       ((register? v) (set! conflict-table (update-table-reg v live-ls conflict-table)))
                                       )
                                      (let* ([a (Triv triv)])
                                        (if (frame-var? v) (set! live-ls (remove triv live-ls)))
                                      `(set! ,v ,a)))]
                   [(set! ,v (,op ,triv1 ,triv2)) (begin
                                                    
                                                    (set! live-ls (remv v live-ls))
                                                    (cond
                                                     ((uvar? v) (set! conflict-table (update-table v live-ls conflict-table)))
                                                     ((register? v) (set! conflict-table (update-table-reg v live-ls conflict-table))))
                                                     
                                                    (let* ([a (Triv triv2)]
                                                           [b (Triv triv1)])
                                                      (if (frame-var? v) (set! live-ls (remove triv1 (remove triv2 live-ls))))
                                                    `(set! ,v (,op ,b ,a))))]
                   [(if ,pred ,ef1 ,ef2) (begin
                                           (let* ([a (Effect ef2)]
                                                  [b (Effect ef1)]
                                                  [c (Pred pred)])
                                           `(if ,c ,b ,a)))]
                   [(begin ,ef* ... ,ef) (begin
                                           (let* ([a (Effect ef)]
                                                  [b (Ef* ef*)])
                                           `(begin ,b ... ,a)))]
                   [else (error who "something went wrong - Effect")])
           (Triv : Triv (x) -> Triv ()
                 [,v (if (or (register? v) (uvar? v)) (set! live-ls (set-cons v live-ls)))
                     `,v]
                 [,i `,i]
                 [,l `,l]))
) ;End Library










