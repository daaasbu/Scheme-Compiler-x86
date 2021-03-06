
(library (Compiler uncover-frame-conflict)
         (export uncover-frame-conflict parse-LuncoverFrameConflict)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LuncoverFrameConflict LuncoverFrameConflict)

         (define-pass uncover-frame-conflict : LexposeAllocationPointer (x) -> LuncoverFrameConflict ()
           (definitions

             (define init-conflict-table
               (lambda (ls)
                 (map list ls)))

             (define conflict-table '())
             (define live-ls '())


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
             (define update-table-reg
               (lambda (reg live* c-table)
                 (map (lambda (x) (if (member (car x) live*)
                                      (cons (car x) (set-cons reg (cdr x)))
                                      x)) c-table)))
                   
             (define remove-reg
               (lambda (ls)
                 (cond
                   ((null? ls) '())
                   ((or (label? (car ls)) (register? (car ls))) (remove-reg (cdr ls)))
                   (else (set-cons (car ls) (remove-reg (cdr ls)))))))

             (define map*
               (lambda (proc ls)
                 (cond
                   ((null? ls) '())
                   (else (let ((cell (proc (car ls)))) 
			   (cons cell (map* proc (cdr ls))))))))

	     (define Ef*
	       (lambda (ef*)
		 (reverse-map Effect ef*)))

	     (define reverse-map
	       (lambda (proc ls)
		 (reverse (map* proc (reverse ls)))))

	     (define nfv?
	       (lambda (nfv)
		 (if (uvar? nfv)
		     (equal? "NFV" (extract-root nfv))
		     #f)))

	     (define rp?
	       (lambda (rp)
		 (if (uvar? rp)
		     (equal? "RP" (extract-root rp))
		     #f)))


;;ADDED THIS FUNCTION TO APPLY TO CFGRAPH TO MATCH ONLINE COMPILER, unsure of why RP's don't conflict with NFV's
	    
	     (define table-scrub
	       (lambda (cfgraph)
		 (map (lambda (x) (cond 
				   [(nfv? (car x)) (filter (lambda (y) (not (rp? y))) x)]
				   [(rp? (car x)) (filter (lambda (y) (not (nfv? y))) x)]
				   [else x]))
		      cfgraph)))


	     (define call-live* '())
	     
             )
;;DIDNT DO ANYTHING TO HANDLE NFV's could be possible source of error in future. 
           (Prog : Prog (x) -> Prog ()
                 [(letrec ([,l* ,[le*]] ...) ,[bd]) `(letrec ([,l* ,le*] ...) ,bd)])

           (LambdaExpr : LambdaExpr (x) -> LambdaExpr ()
                       [(lambda () ,[bd]) (begin (set! call-live* '()) `(lambda () ,bd))])

           (Body : Body (x) -> Body ()
                 [(locals (,uv0* ...) (new-frames ((,uv1* ...) ...) ,tl)) 
		  (begin
		    (set! conflict-table (init-conflict-table (append (apply append uv1*) uv0*)))
		    (set! live-ls '())
		    (let ([tl (Tail tl)])
		      `(locals (,(difference uv0* (filter uvar? call-live*)) ...)(new-frames ((,uv1* ...) ...) 
						      (spills (,(filter uvar? call-live*)...)  
							      (frame-conflict ,(table-scrub conflict-table) (call-live (,call-live* ...) ,tl)))))))])

           (Tail : Tail (x) -> Tail ()
                 [(,triv ,locrf* ...) (begin
                                        (set! live-ls (union (remove-reg (set-cons triv locrf*)) live-ls))    
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
                 [(mref ,[triv0] ,[triv1]) `(mref ,triv0 ,triv1)]
		 [(alloc ,[triv0]) `(alloc ,triv0)])

           (Pred : Pred (x) -> Pred ()
                 [(true) `(true)]
                 [(false) `(false)]
                 [(,relop ,triv0 ,triv1) (begin
                                           (if (or (frame-var? triv0) (uvar? triv0))
                                               (set! live-ls (set-cons triv0 live-ls)))
                                           (if (or (frame-var? triv1) (uvar? triv1))
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
                                           `(begin ,b ... ,a)))])
           (Effect : Effect (x) -> Effect ()
                   [(nop) `(nop)]
		   [(return-point ,l ,tl) (begin (set! call-live* (union call-live* live-ls))
						 (let ((tl (Tail tl))) `(return-point ,l ,tl)))] 
		   [(set! ,v (mref ,triv0 ,triv1)) (begin
						     (set! live-ls (remove v live-ls))
						     (cond
						      ((uvar? v)
						       (set! conflict-table (update-table v live-ls conflict-table)))
						      ((frame-var? v) (set! conflict-table (update-table-reg v live-ls conflict-table))))
						     (let ((triv0 (Triv triv0))
							   (triv1 (Triv triv1)))
						       `(set! ,v (mref ,triv0 ,triv1))))]
		   [(mset! ,[triv0] ,[triv1] ,[triv2]) `(mset! ,triv0 ,triv1 ,triv2)] ;;May need to update table here
        
                   [(set! ,v ,triv) (begin                                                   
				       (set! live-ls (remove v live-ls))				       
                                      (cond
				       ((and (uvar? v) (or (frame-var? triv) (uvar? triv)))
					(set! conflict-table (update-table v (remove triv live-ls) conflict-table)))
                                        ((uvar? v)
                                         (set! conflict-table (update-table v live-ls conflict-table)))
					((and (frame-var? v) (or (frame-var? triv) (uvar? triv))) (set! conflict-table (update-table-reg v (remove triv live-ls) conflict-table)))
					((frame-var? v) (set! conflict-table (update-table-reg v live-ls conflict-table))))	
                                      (let* ([a (Triv triv)])
                                      `(set! ,v ,a)))]
                   [(set! ,v (,op ,triv1 ,triv2)) (begin
                                                    (set! live-ls (remv v live-ls))
                                                    (cond 
                                                     ((uvar? v) (set! conflict-table (update-table v live-ls conflict-table)))
                                                     ((frame-var? v) (set! conflict-table (update-table-reg v live-ls conflict-table))))
                                                    (let* ([a (Triv triv2)]
                                                           [b (Triv triv1)])              
                                                    `(set! ,v (,op ,b ,a))))]
                   [(if ,pred ,ef1 ,ef2) (begin
                                           (let* ([a (Effect ef2)]
                                                  [b (Effect ef1)]
                                                  [c (Pred pred)])
                                           `(if ,c ,b ,a)))]
                   [(begin ,ef* ... ,ef) (begin
                                           (let* ([a (Effect ef)]
                                                  [b (Ef* ef*)])
                                           `(begin ,b ... ,a)))])
           (Triv : Triv (x) -> Triv ()
                 [,v (if (or (frame-var? v) (uvar? v)) (set! live-ls (set-cons v live-ls)))
                     `,v]))
) ;End Library










