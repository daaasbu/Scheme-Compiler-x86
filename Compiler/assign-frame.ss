(library (Compiler assign-frame)
         (export assign-frame parse-LassignFrame)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         #|
	 (define get-reg-conflicts
	 (lambda (var-conflicts c-table)
	 (let ([var (car var-conflicts)]
	 [conflicts (get-conflicts var c-table)]
	 [reg-conflicts (intersection conflicts registers)])      
	 (cond
	 [(null? var-conflicts) '()]
	 [else (append reg-conflicts (get-reg-conflicts var-conflicts c-table))]))))
	 |#

         #|
	 (define low-degree?
	 (lambda (var c-table)
	 (let ([total (conflict-total var c-table)])
	 (< total k))))
	 |#
         (define-parser parse-LassignFrame LassignFrame)

         (define-pass assign-frame : LassignRegisters (x) -> LassignFrame ()
           (definitions
             (define k (length registers))

             (define get-conflicts
               (lambda (var c-table)
                 (let ([found (assq var c-table)])
                   (if found  (cdr found) '()))))

             (define conflict-total
               (lambda (var c-table)
                 (let ([conflicts (get-conflicts var c-table)])
                   (length conflicts))))


             (define get-lowest-degree
               (lambda (conflicts* c-table)

                 (car (sort (lambda (x y)
                              (let* ([con-total-x (conflict-total x c-table)]
                                     [con-total-y (conflict-total y c-table)])
                                (< con-total-x con-total-y)))
                            conflicts*))))

             (define order-by-highest-degree
               (lambda (conflicts* c-table)

                  (sort (lambda (x y)
                              (let* ([con-total-x (conflict-total x c-table)]
                                     [con-total-y (conflict-total y c-table)])
                                (> con-total-x con-total-y)))
			conflicts*)))

	     (define pick-variable
	       (lambda (conflicts* c-table)
		 (car (order-by-highest-degree conflicts* c-table))))

             (define unspillable?
	       (lambda (v)
		 (equal? "UNSP" (extract-root v))))
              

             (define remove-var
               (lambda (var c-table)
                 (map
                  (lambda (conflicts)
                    (if (eqv? (car conflicts) var)
                        conflicts
                        (remv var conflicts)))
                  c-table)))
             (define make-assignment
               (lambda (var fv)
                 `[,var ,fv]))

             (define choose-fv
               (lambda (vars c-table-reduced c-table assignments)
                 (let* ([min (pick-variable vars c-table-reduced)]
                        [vars-reduced (remv min vars)]
                        [c-table-reduced (remove-var min c-table)])
		   (choose-fv-helper min vars-reduced c-table-reduced c-table assignments))))

	    
             (define get-fv-conflicts
               (lambda (var-conflicts assignments)
		 (cond
		  [(null? var-conflicts) '()]
		  [(eqv? #f (assq (car var-conflicts) assignments)) (get-fv-conflicts (cdr var-conflicts) assignments)]
		  [else(cons (cadr (assq (car var-conflicts) assignments)) (get-fv-conflicts (cdr var-conflicts) assignments))])))

	     (define find-free-fv
	       (lambda (total-fv-conflicts n)
		 (let ((fv (index->frame-var n)))
		   (cond
		    [(not (memv fv total-fv-conflicts)) fv]
		    [else (find-free-fv total-fv-conflicts (add1 n))]))))
		 
		 

             (define choose-fv-helper
	       (lambda (pick vars-reduced c-table-reduced c-table assignments)
		 (let* ([conflicts (get-conflicts pick c-table)]
			[fv-conflicts (filter (lambda (x) (not (frame-var? x))) conflicts)]
			[var-conflicts (difference conflicts fv-conflicts)]
			[var-fv-conflicts (get-fv-conflicts var-conflicts assignments)]
			[total-fv-conflicts (union fv-conflicts var-fv-conflicts)])
		   (cond						 
		    [(null? vars-reduced) (cons (make-assignment pick (find-free-fv total-fv-conflicts 0)) assignments)]
		    [else (choose-fv vars-reduced c-table-reduced c-table (cons (make-assignment pick (find-free-fv total-fv-conflicts 0)) assignments))]))))
             
	     (define choose-fv-initialize
               (lambda (vars c-table)
					;(display c-table)
                 (choose-fv vars c-table c-table '())
                 ))
             (define spill-list '())
	     

             
             )
           
	   
#|
	   (Body : Body (x) -> Body ()
		 
		 [(locals (,uv* ...) (register-conflict ,cfgraph ,[tl]))   
		       (if (and (null? cfgraph) (null? uv*)) `(locate () ,tl)
 			`(locate ([,uvar* ,reg*] ...) ,tl))
		 
		      
		      )]
|#		 	   
	   (Body : Body (x) -> Body ()
		 [(locals (,uv1* ...) 
			  (ulocals (,uv2* ...)
				   (spills (,uv4* ...)
					   (locate ([,uv3* ,locrf*] ...)
						   (frame-conflict ,cfgraph1 ,[tl])))))
		  (if (and (null? cfgraph1) (null? uv1*)) `(locals ()
								   (ulocals (,uv2* ...)
									    (locate () (frame-conflict ,cfgraph1 ,tl))))
		      
		      (let ([assignments (choose-fv-initialize uv4* cfgraph1)])
			(let* ([uvar* (map car assignments)]
			       [reg* (map cadr assignments)])
			  `(locals (,uv1* ...)
				   (ulocals (,uv2* ...)
				   (locate ([,(append uvar* uv3*) ,(append reg* locrf*)] ...)
					   (frame-conflict ,cfgraph1 ,tl)))))))]
		 
		 [(locate ((,uv** ,locrf*) ...) ,[tl])  `(locate ((,uv** ,locrf*) ...) ,tl)]
		 [else (error who "Body")]


		 )
	   )
	 ) ;End Library

















