(library (Compiler pre-assign-frame)
         (export pre-assign-frame parse-LpreAssignFrame)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))
         (define-parser parse-LpreAssignFrame LpreAssignFrame)

         (define-pass pre-assign-frame : LuncoverFrameConflict (x) -> LpreAssignFrame ()
           (definitions
             (define k (length registers))

             (define get-conflicts
               (lambda (var c-table)
                 (let ([found (assq var c-table)])
                   (if found (cdr found) '()))))

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
			[fv-conflicts (filter (lambda (x) (frame-var? x)) conflicts)]
			[var-conflicts (difference conflicts fv-conflicts)]
			[var-fv-conflicts (get-fv-conflicts var-conflicts assignments)]

			[total-fv-conflicts (union fv-conflicts var-fv-conflicts)])
		   (cond	
		    [(null? vars-reduced) (cons (make-assignment pick (find-free-fv total-fv-conflicts 0)) assignments)]
		    [else (choose-fv vars-reduced c-table-reduced c-table (cons (make-assignment pick (find-free-fv total-fv-conflicts 0)) assignments))]))))
             
	     (define choose-fv-initialize
               (lambda (vars c-table assignments)
                 (choose-fv vars c-table c-table assignments)
                 ))
             (define spill-list '())

	     )
           
	   (Body : Body (x) -> Body ()
		 [(locals (,uv1* ...)
			  (new-frames ((,uv2* ...) ...)
				      (spills (,uv3* ...)
					      (frame-conflict ,cfgraph
							      (call-live (,uv4* ...) ,[tl])))))
	

		  (if (null? uv3*) 
		      `(locals (,uv1* ...)
			       (new-frames ((,uv2* ...) ...)
					   (locate ()
						   (frame-conflict ,cfgraph
								   (call-live (,uv4* ...) ,tl)))))
		      (let ([assignments (choose-fv-initialize uv3* cfgraph '())])
			(let* ([uvar* (map car assignments)]
			       [fvar* (map cadr assignments)])
		;	  (display "cfgraph: ") (newline) (display cfgraph) (newline)

			  `(locals (,uv1* ...)
				   (new-frames ((,uv2* ...) ...)
					    (locate ([,uvar* ,fvar*] ...)
						    (frame-conflict ,cfgraph
								    (call-live (,uv4* ...) ,tl))))))))]
		 [else (error who "Body")]


		 )
	   )
	 ) ;End Library