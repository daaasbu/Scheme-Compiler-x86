(library (Compiler assign-registers)
         (export assign-registers parse-LassignRegisters)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LassignRegisters LassignRegisters)

;;The purpose of this pass is to use the Conflict Table constructed in the previous pass to determine variables of high degree, and
;;spill these variables as necessary based on the graph coloring algorithm. 
	 (define-pass assign-registers : LuncoverRegisterConflict (x) -> LassignRegisters ()
	   

	   
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

	     (define order-by-highest-degree
	       (lambda (conflicts* c-table)
		 (sort (lambda (x y)
			 (let* ([con-total-x (conflict-total x c-table)]
				[con-total-y (conflict-total y c-table)])
			   (> con-total-x con-total-y)))
		       conflicts*)))

	     (define pick-variable
	       (lambda (conflicts* c-table)
		 (car(append (order-by-highest-degree (filter (lambda (x) (unspillable? x)) conflicts*) c-table)  
			     (order-by-highest-degree (filter (lambda (x) (not (unspillable? x))) conflicts*) c-table)))))

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
	       (lambda (var reg)
		 `[,var ,reg]))

	     (define choose-registers
	       (lambda (vars c-table-reduced c-table assignments)
		 (let* ([min (pick-variable vars c-table-reduced)]
			[vars-reduced (remv min vars)]
			[c-table-reduced (remove-var min c-table)])
		   (choose-registers-helper min vars-reduced c-table-reduced c-table assignments))))

	     (define get-reg-conflicts
	       (lambda (var-conflicts assignments)
		 (cond
		  [(null? var-conflicts) '()]
		  [(eqv? #f (assq (car var-conflicts) assignments)) (get-reg-conflicts (cdr var-conflicts) assignments)]
		  [else(cons (cadr (assq (car var-conflicts) assignments)) (get-reg-conflicts (cdr var-conflicts) assignments))])))

	     (define choose-registers-helper
	       (lambda (pick vars-reduced c-table-reduced c-table assignments)
		 (let* ([conflicts (get-conflicts pick c-table)]
			[reg-conflicts (intersection conflicts registers)]
			[var-conflicts (difference conflicts reg-conflicts)]
			[var-reg-conflicts (get-reg-conflicts var-conflicts assignments)]
			[total-reg-conflicts (union reg-conflicts var-reg-conflicts)]
			[free-regs (difference registers total-reg-conflicts)])
		   
		   (cond
		    [(and (unspillable? pick) (null? vars-reduced) (null? spill-list) (null? free-regs))] 
		    [(and (unspillable? pick) (null? vars-reduced) (null? free-regs)) assignments]
		    [(and (unspillable? pick) (null? free-regs)) (choose-registers vars-reduced c-table-reduced c-table assignments)]
		    [(and (null? free-regs) (null? vars-reduced)) 
		     (begin (set! spill-list (cons pick spill-list)) assignments)]
		    [(null? free-regs) (begin (set! spill-list (cons pick spill-list)) (choose-registers vars-reduced c-table-reduced c-table assignments))]
		    [(null? vars-reduced) (cons (make-assignment pick (car free-regs)) assignments)]
		    [else (choose-registers vars-reduced c-table-reduced c-table (cons (make-assignment pick (car free-regs)) assignments))]))))
	     (define choose-registers-initialize
	       (lambda (vars c-table)
		 (choose-registers vars c-table c-table '())))

	     (define spill-list '())
	     

	     
	     )
	   (Body : Body (x) -> Body ()
		 [(locals (,uv1* ...) 
			  (ulocals (,uv2* ...) 
				   (locate ([,uv3* ,locrf*] ...)
					   (frame-conflict ,cfgraph1
							   (register-conflict ,cfgraph2 ,[tl])))))
		  (if (and (null? cfgraph2) (null? uv1*)) `(locate () ,tl)
		      
		      (let([assignments (choose-registers-initialize (append uv1* uv2*) cfgraph2)])
			(if (null? spill-list)
			    (let* ([uvar* (map car assignments)]
				   [reg* (map cadr assignments)])
			      `(locate ([,uvar* ,reg*] ...) ,tl))
			    
			    `(locals (,(difference uv1* spill-list) ...)
				     (ulocals (,uv2* ...)
					      (spills (,spill-list ...)
						      (locate ([,uv3* ,locrf*] ...)
							      (frame-conflict ,cfgraph1 ,tl))))))))]
		 
		 [(locate ((,uv** ,locrf*) ...) ,[tl])  `(locate ((,uv** ,locrf*) ...) ,tl)])

	   
	   )
	 ) ;End Library


