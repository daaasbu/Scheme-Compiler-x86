(library (Compiler assign-registers)
         (export assign-registers parse-LassignRegisters)
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
         (define-parser parse-LassignRegisters LassignRegisters)

      


#|	   (define-pass assign-registers : LuncoverRegisterConflict (x) -> LassignRegisters ()
	     (definitions

	       ;; The find-used function returns all of the registers that are
	       ;; used by the things (variables or registers) in the list conflict*.
	       ;; The helper function set-cons, used below, is defined in (Framework helpers).
	       (define find-used
		 (lambda (conflict* home*)
		   (cond
		    [(null? conflict*) '()]
		    [(register? (car conflict*))
		     (set-cons (car conflict*) (find-used (cdr conflict*) home*))]
		    [(assq (car conflict*) home*) =>
		     (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*) home*)))]
		    [else (find-used (cdr conflict*) home*)])))
	       ;; The select-register function tries to find a register for variable var
	       ;; that has not already been used for the variables that var is conflicted
	       ;; with.
	       (define select-register
		 (lambda (var conflict* home*)
		   (let ([used* (find-used conflict* home*)])
		     (let ([available* (difference registers used*)])
		       (and (not (null? available*)) (car available*))))))
	       ;; The rem-conflicts! function removes variable var from the conflict set
	       ;; of each of the variables it is conflicted with (conflict*).
	       (define rem-conflicts!
		 (lambda (conflict-graph var conflict*)
		   (for-each
		    (lambda (x)
		      (when (uvar? x)
			    (let ([a (assq x conflict-graph)])
			      (set-cdr! a (remq var (cdr a))))))
		    conflict*)))
	       ;; The find-homes function implements the register allocation algorithm.
	       ;; It tries to assign a register (a home) to each variable in the
	       ;; list var* such that conflicted variables are never assigned to the
	       ;; same home.
	       (define find-homes
		 (lambda (var* conflict-graph)
		   (define k (length registers))
		   (define low-degree?
		     (lambda (var)
		       (< (length (cdr (assq var conflict-graph))) k)))
		   (let loop ([var* var*])
		     (if (null? var*)
			 '()
			 (let ([var (or (find low-degree? var*) (car var*))])
			   (let ([conflict* (cdr (assq var conflict-graph))]
				 [var* (remq var var*)])
			     (rem-conflicts! conflict-graph var conflict*)
			     (let ([home* (loop var*)])
			       (let ([reg (select-register var conflict* home*)])
				 (if reg
				     (cons `[,var ,reg] home*)
				     home*))))))))))
	     ;; End definitions
	     (Body : Body (x) -> Body ()
		   [(locals (,uv1* ...) 
			    (ulocals (,uv2* ...) 
				     (locate ([,uv3* ,locrf*] ...)
					     (frame-conflict ,cfgraph1
							     (register-conflict ,cfgraph2 ,[tl])))))
		    
		    (let ([home* (find-homes (append uv2* uv1*) cfgraph2)])
		      (let ([spill* (difference uv1* (map car home*))])
			(let ((uv* (map car home*))
			      (r* (map cadr home*)))
			  (cond 
			   [(null? spill*) `(locate ([,(append uv* uv3*) ,(append r* locrf*)] ...) ,tl)] ;;changed this
			  ; [(and (null? cfgraph2) (null? uv1*)) `(locate () ,tl)]
			 ;  [(not (and (null? cfgraph2) (null? uv1*))) `(locate ([,uvar* ,reg*] ...) ,tl)]
			   [else `(locals (,(difference uv1* spill*) ...)
					  (ulocals (,uv2* ...)
						   (spills (,spill* ...)
							   (locate ([,uv3* ,locrf*] ...)
								   (frame-conflict ,cfgraph1 ,tl)))))]))))]
			  
		   [(locate ((,uv** ,locrf*) ...) ,[tl])  `(locate ((,uv** ,locrf*) ...) ,tl)]
		   [else (error who "Body")])
	     (Prog : Prog (x) -> Prog ()
		   [(letrec ([,l* (lambda () ,[Body : -> bd*])] ...) ,[Body : -> bd])
		    `(letrec ([,l* (lambda () ,bd*)] ...) ,bd)]
		   [else (error who "invalid Program ~s" x)])))
	     

|#
   










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
		    (let ((order
		    (order-by-highest-degree (filter (lambda (x) (not (unspillable? x))) conflicts*) c-table)))
		    (cond
		    [(null? order) (car (order-by-highest-degree conflicts* c-table))]
		    [else (car order)]))))

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
					;(display "c-table initial:") (newline) (display c-table)
					;(newline) (display "pick: ") (newline) (display min)
					;(newline) (display "c-table-reduced, initial: ") (newline) (display c-table-reduced) (newline)
		    
		    (choose-registers-helper min vars-reduced c-table-reduced c-table assignments))))

		    ;;assignments is a list of lists, want to find all the registers that are in use by conflicted vars.
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
		    [(and (null? free-regs) (null? vars-reduced)) 
		    (begin (set! spill-list (cons pick spill-list))  assignments)]
		    
		    [(null? free-regs) (begin (set! spill-list (cons pick spill-list))
		    (choose-registers vars-reduced c-table-reduced c-table assignments))]
		    [(null? vars-reduced) (cons (make-assignment pick (car free-regs)) assignments)]
		    [else (choose-registers vars-reduced c-table-reduced c-table (cons (make-assignment pick (car free-regs)) assignments))]))))
		    (define choose-registers-initialize
		    (lambda (vars c-table)
					;(display c-table)
		    (choose-registers vars c-table c-table '())
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
		    
		    [(locate ((,uv** ,locrf*) ...) ,[tl])  `(locate ((,uv** ,locrf*) ...) ,tl)]
		    [else (error who "Body")])

		    
		    )
		    ) ;End Library


		    