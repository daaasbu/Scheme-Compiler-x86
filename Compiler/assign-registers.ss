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
                 (car (sort (lambda (x y)
                              (let* ([con-total-x (conflict-total x c-table)]
                                     [con-total-y (conflict-total y c-table)])
                                (> con-total-x con-total-y)))
                            conflicts*))))

             
             

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
                 (let* ([min (order-by-highest-degree vars c-table)]
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
                     [else (cons (cadr (assq (car var-conflicts) assignments)) (get-reg-conflicts (cdr var-conflicts) assignments))])))

             (define choose-registers-helper
                   (lambda (pick vars-reduced c-table-reduced c-table assignments)
                     (let* ([conflicts (get-conflicts pick c-table)]
                            [reg-conflicts (intersection conflicts registers)]
                            [var-conflicts (difference conflicts reg-conflicts)]
                            [var-reg-conflicts (get-reg-conflicts var-conflicts assignments)]
                            [total-reg-conflicts (union reg-conflicts var-reg-conflicts)]
                            [free-regs (difference registers total-reg-conflicts)])
                       ;(display "c-table-reduced" )
                       ;(display c-table-reduced) (newline)
                       ;(display "pick: ")
                       ;(display pick) (newline)
                       ;(display "reg-conflicts ")
                       ;(display reg-conflicts) (newline)
                       ;(display "var-conflicts: ")
                       ;(display var-conflicts) (newline)
                       ;(display "var-reg-conflicts: ")
                      ; (display var-reg-conflicts) (newline)
                     ;  (display "total-reg-conflicts: ")
                    ;   (display total-reg-conflicts) (newline)
                   ;    (display "free-regs: ")
                  ;     (display free-regs) (newline)
                 ;      (display "I HAVE LOOPED") (newline)

                                        ;(display "pick:") (display pick) (newline)
                       
                       
                                        ;  (display "c-table reduced:" )(display c-table-reduced) (newline)
                                        ;   (display "conflicts:") (display conflicts) (newline)
                       
                                        ; (display "reg-conflicts:") (display reg-conflicts) (newline)
                       
                       (cond
                         [(null? vars-reduced) (cons (make-assignment pick (car free-regs)) assignments)]
                         [(null? free-regs) (error 'blah "no more free registers")]
                         [else (choose-registers vars-reduced c-table-reduced c-table (cons (make-assignment pick (car free-regs)) assignments))]))))

             (define choose-registers-initialize
               (lambda (vars c-table)
                 ;(display c-table)
                 (choose-registers vars c-table c-table '())
                 ))
             
                       

             
             )
           
         

(Body : Body (x) -> Body ()
      
      [(locals (,uv* ...) (register-conflict ,cfgraph ,[tl]))   (if (and (null? cfgraph) (null? uv*)) `(locate () ,tl)
                                                                                   (let* ([assignments (choose-registers-initialize uv* cfgraph)]
                                                                                          [uvar* (map car assignments)]
                                                                                          [reg* (map cadr assignments)])
                                                                               ;      (display assignments)
                                                                                     `(locate ([,uvar* ,reg*] ...) ,tl))
                                                                                  
                                                                                   
                                                                                     )]
      )
)
) ;End Library