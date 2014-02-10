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
               (lambda (vars c-table assignments)
                 (let* ([min (get-lowest-degree vars c-table)]
                        [vars-reduced (remv min vars)]
                        [c-table-reduced (remove-var min c-table)])
                 
                 (choose-registers-helper min vars-reduced c-table-reduced assignments))))

;;assignments is a list of lists, want to find all the registers that are in use by conflicted vars.
             (define get-reg-conflicts
               (lambda (var-conflicts assignments)
                   (cond
                     [(null? var-conflicts) '()]
                     [(eqv? #f (assq (car var-conflicts) assignments)) (get-reg-conflicts (cdr var-conflicts) assignments)]
                     [else (cons (cadr (assq (car var-conflicts) assignments)) (get-reg-conflicts (cdr var-conflicts) assignments))])))

             (define choose-registers-helper
                   (lambda (pick vars-reduced c-table-reduced assignments)
                     (let* ([conflicts (get-conflicts pick c-table-reduced)]
                            [reg-conflicts (intersection conflicts registers)]
                            [var-conflicts (difference conflicts reg-conflicts)]
                            [var-reg-conflicts (get-reg-conflicts var-conflicts assignments)]
                            [total-reg-conflicts (union reg-conflicts var-reg-conflicts)]
                            [free-regs (difference registers total-reg-conflicts)])
                       ;(display "pick:") (display pick) (newline)
                       ;(display "c-table-reduced:") 
                    ;   (display "c-table:" )(display c-table-reduced) (newline)
                     ;   (display "conflicts:") (display conflicts) (newline)
                        
                      ; (display "reg-conflicts:") (display reg-conflicts) (newline)
                       
                       (cond
                         [(null? vars-reduced) (cons (make-assignment pick (car free-regs)) assignments)]
                         [(null? free-regs) (error 'fuck "no more free registers")]
                         [else (choose-registers vars-reduced c-table-reduced (cons (make-assignment pick (car free-regs)) assignments))]))))

             (define choose-registers-initialize
               (lambda (vars c-table)
                 ;(display c-table)
                 (choose-registers vars c-table '())
                 ))
                       

             
             )
           
         

(Body : Body (x) -> Body ()
      
      [(locals (,uv* ...) (register-conflict ,cfgraph ,[tl]))   (if (and (null? cfgraph) (null? uv*)) `(locate () ,tl)
                                                                                   (let* ([assignments (choose-registers-initialize uv* cfgraph)]
                                                                                          [uvar* (map car assignments)]
                                                                                          [reg* (map cadr assignments)])
                                                                                     (display assignments)
                                                                                     `(locate ([,uvar* ,reg*] ...) ,tl))
                                                                                  
                                                                                   
                                                                                     )]
      )
)
) ;End Library