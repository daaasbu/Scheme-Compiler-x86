(library (source-grammar)
  (export
    
    unparse-LverifyScheme
    unparse-LuncoverRegisterConflict 
    unparse-LassignRegisters 
    unparse-LdiscardCallLive
    unparse-LfinalizeLocations
    unparse-LexposeFrameVar 
    unparse-LexposeBasicBlocks
    unparse-LflattenProgram
    
    LverifyScheme
    LuncoverRegisterConflict 
    LassignRegisters
    LdiscardCallLive
    LfinalizeLocations
    LexposeFrameVar 
    LexposeBasicBlocks
    LflattenProgram
  )
  (import 
    (Framework nanopass)
    (Framework helpers)
    (chezscheme))
  ;; P423
  ;; Week 4 grammars
  ;;
  ;; Passes:
  ;;   verify-scheme              
  ;; * uncover-register-conflict  
  ;; * assign-registers           
  ;; * discard-call-live          
  ;;   finalize-locations         
  ;;   expose-frame-var           
  ;;   expose-basic-blocks        
  ;;   flatten-program            
  ;;   generate-x86-64            


  ;; Predicates for the define-language form
  (define (binop? x) (if (memq x '(+ - * logand logor sra)) #t #f))
  (define immediate? int64?)
  (define (location? x) (or (disp-opnd? x) (index-opnd? x)))
  (define (rop? x) (if (memq x '(< <= = >= >)) #t #f))
  (define (conflict-graph? ct)
      (and (list? ct)
           (andmap
             (lambda (ls)
               (and (list? ls)
                    (andmap
                      (lambda (x)
                        (or (uvar? x) (register? x) (frame-var? x)))
                      ls)))
             ct)))

  (define-language LverifyScheme
     (entry Prog)
     (terminals 
       (register (r))
       (binop (op))
       (rop (relop))
       (immediate (i))
       (frame-var (fv))
       (uvar (uv))
       (label (l)))
     ;; End terminals 
     (Prog (p)        (letrec ([l* le*] ...) bd))
     (LambdaExpr (le) (lambda () bd))
     (Body (bd)     (locals (uv* ...) tl))
     (Tail (tl)       (triv locrf* ...)
                      (if pred tl0 tl1)
                      (begin ef* ... tl))
     (Pred (pred)     (true)
                      (false) 
                      (relop triv0 triv1) ;; Possible FIXME
                      (if pred0 pred1 pred2)
                      (begin ef* ... pred))
     (Effect (ef)     (nop)
                      (set! v rhs)
                      (if pred ef0 ef1)
                      (begin ef* ... ef))
     (Rhs (rhs)       triv (op triv0 triv1))
     (Loc (locrf)     r fv)
     (Triv (triv)     v i l)
     (Var (v)         uv locrf))

  (define-language LuncoverRegisterConflict 
    (extends LverifyScheme)
    (entry Prog)
    (terminals 
      (+ (conflict-graph (cfgraph))))
    (Body (bd)        (- (locals (uv* ...) tl))
                      (+ (locals (uv* ...) rbd)))
    (RegisterBody (rbd) (+ (register-conflict cfgraph tl))))

  (define-language LassignRegisters 
    (extends LuncoverRegisterConflict)
    (entry Prog)
    (Body (bd)          (- (locals (uv* ...) rbd))
                        (+ (locate ([uv* r*] ...) tl)))
    (RegisterBody (rbd) (- (register-conflict cfgraph tl))))

  (define-language LdiscardCallLive
    (extends LassignRegisters)
    (entry Prog)
    (Tail (tl)      (- (triv locrf* ...))
                    (+ (triv))))

  (define-language LfinalizeLocations 
    (extends LdiscardCallLive)
    (entry Prog)
    (Prog (p)        (- (letrec ([l* le*] ...) bd))
                     (+ (letrec ([l* le*] ...) tl)))
    (LambdaExpr (le) (- (lambda () bd))
                     (+ (lambda () tl)))
    (Body (bd)       (- (locate ([uv* r*] ...) tl)))
    (Effect (ef)     (- (set! v rhs))
                     (+ (set! locrf rhs)))
    (Triv (triv)     (- v) 
                     (+ locrf))
    (Var (v)         (- uv locrf)))

  (define-language LexposeFrameVar 
    (extends LfinalizeLocations)
    (entry Prog)
    ;; Frame-vars are now gone. Instead they are represented as offsets from the
    ;; frame-pointer-register
    (terminals      (+ (location (loc)))
                    (- (frame-var (fv))))
    (Loc (locrf)    (- fv)
                    (+ loc)))

  (define-language LexposeBasicBlocks
    (extends LexposeFrameVar)
    (entry Prog)
    (Tail (tl)    (- (if pred tl0 tl1))
                  (+ (if (relop triv0 triv1) (l0) (l1))))
    (Effect (ef)  (- (if pred ef0 ef1)
                     (nop)))
    (Pred (pred)  (- (true)
                     (false)
                     (relop triv0 triv1)
                     (if pred0 pred1 pred2)
                     (begin ef* ... pred))))

  (define-language LflattenProgram
    (extends LexposeBasicBlocks)
    (entry Prog)
    (LambdaExpr (le) (- (lambda () tl)))
    (Tail (tl)       (- (if (relop triv0 triv1) (l0) (l1))
                       (begin ef* ... tl)))
    (Pred (pred)     (+ (not (relop triv0 triv1))
                        (relop triv0 triv1)))
    (Code (c)        (+ l
                        (set! locrf rhs)
                        (jump triv)
                        (if pred c))) ;; Should be (jump l)
    (Prog (p)        (- (letrec ([l* le*] ...) tl))
                     (+ (code c* ...))))

  )
