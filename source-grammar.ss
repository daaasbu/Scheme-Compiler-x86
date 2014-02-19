(library (source-grammar)
  (export
    
    unparse-LverifyScheme
    unparse-LremoveComplexOpera*
    unparse-LflattenSet!
    unparse-LimposeCallingConventions
    unparse-LuncoverFrameConflict
    unparse-LintroduceAllocationForms
    unparse-LuncoverRegisterConflict
    unparse-LassignRegisters
    unparse-LassignFrame
    unparse-LdiscardCallLive
    unparse-LfinalizeLocations
    unparse-LexposeFrameVar
    unparse-LexposeBasicBlocks
    unparse-LflattenProgram
    
    LverifyScheme
    LremoveComplexOpera*
    LflattenSet!
    LimposeCallingConventions
    LuncoverFrameConflict
    LintroduceAllocationForms
    LuncoverRegisterConflict
    LassignRegisters
    LassignFrame
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
  ;; Week 5 grammars
  ;;
  ;; Passes:
  ;; verify-scheme l-01 -> l-01
  ;; uncover-frame-conflict l-01 -> l-27
  ;; introduce-allocation-forms l-27 -> l-28
  ;; select-instructions l-28 -> l-28
  ;; uncover-register-conflict l-28 -> l-32
  ;; assign-registers l-32 -> l-33
  ;; everybody-home? l-33 -> bool
  ;; assign-frame l-33 -> l-28
  ;; finalize-frame-locations l-28 -> l-28
  ;; discard-call-live l-33 -> l-35
  ;; finalize-locations l-35 -> l-36
  ;; expose-frame-var l-36 -> l-37
  ;; expose-basic-blocks l-37 -> l-39
  ;; flatten-program l-39 -> l-41
  ;; generate-x86-64 l-41 -> ()


  ;; Predicates for the define-language form
  (define (binop? x) (if (memq x '(+ - * logand logor sra)) #t #f))
  (define (val-or-binop? x) (if (or (label? x)(memq x '(+ - * logand logor sra))) #t #f))
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
       (val-or-binop (vop))
       (rop (relop))
       (immediate (i))
       (frame-var (fv))
       (uvar (uv))
       (label (l)))
     ;; End terminals
     (Prog (p) (letrec ([l* le*] ...) bd))
     (LambdaExpr (le) (lambda (uv* ...) bd))
     (Body (bd) (locals (uv* ...) tl))
     (Value (val) triv
                       (prim op val0 val1)
                       (if pred val0 val1)
                       (begin ef* ... val))
     (Tail (tl) triv
                      (prim op val0 val1)
                      (if pred tl0 tl1)
                      (begin ef* ... tl)
                      (call val val* ...))
     (Pred (pred) (true)
                      (false)
                      (prim relop val0 val1)
                      (if pred0 pred1 pred2)
                      (begin ef* ... pred))
     (Effect (ef) (nop)
                      (set! uv val)
                      (if pred ef0 ef1)
                      (begin ef* ... ef))
     (Triv (triv) uv i l))

  (define-language LremoveComplexOpera*
    (extends LverifyScheme)
    (entry Prog)
    (Tail (tl) (- (prim op val0 val1)
                  (call val val* ...))
               (+ (prim op triv0 triv1)
                  (call triv0 triv* ...)))
    (Pred (pred) (- (prim relop val0 val1))
                 (+ (relop triv0 triv1)))
    (Value (val) (- (prim op val0 val1))
                 (+ (prim op triv0 triv1))))

  (define-language LflattenSet!
    (extends LremoveComplexOpera*)
    (entry Prog)
    (Effect (ef) (- (set! uv val))
                 (+ (set! uv rhs)))
    (Value (val) (- (prim op triv0 triv1)
                    triv
                    (if pred val0 val1)
                    (begin ef* ... val)))
    (Rhs (rhs) (+ triv (op triv0 triv1))))

  (define-language LimposeCallingConventions
    (extends LflattenSet!)
    (entry Prog)
    (LambdaExpr (le) (- (lambda (uv* ...) bd))
                     (+ (lambda () bd)))
    (Tail (tl) (- triv (call triv0 triv* ...) (prim op triv0 triv1))
               (+ (triv locrf* ...)))
    (Triv (triv) (- uv)
                 (+ v))
    (Effect (ef) (- (set! uv rhs))
                 (+ (set! v rhs)))
    (Var (v) (+ uv locrf))
    (Loc (locrf) (+ r fv)))


  (define-language LuncoverFrameConflict
    (extends LimposeCallingConventions)
    (entry Prog)
    (terminals (+ (conflict-graph (cfgraph))))
    (Body (bd) (- (locals (uv* ...) tl))
                      (+ (locals (uv* ...) fbd)))
    (FrameBody (fbd) (+ (frame-conflict cfgraph tl))))

  (define-language LintroduceAllocationForms
    (extends LuncoverFrameConflict)
    (entry Prog)
    (Body (bd) (- (locals (uv* ...) fbd))
               (+ (locals (uv* ...) ubd)
                  (locate ([uv** locrf*] ...) tl)))
    (Ubody (ubd) (+ (ulocals (uv* ...) lbd)))
    (Lbody (lbd) (+ (locate ([uv* locrf*] ...) fbd))))
  
  (define-language LuncoverRegisterConflict
    (extends LintroduceAllocationForms)
    (entry Prog)
    (FrameBody (fbd) (- (frame-conflict cfgraph tl))
                            (+ (frame-conflict cfgraph rbd)))
    (RegisterBody (rbd) (+ (register-conflict cfgraph tl))))

  (define-language LassignRegisters
    (extends LuncoverRegisterConflict)
    (entry Prog)
    (FrameBody (fbd) (- (frame-conflict cfgraph rbd))
                        (+ (frame-conflict cfgraph tl)))
    (Ubody (ubd) (- (ulocals (uv* ...) lbd))
                        (+ (ulocals (uv* ...) sbd)))
    (SpillBody (sbd) (+ (spills (uv* ...) lbd)))
    (RegisterBody (rbd) (- (register-conflict cfgraph tl))))

  (define-language LassignFrame
    (extends LassignRegisters)
    (entry Prog)
    (Ubody (ubd) (- (ulocals (uv* ...) sbd))
                 (+ (ulocals (uv* ...) lbd)))
    (SpillBody (sbd) (- (spills (uv* ...) lbd))))


  (define-language LdiscardCallLive
    (extends LassignRegisters)
    (entry Prog)
    ;(FrameBody (fbd) (- (frame-conflict cfgraph tl)))
    ;(Lbody (lbd) (- (locate ((uv* locrf*) ...) fbd)))
    ;(Ubody (ubd) (- (ulocals (uv* ...) lbd)))

    (Tail (tl) (- (triv locrf* ...))
                    (+ (triv))))

  (define-language LfinalizeLocations
    (extends LdiscardCallLive)
    (entry Prog)
    (Prog (p) (- (letrec ([l* le*] ...) bd))
                     (+ (letrec ([l* le*] ...) tl)))
    (LambdaExpr (le) (- (lambda () bd))
                     (+ (lambda () tl)))
    (Body (bd) (- (locate ([uv** locrf*] ...) tl)
                        #;(locals (uv* ...) ubd)))
    (Effect (ef) (- (set! v rhs))
                     (+ (set! locrf rhs)))
    (Triv (triv) (- v)
                     (+ locrf))
    (Var (v) (- uv locrf)))

  (define-language LexposeFrameVar
    (extends LfinalizeLocations)
    (entry Prog)
    ;; Frame-vars are now gone. Instead they are represented as offsets from the
    ;; frame-pointer-register
    (terminals (+ (location (loc)))
                    (- (frame-var (fv))))
    (Loc (locrf) (- fv)
                    (+ loc)))

  (define-language LexposeBasicBlocks
    (extends LexposeFrameVar)
    (entry Prog)
    (Tail (tl) (- (if pred tl0 tl1))
                  (+ (if (relop triv0 triv1) (l0) (l1))))
    (Effect (ef) (- (if pred ef0 ef1)
                     (nop)))
    (Pred (pred) (- (true)
                     (false)
                     (relop triv0 triv1)
                     (if pred0 pred1 pred2)
                     (begin ef* ... pred))))

  (define-language LflattenProgram
    (extends LexposeBasicBlocks)
    (entry Prog)
    (LambdaExpr (le) (- (lambda () tl)))
    (Tail (tl) (- (if (relop triv0 triv1) (l0) (l1))
                       (begin ef* ... tl)))
    (Pred (pred) (+ (not (relop triv0 triv1))
                        (relop triv0 triv1)))
    (Code (c) (+ l
                        (set! locrf rhs)
                        (jump triv)
                        (if pred c))) ;; Should be (jump l)
    (Prog (p) (- (letrec ([l* le*] ...) tl))
                     (+ (code c* ...))))

  )