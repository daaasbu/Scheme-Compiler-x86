<<<<<<< HEAD
;; P423
;; Week 6 grammars
;;
;; Passes:
;;   verify-scheme              l-01 -> l-01
;; * remove-complex-opera*      l-01 -> l-23
;; * flatten-set!               l-23 -> l-24
;; * impose-calling-conventions l-24 -> l-25
;;   uncover-frame-conflict     l-25 -> l-27
;;   introduce-allocation-forms l-27 -> l-28
;;     select-instructions       l-28 -> l-28
;;     uncover-register-conflict l-28 -> l-32
;;     assign-registers          l-32 -> l-33
;;     everybody-home?           l-33 -> bool
;;     assign-frame              l-33 -> l-28
;;     finalize-frame-locations  l-28 -> l-28
;;   discard-call-live          l-33 -> l-35
;;   finalize-locations         l-35 -> l-36
;;   expose-frame-var           l-36 -> l-37
;;   expose-basic-blocks        l-37 -> l-39
;;   flatten-program            l-39 -> l-41
;;   generate-x86-64            l-41 -> ()

;; (*) Updated this week.

(p423-grammars
  (l01-verify-scheme
    (start Prog)
    (Prog
      (letrec ((Label (lambda (UVar *) Body)) *) Body))
    (Body
      (locals (UVar *) Tail))
    (Tail
      (if Pred Tail Tail)
      (begin Effect * Tail)
      (Binop Value Value)
      (Value Value *)
      Triv)
    (Pred
      (true)
      (false)
      (if Pred Pred Pred)
      (begin Effect * Pred)
      (Relop Value Value))
    (Effect
      (nop)
      (set! UVar Value)
      (if Pred Effect Effect)
      (begin Effect * Effect))
    (Value
      (if Pred Value Value)
      (begin Effect * Value)
      (Binop Value Value)
      Triv)
    (Triv
      UVar
      Integer
      Label))

 ;; Replace Value with Triv in arguments of procedure calls and primitive application.
 (l23-remove-complex-opera
   (%remove
     (Tail Binop Value)
     (Pred Relop)
     (Value Binop))
   (%add
     (Tail
       (Binop Triv Triv)
       (Triv Triv *))
     (Pred (Relop Triv Triv))
     (Value (Binop Triv Triv))))

 ;; Remove Value, set! rhs may only be Triv or Binop.
 (l24-flatten-set
   (%remove
     Value
     (Effect set!))
   (%add
     (Effect
       (set! UVar Triv)
       (set! UVar (Binop Triv Triv)))))

 (l25-impose-calling-conventions
   (%remove
     (Prog letrec)
     (Tail Triv Binop)
     (Effect set!)
     (Triv UVar))
   (%add
     (Prog (letrec ((Label (lambda () Body)) *) Body))
     (Tail (Triv Loc *))
     (Effect
       (set! Var Triv)
       (set! Var (Binop Triv Triv)))
     (Loc
       Reg
       FVar)
     (Var
       UVar
       Loc)
     (Triv Var)))

=======
(library (source-grammar)
  (export
    
    unparse-LverifyScheme
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
  ;;   verify-scheme              l-01 -> l-01
  ;;   uncover-frame-conflict     l-01 -> l-27
  ;;   introduce-allocation-forms l-27 -> l-28
  ;;     select-instructions       l-28 -> l-28
  ;;     uncover-register-conflict l-28 -> l-32
  ;;     assign-registers          l-32 -> l-33
  ;;     everybody-home?           l-33 -> bool
  ;;     assign-frame              l-33 -> l-28
  ;;     finalize-frame-locations  l-28 -> l-28
  ;;   discard-call-live          l-33 -> l-35
  ;;   finalize-locations         l-35 -> l-36
  ;;   expose-frame-var           l-36 -> l-37
  ;;   expose-basic-blocks        l-37 -> l-39
  ;;   flatten-program            l-39 -> l-41
  ;;   generate-x86-64            l-41 -> ()

>>>>>>> a5

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

  (define-language LuncoverFrameConflict 
    (extends LverifyScheme)
    (entry Prog)
    (terminals        (+ (conflict-graph (cfgraph))))
    (Body (bd)        (- (locals (uv* ...) tl))
                      (+ (locals (uv* ...) fbd)))
    (FrameBody (fbd)  (+ (frame-conflict cfgraph tl))))

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
    (FrameBody (fbd)        (- (frame-conflict cfgraph tl))
                            (+ (frame-conflict cfgraph rbd)))
    (RegisterBody (rbd) (+ (register-conflict cfgraph tl))))

  ;; Up to here
  (define-language LassignRegisters 
    (extends LuncoverRegisterConflict)
    (entry Prog)
    (FrameBody (fbd)    (- (frame-conflict cfgraph rbd))
                        (+ (frame-conflict cfgraph tl)))
    (Ubody (ubd)        (- (ulocals (uv* ...) lbd))
                        (+ (ulocals (uv* ...) sbd)))
    (SpillBody (sbd)    (+ (spills (uv* ...) lbd)))
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
    ;(Lbody (lbd)     (- (locate ((uv* locrf*) ...) fbd)))
    ;(Ubody (ubd)     (- (ulocals (uv* ...) lbd))) 

    (Tail (tl)      (- (triv locrf* ...))
                    (+ (triv))))

  (define-language LfinalizeLocations 
    (extends LdiscardCallLive)
    (entry Prog)
    (Prog (p)        (- (letrec ([l* le*] ...) bd))
                     (+ (letrec ([l* le*] ...) tl)))
    (LambdaExpr (le) (- (lambda () bd))
                     (+ (lambda () tl)))
    (Body (bd)       (- (locate ([uv** locrf*] ...) tl)
                        #;(locals (uv* ...) ubd)))
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
