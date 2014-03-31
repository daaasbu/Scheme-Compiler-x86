(library (source-grammar)
  (export

    unparse-LverifyScheme
    unparse-LliftLetrec
    unparse-LnormalizeContext
    unparse-LspecifyRepresentation
    unparse-LuncoverLocals
    unparse-LremoveLet
    unparse-LremoveComplexOpera*
    unparse-LflattenSet!
    unparse-LimposeCallingConventions
    unparse-LexposeAllocationPointer
    unparse-LuncoverFrameConflict 
    unparse-LpreAssignFrame
    unparse-LassignNewFrame 
    unparse-LuncoverRegisterConflict 
    unparse-LassignRegisters 
    unparse-LdiscardCallLive
    unparse-LfinalizeLocations
    unparse-LexposeFrameVar 
    unparse-LexposeMemoryOperands
    unparse-LexposeBasicBlocks
    unparse-LflattenProgram
    
    LverifyScheme
    LliftLetrec
    LnormalizeContext
    LspecifyRepresentation
    LuncoverLocals
    LremoveLet
    LremoveComplexOpera*
    LflattenSet!
    LimposeCallingConventions
    LexposeAllocationPointer
    LuncoverFrameConflict
    LpreAssignFrame
    LassignNewFrame
    LuncoverRegisterConflict 
    LassignRegisters
    LdiscardCallLive
    LfinalizeLocations
    LexposeFrameVar 
    LexposeMemoryOperands
    LexposeBasicBlocks
    LflattenProgram
    effect-prim?
    pred-prim?
    value-prim?
    immediate?
    prim?
    prim->arity
  )
  (import 
    (Framework nanopass)
    (Framework helpers)
    (chezscheme))

  ;; Predicates for the define-language form
  (define (binop? x) (if (memq x '(+ - * logand logor sra)) #t #f))
  (define immediate? (lambda (x) (or (int64? x) (fixnum? x) (null? x) (boolean? x))))
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

 ; (define effect-prim? 
 ;   (lambda (x)
 ;    (if (memq x '(set-car! set-cdr! vector-set!)) #t #f)))
 ; (define value-prim? 
 ;    (lambda (x)
 ;       (if (memq x '(+ - * car cdr cons make-vector vector-length vector-ref void)) #t #f)))
 ; (define pred-prim? 
 ;    (lambda (x)
 ;      (if (memq x '(< <= = >= > boolean? eq? fixnum? null? pair? vector?))
 ;          #t #f)))
  
(define-syntax define-primitives
    (lambda (x)
      (define ->string
        (lambda (x)
          (cond
            [(string? x) x]
            [(identifier? x) (symbol->string (syntax->datum x))]
            [(symbol? x) (symbol->string x)]
            [(number? x) (number->string x)])))
      (define build-id
        (lambda (tid . parts)
          (datum->syntax tid
            (string->symbol
              (apply string-append (map ->string parts))))))
      (syntax-case x ()
        [(_ name (type [primname primarity] ...) ...)
          (with-syntax ([name? (build-id #'name #'name "?")]
                         [(type-name? ...)
                           (map
                             (lambda (type)
                               (build-id #'name type "-" #'name "?"))
                             #'(type ...))]
                         [name->arity (build-id #'name #'name "->arity")])
            #'(begin
                (define name?
                  (lambda (x)
                    (or (type-name? x) ...)))
                (define type-name?
                  (lambda (x)
                    (memq x '(primname ...))))
                ...
                (define name->arity
                  (lambda (x)
                    (cond
                      [(assq x '((primname . primarity) ... ...)) => cdr]
                      [else #f])))))])))

(define-primitives prim
    (value [+ 2] [- 2] [* 2] [car 1] [cdr 1] [cons 2] [make-vector 1]
      [vector-length 1] [vector-ref 2] [void 0])
    (pred [< 2] [<= 2] [= 2] [>= 2] [> 2] [boolean? 1] [eq? 2]
      [fixnum? 1] [null? 1] [pair? 1] [vector? 1])
    (effect [set-car! 2] [set-cdr! 2] [vector-set! 3]))

(define-language LverifyScheme
  (entry Expr)
  (terminals
   (label (l))
   (immediate (i))
   (prim (prim))
   (uvar (uv)))
  (Expr (expr)
        l
        uv
        (let ([uv* expr*] ...) expr)
        (quote i)
        (if expr0 expr1 expr2)
        (begin expr* ... expr)
        (letrec ([l* le*] ...) expr)
        (prim expr* ...)
        (call expr expr* ...))
  (LambdaExpr (le)
              (lambda (uv* ...) expr)))

(define-language LliftLetrec 
(extends LverifyScheme) 
(entry Prog)
  (Prog (prog) (+ (letrec ([l* le*] ...) expr)))
  (Expr (expr) (- (letrec ([l* le*] ...) expr))))

#;(define-language LliftLetrec 
  (entry Prog)
  (terminals
   (label (l))
   (immediate (i))
   (prim (prim))
   (uvar (uv)))
  (Expr (expr)
        l
        uv
        (let ([uv* expr*] ...) expr)
        (quote i)
        (if expr0 expr1 expr2)
        (begin expr* ... expr)
        (prim expr* ...)
        (call expr expr* ...))
  (LambdaExpr (le)
              (lambda (uv* ...) expr))
  (Prog (prog)
        (letrec ([l* le*] ...) expr)))
  
(define-language LnormalizeContext
  (extends LliftLetrec)
  (entry Prog)
  (terminals
   (- (prim (prim)))
   (+ (value-prim (vprim))
      (effect-prim (eprim))
      (pred-prim (pprim))))
  (Effect (ef)
          (+ (nop) (if pred ef0 ef1) (begin ef* ... ef)
             (let ([uv* val*] ...) ef) (eprim val* ...)
             (call val val* ...)))
  (Pred (pred)
        (+ (true) (false) (if pred0 pred1 pred2)
           (begin ef* ... pred) (let ([uv* val*] ...) pred)
           (pprim val* ...)))
  (Value (val)
         (+ l uv 'i (if pred val0 val1) (begin ef* ... val)
            (let ([uv* val*] ...) val) (vprim val* ...)
            (call val val* ...)))
  (Prog (prog)
        (- (letrec ([l* le*] ...) expr))
        (+ (letrec ([l* le*] ...) val)))
  (LambdaExpr (le)
              (- (lambda (uv* ...) expr))
              (+ (lambda (uv* ...) val)))
  (Expr (expr)
        (- l uv (let ([uv* expr*] ...) expr) 'i
           (if expr0 expr1 expr2) (begin expr* ... expr)
           (prim expr* ...) (call expr expr* ...))))

#;(define-language LnormalizeContext
  (entry Prog)
  (terminals 
    (pred-prim (pprim))
    (effect-prim (eprim))
    (value-prim (vprim))
    (immediate (i))
    (uvar (uv))
    (label (l)))
  (LambdaExpr (le) (lambda (uv* ...) val))
  (Value (val)
     l uv 
     (quote i)
     (if pred val0 val1)
     (begin ef* ... val)
     (let ([uv* val*] ...) val)
     (vprim val* ...)
     (call val val* ...))
  (Pred (pred) 
     (true)
     (false)
     (if pred0 pred1 pred2)
     (begin ef* ... pred)
     (let ([uv* val*] ...) pred)
     (pprim val* ...))
  (Effect (ef)
    (nop)
    (if pred ef0 ef1)
    (begin ef* ... ef)
    (let ([uv* val*] ...) ef)
    (eprim val* ...)
    (call val val* ...))
  (Prog (prog)
     (letrec ([l* le*] ...) val)))

(define-language LspecifyRepresentation 
  (extends LnormalizeContext) 
  (entry Prog)
  (terminals
    (- (value-prim (vprim))
       (effect-prim (eprim))
       (pred-prim (pprim)))
    (+ (frame-var (fv))
       (rop (relop))
       (binop (op))
       (register (r))))
  (Triv (triv) (+ uv i l))
  (Tail (tl)
    (+ triv (alloc val) (mref val0 val1) (prim op val0 val1)
       (let ([uv* val*] ...) tl) (if pred tl0 tl1)
       (begin ef* ... tl) (call val val* ...)))
  (Prog (p)
    (- (letrec ([l* le*] ...) val))
    (+ (letrec ([l* le*] ...) tl)))
  (Effect (ef)
    (- (eprim val* ...))
    (+ (set! uv val) (mset! val0 val1 val2)))
  (Pred (pred)
    (- (pprim val* ...))
    (+ (prim relop val0 val1)))
  (Value (val)
    (- (vprim val* ...) 'i uv l)
    (+ (prim op val0 val1) (mref val0 val1) (alloc val) triv))
  (LambdaExpr (le)
    (- (lambda (uv* ...) val))
    (+ (lambda (uv* ...) tl))))

  (define-language LuncoverLocals
    (extends LspecifyRepresentation)
    (entry Prog)
    (Prog (prog)
      (- (letrec ([l* le*] ...) tl))
      (+ (letrec ([l* le*] ...) bd)))
    (LambdaExpr (le)
      (- (lambda (uv* ...) tl))
      (+ (lambda (uv* ...) bd)))
    (Body (bd)
      (+ (locals (uv* ...) tl)))) 

 (define-language LremoveLet
    (extends LuncoverLocals)
    (entry Prog)
    (Value (val)
      (- (let ([uv* val*] ...) val)))
    (Effect (ef)
      (- (let ([uv* val*] ...) ef))
      )
    (Tail (tl) 
      (- (let ([uv* val*] ...) tl)))
    (Pred (pred)
      (- (let ([uv* val*] ...) pred))))   

  (define-language LremoveComplexOpera*
    (extends LremoveLet)
    (entry Prog)
    (Tail (tl) (- (prim op val0 val1)
                  (mref val0 val1)
                  (alloc val)
                  (call val val* ...))
               (+ (prim op triv0 triv1)
                  (mref triv0 triv1)
                  (alloc triv)
                  (call triv0 triv* ...)))
    (Effect (ef) (- (call val val* ...)
                     (mset! val0 val1 val2))
                 (+ (call triv triv* ...)
                    (mset! triv0 triv1 triv2)))
    (Pred (pred) (- (prim relop val0 val1))
                 (+ (relop triv0 triv1)))
    (Value (val) (- (prim op val0 val1) (call val val* ...)
                    (mref val0 val1)
                    (alloc val))
                 (+ (prim op triv0 triv1) (call triv triv* ...)
                     (mref triv0 triv1)
                     (alloc triv))))

  (define-language LflattenSet!
    (extends LremoveComplexOpera*)
    (entry Prog)
    (Effect (ef) (- (set! uv val))
                 (+ (set! uv rhs)))
    (Value (val) (- (prim op triv0 triv1) 
                   (call triv triv* ...)
                   (mref triv0 triv1)
                   (alloc triv)
                    triv 
                    (if pred val0 val1)
                    (begin ef* ... val)))
    (Rhs (rhs) (+ triv (op triv0 triv1) 
                 (call triv triv* ...)
                 (alloc triv)
                 (mref triv0 triv1))))

  (define-language LimposeCallingConventions
    (extends LflattenSet!)
    (entry Prog)
    (Body (bd) (- (locals (uv* ...) tl))
               (+ (locals (uv* ...) nftl)))
    (NewFrameTail (nftl) (+ (new-frames ((uv* ...) ...) tl)))
    (LambdaExpr (le) (- (lambda (uv* ...) bd))
                     (+ (lambda () bd)))
    (Tail (tl) (- triv (call triv0 triv* ...) (prim op triv0 triv1))
               (+ (triv locrf* ...)))
    (Triv (triv) (- uv)
                 (+ v))
    (Effect (ef) (- (set! uv rhs) 
                    (call triv triv* ...))
                 (+ (set! v rhs) 
                   (return-point l tl)))
    (Var (v) (+ uv locrf))
    (Loc (locrf) (+ r fv uv))
    (Rhs (rhs) (- (call triv triv* ...))))

  (define-language LexposeAllocationPointer
    (extends LimposeCallingConventions)
    (entry Prog)
    (Rhs (rhs)
      (- (alloc triv))))

  (define-language LuncoverFrameConflict 
    (extends LexposeAllocationPointer)
    (entry Prog)
    (terminals        (+ (conflict-graph (cfgraph))))
    (NewFrameTail (nftl) (- (new-frames ((uv* ...) ...) tl))
                         (+ (new-frames ((uv* ...) ...) stl)))
    (SpillsTail (stl)     (+ (spills (uv* ...) fbd)))
    
    (CallLiveBody (cbd) (+ (call-live (uv* ...) tl)))
    (FrameBody (fbd)  (+ (frame-conflict cfgraph cbd))))

  (define-language LpreAssignFrame
    (extends LuncoverFrameConflict)
    (entry Prog)
    (SpillsTail (stl)     (- (spills (uv* ...) fbd)))
    (NewFrameTail (nftl) (- (new-frames ((uv* ...) ...) stl))
                         (+ (new-frames ((uv* ...) ...) lbd)))
    (Lbody (lbd) (+ (locate ([uv* locrf*] ...) fbd))))

  (define-language LassignNewFrame
    (extends LpreAssignFrame)
    (entry Prog)
    (NewFrameTail (nftl)
      (- (new-frames ((uv* ...) ...) lbd)))
    (CallLiveBody (cbd)
      (- (call-live (uv* ...) tl)))
    (FrameBody (fbd)
      (- (frame-conflict cfgraph cbd))
      (+ (frame-conflict cfgraph tl)))
    (Body (bd)
      (- (locals (uv* ...) nftl))
      (+ (locals (uv* ...) ubd)
         (locate ([uv* locrf*] ...) tl)))
    (Ubody (ubd)  
      (+ (ulocals (uv* ...) lbd))))

  (define-language LuncoverRegisterConflict 
    (extends LassignNewFrame)
    (entry Prog)
    (FrameBody (fbd)        (- (frame-conflict cfgraph tl))
                            (+ (frame-conflict cfgraph rbd)))
    (RegisterBody (rbd) (+ (register-conflict cfgraph tl))))

  (define-language LassignRegisters 
    (extends LuncoverRegisterConflict)
    (entry Prog)
    (FrameBody (fbd)    (- (frame-conflict cfgraph rbd))
                        (+ (frame-conflict cfgraph tl)))
    (Ubody (ubd)        (- (ulocals (uv* ...) lbd))
                        (+ (ulocals (uv* ...) sbd)))
    (SpillBody (sbd)    (+ (spills (uv* ...) lbd)))
    (RegisterBody (rbd) (- (register-conflict cfgraph tl))))

  (define-language LdiscardCallLive
    (extends LassignRegisters)
    (entry Prog)
    (Body (bd) (- (locals (uv* ...) ubd)))
    (Ubody (ubd) (- (ulocals (uv* ...) sbd)))
    (SpillBody (sbd) (- (spills (uv* ...) lbd)))
    (Lbody (lbd) (- (locate ((uv* locrf*) ...) fbd)))
    (FrameBody (fbd) (- (frame-conflict cfgraph tl)))
    (Loc (locrf) (- uv))

    (Tail (tl)      (- (triv locrf* ...))
                    (+ (triv))))

  (define-language LfinalizeLocations 
    (extends LdiscardCallLive)
    (entry Prog)
    (Prog (p)        (- (letrec ([l* le*] ...) bd))
                     (+ (letrec ([l* le*] ...) tl)))
    (LambdaExpr (le) (- (lambda () bd))
                     (+ (lambda () tl)))
    (Body (bd)       (- (locate ((uv* locrf*) ...) tl)
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

  (define-language LexposeMemoryOperands
    (extends LexposeFrameVar)
    (entry Prog)
    (Rhs (rhs)
      (- (mref triv0 triv1)))
    (Effect (ef)
      (- (mset! triv0 triv1 triv2))))
  (define-language LexposeBasicBlocks
    (extends LexposeMemoryOperands)
    (entry Prog)
    (Tail (tl)    (- (if pred tl0 tl1))
                  (+ (if (relop triv0 triv1) (l0) (l1))))
    (Effect (ef)  (- (if pred ef0 ef1)
                     (return-point l tl)
                     ;(begin ef* ... ef)
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
