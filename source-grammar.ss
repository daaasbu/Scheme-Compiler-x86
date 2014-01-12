(library (source-grammar)
  (export
    
    unparse-LverifyScheme
    unparse-LexposeFrameVar 
    unparse-LflattenProgram
    
    LverifyScheme
    LexposeFrameVar 
    LflattenProgram
  )
  (import 
    (Framework nanopass)
    (Framework helpers)
    (chezscheme))
  ;; P423
  ;; Week 2 grammars
  ;;
  ;; Passes:
  ;; * verify-scheme
  ;; * expose-frame-var
  ;; * flatten-program
  ;; * generate-x86-64


  ;; Predicates for the define-language form
  (define (binop? x) (if (memq x '(+ - * logand logor sra)) #t #f))
  (define immediate? int64?)
  (define (location? x) (or (disp-opnd? x) (index-opnd? x)))

  (define-language LverifyScheme
     (entry Prog)
     (terminals 
       (register (r))
       (binop (op))
       (immediate (i))
       (frame-var (fv))
       (label (l)))
     ;; End terminals 
     (Prog (p)        (letrec ([l* le*] ...) tl))
     (LambdaExpr (le) (lambda () tl))
     (Tail (tl)       (triv)
                      (begin ef* ... tl))
     (Effect (ef)     (set! v rhs))
     (Rhs (rhs)       triv (op triv0 triv1))
     (Triv (triv)     i v l)
     (Var (v)         r fv))

  (define-language LexposeFrameVar 
    (extends LverifyScheme)
    (entry Prog)
    ;; Frame-vars are now gone. Instead they are represented as offsets from the
    ;; frame-pointer-register
    (terminals 
      (+ (location (loc)))
      (- (frame-var (fv))))
    (Var (v)
      (- fv)
      (+ loc)))

  (define-language LflattenProgram 
    (extends LexposeFrameVar)
    (entry Prog)
    (LambdaExpr (le)   (- (lambda () tl)))
    (Tail (tl)         (- (begin ef* ... tl) 
                          (triv)))
    (Code (c)          (+ (set! v rhs)
                          (jump triv)
                          l))
    
    (Prog (p)          (- (letrec ([l* le*] ...) tl))
                       (+ (code c* ...))))
  ) ;; end lib
