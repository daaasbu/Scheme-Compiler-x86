(library (source-grammar)
  (export
    
    unparse-LverifyScheme
    
    LverifyScheme
  )
  (import 
    (Framework nanopass)
    (Framework match)
    (Framework helpers)
    (chezscheme))
  ;; P423
  ;; Week 1 grammars
  ;;
  ;; Passes:
  ;; * verify-scheme
  ;; * generate-x86-64


  (define binop? 
    (lambda (x)
      (if (memq x '(+ - *))
        #t #f)))

  (define immediate? int64?)

  (define-language LverifyScheme
    (entry Prog)
    (terminals 
      (register (r))
      (binop (op))
      (immediate (i)))
    (Prog (p) (begin s s* ...))
    (Statement (s) (set! v rhs))
    (Rhs (rhs) i v (op v arg))
    (Argument (arg) i v)
    (Var (v) r))
  )


;;(p423-grammars
;;  (l01-verify-scheme
;;    (start Prog)
;;    (Prog
;;      (begin Statement * Statement))
;;    (Statement
;;      (set! Var Integer)
;;      (set! Var Var)
;;      (set! Var (Binop Var Integer))
;;      (set! Var (Binop Var Var)))
;;    (Var Reg)))
