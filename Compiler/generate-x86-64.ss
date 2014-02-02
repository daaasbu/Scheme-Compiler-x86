;;generate-x86-64, goes from LflattenProgram to assembly code. Uses emit-program to give boilerplate code, for-each to process each effect, and emit to place
;; each line of assembly code. Has the helper bin->assem to translate binary operators to their assembly counterparts.
(library (Compiler generate-x86-64)
         (export generate-x86-64)
         (import
          (source-grammar)
          (chezscheme)
          (Framework nanopass)
          (Framework helpers))


(define-pass generate-x86-64 : LflattenProgram (x) -> * ()
  (definitions
    (define bin->assem
      (lambda (op)
        (cond
          [(eq? op '+) 'addq]
          [(eq? op '-) 'subq]
          [(eq? op '*) 'imulq]
          [(eq? op 'logand) 'andq]
          [(eq? op 'logor) 'orq]
          [(eq? op 'sra) 'sarq]))))
         
  (Prog : Prog (x) -> * ()
        [(code ,c* ...) (emit-program (for-each (lambda (x) (Code x)) c*))])
  (Code : Code (x) -> * ()
        [(jump ,triv) (emit-jump 'jmp triv)]
        [,l (emit-label l)]
        [(set! ,locrf ,l) (emit 'leaq l locrf)]
        [(set! ,locrf ,i) (emit 'movq i locrf)]
        [(set! ,locrf1 ,locrf2) (emit 'movq locrf2 locrf1)]
        [(set! ,locrf (,op ,locrf1 ,i)) (emit (bin->assem op) i locrf1)]
        [(set! ,locrf (,op ,locrf1 ,locrf2)) (emit (bin->assem op) locrf2 locrf1)]
        [else (error who "Failure in code")])))


