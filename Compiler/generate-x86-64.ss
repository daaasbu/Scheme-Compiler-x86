(library (Compiler generate-x86-64)
         (export generate-x86-64)
         (import
          (source-grammar)
          (chezscheme)
          (Framework driver)
          (Framework wrappers)
          (Framework nanopass)
          (Framework helpers))


(define-pass generate-x86-64 : LflattenProgram (x) -> * ()
  (Prog : Prog (x) -> * ()
[(code ,c* ...) (emit-program (for-each (lambda (x) (Code x)) c*))])
  (Code : Code (x) -> * ()
[(jump ,triv) (emit-jump 'jmp triv)]
[,l (emit-label l)]
[(set! ,v ,i) (emit 'movq i v)]
        [(set! ,v1 ,v2) (emit 'movq v2 v1)]
        [(set! ,v (,op ,v1 ,i)) (emit (bin op) i v1)]
        [(set! ,v (,op ,v1 ,v2)) (emit (bin op) v2 v1)]
[else (error who "Fail")]))


(define bin
  (lambda (o)
    (cond
     [(eq? o '+) 'addq]
     [(eq? o '-) 'subq]
     [(eq? o '*) 'imulq]
     [(eq? o 'logand) 'andq]
     [(eq? o 'logor) 'orq]
     [(eq? o 'sra) 'sarq])))

)
         