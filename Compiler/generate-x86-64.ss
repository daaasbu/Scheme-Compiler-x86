(library (Compiler generate-x86-64)
         (export generate-x86-64)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))
(define bin->assem
  (lambda (x)
    (cond
      ((eq? x '+) 'addq)
      ((eq? x '-) 'subq)
      ((eq? x '*) 'imulq))))

(define-pass generate-x86-64 : LverifyScheme (x) -> * ()
  (Prog : Prog (x) -> * ()
        [(begin ,s ,s* ...) (emit-program (for-each (lambda (x) (Statement x)) (cons s s*)))])
         (Statement : Statement (x) -> * ()
                    [(set! ,v (,op ,v1 ,v2)) (emit (bin->assem op) v2 v1)]
                    [(set! ,v (,op ,v1 ,i)) (emit (bin->assem op) i v1)]
             [(set! ,v1 ,v2) (emit 'movq v2 v1)]
             [(set! ,v ,i) (emit 'movq i v)]
             [else (error who "Not a valid input")]))
)





         