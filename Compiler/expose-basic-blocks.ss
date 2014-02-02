
(library (Compiler expose-basic-blocks)
         (export expose-basic-blocks)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers)
          (Framework match))






         (define-pass expose-basic-blocks : LexposeFrameVar (x) -> LexposeBasicBlocks ()
           (definitions
             
                            
             (define make-basic-block
               (lambda (label expr)
                 `((,label (lambda () ,expr))))))
   
            (Prog : Prog (x) -> * ()
                  [(letrec ([,l* (lambda () ,tl1)] ...) ,tl2)
                   (let*-values ([(bnds* tail*) (Tail tl1)]
                                 [(bnd tail) (Tail tl2)])
                     x)]
                  [else "blah"])
            (Tail : Tail (x) -> * (bb b*)
                  [(if ,pred ,tl1 ,tl2) x]
                  [(begin ,ef* ... ,tl)
                   x]	
                  [(,triv) (values '() `(,triv))])
             
            (Effect : Effect (expr before* after*) -> * (bb b*)
                    [(if ,pred0 ,ef1 ,ef2)
                     (let ((c-label (unique-label 'c))
                           (a-lab (unique-label 'a))
                           (jmp-label (unique-label 'j)))
                      
                      x)]
                    [(set! ,locrf ,rhs) x]
                    [(begin ,ef* ... ,ef)
                     x]
                    [(nop) x])
  
            (Pred : Pred (x true-label false-label) -> * (bb b*)
                  [(true) x]
                  [(false) x]
                  [(begin ,ef* ... ,pred)
                   (let*-values
                       ([(p-bnds p-exprs) (Pred pred true-label false-label)])
                     
                     x)]
                  [(,relop ,triv1 ,triv2) (values '() `(if ,exp (,true-label) (,false-label)))]
                  [(if ,pred0 ,pred1 ,pred2)
                   (let ((c-label (unique-label 'c))
                         (a-lab (unique-label 'a)))
                   
                       x)])))