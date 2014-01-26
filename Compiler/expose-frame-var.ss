(library (Compiler expose-frame-var)
         (export expose-frame-var)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         
          (define make-frame-var
            (lambda (x)
              (make-disp-opnd 'rbp (* 8 (frame-var->index x)))))

         (define-parser parse-LexposeFrameVar LexposeFrameVar)

         (define-pass expose-frame-var : LverifyScheme (x) -> LexposeFrameVar ()
           (Var : Var (x) -> Var ()
                [,fv (make-frame-var fv)]))
) 
  