;;expose-frame-var, goes from LverifyScheme to LexposeFrameVar. Takes our subset of scheme in verify, and updates the frame-vars with the displacements.
;;Relatively simple pass, that makes use of make-dis-opnd. Doesn't change any other parts of the language beside frame-vars.

(library (Compiler expose-frame-var)
         (export expose-frame-var)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         

         (define-parser parse-LexposeFrameVar LexposeFrameVar)

         (define-pass expose-frame-var : LverifyScheme (x) -> LexposeFrameVar ()
           (definitions
             (define make-frame-var
               (lambda (x)
                 (make-disp-opnd 'rbp (* 8 (frame-var->index x))))))
           (Var : Var (x) -> Var ()
                [,fv (make-frame-var fv)]))) 
  