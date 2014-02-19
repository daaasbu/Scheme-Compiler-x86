(library (Compiler introduce-allocation-forms)
         (export introduce-allocation-forms parse-LintroduceAllocationForms)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LintroduceAllocationForms LintroduceAllocationForms)

         (define-pass introduce-allocation-forms : LuncoverFrameConflict (x) -> LintroduceAllocationForms ()
           (Body : Body (x) -> Body ()
                 [(locals (,uv* ...) ,[fbd])
                  `(locals (,uv* ...) (ulocals () (locate () ,fbd)))]
                 [else (error who "something went wrong - Body")]))
) ;End Library