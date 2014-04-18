
(library (Compiler discard-call-live)
         (export discard-call-live parse-LdiscardCallLive)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))
;;The purpose of this pass is to get rid of the list of locrf* in each tail, because from this point on all our calls are functions of no variables.
         (define-parser parse-LdiscardCallLive LdiscardCallLive)

         (define-pass discard-call-live : LassignRegisters (x) -> LdiscardCallLive ()
           (Tail : Tail (x) -> Tail ()
                 [(,triv ,locrf* ...) `(,triv)])




)
     
) ;End Library