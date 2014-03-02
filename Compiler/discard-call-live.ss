;discard-call-live
(library (Compiler discard-call-live)
         (export discard-call-live parse-LdiscardCallLive)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LdiscardCallLive LdiscardCallLive)

         (define-pass discard-call-live : LassignRegisters (x) -> LdiscardCallLive ()
           (Tail : Tail (x) -> Tail ()
                 [(,triv ,locrf* ...) `(,triv)])




)
     
) ;End Library