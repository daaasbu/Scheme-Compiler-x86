(library (Compiler verify-scheme)
         (export verify-scheme parse-LverifyScheme)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

(define-parser parse-LverifyScheme LverifyScheme)

(define-pass verify-scheme : LverifyScheme (x) -> LverifyScheme ()
  (Prog : Prog (x) -> Prog ()
        [(begin ,[s] ,[s*] ...) x])
  (Statement : Statement (x) -> Statement ()
             [(set! ,v (,op ,v1 ,v2))
              (unless (eqv? v v1)
                (error who "Registers must match" v v1))]
             [(set! ,v (,op ,v1 ,i))
              (if (int32? i)
                  (if (and (register? v) register? v1)
                      (unless (eqv? v v1)
                        (error who "Registers must match" v v1))
                      (error who "Not a valid register" v v1))
                  (error who "Not an int64" i))]
             [(set! ,v1 ,v2) (if (and (register? v1) (register? v2)) x (error who "Not a valid register" v1 v2))]
             [(set! ,v ,i) (if (register? v) x (error who "Not a valid register" v))]))
)