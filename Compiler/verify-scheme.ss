;;select-instructions, takes our subset of scheme consisting of mainly letrecs, effects,registers,frame-vars, labels and lambda expressions, and goes through a series of tests that targets a certain machine.These tests could change depending on the target of our compiler.  Goes from LselectInstructions to LselectInstructions.
;;
(library (Compiler select-instructions)
         (export select-instructions parse-LselectInstructions)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))
;;BINOP1
(define 


         (define-parser parse-LselectInstructions LselectInstructions)

         (define-pass select-instructions : LselectInstructions (x) -> LselectInstructions ()
           (Tail : Tail (x) -> Tail ()
                 [(begin ,ef* ... ,tl1)   `(begin ,(map (lambda (x) (Effect x)) ef*) ... ,(Tail tl1  ))]
                 [(,triv ,locrf* ...)
;                  (unless (or (register? triv) (label? triv) (frame-var? triv))
;                      (error who "triv must be a label or variable" triv))
                  (if (or (uvar? triv) (label? triv))
                      (unless (lookup triv ) (error who "unbound label"))) x]
                 [(if ,pred ,tl1 ,tl2) (Pred pred) (Tail tl1) (Tail tl2) x])
           (Pred : Pred (x  ) -> Pred ()
                 [(true) x]
                 [(false) x]
                 [(,relop ,triv1 ,triv2)
                  (if (or (label? triv1) (uvar? triv1))
                      (unless (lookup triv1 ) (error who "not in ")))
                  (if (or (label? triv2) (uvar? triv2))
                      (unless (lookup triv2 ) (error who "not in ")))
;                  (unless (or (not (frame-var? triv1)) (not (frame-var? triv2)))
;                          (error who "Two frame vars while using a relop"))
;                  (if (integer? triv1)
;                      (unless (int32? triv1) (error who "must use a 32 bit integer")))
;                  (if (integer? triv2)
;                      (unless (int32? triv2) (error who "must use a 32 bit integer")))
;                  (if (and (integer? triv1) (register? triv2))
;                      (error who "int32 must be the first arg to cmpq"))
                  x]
                 [(if ,pred1 ,pred2 ,pred3) (Pred pred1  ) (Pred pred2  ) (Pred pred3  ) x]
                 [(begin ,ef* ... ,pred) `(begin ,(map (lambda (x) (Effect x  )) ef*) ... ,(Pred pred))])
           (Effect : Effect (x  ) -> Effect ()
                   [(set! ,v ,triv)
                    (if (uvar? v)  (unless (lookup v ) (error who "not in ")))
                    (if (uvar? triv)  (unless (lookup triv ) (error who "not in ")))
;                    (if (and (frame-var? v) (frame-var? triv))
;                        (error who "Can't set! a frame variable to a frame variable" v triv))
;                    (if (label? triv)
;                        (unless (or (register? v) (label? v) (uvar? v)) ;;;;;changed this in a4 -> or label?/uvar?
;                                (error who "If triv is a label, then var must be a register" v triv)))
;                    (if (or (int32? triv) (int64? triv))
;                        (unless (or (int32? triv) (and (register? v) (int64? triv)))
;                                (error who "Triv not valid" triv)))
                    x]
                   [(set! ,v (,op ,triv1 ,triv2))
                    (if (uvar? v)  (unless (lookup v ) (error who "not in ")))
                    (if (uvar? triv1)  (unless (lookup triv1 ) (error who "not in ")))
                    (if (uvar? triv2)  (unless (lookup triv2 ) (error who "not in ")))
;                    (unless (eqv? v triv1)
;                            (error who "LHS must match" v triv1))
;                    (if (label? triv1)
;                        (error who "Triv1 must be a label" triv1)
;                        (if (label? triv2)
;                            (error who "Triv2 must be a label" triv2)))
;                    (if (and (frame-var? triv1) (frame-var? triv2))
;                        (error who "Can't have two frame variables" triv1 triv2))
;                    (if (eqv? op `*)
;                        (unless (or (register? v) (uvar? v))
;                                (error who "When using * the variable must be a register" v)))
                    (if (eqv? op `sra)
                        (unless (and (<= 0 triv2) (>= 63 triv2))
                                (error who "sra out of bounds" triv2)))
 ;                   (unless (or (register? triv1) (frame-var? triv1) (label? triv1) (uvar? triv1))
 ;                           (unless (int32? triv1)
 ;                                   (error who "int triv1 is out of bounds")))
 ;                   (unless (or (register? triv2) (frame-var? triv2) (label? triv2) (uvar? triv2))
 ;                           (unless (int32? triv2)
 ;                                   (error who "int triv2 is out of bounds")))
                    x]
                   [(if ,pred ,ef1 ,ef2) (Pred pred  ) (Effect ef1  ) (Effect ef2) x]
                   [(begin ,ef* ... ,ef1) `(begin ,(map (lambda (x) (Effect x  )) ef*) ... ,(Effect ef1  ))]
                   [(nop) x]))
) 