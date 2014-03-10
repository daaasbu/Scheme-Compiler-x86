;;expose-allocation-pointer
(library (Compiler expose-allocation-pointer)
         (export expose-allocation-pointer)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         ;(define-parser parse-LexposeAllocationPointer )

         (define-pass expose-allocation-pointer : LimposeCallingConventions (x) -> LexposeAllocationPointer ()


	   (Effect : Effect (x) -> Effect ()
		   [(set! ,v (alloc ,triv)) (let ((ef1 `(set! ,v ,allocation-pointer-register))
						  (ef2 `(set! ,allocation-pointer-register (+ ,allocation-pointer-register ,triv))))
		     
					      `(begin ,ef1 ,ef2))])
	   

#|
	   (Tail : Tail (x) -> Tail ()
		 [(alloc ,triv) (let* (ef `(set! ,allocation-pointer-register (+ ,allocation-pointer-register ,triv)))]
		 [(,triv ,locrf* ...) `(,triv ,locrf* ...)]
		 [(mref ,triv0 ,triv1) `(mref ,triv0 ,triv1)]
		 [(if ,[pred] ,[tl0] ,[tl1]) `(if ,pred ,tl0 ,tl1)]
		 [(begin ,[ef*] ... ,[tl]) `(begin ,ef* ... ,tl)]
		 [else (error who "In Tail")])
|#


)
     
) ;End Library