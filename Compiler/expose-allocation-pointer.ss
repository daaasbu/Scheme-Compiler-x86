;;expose-allocation-pointer
(library (Compiler expose-allocation-pointer)
         (export expose-allocation-pointer)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

	 ;;Goes from LimposeCallingConventions to LexposeAllocationPointer. This passes main purpose is to remove alloc from our language by setting whatever was being set to alloc, and then we bump the allocation pointer by the amount          ;;of space that we wanted to allocate.
         (define-pass expose-allocation-pointer : LimposeCallingConventions (x) -> LexposeAllocationPointer ()
	   (Effect : Effect (x) -> Effect ()
		   [(set! ,v (alloc ,triv)) (let ((ef1 `(set! ,v ,allocation-pointer-register))
						  (ef2 `(set! ,allocation-pointer-register (+ ,allocation-pointer-register ,triv))))
		     
					      `(begin ,ef1 ,ef2))]))) ;End Library