(library (Compiler expose-memory-operands)
  (export expose-memory-operands)
  (import (chezscheme)
    (source-grammar)
    (Framework nanopass)
    (Framework helpers))

;;The purpose of this pass is to calculate the displacement of msets and mrefs, and replace them with displacement operands.
  (define-pass expose-memory-operands : LexposeFrameVar (x) -> LexposeMemoryOperands ()
  
    (Effect : Effect (x) -> Effect ()
	    [(mset! ,triv0 ,triv1 ,triv2) (if (and (register? triv0) (register? triv1))			      
					      `(set! ,(make-index-opnd triv0 triv1) ,triv2)
					      `(set! ,(make-disp-opnd triv0 triv1) ,triv2))]
	    [(set! ,locrf (mref ,triv0 ,triv1)) (if (and (register? triv0) (register? triv1))
						    `(set! ,locrf ,(make-index-opnd triv0 triv1))
						    `(set! ,locrf ,(make-disp-opnd triv0 triv1)))])))
 