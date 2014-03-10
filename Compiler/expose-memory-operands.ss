(library (Compiler expose-memory-operands)
  (export expose-memory-operands)
  (import (chezscheme)
    (source-grammar)
    (Framework nanopass)
    (Framework helpers))

;;; expose-memory-operand traverses the scheme source in the same grammar
;;; accepted by verify-scheme and changes frame-vars in the form
;;; fv0, fv1, etc. into explicit integer offsets from the register
;;; pointing to the frame-pointer register. To accomplish this,
;;; expose-memory-operand makes use of make-disp-opnd which creates a
;;; displacement operand record expressing a register and fixed
;;; number displacment each displacement is the original frame var
;;; number multiplied by the word size (8 for 64-bit target machine)
;;; to get the byte offset.
;;; (i.e. fv0 => (make-disp-opnd frame-pointer-register 0)
;;; fv1 => (make-disp-opnd frame-pointer-register 8)
;;; fv2 => (make-disp-opnd frame-pointer-register 16)
;;; fv3 => (make-disp-opnd frame-pointer-register 24)
;;; ... well you get the idea.)
;;;
;;; Note: we use shift left by word-shift (3 for 64-bit target
;;; machine) to calculate the multiplication.

  (define-pass expose-memory-operands : LexposeFrameVar (x) -> LexposeMemoryOperands ()
  
    (Effect : Effect (x) -> Effect ()
	    [(mset! ,triv0 ,triv1 ,triv2) (if (integer? triv1)
					      `(set! ,(make-disp-opnd triv0 triv1) ,triv2)
					      `(set! ,(make-index-opnd triv0 triv1) ,triv2))]
	    [(set! ,locrf (mref ,triv0 ,triv1)) (if 
						 (integer? triv1)
						    `(set! ,locrf ,(make-disp-opnd triv0 triv1))
						    `(set! ,locrf ,(make-index-opnd triv0 triv1)))])))
 