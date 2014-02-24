(library (Compiler expose-frame-var)
  (export expose-frame-var)
  (import (chezscheme)
    (source-grammar)
    (Framework nanopass)
    (Framework helpers))

;;; expose-frame-var traverses the scheme source in the same grammar
;;; accepted by verify-scheme and changes frame-vars in the form
;;; fv0, fv1, etc. into explicit integer offsets from the register
;;; pointing to the frame-pointer register. To accomplish this,
;;; expose-frame-var makes use of make-disp-opnd which creates a
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

(define-pass expose-frame-var : LfinalizeLocations (x) -> LexposeFrameVar ()
  (Loc : Loc (locrf) -> Loc ()
    [,fv
     (make-disp-opnd frame-pointer-register
         (ash (frame-var->index fv) word-shift))]
    [,r r]))
)