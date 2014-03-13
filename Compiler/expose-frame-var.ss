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

  (define-pass expose-frame-var : LfinalizeLocations (x) -> LexposeFrameVar ()
    (definitions
      (define fp?
	(lambda (r)
	  (equal? r frame-pointer-register)))

      (define flip-sign
	(lambda (op)
	  (case op
	    (+ -)
	    (- +))))


      (define offset 0)

      (define map^
	(lambda (proc ls)
	  (cond
	   ((null? ls) ls)
	   (else (let ((cell (proc (car ls)))) (cons cell (map^ proc (cdr ls))))))))
)
    (Tail : Tail (x) -> Tail ()
	  [(begin ,ef* ... ,tl) (let* ((ef* (map^ (lambda (x) (Effect x)) ef*)) (tl (Tail tl))) `(begin ,ef* ... ,tl))])
    
    (Pred : Pred (x) -> Pred ()
	  [(begin ,ef* ... ,pred) (let* ((ef* (map^ (lambda (x) (Effect x)) ef*)) (pred (Pred pred))) `(begin ,ef* ... ,pred))])


    (Effect : Effect (x) -> Effect ()
	    [(begin ,ef* ... ,ef) (let* ((ef* (map^ (lambda (x) (Effect x)) ef*)) (ef (Effect ef))) `(begin ,ef* ... ,ef))]
	    [(return-point ,l ,[tl offset -> tl]) `(return-point ,l ,tl)]
	    [(set! ,r (,op ,triv0 ,triv1)) (if (and (equal? r triv0) (fp? r) (integer? triv1)) (begin  (set! offset ((flip-sign op) offset triv1))  `(set! ,r (,op ,triv0 ,triv1))) `(set! ,r (,op ,(Triv triv0) ,(Triv triv1))))]) 

    

    (Triv : Triv (x) -> Triv ())

    (Loc : Loc (locrf) -> Loc ()
	 [,fv
	   (make-disp-opnd frame-pointer-register
					   (+ offset (ash (frame-var->index fv) word-shift)))]
	 [,r r]))
  )