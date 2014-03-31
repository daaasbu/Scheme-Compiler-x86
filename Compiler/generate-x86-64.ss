
(library (Compiler generate-x86-64)
         (export generate-x86-64)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

	 ;;generate-x86-64, goes from LflattenProgram to assembly code. Uses emit-program to give boilerplate code, for-each to process each effect, and emit to place
	 ;; each line of assembly code. Has the helper bin->assem to translate binary operators to their assembly counterparts.

	 (define-pass generate-x86-64 : LflattenProgram (x) -> * ()
	   (definitions

	     (define prim->opcode
	       (lambda (prim)
		 (cdr (assq prim
			    '((+ . addq) (- . subq) (* . imulq)
			      (logand . andq) (logor . orq) (sra . sarq))))))

	     (define relop->opcode
	       (lambda (relop not?)
		 (cdr (assq relop (if not?
				      '((= . jne) (< . jge) (<= . jg) (> . jle) (>= . jl))
				      '((= . je) (< . jl) (<= . jle) (> . jg) (>= . jge))))))))
	   
	   (Code : Code (x) -> * ()
		 [(set! ,locrf1 (,op ,locrf2 ,i))
		  (emit (prim->opcode op) i locrf1)]
		 [(set! ,locrf1 (,op ,locrf2 ,locrf3))
		  (emit (prim->opcode op) locrf3 locrf1)]
		 [(set! ,locrf ,i)
		  (emit 'movq i locrf)]
		 [(set! ,locrf1 ,locrf2)
		  (emit 'movq locrf2 locrf1)]
		 [(set! ,locrf ,l)
		  (guard (label? l))
		  (emit 'leaq l locrf)]
		 [(jump ,triv) (emit-jump 'jmp triv)]
		 [,l (guard (label? l)) (emit-label l)]
		 [(if (not (,relop ,triv1 ,triv2)) (jump ,triv))
		  (emit 'cmpq triv2 triv1)
		  (emit-jump (relop->opcode relop #t) triv)]
		 [(if (,relop ,triv1 ,triv2) (jump ,triv))
		  (emit 'cmpq triv2 triv1)
		  (emit-jump (relop->opcode relop #f) triv)]
		 [else (error who "invalid input to assembly generation ~s" x)])

	   (Prog : Prog (x) -> * ()
		 [(code ,c* ...) (emit-program (for-each Code c*))]))
	 )