#|(library (Compiler impose-calling-conventions)
         (export impose-calling-conventions parse-LimposeCallingConventions)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LimposeCallingConventions LimposeCallingConventions)
; Helpers:
; parameter-registers, frame-pointer-register, return-value-register, and return-address-register
 

;;This pass goes from LflattenSet! to LimposeCallingConventions. Its purpose is to make our code in the form that uncover register conflicts is expect. 
;;So it goes through tail calls, and replace everything with frame-variables and parameter registers.


	 (define-pass impose-calling-conventions : LflattenSet! (x) -> LimposeCallingConventions ()
	   (definitions

#|	     
	     (define rp (void))

	     (define rp2 (void))

	     (define make-rp2
	       (lambda ()
		 (begin (set! rp2 (unique-name 'RP)) rp2)))

	     (define make-rp
	       (lambda ()
		 (begin (set! rp (unique-name 'RP)) rp)))
|#

	     (define make-rp
	       (lambda ()
		 (unique-name 'RP)))

	     (define make-set!-block
	       (lambda (uv* pr* n rp)
		 (cond
		  [(null? uv*) '()]
		  [(null? pr*) (let ((fv (index->frame-var n)) (first (car uv*))) (cons (with-output-language (LimposeCallingConventions Effect) `(set! ,first ,fv)) (make-set!-block (cdr uv*) pr* (add1 n) rp)))]
		  [else (cons (with-output-language (LimposeCallingConventions Effect) `(set! ,(car uv*) ,(car pr*))) (make-set!-block (cdr uv*) (cdr pr*) n rp))])))

	     (define input* '())

	     (define make-call-block
	       (lambda (triv* pr* n triv*-len pr-len rp)
	 (let ((fv-num (- triv*-len pr-len)))
		   (cond
		    [(null? triv*) (list (with-output-language (LimposeCallingConventions Effect) `(set! ,return-address-register ,rp)))]
		    [(and (> fv-num 0) (< n fv-num)) (begin (set! input* (cons (index->frame-var n) input*)) (cons (with-output-language (LimposeCallingConventions Effect) 
													       `(set! ,(index->frame-var n) ,(car triv*))) (make-call-block (cdr triv*) pr* (add1 n) triv*-len pr-len rp)))]
		    [else (begin (set! input* (cons (car pr*) input*)) (cons (with-output-language (LimposeCallingConventions Effect) `(set! ,(car pr*) ,(car triv*))) (make-call-block (cdr triv*) (cdr pr*) n triv*-len pr-len rp)))]))))
      


)
		 

	     
	  (Prog : Prog (x) -> Prog ()
		[(letrec ([,l* ,le*] ...) (locals (,uv* ...) ,tl)) (let ((rp (make-rp))) (begin  `(letrec ([,l* ,(map (lambda (x) (LambdaExpr x (make-rp))) le*)] ...) (locals (,uv* ... ,rp) (begin (set! ,rp ,return-address-register) ,(Tail tl rp))))))])


	  (LambdaExpr : LambdaExpr (x rp) -> LambdaExpr ()
		      [(lambda (,uv0* ...) (locals (,uv1* ...) ,tl)) 
		       `(lambda () (locals (,rp ,uv0* ...  ,uv1* ...) (begin ,(cons `(set! ,rp ,return-address-register) (make-set!-block uv0* parameter-registers 0 rp)) ... ,(Tail tl rp))))])


	  (Tail : Tail (x rp) -> Tail ()
		[,triv  `(begin (set! ,return-value-register ,triv) (,rp ,frame-pointer-register ,return-value-register)) ]
		[(call ,triv0 ,triv* ...)(begin  (set! input* '())  `(begin ,(make-call-block triv* parameter-registers 0 (length triv*) (length parameter-registers) rp) ... (,triv0 ,return-address-register ,frame-pointer-register ,input* ...)))]
		[(prim ,op ,[triv0] ,[triv1]) `(begin (set! ,return-value-register (,op ,triv0 ,triv1)) (,rp ,frame-pointer-register ,return-value-register)) ]
		[(if ,[pred0] ,tl0 ,tl1) `(if ,pred0 ,(Tail tl0 rp) ,(Tail tl1 rp))]
		[(begin ,[ef*] ... ,tl) `(begin ,ef* ... ,(Tail tl rp))])

	  (Effect : Effect (x) -> Effect ()

		  [(set! ,uv ,[rhs]) `(set! ,uv ,rhs) ]


) 
))


	 
|#



(library (Compiler impose-calling-conventions)
  (export impose-calling-conventions)
  (import (chezscheme)
    (Framework nanopass)
    (Framework helpers)
    (source-grammar))

  ;;impose-calling-conventions takes a flattened(set!)
  ;;language and adds in initial information about registers
  ;;ie
  ;; return, frame-pointer, return-value
  ;;
  ;;it does this by storing values that need to be "passed"
  ;;before a function call and unpacking them at the beginning
  ;;of a function
  (define-pass impose-calling-conventions : LflattenSet! (x)
    -> LimposeCallingConventions ()

   (definitions
     (define ar '())
     (define afl '())

     (define find-max
       (lambda (ls)
         (cond
           [(null? ls) 0]
           [(null? (cdr ls)) (car ls)]
           [else (max (car ls) (find-max (cdr ls)))])))

     (with-output-language (LimposeCallingConventions Effect)
       (define (assign-parameters uv)
         (cond
           [(< (length ar) (length parameter-registers))
            (let ([binding (car (difference parameter-registers ar))])
              (begin (set! ar (cons binding ar))
                     `(set! ,uv ,binding)))]
           [else
             (let* ([max-val (find-max
                               (map (lambda (z)
                                      (if (frame-var? z)
                                        (frame-var->index z)
                                        -1))
                                    afl))]
                    [binding (index->frame-var (+ 1 max-val))])
               (begin (set! afl (cons binding afl))
                      `(set! ,uv ,binding)))]))
       (define (assign-parameters^ uv)
         (cond
           [(< (length ar) (length parameter-registers))
            (let ([binding (car (difference parameter-registers ar))])
              (begin (set! ar (cons binding ar))
                     `(set! ,binding ,uv)))]
           [else
             (let* ([max-val (find-max
                               (map (lambda (z)
                                      (if (frame-var? z)
                                        (frame-var->index z)
                                        -1))
                                    afl))]
                    [binding (index->frame-var (+ 1 max-val))])
               (begin (set! afl (cons binding afl))
                      `(set! ,binding ,uv)))]))
       (define store-return-value
         (lambda (rhs)
           `(set! ,return-value-register ,rhs)))
      
     )
    (with-output-language (LimposeCallingConventions Tail)
      (define (make-begin ef* tl)
        `(begin ,ef* ... ,tl))
    ))

   (Prog : Prog (p) -> Prog ()
     [(letrec ([,l* ,[le*]] ...) ,[bd '() -> bd])
      `(letrec ([,l* ,le*] ...) ,bd)]
   )

   (LambdaExpr : LambdaExpr (le) -> LambdaExpr ()
     [(lambda (,uv* ...) ,[bd uv* -> bd]) `(lambda () ,bd)])

   (Body : Body (bd fml*) -> Body ()
     [(locals (,uv* ...) ,tl)
      (let* ([return (unique-name 'return)]
             [assignments (map assign-parameters fml*)]
             [begin-expr
              (cons (in-context Effect
                      `(set! ,return ,return-address-register))
                assignments)])
      (begin (set! ar '()) (set! afl '())
        `(locals (,(append (list return) fml* uv*) ...)
          ,(make-begin begin-expr (Tail tl return)))))]
   )

   (Tail : Tail (tl return) -> Tail ()
     [,triv
      (let ([rval (store-return-value triv)]
            [rexpr `(,return ,return-value-register
                             ,frame-pointer-register)])
        `(begin ,(list rval) ... ,rexpr))]
     [(prim ,op ,triv0 ,triv1)
      (let ([ef* (list (store-return-value (in-context Rhs `(,op ,triv0 ,triv1))))]
            [tl `(,return ,return-value-register
                             ,frame-pointer-register)])
        `(begin ,ef* ... ,tl))]
     [(call ,triv ,triv* ...)
      (let* ((formal-assignments (map assign-parameters^ triv*))
             (return-exp (in-context Effect `(set! ,return-address-register ,return)))
             (locrf*
               (append (list return-address-register frame-pointer-register)
                 (union ar afl)))
             (tl `(,triv ,locrf* ...)))
        (begin (set! ar '()) (set! afl '())
          `(begin ,(append formal-assignments (list return-exp)) ... ,tl)))]
     [(if ,[pred] ,[tl return -> tl] ,[tl1 return -> tl1]) `(if ,pred ,tl ,tl1)]
     [(begin ,[ef*] ... ,[tl return -> tl]) `(begin ,ef* ... ,tl)])
     

   (Effect : Effect (ef) -> Effect ()
     [(set! ,uv ,[rhs]) `(set! ,uv ,rhs)]
   )

  )
) ;; end of library
