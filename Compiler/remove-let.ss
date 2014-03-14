(library (Compiler remove-let)
  (export remove-let)
  (import
    (chezscheme)
    (source-grammar)
    (Framework helpers)
    (Framework nanopass))

;; Midterm TODO: Complete this pass definition.



(define-pass remove-let : LuncoverLocals (x) -> LremoveLet ()
  ; (Prog : Prog (x) -> Prog ()
  ;   [(letrec ([,l* ,[le*]] ...) ,bd) (error who "Fill in this pass.")])

  ;; You might find yourself copy-pasting here.
  (Value : Value (x) -> Value ()
    [(let ([,uv* ,[val*]] ...) ,[val]) 
     (let ((ef* (map (lambda (uv val) (in-context Effect `(set! ,uv ,val))) uv* val*)))
       `(begin ,ef* ... ,val))]
    )
  (Tail : Tail (x) -> Tail () 
    [(let ([,uv* ,[val*]] ...) ,[tl]) 
     (let ((ef* (map (lambda (uv val) (in-context Effect `(set! ,uv ,val))) uv* val*)))
       `(begin ,ef* ... ,tl))])
   
  (Pred : Pred (x) -> Pred ()
     [(let ([,uv* ,[val*]] ...) ,[pred]) 
     (let ((ef* (map (lambda (uv val) (in-context Effect `(set! ,uv ,val))) uv* val*)))
       `(begin ,ef* ... ,pred))])
  (Effect : Effect (x) -> Effect ()
      [(let ([,uv* ,[val*]] ...) ,[ef]) 
     (let ((ef* (map (lambda (uv val) (in-context Effect `(set! ,uv ,val))) uv* val*)))
       `(begin ,ef* ... ,ef))])
  ))
