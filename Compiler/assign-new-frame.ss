
(library (Compiler assign-new-frame)
         (export assign-new-frame parse-LassignNewFrame)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LassignNewFrame LassignNewFrame)

         (define-pass assign-new-frame : LpreAssignFrame (x) -> LassignNewFrame ()
           (definitions
	     
	     (define if-not-null-zero
	       (lambda (ls)
		 (if (null? ls) 0 (car ls))))

	     (define find-max-index
	       (lambda (assignments* call-live*)
		 (define find-max-h
		   (lambda (locate-index-max call-index-max)
		     (add1 (max locate-index-max call-index-max))))
		 (find-max-h (if-not-null-zero (sort > (map (lambda (pair) (frame-var->index (cdr pair))) assignments*)))
			     (if-not-null-zero (sort > (map (lambda (fv) (frame-var->index fv)) (filter frame-var? call-live*)))))))


	     
	     (define assign-locs
	       (lambda (start-index Frame*)
		 (let* ((associations (apply append (map (lambda (Frame) (make-assoc start-index Frame)) Frame*)))
		       (uv* (map car associations))
		       (locrf* (map cdr associations)))
		   (values uv* locrf*))))
             
	     (define make-assoc
	       (lambda (index Frame)
		 (cond
		  ((null? Frame) '())
		  (else (let* ((fv (index->frame-var index)) (assoc (cons (car Frame) fv)))
			  (cons assoc (make-assoc (add1 index) (cdr Frame))))))))

	     )

           (Body : Body (x) -> Body ()
                 [(locals (,uv0* ...) (new-frames ((,uv1* ...) ...) (locate ((,uv2* ,locrf2*) ...)
									    (frame-conflict ,cfgraph
											    (call-live (,uv3* ...) ,tl)))))
		
		  (let* ((assignments (map cons uv2* locrf2*))
			 (start-index (find-max-index assignments uv3*))
			 (tl (Tail tl start-index)))
		
		    (let-values (((uv* locrf*) (assign-locs start-index uv1*)))
		    `(locals (,uv0* ...) (ulocals () (locate ((,(append uv2* uv*) ,(append locrf2* locrf*)) ...)
							     (frame-conflict ,cfgraph ,tl))))))]
		 [else (error who "something went wrong - Body")])
	
	   (Tail : Tail (x index) -> Tail ())

	   (Effect : Effect (x index) -> Effect ()
		   [(return-point ,l ,[tl index -> tl])  (let* ((nb (ash index word-shift))
						  (set!-fp+ `(set! ,frame-pointer-register (+ ,frame-pointer-register ,nb)))
						  (set!-fp- `(set! ,frame-pointer-register (- ,frame-pointer-register ,nb))))
						  
		    `(begin ,set!-fp+ (return-point ,l ,tl) ,set!-fp-))])
	   )
	 ) ;End Library










