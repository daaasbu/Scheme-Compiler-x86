;;Expose-basic-blocks. Goes from LexposeFrameVar to LexposeBasicBlocks. Greatly simplifies if expressions and begins. All alternate and conseq branches of if-;;statements are labels that point to the next stage of execution of the code. We get rid of the pred processor, and leave relops in if's. All consecutive
;;set!'s are bundled into a basic block, (lambda () with a begin block). 

(library (Compiler expose-basic-blocks)
         (export expose-basic-blocks parse-LexposeBasicBlocks)
         (import
          (chezscheme)
          (source-grammar)
          (Framework nanopass)
          (Framework helpers))

         (define-parser parse-LexposeBasicBlocks LexposeBasicBlocks)

         (define-pass expose-basic-blocks : LexposeFrameVar (x) -> LexposeBasicBlocks ()
           (definitions
             (define make-basic-block
               (lambda (l bb)
                 `[,l (lambda () ,bb)]))

             (define Effect*
               (lambda (ef* after-bb b**)
                 (cond
                   ((null? ef*) (values after-bb '()))
                   (else (let*-values ([(bb b*) (Effect* (cdr ef*) after-bb b**)]
                                       [(bb2 b2*) (Effect (car ef*) bb)])
                           (values bb2 (append b* b2*)))))))

             (define LambdaExpr*
               (lambda (e* l*)
                 (if (null? e*)
                     '()
                     (let ([b1* (LambdaExpr* (cdr e*) (cdr l*))]
                           [b2* (LambdaExpr (car e*) (car l*))])
                       (append b1* b2*)))))

             (define make-lambda
               (with-output-language (LexposeBasicBlocks LambdaExpr)
                                     (lambda (bb)
                                       `(lambda () ,bb)))))

           (Prog : Prog (x) -> Prog ()
                 [(letrec ([,l* ,le*] ...) ,tl)
                  (let-values ([(bb b2*) (Tail tl)])
                    (let* ([b1* (LambdaExpr* le* l*)]
                           [b* (append b1* b2*)]
                           [l* (map (lambda (x) (if (list? x) (car x) x)) (map car b*))]
                           [le* (map cdr b*)])
                      `(letrec ([,l* ,le*] ...) ,(in-context Tail `(begin ,(reverse (cdr (reverse bb))) ... ,(car (last-pair bb)))))))]
                 [else (error who "something went wrong - Prog")])
           (LambdaExpr : LambdaExpr (x lab) -> * (b*)
                       [(lambda () ,[* bb b*])
                        (cons (cons lab
                                    (make-lambda (in-context Tail `(begin ,(reverse (cdr (reverse bb))) ... ,(car (last-pair bb)))))) b*)]
                       [else (error who "something went wrong - LambdaExpr")])
           (Tail : Tail (x) -> * (bb b*)
                 [(,triv) (values (list (in-context Tail `(,triv))) '())]
                 [(if ,pred ,tl1 ,tl2) (let*-values ([clab (unique-label 'c)]
                                                     [alab (unique-label 'a)]
                                                     [(predbb predb*) (Pred pred clab alab)]
                                                     [(tl1bb tl1b*) (Tail tl1)]
                                                     [(tl2bb tl2b*) (Tail tl2)])
                                         (values predbb
                                                 (append
                                                  (list
                                                   (cons clab (in-context LambdaExpr `(lambda () (begin ,(reverse (cdr (reverse tl1bb))) ... ,(car (last-pair tl1bb))))))
                                                   (cons alab (in-context LambdaExpr `(lambda () (begin ,(reverse (cdr (reverse tl2bb))) ... ,(car (last-pair tl2bb)))))))
                                                  tl1b* tl2b* predb*)))]
                 [(begin ,ef* ... ,tl) (let*-values ([(bbtl bbtl*) (Tail tl)]
                                                     [(bb1 b1*) (Effect* ef* bbtl '())])
                                         (values bb1 (append b1* bbtl*)))]
                 [else (error who "something went wrong - Tail")])
           (Pred : Pred (x tlab flab) -> * (bb b*)
                 [(true) (let ([tlab (if (list? tlab) (car tlab) tlab)])
                           (values (list (in-context Tail `(,tlab))) '()))]
                 [(false) (let ([flab (if (list? flab) (car flab) flab)]) (values (list (in-context Tail `(,flab))) '()))]
                 [(,relop ,triv1 ,triv2)
                  (let ([tlab (if (list? tlab) (car tlab) tlab)] [flab (if (list? flab) (car flab) flab)])
                    (values (list (in-context Tail `(if (,relop ,triv1 ,triv2) (,tlab) (,flab)))) '()))]
                 [(if ,pred ,pred1 ,pred2)
                  (let ([clab (unique-label 'c)]
                        [alab (unique-label 'a)])
                    (let-values ([(bb1 b1*) (Pred pred1 tlab flab)]
                                 [(bb2 b2*) (Pred pred2 tlab flab)]
                                 [(pred-bb b3*) (Pred pred clab alab)])
                      (values pred-bb
                              (append (list (cons clab (in-context LambdaExpr `(lambda () ,(car bb1))))
                                            (cons alab  (in-context LambdaExpr `(lambda () ,(car bb2)))))
                                      b1* b2* b3*))))]
                 [(begin ,ef* ... ,pred)
                  (let*-values ([(bb0 b1*) (Pred pred tlab flab)]
                                [(bb1 b2*) (Effect* ef* bb0 '())])
                    (values bb1 (append b1* b2*)))]
                 [else (error who "something went wrong - Pred")])
           (Effect : Effect (x after-bb) -> * (bb b*)
                   [(nop) (values after-bb '())]
                   [(set! ,locrf ,triv)  (values (cons (in-context Effect `(set! ,locrf ,triv)) after-bb) '())]
                   [(set! ,locrf (,op ,triv1 ,triv2)) (values (cons
                                                               (in-context Effect `(set! ,locrf (,op ,triv1 ,triv2)))
                                                               after-bb) '())]
                   [(if ,pred ,ef1 ,ef2)
                    (let ([clab (unique-label 'c)]
                          [alab (unique-label 'a)]
                          [jlab (unique-label 'j)])
                      (let-values ([(bb1 b1*) (Effect ef1 (list (in-context Tail `(,jlab))))]
                                   [(bb2 b2*) (Effect ef2 (list (in-context Tail `(,jlab))))]
                                   [(predbb b3*) (Pred pred clab alab)])
                        (values predbb
                                (append (list (cons jlab  (in-context LambdaExpr `(lambda () (begin ,(reverse (cdr (reverse after-bb))) ... ,(car (last-pair after-bb))))))
                                              (cons clab  (in-context LambdaExpr `(lambda () (begin ,(reverse (cdr (reverse bb1))) ... ,(car (last-pair bb1))))))
                                              (cons alab  (in-context LambdaExpr `(lambda () (begin ,(reverse (cdr (reverse bb2))) ... ,(car (last-pair bb2)))))))
                                        b1* b2* b3*))))]
                   [(begin ,ef* ... ,[* bb0 b1*]) (let-values ([(bb b2*) (Effect* ef* bb0 '())])
                                                    (values bb (append b1* b2*)))]
                   [else (error who "something went wrong - Effect")]))
         ) ;End Library