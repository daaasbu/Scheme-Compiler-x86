;;a4
(letrec ([F$0 (lambda ()
                          (locals ()
                            (begin
                              (set! fv0 (+ fv0 1))
                              (F$1 fv0 fv1 rbp))))]
             [F$1 (lambda ()
                         (locals ()
                           (begin
                             (set! r12 fv0)
                             (fv1 r12 rbp))))])
     (locals ()
        (begin (set! fv1 r15) (F$0 fv0 fv1 rbp))))
