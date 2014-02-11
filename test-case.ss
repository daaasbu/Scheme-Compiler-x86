
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
;;Week 2
(letrec ([setrax1 (lambda ()
                      (begin
                        (set! rax (+ rax 5))
                        (setrax2)))]
          [setrax2 (lambda ()
                     (begin
                       (set! rax (* rax rax))
                       (r15)))])
;;Week 3
 (letrec ()
   (locate ()
           (begin
             (set! rax 10)
             (if (if (= rax 10) (nop) (= rbx 5))
                 (set! rax 3))
             (r15))))
