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
