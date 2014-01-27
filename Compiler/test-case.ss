(letrec ([setrax1 (lambda ()
                      (begin
                        (set! rax (+ rax 5))
                        (setrax2)))]
          [setrax2 (lambda ()
                     (begin
                       (set! rax (* rax rax))
                       (r15)))])
