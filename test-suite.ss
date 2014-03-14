(invalid 3 (begin rax 5) (letrec () (set! rax 5))
 (letrec () (set! rax 5) (r15))
 (letrec () (begin (set! rax 5) (r15)))
 (letrec ([f$5 (lambda (rax) (begin (r15)))]) (call f$5))
 (letrec () (letrec () 7)) (letrec () (locals () 3))
 (letrec ([f$17 (lambda () (locals () 7))]) (call f$17))
 (letrec (locals () [begin (set! rax 5) (r15 rax)]))
 (letrec () (begin (set! x.1 5.5) x.1))
 (letrec ([double$1 (lambda () (let ([x.1 1]) x.1))]
          [triple$1 (lambda () (let ([x.2 (prim * x.2 3)]) x.2))])
   (call double$1))
 (letrec ([foo (lambda () (let ([a.1 5]) a.1))]) (foo))
 (letrec ([foo$0 (lambda () (let ([a.1 5]) a.1))])
   (call bar$1))
 (letrec ([even?$1 (lambda (n.1)
                     (if (prim = 0 n.1) 1 (call odd?$2 (prim - n.1 1))))]
          [odd?$2 (lambda (n.1)
                    (if (prim = 0 n.1) 0 (call even?$1 (prim - n.1 1))))])
   (if (call odd?$1 17) 10 0))
 (letrec () x) (letrec () 9223372036854775808)
 (letrec () -9223372036854775809)
 (letrec ()
   (let ([x.1 (set! x.1 0)]) (prim + x.1 9223372036854775808)))
 (letrec ()
   (let ([x.1 0]) (prim + x.1 -9223372036854775809)))
 (letrec () 12.5)
 (letrec ()
   (let ([x.1 5]) (let ([x.2 (prim sra x.1 -1)]) x.2)))
 (letrec ()
   (let ([x.1 5]) (let ([x.2 (prim sra x.1 64)]) x.2)))
 (letrec ()
   (let ([x.1 5] [y.2 6])
     (let ([x.2 (prim sra x.1 y.2)]) x.2)))
 (letrec ([foo$1 (lambda () 7)])
   (let ([x.1 5]) (prim sra x.1 call foo$1)))
 (letrec () (let ([x.1 15]) (/ x.1 5)))
 (letrec ()
   (let ([x.1 7] [y.2 8] [z.3 9])
     (if (prim + z.3 5) (x.1) (z.3))))
 (letrec ()
   (let ([x.1 (alloc 8)])
     (begin
       (mset! x.1 0 0)
       (if (prim = (mref x.1 0) 0) (nop) (mref x.1 0))
       (mset! x.1 0 5)
       (mref x.1 0))))
 (letrec ()
   (let ([x.1 (alloc 8)])
     (begin
       (mset! x.1 0 0)
       (if (prim = (mref x.1 0) 0) (nop) (prim > x.1 9))
       (mset! x.1 0 5)
       (mref x.1 0))))
 (letrec ()
   (let ([x.1 1]) (begin (if (let ([x.2 10]) x.2) x.1 x.1))))
 (letrec ([foo$1 (lambda () 7)])
   (begin
     (if (let ([x.1 10]) (call foo$1))
         (call foo$1)
         (call foo$1))))
 (letrec ()
   (let ([x.1 1]) (begin (if (let ([x.2 10]) (nop)) x.1 x.1))))
 (letrec ()
   (let ([x.1 (alloc 8)] [x2 (alloc 8)] [x3 (alloc 8)])
     (begin
       (if (begin
             (mset! x.1 0 10)
             (mset! y.2 0 10)
             (prim = (mref x.1 0) (mref y.2 0)))
           (set! (mref z.3 0) 10)
           (mref x.1 0)))))
 (letrec ()
   (let ([x.1 (alloc 8)] [x2 (alloc 8)] [x3 (alloc 8)])
     (begin
       (if (begin
             (mset! x.1 0 10)
             (nop)
             (prim = (mref x.1 0) (mref y.2 0)))
           (mset! z.3 0 10)
           (mref x.1 0)))))
 (letrec ([main$0 (lambda ()
                    (let ([y.2 5])
                      (let ([x.1 y.2] [z.3 15])
                        (if (prim + z.3 5) x.1 y.2))))])
   (call main$0))
 (letrec ([main$0 (lambda ()
                    (let ([x.1 (alloc 8)])
                      (begin
                        (if (begin (mset! x.1 0 10) (nop))
                            (mref x.1 0)
                            (mref x.1 0)))))])
   (call main$0))
 (letrec ([foo$1 (lambda () 7)]
          [main$0 (lambda ()
                    (begin
                      (if (let ([x.1 10]) (nop))
                          (call foo$1)
                          (call foo$1))))])
   (call main$0))
 (letrec ([main$0 (lambda ()
                    (let ([x.1 (alloc 8)] [y.2 10])
                      (begin
                        (if (begin
                              (mset! x.1 0 10)
                              (prim = (mref x.1 0) y.2))
                            (mset! x.1 0 10)
                            (mref x.1 0)))))])
   (call main$0))
 (letrec ([main$0 (lambda ()
                    (let ([x.1 15])
                      (if (prim = 0 x.1) (call f$0) (call f$1))))]
          [f$0 (lambda () 5)]
          [f$1 (lambda () 6)])
   (call main$0))
 (letrec ()
   (let ([x.1 17])
     (if (prim = x.1 9223372036854775808) x.1 x.1)))
 (letrec ()
   (let ([x.1 17])
     (if (prim = x.1 -9223372036854775809) x.1 x.1)))
 (letrec () (let ([x.1 4]) (if (prim = x.1 12.5) x.1 x.1)))
 (letrec ()
   (let ([x.1 (alloc 8)])
     (begin
       (mset! x.1 0 7)
       (if (if (prim < (mref x.1 0) 10)
               (if (prim < (mref x.1 0) 5) (false) (true))
               #f)
           (begin
             (mset! x.1 0 (prim * (mref x.1 0) (mref x.1 0)))
             (mref x.1 0))
           (mref x.1 0)))))
 (letrec () (let ([x 15]) x))
 (letrec ()
   (let ([x.1 (alloc 8)] [y.2 (alloc 8)])
     (begin
       (mset! x.1 0 5)
       (mset! y.2 0 2)
       (mset! z.5 0 (prim + (mref z.5 0) (mref x.1 0)))
       (mset! z.5 0 (prim + (mref z.5 0) (mref x.1 0)))
       z.5)))
 (letrec ()
   (let ([x.1 (alloc 8)] [y.1 (alloc 8)])
     (begin
       (mset! x.1 0 (true))
       (if x.1 (mset! y.2 0 5) (mset! y.2 0 100))
       y.2)))
 (letrec ()
   (let ([x.1 (alloc 8)] [y.2 0] [acc.3 (alloc 8)])
     (begin
       (mset! x.1 0 5)
       (mset! acc.3 0 0)
       call
       loop$1
       (mset! acc.3 0 (prim + (mref acc.3 0) (mref x.1 0)))
       (mset! x.1 0 (prim - (mref x.1 0) 1))
       (if (prim = y.2 (mref x.1 0)) (nop) (call loop$1))
       (mref acc.3 0))))
 (letrec ([double$1 (lambda (a.3 b.4) 3)])
   (let ([x.2 5] [fv0 17]) (call double$1 x.2 fv0)))
 (letrec ([double$1 (lambda () 3)])
   (let ([x.2 5] [fv0 17]) (call double$1 x.2 fv0)))
 (letrec ([double$1 (lambda (a.3 b.4) 3)])
   (let ([x.2 5] [fv0 17]) (call double$1 x.2 17)))
 (letrec () (let ([x.1 rax]) (prim + x.1 5)))
 (letrec () (let ([x.1 4]) (prim + x.1 rax)))
 (letrec ([f$1 (lambda (x.3) x.3)] [g$2 (lambda (y.3) y.3)])
   (call f$1 (call g$2 17)))
 (letrec ([f$1 (lambda (x.3) x.3)] [g$2 (lambda (y.4) y.4)])
   (let ([z.4 17]) (call f$1 (call g$2 z.4))))
 (letrec ([f$1 (lambda (x.3) (let ([x.4 x.3]) x.4))]
          [g$2 (lambda (y.4) y.4)])
   (let ([z.5 17]) (call f$1 (call g$2 z.5))))
 (letrec ([f$1 (lambda (x.3) x.3)] [g$2 (lambda (y.4) y.4)])
   (let ([z.5 (alloc 8)])
     (mset! z.5 0 15)
     (call f$1 (call g$2 (mref z.5 0)))))
 (letrec ([f$1 (lambda (x.3) x.3)] [g$2 (lambda (y.4) y.4)])
   (begin
     (let ([z.5 (alloc 8)])
       (mset! z.5 0 15)
       (call f$1 (call g$2 (mref z.5 0))))
     7))
 (letrec ([f$1 (lambda (x.3) x.3)] [g$2 (lambda (y.4) y.4)])
   (if (let ([z.5 (alloc 8)])
         (mset! z.5 0 15)
         (prim = (mref z.5 0) 7))
       17
       45))
 (letrec ()
   (prim
     +
     (let ([z.5 (alloc 8)])
       (mset! z.5 0 15)
       (prim + (mref z.5 0) 7))
     17)))

(valid (letrec () 7) (letrec () (prim + 5 7))
 (letrec () (prim + 7 (prim * 5 7)))
 (letrec () (prim * (prim + 2 4) (prim + (prim + 6 7) 4)))
 (letrec ()
   (if (prim
         =
         (prim + 7 (prim * 2 4))
         (prim - 20 (prim + (prim + 1 1) (prim + (prim + 1 1) 1))))
       (prim + 1 (prim + 1 (prim + 1 (prim + 1 (prim + 1 10)))))
       0))
 (letrec ()
   (let ([a.1 10])
     (let ([b.2 (if (prim < 7 a.1) a.1 (prim + a.1 a.1))]) b.2)))
 (letrec ()
   (let ([a.1 5])
     (let ([b.2 (if (prim < 7 a.1) a.1 (prim + a.1 a.1))]) b.2)))
 (letrec ()
   (let ([c.1 10] [a.2 5]) (if (prim < a.2 c.1) a.2 c.1)))
 (letrec ()
   (let ([a.1 5])
     (let ([b.2 (if (prim < a.1 10) (prim + a.1 a.1) a.1)])
       b.2)))
 (letrec ([f$0 (lambda (x.1) (prim + 1 x.1))])
   (call f$0 (let ([f.2 3]) (prim + f.2 1))))
 (letrec ([f$0 (lambda (h.1 v.2) (prim * h.1 v.2))]
          [k$1 (lambda (x.3) (prim + x.3 5))]
          [g$2 (lambda (x.4) (prim + 1 x.4))])
   (let ([x.5 15])
     (call k$1 (call g$2 (let ([g.6 3]) (call f$0 g.6 x.5))))))
 (letrec () (prim + (let ([x.1 3]) x.1) 3))
 (letrec () (begin (let ([x.1 3]) (nop)) 4))
 (letrec () (if (let ([x.1 3]) (prim = x.1 4)) 5 6))
 (letrec ()
   (begin (if (let ([x.1 3]) (prim = x.1 4)) (nop) (nop)) 8))
 (letrec ()
   (prim
     +
     (if (begin (nop) (let ([x.1 3]) (prim = x.1 4))) 3 4)
     5))
 (letrec ()
   (begin (mset! (let ([x.1 (alloc 8)]) x.1) 0 5) 16))
 (letrec ([one$1 (lambda (n.1)
                   (if (prim = 0 n.1) 1 (call one$1 (prim - n.1 1))))])
   (call one$1 13))
 (letrec ([f$0 (lambda (p.2)
                 (prim - (mref p.2 8) (mref p.2 0)))])
   (let ([x.1 (alloc 16)])
     (begin (mset! x.1 0 73) (mset! x.1 8 35) (call f$0 x.1))))
 (letrec ([f$0 (lambda (p.2 i.3 i.4)
                 (prim - (mref p.2 i.3) (mref p.2 i.4)))])
   (let ([x.1 (alloc 16)])
     (begin
       (mset! x.1 0 73)
       (mset! x.1 8 35)
       (prim + (call f$0 x.1 0 8) -41))))
 (letrec ([f$0 (lambda (p.3)
                 (prim
                   -
                   (mref
                     (mref (mref (mref (mref p.3 0) 0) 8) 0)
                     (mref (mref p.3 8) (mref (mref p.3 0) 32)))
                   (mref
                     (mref p.3 (mref p.3 16))
                     (mref (mref p.3 0) (mref p.3 32)))))])
   (let ([x.1 (alloc 48)] [x.2 (alloc 56)])
     (begin
       (mset! x.1 0 x.2)
       (mset! x.1 8 x.1)
       (mset! x.2 0 x.1)
       (mset! x.2 8 -4421)
       (mset! x.1 16 0)
       (mset! x.1 24 -37131)
       (mset! x.1 32 32)
       (mset! x.1 40 48)
       (mset! x.2 16 -55151)
       (mset! x.2 24 -32000911)
       (mset! x.2 32 40)
       (mset! x.2 40 55)
       (mset! x.2 48 -36)
       (prim * (call f$0 x.1) 2))))
 (letrec ([make-vector$0 (lambda (size.1)
                           (let ([v.2 (alloc (prim + (prim * size.1 8) 8))])
                             (begin (mset! 0 v.2 size.1) v.2)))]
          [chained-vector-set!$1 (lambda (v.3 off.4 val.5)
                                   (begin
                                     (mset!
                                       (prim * (prim + off.4 1) 8)
                                       v.3
                                       val.5)
                                     v.3))]
          [vector-length$4 (lambda (v.8) (mref v.8 0))]
          [find-greatest-less-than$2 (lambda (v.6 val.7)
                                       (call fglt-help$3 v.6 val.7
                                         (prim + v.6 8)
                                         (call vector-length$4 v.6)))]
          [fglt-help$3 (lambda (v.9 val.10 curr.11 size.12)
                         (if (if (prim
                                   >
                                   curr.11
                                   (prim
                                     +
                                     (prim + v.9 (prim * size.12 8))
                                     8))
                                 (true)
                                 (prim > (mref curr.11 0) val.10))
                             (mref curr.11 -8)
                             (call fglt-help$3 v.9 val.10
                               (prim + curr.11 8) size.12)))])
   (let ([v.13 (call
                 chained-vector-set!$1
                 (call
                   chained-vector-set!$1
                   (call
                     chained-vector-set!$1
                     (call
                       chained-vector-set!$1
                       (call
                         chained-vector-set!$1
                         (call
                           chained-vector-set!$1
                           (call
                             chained-vector-set!$1
                             (call
                               chained-vector-set!$1
                               (call
                                 chained-vector-set!$1
                                 (call
                                   chained-vector-set!$1
                                   (call make-vector$0 10)
                                   0
                                   0)
                                 1
                                 10)
                               2
                               20)
                             3
                             30)
                           4
                           40)
                         5
                         50)
                       6
                       60)
                     7
                     70)
                   8
                   80)
                 9
                 90)])
     (call find-greatest-less-than$2 v.13 76)))
 (letrec ([vector-scale!$0 (lambda (vect.1 scale.2)
                             (let ([size.3 (mref vect.1 0)])
                               (call
                                 vector-scale!$1
                                 size.3
                                 vect.1
                                 scale.2)))]
          [vector-scale!$1 (lambda (offset.4 vect.5 scale.6)
                             (if (prim < offset.4 1)
                                 0
                                 (begin
                                   (mset!
                                     vect.5
                                     (prim * offset.4 8)
                                     (prim
                                       *
                                       (mref vect.5 (prim * offset.4 8))
                                       scale.6))
                                   (call
                                     vector-scale!$1
                                     (prim - offset.4 1)
                                     vect.5
                                     scale.6))))]
          [vector-sum$2 (lambda (vect.7)
                          (call vector-sum$3 (mref vect.7 0) vect.7))]
          [vector-sum$3 (lambda (offset.9 vect.10)
                          (if (prim < offset.9 1)
                              0
                              (prim
                                +
                                (mref vect.10 (prim * offset.9 8))
                                (call
                                  vector-sum$3
                                  (prim - offset.9 1)
                                  vect.10))))])
   (let ([vect.11 (alloc 48)])
     (begin
       (mset! vect.11 0 5)
       (mset! vect.11 8 123)
       (mset! vect.11 16 10)
       (mset! vect.11 24 7)
       (mset! vect.11 32 12)
       (mset! vect.11 40 57)
       (call vector-scale!$0 vect.11 10)
       (call vector-sum$2 vect.11))))
 (letrec ([::$0 (lambda (fst.1 snd.2)
                  (let ([ptr.3 (alloc 16)])
                    (begin
                      (mset! ptr.3 0 fst.1)
                      (mset! ptr.3 8 snd.2)
                      ptr.3)))]
          [fst$1 (lambda (ptr.4) (mref ptr.4 0))]
          [snd$2 (lambda (ptr.5) (mref ptr.5 8))]
          [length$3 (lambda (ptr.6)
                      (if (prim = ptr.6 0)
                          0
                          (prim + 1 (call length$3 (call snd$2 ptr.6)))))])
   (call
     length$3
     (call
       ::$0
       5
       (call
         ::$0
         10
         (call ::$0 11 (call ::$0 5 (call ::$0 15 0)))))))
 (letrec ([::$0 (lambda (fst.1 snd.2)
                  (let ([ptr.3 (alloc 16)])
                    (begin
                      (mset! ptr.3 0 fst.1)
                      (mset! ptr.3 8 snd.2)
                      ptr.3)))]
          [fst$1 (lambda (ptr.4) (mref ptr.4 0))]
          [snd$2 (lambda (ptr.5) (mref ptr.5 8))]
          [count-leaves$3 (lambda (ptr.6)
                            (if (prim = ptr.6 0)
                                1
                                (prim
                                  +
                                  (call count-leaves$3 (call fst$1 ptr.6))
                                  (call
                                    count-leaves$3
                                    (call snd$2 ptr.6)))))])
   (call
     count-leaves$3
     (call
       ::$0
       (call ::$0 0 (call ::$0 0 0))
       (call
         ::$0
         (call ::$0 (call ::$0 (call ::$0 0 (call ::$0 0 0)) 0) 0)
         (call
           ::$0
           (call ::$0 (call ::$0 0 0) (call ::$0 0 (call ::$0 0 0)))
           (call ::$0 (call ::$0 0 0) 0))))))
 (letrec ([::$0 (lambda (fst.1 snd.2)
                  (let ([ptr.3 (alloc 16)])
                    (begin
                      (mset! ptr.3 0 fst.1)
                      (mset! ptr.3 8 snd.2)
                      ptr.3)))]
          [fst$1 (lambda (ptr.4) (mref ptr.4 0))]
          [snd$2 (lambda (ptr.5) (mref ptr.5 8))]
          [add1$3 (lambda (n.6) (prim + n.6 1))]
          [map$4 (lambda (f.7 ls.8)
                   (if (prim = ls.8 0)
                       0
                       (call
                         ::$0
                         (call f.7 (call fst$1 ls.8))
                         (call map$4 f.7 (call snd$2 ls.8)))))]
          [sum$5 (lambda (ls.9)
                   (if (prim = 0 ls.9)
                       0
                       (prim
                         +
                         (call fst$1 ls.9)
                         (call sum$5 (call snd$2 ls.9)))))])
   (let ([ls.10 (call
                  ::$0
                  5
                  (call
                    ::$0
                    4
                    (call ::$0 3 (call ::$0 2 (call ::$0 1 0)))))])
     (let ([ls.11 (call
                    ::$0
                    10
                    (call
                      ::$0
                      9
                      (call ::$0 8 (call ::$0 7 (call ::$0 6 ls.10)))))])
       (call sum$5 (call map$4 add1$3 ls.11)))))
 (letrec ([::$0 (lambda (fst.1 snd.2)
                  (let ([ptr.3 (alloc 16)])
                    (begin
                      (mset! ptr.3 0 fst.1)
                      (mset! ptr.3 8 snd.2)
                      ptr.3)))]
          [fst$1 (lambda (ptr.4) (mref ptr.4 0))]
          [snd$2 (lambda (ptr.5) (mref ptr.5 8))]
          [list-ref$3 (lambda (ls.11 offset.12)
                        (if (prim = offset.12 0)
                            (call fst$1 ls.11)
                            (call
                              list-ref$3
                              (call snd$2 ls.11)
                              (prim - offset.12 1))))]
          [add$6 (lambda (v.13 w.14) (prim + v.13 w.14))]
          [sub$7 (lambda (v.15 w.16) (prim - v.15 w.16))]
          [mult$8 (lambda (v.17 w.18) (prim * v.17 w.18))]
          [expt$9 (lambda (v.217 w.218)
                    (if (prim = w.218 0)
                        1
                        (prim
                          *
                          v.217
                          (call expt$9 v.217 (prim - w.218 1)))))]
          [selector$4 (lambda (op*.7 sel.19 rand1.20 rand2.21)
                        (if (prim = sel.19 0)
                            0
                            (call
                              ::$0
                              (call
                                (call list-ref$3 op*.7 (call fst$1 sel.19))
                                (call fst$1 rand1.20)
                                (call fst$1 rand2.21))
                              (call selector$4 op*.7 (call snd$2 sel.19)
                                (call snd$2 rand1.20)
                                (call snd$2 rand2.21)))))]
          [sum$5 (lambda (ls.9)
                   (if (prim = 0 ls.9)
                       0
                       (prim
                         +
                         (call fst$1 ls.9)
                         (call sum$5 (call snd$2 ls.9)))))])
   (call
     sum$5
     (call selector$4
       (call
         ::$0
         add$6
         (call ::$0 sub$7 (call ::$0 mult$8 (call ::$0 expt$9 0))))
       (call
         ::$0
         2
         (call ::$0 0 (call ::$0 1 (call ::$0 3 (call ::$0 2 0)))))
       (call
         ::$0
         5
         (call ::$0 9 (call ::$0 10 (call ::$0 2 (call ::$0 3 0)))))
       (call
         ::$0
         3
         (call
           ::$0
           1
           (call ::$0 3 (call ::$0 3 (call ::$0 8 0))))))))
 (letrec ([thunk-num$0 (lambda (n.1)
                         (let ([th.2 (alloc 16)])
                           (begin
                             (mset! th.2 0 force-th$1)
                             (mset! th.2 8 n.1)
                             th.2)))]
          [force-th$1 (lambda (cl.3) (mref cl.3 8))]
          [add-ths$2 (lambda (cl1.4 cl2.5 cl3.6 cl4.7)
                       (prim
                         +
                         (prim
                           +
                           (call (mref cl1.4 0) cl1.4)
                           (call (mref cl2.5 0) cl2.5))
                         (prim
                           +
                           (call (mref cl3.6 0) cl3.6)
                           (call (mref cl4.7 0) cl4.7))))])
   (call add-ths$2 (call thunk-num$0 5) (call thunk-num$0 17)
     (call thunk-num$0 7) (call thunk-num$0 9)))
 (letrec ([make-vector$0 (lambda (size.1)
                           (let ([v.20 (alloc (prim * (prim + size.1 1) 8))])
                             (begin (mset! 0 v.20 size.1) v.20)))]
          [vector-set!$1 (lambda (vect.2 off.3 val.4)
                           (begin
                             (if (prim > off.3 (mref vect.2 0))
                                 (nop)
                                 (mset!
                                   (prim * (prim + off.3 1) 8)
                                   vect.2
                                   val.4))
                             0))]
          [vector-equal?$3 (lambda (vect1.8 vect2.9)
                             (if (prim = (mref 0 vect1.8) (mref 0 vect2.9))
                                 (call
                                   vector-equal?$4
                                   vect1.8
                                   vect2.9
                                   (prim - (mref 0 vect1.8) 1))
                                 0))]
          [vector-equal?$4 (lambda (vect1.11 vect2.12 off.10)
                             (if (prim < off.10 0)
                                 1
                                 (if (prim
                                       =
                                       (mref
                                         (prim * (prim + off.10 1) 8)
                                         vect1.11)
                                       (mref
                                         vect2.12
                                         (prim * (prim + off.10 1) 8)))
                                     (call
                                       vector-equal?$4
                                       vect1.11
                                       vect2.12
                                       (prim - off.10 1))
                                     0)))])
   (let ([v1.13 (call make-vector$0 5)])
     (begin
       (call vector-set!$1 v1.13 0 134)
       (call vector-set!$1 v1.13 1 123)
       (call vector-set!$1 v1.13 2 503)
       (call vector-set!$1 v1.13 3 102)
       (call vector-set!$1 v1.13 4 255)
       (let ([v2.14 (call make-vector$0 5)])
         (begin
           (call vector-set!$1 v2.14 0 134)
           (call vector-set!$1 v2.14 1 123)
           (call vector-set!$1 v2.14 2 503)
           (call vector-set!$1 v2.14 3 102)
           (call vector-set!$1 v2.14 4 255)
           (if (prim = (call vector-equal?$3 v1.13 v2.14) 0)
               100
               -100))))))
 (letrec ([stack-new$0 (lambda (size.1)
                         (let ([store.3 (alloc (prim * 8 size.1))]
                               [meths.4 (alloc (prim * 3 8))]
                               [stack.2 (alloc (prim * 3 8))])
                           (begin
                             (mset! meths.4 0 stack-push$2)
                             (mset! meths.4 8 stack-pop$3)
                             (mset! meths.4 16 stack-top$4)
                             (mset! stack.2 0 meths.4)
                             (mset! stack.2 8 0)
                             (mset! stack.2 16 store.3)
                             stack.2)))]
          [invoke$1 (lambda (obj.5 meth-idx.6)
                      (mref (mref obj.5 0) (prim * meth-idx.6 8)))]
          [stack-push$2 (lambda (self.7 val.8)
                          (begin
                            (mset!
                              (mref self.7 16)
                              (prim * (mref self.7 8) 8)
                              val.8)
                            (mset! self.7 8 (prim + (mref self.7 8) 1))
                            self.7))]
          [stack-pop$3 (lambda (self.9)
                         (begin
                           (mset! self.9 8 (prim - (mref 8 self.9) 1))
                           (mref
                             (mref self.9 16)
                             (prim * (mref self.9 8) 8))))]
          [stack-top$4 (lambda (self.209)
                         (mref
                           (mref self.209 16)
                           (prim * (prim - (mref 8 self.209) 1) 8)))])
   (let ([s1.10 (call stack-new$0 10)])
     (begin
       (call (call invoke$1 s1.10 0) s1.10 10)
       (call (call invoke$1 s1.10 0) s1.10 20)
       (call (call invoke$1 s1.10 0) s1.10 30)
       (call (call invoke$1 s1.10 0) s1.10 40)
       (call (call invoke$1 s1.10 0) s1.10 50)
       (call (call invoke$1 s1.10 0) s1.10 60)
       (call (call invoke$1 s1.10 0) s1.10 70)
       (call (call invoke$1 s1.10 0) s1.10 80)
       (call (call invoke$1 s1.10 0) s1.10 90)
       (call (call invoke$1 s1.10 0) s1.10 100)
       (let ([s2.11 (call stack-new$0 6)])
         (begin
           (call
             (call invoke$1 s2.11 0)
             s2.11
             (call (call invoke$1 s1.10 1) s1.10))
           (call (call invoke$1 s1.10 1) s1.10)
           (call
             (call invoke$1 s2.11 0)
             s2.11
             (call (call invoke$1 s1.10 1) s1.10))
           (call (call invoke$1 s1.10 1) s1.10)
           (call
             (call invoke$1 s2.11 0)
             s2.11
             (call (call invoke$1 s1.10 1) s1.10))
           (call (call invoke$1 s1.10 1) s1.10)
           (call
             (call invoke$1 s2.11 0)
             s2.11
             (call (call invoke$1 s1.10 1) s1.10))
           (call (call invoke$1 s1.10 1) s1.10)
           (call
             (call invoke$1 s2.11 0)
             s2.11
             (call (call invoke$1 s1.10 1) s1.10))
           (call
             (call invoke$1 s2.11 0)
             s2.11
             (call (call invoke$1 s1.10 1) s1.10))
           (let ([x.1000 (prim
                           +
                           (call (call invoke$1 s2.11 1) s2.11)
                           (call (call invoke$1 s2.11 1) s2.11))])
             (prim
               *
               (prim
                 +
                 (let ([x.1001 (prim
                                 +
                                 (call (call invoke$1 s2.11 2) s2.11)
                                 (call (call invoke$1 s2.11 2) s2.11))])
                   (prim
                     -
                     x.1001
                     (prim
                       +
                       (call (call invoke$1 s2.11 1) s2.11)
                       (call (call invoke$1 s2.11 1) s2.11))))
                 (let ([x.1002 (prim
                                 +
                                 (call (call invoke$1 s2.11 2) s2.11)
                                 (call (call invoke$1 s2.11 2) s2.11))])
                   (prim
                     -
                     (prim
                       +
                       (call (call invoke$1 s2.11 1) s2.11)
                       (call (call invoke$1 s2.11 1) s2.11))
                     x.1002)))
               x.1000)))))))
 (letrec ([a$0 (lambda (u.1 v.2 w.3 x.4)
                 (if (prim = u.1 0)
                     (call b$1 v.2 w.3 x.4)
                     (call a$0 (prim - u.1 1) v.2 w.3 x.4)))]
          [b$1 (lambda (q.5 r.6 x.7)
                 (let ([p.8 (prim * q.5 r.6)])
                   (call e$3 (prim * q.5 r.6) p.8 x.7)))]
          [c$2 (lambda (x.9) (prim * 5 x.9))]
          [e$3 (lambda (n.10 p.11 x.12)
                 (if (prim = n.10 0)
                     (call c$2 p.11)
                     (call o$4 (prim - n.10 1) p.11 x.12)))]
          [o$4 (lambda (n.13 p.14 x.15)
                 (if (prim = 0 n.13)
                     (call c$2 x.15)
                     (call e$3 (prim - n.13 1) p.14 x.15)))])
   (let ([x.16 5]) (call a$0 3 2 1 x.16)))
 (letrec ([f$0 (lambda () 80)])
   (let ([a.1 (call f$0)] [b.2 (call f$0)]) (prim * a.1 b.2)))
 (letrec ([f$0 (lambda () 80)] [g$1 (lambda () 50)])
   (let ([a.1 (call f$0)] [b.2 (call g$1)]) (prim * a.1 b.2)))
 (letrec ([f$0 (lambda (x.1) (prim + x.1 1))]
          [g$1 (lambda (y.2) (call f$0 (call f$0 y.2)))])
   (prim + (call f$0 1) (call g$1 1)))
 (letrec ([fact$0 (lambda (n.1)
                    (if (prim = n.1 0)
                        1
                        (prim * n.1 (call fact$0 (prim - n.1 1)))))])
   (call fact$0 10))
 (letrec ()
   (let ([a.1 5] [b.2 1])
     (let ([b.3 (prim * b.2 a.1)] [a.4 (prim - a.1 1)])
       (let ([b.5 (prim * b.3 a.4)] [a.6 (prim - a.4 1)])
         (let ([b.7 (prim * b.5 a.6)] [a.8 (prim - a.6 1)])
           (let ([b.9 (prim * b.7 a.8)] [a.10 (prim - a.8 1)])
             (let ([b.11 (prim * b.9 a.10)]) b.11)))))))
 (letrec ()
   (let ([n.1 5])
     (let ([a.2 1])
       (let ([a.3 (prim * a.2 n.1)])
         (let ([n.4 (prim - n.1 1)])
           (let ([a.5 (prim * a.3 n.4)])
             (let ([n.6 (prim - n.4 1)])
               (let ([a.7 (prim * a.5 n.6)])
                 (let ([n.8 (prim - n.6 1)])
                   (let ([a.9 (prim * a.7 n.8)]) a.9))))))))))
 (letrec ([double$0 (lambda (a.1) (prim + a.1 a.1))])
   (call double$0 10))
 (letrec ([double$1 (lambda (x.1) (prim * x.1 2))])
   (call double$1 5))
 (letrec ()
   (let ([x.5 (let ([y.10 10])
                (let ([x.15 15]) (prim * y.10 x.15)))])
     x.5))
 (letrec ([f$0 (lambda (x.1) (prim + 1 x.1))]
          [g$1 (lambda (x.2) (prim - x.2 1))]
          [t$2 (lambda (x.3) (prim - x.3 1))]
          [j$3 (lambda (x.4) (prim - x.4 1))]
          [i$4 (lambda (x.5) (prim - x.5 1))]
          [h$5 (lambda (x.6) (prim - x.6 1))])
   (let ([x.7 80])
     (let ([a.8 (call f$0 x.7)]
           [b.9 (call g$1 x.7)]
           [c.10 (call h$5 (call i$4 (call j$3 (call t$2 x.7))))])
       (prim * a.8 (prim * b.9 (prim + c.10 0))))))
 (letrec ([fact$0 (lambda (n.1)
                    (if (prim = n.1 0)
                        1
                        (let ([t.2 (prim - n.1 1)])
                          (let ([t.3 (call fact$0 t.2)])
                            (prim * n.1 t.3)))))])
   (call fact$0 10))
 (letrec ([fib$0 (lambda (n.1)
                   (if (if (prim = 0 n.1) (true) (prim = 1 n.1))
                       1
                       (prim
                         +
                         (call fib$0 (prim - n.1 1))
                         (call fib$0 (prim - n.1 2)))))])
   (call fib$0 10))
 (letrec ([even$0 (lambda (n.1)
                    (if (prim = n.1 0) 1 (call odd$1 (prim - n.1 1))))]
          [odd$1 (lambda (n.2)
                   (if (prim = n.2 0) 0 (call even$0 (prim - n.2 1))))])
   (call even$0 17))
 (letrec ()
   (let ([result.3 (let ([y.2 10])
                     (prim
                       +
                       (let ([x.1 5])
                         (if (prim < 11 x.1)
                             (prim + x.1 y.2)
                             (prim + y.2 100)))
                       (let ([x.5 10] [y.4 20]) (prim * x.5 y.4))))])
     result.3))
 (letrec () (let ([x.5 5]) x.5))
 (letrec () (let ([x.5 5] [y.6 6]) (prim + x.5 y.6)))
 (letrec () (let ([x.5 5]) (let ([y.6 6]) (prim + x.5 y.6))))
 (letrec ([div$0 (lambda (x.2)
                   (let ([x.3 (prim sra x.2 1)]) (call div$1 x.3)))]
          [div$1 (lambda (result.4) result.4)])
   (let ([label-temp.5 div$0]) (call label-temp.5 64)))
 (letrec ([expt$0 (lambda (n.3 m.4)
                    (if (prim = m.4 1)
                        n.3
                        (prim * n.3 (call expt$0 n.3 (prim - m.4 1)))))]
          [div$1 (lambda (n.5 d.6)
                   (call div-helper$2 31 (prim - (prim * 2 n.5) d.6)
                     (prim * d.6 (call expt$0 2 32)) 0))]
          [div-helper$2 (lambda (i.7 p.8 d.9 q.10)
                          (if (prim > 0 i.7)
                              q.10
                              (if (prim >= p.8 0)
                                  (call div-helper$2 (prim - i.7 1)
                                    (prim - (prim * 2 p.8) d.9) d.9
                                    (prim logor (call expt$0 2 i.7) q.10))
                                  (call div-helper$2 (prim - i.7 1)
                                    (prim
                                      -
                                      (prim * 2 (prim + p.8 d.9))
                                      d.9)
                                    d.9 q.10))))])
   (call div$1 153 17))
 (letrec ([setbit3$0 (lambda (x.2)
                       (let ([x.3 (prim logor x.2 8)])
                         (call return$1 x.3)))]
          [return$1 (lambda (x.4) x.4)])
   (call setbit3$0 1))
 (letrec ([zero?$0 (lambda (n.2)
                     (let ([x.5 0])
                       (let ([x.6 (prim - x.5 n.2)])
                         (let ([x.7 (prim sra x.6 63)])
                           (let ([x.8 (prim logand x.7 1)])
                             (call return$1 x.8))))))]
          [return$1 (lambda (x.4) x.4)])
   (call zero?$0 5))
 (letrec ([sqr-double$0 (lambda (z.5)
                          (let ([z.6 (prim * z.5 z.5)])
                            (call double$1 z.6)))]
          [double$1 (lambda (w.4)
                      (let ([w.7 (prim + w.4 w.4)]) (call return$3 w.7)))]
          [return$3 (lambda (result.8) result.8)])
   (begin (call sqr-double$0 3) (call sqr-double$0 5)))
 (letrec ([square$1 (lambda (x.1) (prim * x.1 x.1))])
   (call square$1 7))
 (letrec ([sum$1 (lambda (x.1 y.2 z.3 w.4)
                   (prim + x.1 (prim + y.2 (prim + z.3 w.4))))])
   (let ([a.6 (alloc 8)])
     (call sum$1 (begin (mset! a.6 0 1) (mref a.6 0))
       (begin (mset! a.6 0 2) (mref a.6 0))
       (begin (mset! a.6 0 3) (mref a.6 0))
       (begin (mset! a.6 0 4) (mref a.6 0)))))
 (letrec ([sum$1 (lambda (x.1 y.2 z.3 w.4)
                   (prim + x.1 (prim + y.2 (prim + z.3 w.4))))])
   (let ([a.6 (alloc 8)])
     (let ([b.7 (begin (mset! a.6 0 1) (mref a.6 0))]
           [c.8 (begin (mset! a.6 0 2) (mref a.6 0))]
           [d.9 (begin (mset! a.6 0 3) (mref a.6 0))]
           [e.10 (begin (mset! a.6 0 4) (mref a.6 0))])
       (call sum$1 b.7 c.8 d.9 e.10))))
 (letrec ([f$1 (lambda (n.2 a.3 b.4 c.5 x.6)
                 (if (prim = n.2 0)
                     (prim
                       +
                       (prim * a.3 (prim * x.6 x.6))
                       (prim + (prim * b.4 x.6) c.5))
                     (prim
                       +
                       (call f$1 (prim sra n.2 3)
                         (prim + a.3 (prim logand n.2 4))
                         (prim + b.4 (prim logand n.2 2))
                         (prim + c.5 (prim logand n.2 1)) x.6)
                       1)))])
   (call f$1 16434824 1 0 -1 7))
 (letrec ([f$1 (lambda (n.2 a.3 b.4 c.5 x.6)
                 (if (prim = n.2 0)
                     (prim
                       +
                       (prim * a.3 (prim * x.6 x.6))
                       (prim + (prim * b.4 x.6) c.5))
                     (prim
                       -
                       (call f$1 (prim sra n.2 3)
                         (prim + a.3 (prim logand n.2 4))
                         (prim + b.4 (prim logand n.2 2))
                         (prim + c.5 (prim logand n.2 1)) x.6)
                       (call g$0 n.2 a.3 b.4 c.5))))]
          [g$0 (lambda (n.7 a.8 b.9 c.10)
                 (prim + (prim - n.7 a.8) (prim - b.9 c.10)))])
   (call f$1 16434824 1 0 -1 7))
 (letrec ([square$0 (lambda (n.1) (prim * n.1 n.1))])
   (call square$0 10))
 (letrec ([fact$0 (lambda (n.2) (call fact$1 n.2 1))]
          [fact$1 (lambda (n.3 a.4)
                    (if (prim = n.3 0)
                        a.4
                        (call fact$1 (prim - n.3 1) (prim * n.3 a.4))))])
   (call fact$0 10))
 (letrec ([gcd$0 (lambda (x.1 y.2)
                   (if (prim = y.2 0)
                       x.1
                       (call
                         gcd$0
                         (if (prim > x.1 y.2) (prim - x.1 y.2) x.1)
                         (if (prim > x.1 y.2) y.2 (prim - y.2 x.1)))))])
   (call gcd$0 1071 1029))
 (letrec ([sub1$1 (lambda (n.2) (prim - n.2 1))]
          [fib$0 (lambda (n.3)
                   (if (prim = 0 n.3)
                       0
                       (if (prim = 1 n.3)
                           1
                           (prim
                             +
                             (call fib$0 (call sub1$1 n.3))
                             (call
                               fib$0
                               (call sub1$1 (call sub1$1 n.3)))))))])
   (call fib$0 10))
 (letrec ([ack$0 (lambda (m.1 n.2)
                   (if (prim = m.1 0)
                       (prim + n.2 1)
                       (if (if (prim > m.1 0) (prim = n.2 0) (false))
                           (call ack$0 (prim - m.1 1) 1)
                           (let ([tmp.3 (call ack$0 m.1 (prim - n.2 1))])
                             (call ack$0 (prim - m.1 1) tmp.3)))))])
   (call ack$0 2 4))
 (letrec ([ack$0 (lambda (m.1 n.2)
                   (if (prim = m.1 0)
                       (prim + n.2 1)
                       (if (if (prim > m.1 0) (prim = n.2 0) (false))
                           (call ack$0 (prim - m.1 1) 1)
                           (call
                             ack$0
                             (prim - m.1 1)
                             (call ack$0 m.1 (prim - n.2 1))))))])
   (call ack$0 2 4))
 (letrec ([fib$0 (lambda (n.2) (call fib$1 n.2 0 1))]
          [fib$1 (lambda (n.3 a.4 b.5)
                   (if (prim = n.3 0)
                       a.4
                       (call fib$1 (prim - n.3 1) b.5 (prim + b.5 a.4))))])
   (call fib$0 5))
 (letrec ([if-test$1 (lambda ()
                       (let ([x.5 5])
                         (prim
                           *
                           (if (prim = x.5 5)
                               (prim + x.5 10)
                               (prim - x.5 10))
                           10)))])
   (call if-test$1))
 (letrec ([if-test$1 (lambda ()
                       (let ([x.5 (alloc 16)])
                         (prim
                           *
                           (if (begin
                                 (mset! x.5 8 5)
                                 (prim = (mref x.5 8) 5))
                               (prim + (mref x.5 8) 10)
                               (prim - (mref x.5 8) 10))
                           10)))])
   (call if-test$1))
 (letrec ([if-test$2 (lambda ()
                       (let ([x.5 (alloc 8)])
                         (begin
                           (mset!
                             x.5
                             0
                             (if (begin
                                   (mset! x.5 0 7)
                                   (if (prim < (mref x.5 0) 1)
                                       (false)
                                       (prim < (mref x.5 0) 10)))
                                 (prim * (mref x.5 0) 2)
                                 (prim + (mref x.5 0) 5)))
                           (mref x.5 0))))])
   (call if-test$2))
 (letrec ([if-test$3 (lambda (n.1)
                       (if (if (prim = n.1 0)
                               (true)
                               (if (prim = n.1 1) (true) (prim = n.1 2)))
                           (prim * n.1 5)
                           (prim - n.1 5)))])
   (call if-test$3 2))
 (letrec ([if-test$4 (lambda (x.5)
                       (prim
                         *
                         (if (if (prim = x.5 10) (false) (true))
                             (prim + x.5 10)
                             (prim - x.5 2))
                         10))])
   (call if-test$4 2))
 (letrec ([if-test$5 (lambda (n.1 x.2 y.3)
                       (begin
                         (if (prim = n.1 0)
                             (mset!
                               x.2
                               0
                               (prim + (mref x.2 0) (mref y.3 0)))
                             (mset!
                               y.3
                               0
                               (prim + (mref y.3 0) (mref x.2 0))))
                         (mset! x.2 0 (prim + (mref x.2 0) n.1))
                         (if (if (prim = n.1 (mref y.3 0)) (false) (true))
                             (prim + n.1 (mref x.2 0))
                             (prim + n.1 (mref y.3 0)))))])
   (let ([q.6 (alloc 8)] [p.7 (alloc 8)])
     (begin
       (mset! q.6 0 1)
       (mset! p.7 0 2)
       (call if-test$5 3 q.6 p.7))))
 (letrec ([if-test$6 (lambda (n.0)
                       (let ([n.1 (alloc 8)]
                             [x.2 (alloc 8)]
                             [y.3 (alloc 8)])
                         (begin
                           (mset! n.1 0 n.0)
                           (mset! x.2 0 1)
                           (mset! y.3 0 1)
                           (if (prim = (mref n.1 0) 0)
                               (mset!
                                 (mref x.2 0)
                                 0
                                 (prim + (mref x.2 0) (mref y.3 0)))
                               (mset!
                                 y.3
                                 0
                                 (prim + (mref y.3 0) (mref x.2 0))))
                           (mset! x.2 0 (prim + (mref x.2 0) (mref n.1 0)))
                           (if (if (prim = (mref n.1 0) (mref y.3 0))
                                   (false)
                                   (true))
                               (mset!
                                 n.1
                                 0
                                 (prim + (mref n.1 0) (mref x.2 0)))
                               (mset!
                                 n.1
                                 0
                                 (prim + (mref n.1 0) (mref y.3 0))))
                           (prim + (mref x.2 0) (mref n.1 0)))))])
   (call if-test$6 1))
 (letrec ()
   (let ([x.1 0] [y.2 1] [z.3 (alloc 8)])
     (begin
       (if (if (prim = x.1 0) (prim = y.2 1) (false))
           (mset! z.3 0 5)
           (begin
             (mset! z.3 0 5)
             (mset! z.3 0 (prim + (mref z.3 0) (mref z.3 0)))))
       (mref z.3 0))))
 (letrec ([main$0 (lambda (x.1 y.2)
                    (let ([z.3 (if (if (prim = x.1 1)
                                       (true)
                                       (prim = y.2 1))
                                   1
                                   0)])
                      (prim * z.3 5)))])
   (call main$0 1 0))
 (letrec ([main$0 (lambda (a.3 b.4)
                    (let ([a.1 (alloc 8)] [b.2 (alloc 8)])
                      (begin
                        (mset! a.1 0 a.3)
                        (mset! b.2 0 b.4)
                        (if (if (prim = (mref a.1 0) 1)
                                (prim = (mref b.2 0) 1)
                                (true))
                            (mset! a.1 0 1)
                            (mset! b.2 0 0))
                        (mset! b.2 0 (prim * (mref b.2 0) 10))
                        (mset! a.1 0 (prim + (mref a.1 0) (mref b.2 0)))
                        (mref a.1 0))))])
   (call main$0 0 1))
 (letrec ([main$0 (lambda (a.1 b.2)
                    (if (if (prim = a.1 1) (prim = b.2 1) (true)) 1 0))])
   (call main$0 1 0))
 (letrec ([main$0 (lambda (a.1 b.2)
                    (if (if (prim = a.1 1) (prim = b.2 1) (true)) 1 0))])
   (call main$0 0 0))
 (letrec ()
   (let ([a.1 1] [b.2 1])
     (if (if (prim = a.1 1) (prim = b.2 1) (true)) 1 0)))
 (letrec ()
   (let ([n.1 (let ([p.7 (alloc 8)])
                (begin (mset! p.7 0 1) p.7))])
     (begin
       (let ([a.2 2])
         (begin
           (let ([b.3 3])
             (begin
               (mset!
                 n.1
                 0
                 (prim
                   +
                   (mref n.1 0)
                   (if (prim = (prim + (mref n.1 0) b.3) b.3) 5 10)))
               (mset! n.1 0 (prim + (mref n.1 0) b.3))))
           (mset! n.1 0 (prim + (mref n.1 0) a.2))))
       (prim + (mref n.1 0) (mref n.1 0)))))
 (letrec ()
   (let ([a.1 1] [b.2 2] [c.3 3] [d.4 4] [e.5 5])
     (prim + (prim + (prim + (prim + e.5 d.4) c.3) b.2) a.1)))
 (letrec ()
   (let ([a.1 1] [b.2 2] [c.3 3] [d.4 4] [e.5 5] [f.6 6])
     (let ([a.7 (if (prim > (prim + a.1 d.4) f.6)
                    (prim * a.1 (prim + c.3 f.6))
                    (prim * a.1 (prim + b.2 e.5)))])
       a.7)))
 (letrec ([dot$0 (lambda (a.1 a.2 a.3 a.4 b.5 b.6 b.7 b.8)
                   (prim
                     +
                     (prim * a.1 b.5)
                     (prim
                       +
                       (prim * a.2 b.6)
                       (prim + (prim * a.3 b.7) (prim * a.4 b.8)))))])
   (call dot$0 2 4 6 8 1 3 5 7))
 (letrec ([dot-double-first$51 (lambda (a.1 a.2 a.3 a.4 b.5
                                        b.6 b.7 b.8)
                                 (call dot$50 (prim + a.1 a.1) (prim + a.2 a.2)
                                   (prim + a.3 a.3) (prim + a.4 a.4) b.5
                                   b.6 b.7 b.8))]
          [dot$50 (lambda (a.11 a.12 a.13 a.14 b.15 b.16 b.17 b.18)
                    (prim
                      +
                      (prim * a.11 b.15)
                      (prim
                        +
                        (prim * a.12 b.16)
                        (prim + (prim * a.13 b.17) (prim * a.14 b.18)))))])
   (call dot-double-first$51 2 4 6 8 1 3 5 7))
 (letrec ([dot-double-first$51 (lambda (a.1 a.2 a.3 a.4 b.5
                                        b.6 b.7 b.8)
                                 (let ([a.21 (prim + a.1 a.1)]
                                       [a.22 (prim + a.2 a.2)]
                                       [a.23 (prim + a.3 a.3)]
                                       [a.24 (prim + a.4 a.4)])
                                   (call dot$50 a.21 a.22 a.23 a.24 b.5 b.6
                                     b.7 b.8)))]
          [dot$50 (lambda (a.11 a.12 a.13 a.14 b.15 b.16 b.17 b.18)
                    (prim
                      +
                      (prim * a.11 b.15)
                      (prim
                        +
                        (prim * a.12 b.16)
                        (prim + (prim * a.13 b.17) (prim * a.14 b.18)))))])
   (call dot-double-first$51 2 4 6 8 1 3 5 7))
 (letrec ()
   (let ([a.1 1]
         [b.2 2]
         [c.3 3]
         [d.4 4]
         [e.5 5]
         [f.6 6]
         [g.7 7]
         [h.8 8]
         [i.9 9]
         [j.10 10]
         [k.11 11]
         [l.12 12]
         [m.13 13])
     (let ([a.51 (prim
                   +
                   (prim - (prim + a.1 b.2) (prim + (prim - c.3 d.4) e.5))
                   f.6)])
       (let ([a.52 (prim
                     +
                     (prim - a.51 g.7)
                     (prim + h.8 (prim - i.9 (prim + j.10 k.11))))])
         (let ([a.53 (prim + a.52 (prim + l.12 m.13))])
           (let ([n.14 14]
                 [o.15 15]
                 [p.16 16]
                 [q.17 17]
                 [r.18 18]
                 [s.19 19]
                 [t.20 20]
                 [u.21 21]
                 [v.22 22]
                 [w.23 23]
                 [x.24 24]
                 [y.25 25])
             (let ([a.54 (prim
                           +
                           (prim
                             +
                             (prim
                               +
                               (prim
                                 +
                                 (prim
                                   +
                                   (prim
                                     +
                                     (prim
                                       +
                                       (prim
                                         +
                                         (prim
                                           +
                                           (prim
                                             +
                                             (prim
                                               +
                                               (prim + a.53 n.14)
                                               o.15)
                                             p.16)
                                           q.17)
                                         r.18)
                                       s.19)
                                     t.20)
                                   u.21)
                                 v.22)
                               w.23)
                             x.24)
                           y.25)])
               (let ([z.26 26]
                     [b.82 27]
                     [c.83 28]
                     [d.84 29]
                     [e.85 30]
                     [f.86 31]
                     [g.87 32]
                     [h.88 33]
                     [i.89 34]
                     [j.810 35]
                     [k.811 36]
                     [l.812 37])
                 (prim
                   +
                   a.54
                   (prim
                     +
                     z.26
                     (prim
                       +
                       b.82
                       (prim
                         +
                         c.83
                         (prim
                           +
                           d.84
                           (prim
                             +
                             e.85
                             (prim
                               +
                               f.86
                               (prim
                                 +
                                 g.87
                                 (prim
                                   +
                                   h.88
                                   (prim
                                     +
                                     i.89
                                     (prim
                                       +
                                       j.810
                                       (prim
                                         +
                                         k.811
                                         l.812))))))))))))))))))))
 (letrec ()
   (let ([a.1 1] [b.2 2])
     (let ([c.3 a.1] [d.4 4] [e.5 5] [f.6 b.2])
       (let ([f.16 (prim + f.6 c.3)])
         (let ([f.26 (prim + f.16 d.4)])
           (let ([f.36 (prim + f.26 e.5)] [g.7 7])
             (prim + f.36 g.7)))))))
 (letrec ()
   (let ([h.8 77]
         [i.9 88]
         [j.10 99]
         [k.11 111]
         [a.1 1]
         [b.2 2])
     (let ([c.3 a.1] [d.4 4] [e.5 5] [f.6 b.2])
       (let ([f.16 (prim + f.6 c.3)])
         (let ([f.26 (prim + f.16 d.4)])
           (let ([f.36 (prim + f.26 e.5)] [g.7 7])
             (let ([f.46 (prim + f.36 g.7)])
               (let ([f.56 (prim + f.46 i.9)])
                 (let ([f.66 (prim + f.56 j.10)])
                   (let ([f.76 (prim + f.66 k.11)])
                     (prim + f.76 h.8)))))))))))
 (letrec ()
   (let ([a.1 1]
         [b.2 2]
         [c.3 3]
         [d.4 4]
         [e.5 5]
         [f.6 6]
         [g.7 7]
         [h.8 8]
         [i.9 9]
         [j.10 10]
         [k.11 11]
         [l.12 12]
         [m.13 13]
         [n.14 14]
         [o.15 15]
         [p.16 16]
         [q.17 17]
         [r.18 18]
         [s.19 19]
         [t.20 20]
         [u.21 21]
         [v.22 22]
         [w.23 23]
         [x.24 24]
         [y.25 25]
         [z.26 26])
     (let ([a.101 (prim
                    +
                    a.1
                    (prim
                      +
                      b.2
                      (prim
                        +
                        c.3
                        (prim
                          +
                          d.4
                          (prim
                            +
                            e.5
                            (prim
                              +
                              f.6
                              (prim
                                +
                                g.7
                                (prim
                                  +
                                  h.8
                                  (prim
                                    +
                                    i.9
                                    (prim
                                      +
                                      j.10
                                      (prim
                                        +
                                        k.11
                                        (prim
                                          +
                                          l.12
                                          (prim
                                            +
                                            m.13
                                            (prim
                                              +
                                              n.14
                                              (prim
                                                +
                                                o.15
                                                (prim
                                                  +
                                                  p.16
                                                  (prim
                                                    +
                                                    q.17
                                                    (prim
                                                      +
                                                      r.18
                                                      (prim
                                                        +
                                                        s.19
                                                        (prim
                                                          +
                                                          t.20
                                                          (prim
                                                            +
                                                            u.21
                                                            (prim
                                                              +
                                                              v.22
                                                              (prim
                                                                +
                                                                w.23
                                                                (prim
                                                                  +
                                                                  x.24
                                                                  (prim
                                                                    +
                                                                    y.25
                                                                    z.26)))))))))))))))))))))))))]
           [b.202 27]
           [c.203 28]
           [d.204 29]
           [e.205 30]
           [f.206 31]
           [g.207 32]
           [h.208 33]
           [i.209 34]
           [j.2010 35]
           [k.2011 36]
           [l.2012 37]
           [m.2013 38]
           [n.2014 39]
           [o.2015 40])
       (let ([a.102 (prim
                      +
                      a.101
                      (prim
                        +
                        b.202
                        (prim
                          +
                          c.203
                          (prim
                            +
                            d.204
                            (prim
                              +
                              e.205
                              (prim
                                +
                                f.206
                                (prim
                                  +
                                  g.207
                                  (prim
                                    +
                                    h.208
                                    (prim
                                      +
                                      i.209
                                      (prim
                                        +
                                        j.2010
                                        (prim
                                          +
                                          k.2011
                                          (prim
                                            +
                                            l.2012
                                            (prim
                                              +
                                              m.2013
                                              (prim
                                                +
                                                n.2014
                                                o.2015))))))))))))))])
         (prim + a.102 a.1))))))
