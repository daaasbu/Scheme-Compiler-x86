;; P423 Drivers

;; Copyright (C) 2011 Aaron W. Hsu {arcfide@sacrideo.us}
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adapted for P423 Spring 2012 by Claire Alvis
;; Adapted for P423/P523 Spring 2014 (nanopass) by Tim Zakian
;;
;; These drivers allow you to define compilers and wrappers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; DEFINING COMPILERS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This library can be used to define custom sets of compiler
;; passes. Generally this is done to chain the full set of passes for
;; each assignment together, can also be used to run a subset of those
;; passes. For each pass, the library tests the output value of the
;; expression by using the associated wrapper, and so tries to ensure
;; correctness at each step of the compiler.
;;
;; Simple usage:
;; (define-compiler (<name> <name-stepper> <wrapper-proc>)  <specs> ...)
;;
;; This defines a compiler <name> and and alternate compiler for 
;; debugging called <name-passes>, and uses a procedure <wrapper-proc>
;; to retrieve the appropriate wrapper for each pass.  An example of a
;; <wrapper-proc> is the pass->wrapper procedure provided in
;; wrappers.ss. Each <spec> should be one of the following:
;;
;;   (<pass>)
;;       where pass is the name of the pass to call
;;   (<pass> <emit>)
;;       where pass is the name of the pass to call, and <emit> is a
;;       procedure to run immediately afterwards. <emit> should print
;;       an expression an output file, returning where that file can
;;       be found. For example, the generate-x86-64 pass of the P423
;;       compiler uses an "assemble" procedure to output the x86_64 to
;;       a file.
;;
;; Or, one of the customizations found below...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customizations:
;; If you already have a compiler defined, and would like to customize
;; it a bit, there are a few additional options for specs.
;;
;;   (trace <pass>)
;;      Traces the pass, printing the input and output for each
;;      invocation of the pass.
;;   (iterate <spec>+)
;;      Iterates over the set defined by <spec>, where each <spec> is
;;      a compiler pass. Iteration ends when a condition defined by
;;      the "break/when" form holds.
;;   (break/when predicate?)
;;      Specifies the stopping condition for an "iterate" form.
;;
;; For example, if we have passes "pass1" "pass2" "pass3", and we want
;; to repeat them until a predicate "everything-okay?" is true, we
;; would write:
;;   
;;   (iterate (pass1) (pass2) (pass3)
;;      (break/when everything-okay?))
;;
;; If at the same time we wanted to trace pass2, we could add a trace form...
;;
;;   (iterate (pass1) (trace pass2) (pass3)
;;      (break/when everything-okay?))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; DEFINING WRAPPERS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These drivers also allow you to define language-wrappers, to
;; transform an incoming expression into an expression that can be
;; evaluated in Scheme.
;;
;; (define-language-wrapper <name>
;;   (<input>)
;;   (environment <env>)?
;;   <expression>+)
;;       Defines a single wrapper with name <name>, <input> as the
;;       expression to evaluate, and the <expression>+ to define the code
;;       to convert the <input> to a Scheme-evaluable expression. The
;;       macro optionally accepts an environment argument in the
;;       (environment <env>) form.
;;
;; (define-language-wrapper (<name>+)
;;   (<input>)
;;   (environment <env>)?
;;   <expression>+)
;;    Similar to the definition above, except it accepts one or more
;;    wrappers with the set of names <name>+ given. It takes a single
;;    <input> and <expression> and defines the same wrapper for each
;;    <name>. This form also takes an optional environment argument in
;;    the (environment <env>).
;;
;; All the wrappers needed for P423 are defined in wrappers.ss as they
;; become necessary.

#!chezscheme
(library
  (Framework driver aux)
  (export
    iterate
    break/when
    verify-pass-specifications
    verify-iterated-pass-specifications
    &pass-verification-violation
    wrapper-violation?
    wrapper-violation-name
    &wrapper-violation
    make-wrapper-violation
    make-pass-verification-violation
    pass-verification-violation?
    pass-verification-violation-pass
    pass-verification-violation-input
    pass-verification-violation-output
    pass-verification-violation-input-result
    pass-verification-violation-output-result)
  (import (chezscheme))

(define-syntax (iterate x)
  (syntax-violation #f "misplaced aux keyword" x))
(define-syntax (break/when x)
  (syntax-violation #f "misplaced aux keyword" x))

(define (verify-pass-specifications x)
  (syntax-case x (iterate break/when trace)
    [() #t]
    [((iterate . specs) . rest)
     (begin
       (verify-iterated-pass-specifications #'specs)
       (verify-pass-specifications #'rest))]
    [((break/when pred?) . rest)
     (syntax-violation 'define-compiler
       "break encountered outside of an iteration"
       x #'(break/when pred?))]
    [((pass wrapper unparser) . rest) (identifier? #'pass)
     (verify-pass-specifications #'rest)]
    [((trace pass wrapper unparser) . rest) (identifier? #'trace)
     (verify-pass-specifications #'rest)]
    [((trace pass wrapper unparser assemble) . rest) (identifier? #'trace)
     (syntax-violation 'define-compiler
       "Cannot trace emit passes" #'pass)]
    [((pass wrapper unparser assemble) . rest) (identifier? #'pass)
     (verify-pass-specifications #'rest)]
    [(bad . rest)
     (syntax-violation 'define-compiler
       "invalid pass specification" x #'bad)]
    [else
      (syntax-violation 'define-compiler
        "invalid pass specifications" x)]))

(define verify-iterated-pass-specifications
  (case-lambda
    [(x) (verify-iterated-pass-specifications x #f x)]
    [(orig break? x)
     (syntax-case x (iterate break/when trace)
       [()
        (unless break?
          (syntax-violation 'define-compiler "no break in iterate" orig))
        #t]
       [((break/when pred?) . rest)
        (verify-iterated-pass-specifications orig #t #'rest)]
       [((iterate spec1 spec2 ...) . rest)
        (verify-iterated-pass-specifications #'(spec1 spec2 ...))
        (verify-iterated-pass-specifications orig break? #'rest)]
       [((pass wrapper unparser) . rest) (identifier? #'pass)
        (verify-iterated-pass-specifications orig break? #'rest)]
       [((trace pass wrapper unparser) . rest) (identifier? #'pass)
        (verify-iterated-pass-specifications orig break? #'rest)]
       [((pass wrapper unparser assemble) . rest) (identifier? #'pass)
        (syntax-violation 'define-compiler
          "emit pass encountered during iteration"
          orig #'(pass wrapper assemble))]
       [(wrong . rest)
        (syntax-violation 'define-compiler "invalid pass" orig #'wrong)]
       [wrong
         (syntax-violation
           'define-compiler "invalid, dotted form" orig)])]))

(define-condition-type &wrapper-violation &error
  make-wrapper-violation wrapper-violation?
  (name wrapper-violation-name))

(define-condition-type &pass-verification-violation &condition
  make-pass-verification-violation
  pass-verification-violation?
  (pass pass-verification-violation-pass)
  (input pass-verification-violation-input)
  (output pass-verification-violation-output)
  (input-result pass-verification-violation-input-result)
  (output-result pass-verification-violation-output-result))

)

#!chezscheme
(library
  (Framework driver)
  (export
    iterate
    break/when
    environment
    &wrapper-violation
    make-wrapper-violation
    make-pass-verification-violation
    wrapper-violation-name
    wrapper-violation?
    pass-verification-violation?
    pass-verification-violation-pass
    &pass-verification-violation
    pass-verification-violation-input
    pass-verification-violation-input-result
    pass-verification-violation-output
    pass-verification-violation-output-result
    display-pass-verification-violation
    define-compiler
    define-language-wrapper)
  (import
    (chezscheme)
    (Framework driver aux))

;;Defining Wrappers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-language-wrapper
  (syntax-rules (environment)
    [(_ (n1 n2 ...) (args ...) exps ...)
     (begin
       (define-language-wrapper n1 (args ...) exps ...)
       (define-language-wrapper n2 (args ...) exps ...)
       ...)]
    [(_ name (args ...) (environment env) exps ...)
     (define name
       (let ([the-env env])
         (lambda (args ...)
           (wrapper-body name the-env exps ...))))]
    [(_ name (args ...) exps ...)
     (begin (define env (environment '(chezscheme)))
            (define-language-wrapper name (args ...)
              (environment env)
              exps ...))]))

(define-syntax wrapper-body
  (syntax-rules ()
    ((_ name the-env . exps)
     (with-exception-handler
       (lambda (c)
         (cond
           [(warning? c) (display-condition c)]
           [else (raise (condition (make-wrapper-violation 'name) c))]))
       (lambda () (eval `(let () . exps) the-env))))))

;;Defining Compilers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax compose-passes
  (syntax-rules (iterate break/when trace)
    [(_ k (input source-wrapper) source-unparser) input]
    [(_ k (input source-wrapper) source-unparser (iterate . specs) . rest)
     (compose-passes k
       ((run-iterated-pass input source-wrapper source-unparser . specs)
        (next-wrapper source-wrapper source-unparser . specs))
        (next-unparser source-wrapper source-unparser . specs) . rest)]
    [(_ k (input source-wrapper) source-unparser (break/when pred?) . rest)
     (begin
       (when (not (syntax->datum #'k))
         (syntax-violation 'define-compiler
           "break encountered outside of iterate clause"
           #'(break/when pred?)))
       #t)
     (compose-passes k
       ((let ([inv input]) (if (pred? inv) (k inv) inv))
        source-wrapper)
       source-unparser . rest)]
    [(_ k (input source-wrapper) source-unparser (pass wrapper unparser) . rest)
     (compose-passes k
       ((run-pass input source-wrapper source-unparser wrapper unparser pass) wrapper)
       unparser . rest)]
    [(_ k (input source-wrapper) source-unparser (trace pass wrapper unparser) . rest)
     ;; Unparse to make it human readble
     (let ([pass (lambda (x) 
                   (printf "<~a>\n" 'pass) 
                   (pretty-print (source-unparser x)) 
                   (newline) 
                   (pass x))])
       (compose-passes k (input source-wrapper) 
                       source-unparser (pass wrapper unparser) . rest))]
    [(_ k (input source-wrapper) source-unparser (pass wrapper unparser assemble) . rest)
     (begin
       (when (syntax->datum #'k)
         (syntax-violation 'define-compiler
           "unexpected emit pass inside iteration"
           #'(pass wrapper assemble)))
       (when (not (null? (syntax->datum #'rest)))
         (syntax-violation 'define-compiler
           "non-final assemble pass"
           #'(pass wrapper assemble)))
       #t)
     (run-emit-pass input source-wrapper source-unparser wrapper assemble pass)]))

(define-syntax next-wrapper
  (syntax-rules (iterate break/when trace)
    [(_ src specs ... (pass wrapper unparser) (break/when . rest)) wrapper]
    [(_ src specs ... (trace pass wrapper unparser) (break/when . rest)) wrapper]
    [(_ src (break/when . rest)) src]
    [(_ src specs ... last) (next-wrapper src specs ...)]))

(define-syntax next-unparser
  (syntax-rules (iterate break/when trace)
    [(_ src specs ... (pass wrapper unparser) (break/when . rest)) unparser]
    [(_ src specs ... (trace pass wrapper unparser) (break/when . rest)) unparser]
    [(_ src (break/when . rest)) src]
    [(_ src specs ... last) (next-unparser src specs ...)]))

(define-syntax run-pass
  (syntax-rules ()
    [(_ input input-wrapper input-unparser output-wrapper output-unparser pass)
     (let ([inv input])
       (let ([output (pass inv)])
         (let ([input-res (input-wrapper (input-unparser inv))]
               [output-res (output-wrapper (output-unparser output))])
           (verify-against (input-unparser inv) input-res (output-unparser output) output-res pass))
         output))]))

(define-syntax run-emit-pass
  (syntax-rules ()
    [(_ input input-wrapper input-unparser output-wrapper assemble pass)
     (let ([inv input])
       (let ([output (assemble (lambda () (pass inv)))])
         (let ([input-res (input-wrapper (input-unparser inv))]
               [output-res (output-wrapper output)])
           (verify-against (input-unparser inv) input-res output output-res pass))
         (void)))]))

(define-syntax run-iterated-pass
  (syntax-rules ()
    [(_ input input-wrapper input-unparser . specs)
     (call-with-current-continuation
       (lambda (k)
         (let loop ([x input])
           (loop (compose-passes k (x input-wrapper) input-unparser . specs)))))]))

(define-syntax define-compiler-aux
  (syntax-rules ()
    ((_ (bindings ...) (name source-wrapper) (n source-unparser) parser . specs)
     (verify-pass-specifications #'specs)
     (define name
       (lambda (x)
         ((lambda (input)
            (let (bindings ...)
              (compose-passes #f (input source-wrapper) source-unparser . specs)))
          (parser x)))))))

(define-syntax rewrite-specs
  (syntax-rules (iterate trace break/when %)
    [(_ name wp up parser (specs ...) (b ...))
     (define-compiler-aux ((sw (wp 'source)) (su (up 'source)) b ...) (name sw) (name su) parser specs ...)]
    [(_ name wp up parser ((specs ...) % ispecs ...) (b ...) % rest ...)
     (rewrite-specs name wp up parser 
       (specs ... (iterate ispecs ...)) (b ...) rest ...)]
    [(_ name wp up parser (specs ...) (b ...) (iterate spec1 spec2 ...) rest ...)
     (rewrite-specs name wp up parser
       ((specs ...) %) (b ...) spec1 spec2 ... % rest ...)]
    [(_ name wp up parser (specs ...) (b ...) (trace pass foo ...) rest ...)
     (rewrite-specs name wp up parser
       (specs ... (trace pass w u foo ...))
       (b ... (u (up 'pass)) (w (wp 'pass)))
       rest ...)]
    [(_ name wp up parser (specs ...) (b ...) (break/when foo ...) rest ...)
     (rewrite-specs name wp up parser
       (specs ... (break/when foo ...)) (b ...) rest ...)]
    [(_ name wp up parser (specs ...) (b ...) (pass foo ...) rest ...)
     (rewrite-specs name wp up parser
       (specs ... (pass w u foo ...))
       (b ... (u (up 'pass)) (w (wp 'pass)))
       rest ...)]))

(define-syntax define-compiler
  (syntax-rules ()
    ((_ (name wrapper-proc unparser-proc parser) spec1 spec2 ...)
     (rewrite-specs name wrapper-proc unparser-proc parser () () spec1 spec2 ...))
    ((_ (name name-step wrapper-proc unparser-proc parser) spec1 spec2 ...)
     (begin
       (define-compiler (name wrapper-proc unparser-proc parser) spec1 spec2 ...)
       (define-compiler-step name-step unparser-proc parser spec1 spec2 ...)))))

;;Defining the Stepper;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax run-one-pass
  (syntax-rules ()
    ((_ unparser pass inp)
     (begin
       (printf "\nPass: ~s\n" pass)
       (let ((res (pass inp)))
         (begin
           (printf "\nOutput: \n")
           (pretty-print ((unparser 'pass) res))
           res))))))

(define-syntax define-compiler-step
  (syntax-rules ()
    ((_ name unparser parser spec1 . spec)
     (define name
       (case-lambda
         ((prog)
          (name prog #f))
         ((prog n)
          (let ((inp (parser prog)) (i n))
            (let-values (((outpur-unparser inp _)
                          (define-compiler-loop unparser inp i spec1 . spec)))
              inp))))))))

(define-syntax define-compiler-loop
  (syntax-rules (trace iterate break/when %)
    ((_ unparser inp i) (values unparser inp i))
    ((_ unparser inp i (trace pass . foo) . rest)
     (define-compiler-loop unparser inp i (pass . foo) . rest))
    ((_ unparser inp i (iterate . ispecs) . rest)
     (letrec ((loop
                (lambda (unparser^ inp^ i^)
                  (define-iteration-loop unparser^ inp^ i^ loop . ispecs))))
       (let-values (((unparser^ inp^ i^) (loop unparser inp i)))
         (define-compiler-loop unparser^ inp^ i^ . rest))))
    ((_ unparser inp i (pass . foo) . rest)
     (if (and i (zero? i))
         (values unparser inp i)
         (let ((res (run-one-pass unparser pass inp)) (i (and i (sub1 i))) )
           (define-compiler-loop unparser res i . rest))))))

(define-syntax define-iteration-loop
  (syntax-rules ()
    ((_ unparser inp i jump)
     (jump unparser inp i))
    ((_ unparser inp i jump (break/when pred?) . rest)
     (if (and i (zero? i))
         (values unparser inp i)
         (let ((stop? (pred? inp)))
           (if stop?
               (begin
                 (printf "\nBreaking iteration after ~s\n" pred?)
                 (values unparser inp i))
               (begin
                 (printf "\nBreak/when predicate ~s was not true, iteration continues\n" pred?)
                 (define-iteration-loop unparser inp i jump . rest))))))
    ((_ unparser inp i jump spec . rest)
     (if (and i (zero? i))
         (values unparser inp i)
         (let-values (((unparser^ inp^ i^) (define-compiler-loop unparser inp i spec)))
           (define-iteration-loop unparser^ inp^ i^ jump . rest))))))

;;Other;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (verify-against inv input-res output output-res pass)
  (define (stringify x)
    (if (string? x)
        x
        (with-output-to-string (lambda () (write x)))))
  (unless (string=? (stringify input-res) (stringify output-res))
    (raise
      (condition
        (make-error)
        (make-format-condition)
        (make-irritants-condition (list pass))
        (make-pass-verification-violation
          pass inv output input-res output-res)
        (make-message-condition "~a failed verification")))))

(define display-pass-verification-violation
  (case-lambda
    [(condition)
     (%dpvv condition (current-output-port))]
    [(condition oport) 
     (%dpvv condition oport)]))

(define (%dpvv c p)
  (begin
    (assert (pass-verification-violation? c))
    (assert (output-port? p))
    (assert (textual-port? p))
    (format p
      "Verification of pass ~a failed.~n"
      (pass-verification-violation-pass c))
    (format p "~8,8tInput Pass:~n")
    (parameterize ([pretty-initial-indent 0])
      (pretty-print (pass-verification-violation-input c) p))

    (format p "Result of evauluating Input: ~a~n~n"
      (pass-verification-violation-input-result c))

    (format p "~8,8tPass Output:~n")
    (parameterize ([pretty-initial-indent 0])
      (pretty-print (pass-verification-violation-output c) p))
    (format p "Result of evauluating Output: ~a~n"
      (pass-verification-violation-output-result c))))

)


