(library (Framework wrappers aux)
  (export
    prim
    call
    handle-overflow
    set!
    rewrite-opnds
    code
    jump
    locals
    ulocals
    spills
    (rename (lambda-p423 lambda))
    register-conflict
    locate
    true
    false
    nop
    frame-conflict)
  (import
    (except (chezscheme) set!)
    (Framework match)
    (Framework helpers))

(define int64-in-range?
  (lambda (x)
    (<= (- (expt 2 63)) x (- (expt 2 63) 1))))

(define handle-overflow
  (lambda (x)
    (cond
      [(not (number? x)) x]
      [(int64-in-range? x) x]
      [(not (= x (logand 18446744073709551615 x)))
       (handle-overflow (logand 18446744073709551615 x))]
      [(< x 0) (handle-overflow (+ x (expt 2 64)))]
      [else (handle-overflow (- x (expt 2 64)))])))

(define rewrite-opnds
  (lambda (x)
    (match x
      ;; Begin Haskell hack for disp/index-opnd read/show invariance
      [(disp ,r ,o)
       `(mref ,r ,o)]
      [(index ,r1 ,r2)
       `(mref ,r1 ,r2)]
      [(set! (disp ,r ,o) ,[expr])
       `(mset! ,r ,o ,expr)]
      [(set! (index ,r1 ,r2) ,[expr])
       `(mset! ,r1 ,r2 ,expr)]
      ;; End hack
      [,r (guard (disp-opnd? r))
       `(mref ,(disp-opnd-reg r) ,(disp-opnd-offset r))]
      [,r (guard (index-opnd? r))
       `(mref ,(index-opnd-breg r) ,(index-opnd-ireg r))]
      [(set! ,r ,[expr]) (guard (disp-opnd? r))
       `(mset! ,(disp-opnd-reg r) ,(disp-opnd-offset r) ,expr)]
      [(set! ,r ,[expr]) (guard (index-opnd? r))
       `(mset! ,(index-opnd-breg r) ,(index-opnd-ireg r) ,expr)]
      [(,[expr] ...) expr]
      [,x x])))

(define-syntax set!
  (let ()
    (import (chezscheme))
    (syntax-rules ()
      [(_ x expr)
       (set! x (handle-overflow expr))])))

(define-syntax code
  (lambda (x)
    (define build
      (lambda (body)
        (syntax-case body ()
          [() #'(())]
          [(label expr ...)
           (identifier? #'label)
           (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
             #'(((bounce label))
                (define label
                  (lambda ()
                    (bounce (lambda () expr ...))))
                defn ...))]
          [(expr1 expr ...)
           (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
             #'((expr1 expr ...) defn ...))])))
    (syntax-case x ()
      [(k expr ...)
       (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
         #'((call/cc
              (lambda (bounce)
                defn ...
                expr ...))))])))

(define-syntax jump
  (syntax-rules ()
    [(_ target) (target)]))

  (define-syntax locals
    (syntax-rules ()
      [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax spills
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax ulocals
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax lambda-p423
    (let ()
      (import scheme)
      (syntax-rules ()
        [(lambda () body) (lambda arg-list body)]
        [(lambda arg-list e e* ...) (lambda arg-list e e* ...)])))

(define-syntax frame-conflict
  (syntax-rules ()
    [(_ ct body) body]))

(define-syntax register-conflict
  (syntax-rules ()
    [(_ ct body) body]))

(define-syntax locate
  (let ()
    (import scheme)
    (syntax-rules ()
      [(_ ([x* loc*] ...) body)
       (let-syntax ([x* (identifier-syntax
                          (id loc*)
                          ((set! id e)
                           (set! loc* (handle-overflow e))))] ...)
         body)])))

(define (true) #t)

(define (false) #f)

(define (nop) (void))

(define-syntax call
  (syntax-rules ()
    ((_ x x* ...) (x x* ...))))

(define-syntax prim
  (syntax-rules ()
    ((_ x x* ...) (x x* ...))))
)

(library (Framework wrappers)
  (export
    pass->unparser
    pass->wrapper
    source/wrapper
    verify-scheme/wrapper
    remove-complex-opera*/wrapper
    flatten-set!/wrapper
    impose-calling-conventions/wrapper
    uncover-frame-conflict/wrapper
    introduce-allocation-forms/wrapper
    select-instructions/wrapper
    uncover-register-conflict/wrapper
    assign-registers/wrapper
    assign-frame/wrapper
    finalize-frame-locations/wrapper
    discard-call-live/wrapper
    finalize-locations/wrapper
    expose-frame-var/wrapper
    expose-basic-blocks/wrapper
    flatten-program/wrapper
    generate-x86-64/wrapper)
  (import
    (chezscheme)
    (source-grammar)
    (Framework match)
    (Framework helpers)
    (Framework driver)
    (only (Framework wrappers aux) rewrite-opnds))

(define env
  (environment
    '(except (chezscheme) set!)
    '(Framework helpers)
    '(Framework helpers frame-variables)))

(define pass->unparser
  (lambda (pass)
    (case pass
      ((source) unparse-LverifyScheme)
      ((verify-scheme) unparse-LverifyScheme)
      ((remove-complex-opera*) unparse-LremoveComplexOpera*)
      ((flatten-set!) unparse-LflattenSet!)
      ((impose-calling-conventions) unparse-LimposeCallingConventions)
      ((uncover-frame-conflict) unparse-LuncoverFrameConflict)
      ((introduce-allocation-forms) unparse-LintroduceAllocationForms)
      ((select-instructions) unparse-LintroduceAllocationForms)
      ((uncover-register-conflict) unparse-LuncoverRegisterConflict)
      ((assign-registers) unparse-LassignRegisters)
      ((assign-frame) unparse-LassignFrame)
      ((finalize-frame-locations) unparse-LintroduceAllocationForms)
      ((discard-call-live) unparse-LdiscardCallLive)
      ((finalize-locations) unparse-LfinalizeLocations)
      ((expose-frame-var) unparse-LexposeFrameVar)
      ((expose-basic-blocks) unparse-LexposeBasicBlocks)
      ((flatten-program) unparse-LflattenProgram)
      ((generate-x86-64) (lambda (x) x))
      (else (errorf 'pass->unparser
              "Unparser for pass ~s not found" pass)))))

(define pass->wrapper
  (lambda (pass)
    (case pass
      ((source) source/wrapper)
      ((verify-scheme) verify-scheme/wrapper)
      ((remove-complex-opera*) remove-complex-opera*/wrapper)
      ((flatten-set!) flatten-set!/wrapper)
      ((impose-calling-conventions) impose-calling-conventions/wrapper)
      ((uncover-frame-conflict) uncover-frame-conflict/wrapper)
      ((introduce-allocation-forms) introduce-allocation-forms/wrapper)
      ((select-instructions) select-instructions/wrapper)
      ((uncover-register-conflict) uncover-register-conflict/wrapper)
      ((assign-registers) assign-registers/wrapper)
      ((assign-frame) assign-frame/wrapper)
      ((finalize-frame-locations) finalize-frame-locations/wrapper)
      ((discard-call-live) discard-call-live/wrapper)
      ((finalize-locations) finalize-locations/wrapper)
      ((expose-frame-var) expose-frame-var/wrapper)
      ((expose-basic-blocks) expose-basic-blocks/wrapper)
      ((flatten-program) flatten-program/wrapper)
      ((generate-x86-64) generate-x86-64/wrapper)
      (else (errorf 'pass->wrapper
              "Wrapper for pass ~s not found" pass)))))

;;-----------------------------------
;; source/wrapper
;; verify-scheme/wrapper
;;-----------------------------------
(define-language-wrapper
  (source/wrapper verify-scheme/wrapper)
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      set! handle-overflow locals true false nop call prim)
    (only (chezscheme) lambda))
  (reset-machine-state!)
  ,x ;,(if (grammar-verification) (verify-grammar:l01-verify-scheme x) x)
  )

;;-----------------------------------
;; remove-complex-opera*/wrapper
;;-----------------------------------
(define-language-wrapper
  (remove-complex-opera*/wrapper)
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      set! handle-overflow locals true false nop call prim)
    (only (chezscheme) lambda))
  (reset-machine-state!)
  ,x ;,(if (grammar-verification) (verify-grammar:l23-remove-complex-opera x) x)
  )

;;-----------------------------------
;; flatten-set!/wrapper
;;-----------------------------------
(define-language-wrapper
  (flatten-set!/wrapper)
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      set! handle-overflow locals true false nop call prim)
    (only (chezscheme) lambda))
  (reset-machine-state!)
  ,x ; ,(if (grammar-verification) (verify-grammar:l24-flatten-set x) x)
  )

;;-----------------------------------
;; impose-calling-conventions/wrapper
;;-----------------------------------
(define-language-wrapper impose-calling-conventions/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      set! handle-overflow lambda locals true false nop call prim))
  (call/cc (lambda (k) (set! ,return-address-register k)
       ,x))
  ,return-value-register)


;;-----------------------------------
;; uncover-frame-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-frame-conflict/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      set! handle-overflow locals lambda true false nop frame-conflict))
  (call/cc (lambda (k) (set! ,return-address-register k)
       ,x))
  ,return-value-register)

;;-----------------------------------
;; introduce-allocation-forms/wrapper
;; finalize-frame-locations/wrapper
;; select-instructions/wrapper
;; assign-frame/wrapper
;;-----------------------------------
(define-language-wrapper
  (introduce-allocation-forms/wrapper
   finalize-frame-locations/wrapper
   select-instructions/wrapper
   assign-frame/wrapper)
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      locals ulocals locate set! handle-overflow
      lambda true false nop frame-conflict))
  (call/cc (lambda (k) (set! ,return-address-register k)
       ,x))
  ,return-value-register)

;;-----------------------------------
;; uncover-register-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-register-conflict/wrapper (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set! locate locals ulocals
      lambda register-conflict frame-conflict true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k)
       ,x))
  ,return-value-register)

;;-----------------------------------
;; assign-registers/wrapper
;;-----------------------------------
(define-language-wrapper assign-registers/wrapper (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set! locate locals ulocals
      spills frame-conflict lambda true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k)
       ,x))
  ,return-value-register)

(define-language-wrapper discard-call-live/wrapper (x)
  (environment env)
  (import (only (Framework wrappers aux)
             handle-overflow set! locate
            true false nop)
    (only (chezscheme) lambda))
  (call/cc (lambda (k) (set! ,return-address-register k)
       ,x))
  ,return-value-register)

(define-language-wrapper finalize-locations/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set! true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k)
       ,x))
  ,return-value-register)

(define-language-wrapper expose-frame-var/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set! true false nop))
  (call/cc
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

(define-language-wrapper expose-basic-blocks/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set!))
  (call/cc
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

(define-language-wrapper flatten-program/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set! code jump))
  (call/cc
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

(define (generate-x86-64/wrapper program)
  (let-values ([(out in err pid)
                (open-process-ports
                  (format "exec '~a'" program)
                  (buffer-mode block)
                  (native-transcoder))])
    (read in)))

)