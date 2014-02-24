(library (Compiler compile)
(export p423-compile p423-step)
(import
;; Load Chez Scheme primitives:
(chezscheme)
;; Load provided compiler framework:
(Framework driver)
(Framework wrappers)
(Framework nanopass)
(Framework helpers)
;; Load your passes from the files you wrote them in:
(Compiler verify-scheme)
(Compiler remove-complex-opera*)
(Compiler flatten-set!)
(Compiler impose-calling-conventions)
(Compiler uncover-frame-conflict)
(Compiler introduce-allocation-forms)
(Compiler select-instructions)
(Compiler uncover-register-conflict)
(Compiler assign-registers)
(Compiler everybody-home?)
(Compiler assign-frame)
(Compiler finalize-frame-locations)
(Compiler discard-call-live)
(Compiler finalize-locations)
(Compiler expose-frame-var)
(Compiler expose-basic-blocks)
(Compiler flatten-program)
(Compiler generate-x86-64)
)

;; Given a thunk that generates assembly code, this will compile the

;; resulting assembly code and output it to a file named t.s
(define (assemble thunk)
(with-output-to-file "t.s"
thunk
'replace)
(unless (zero? (system "cc -m64 -o t t.s Framework/runtime.c"))
(error 'assemble "assembly failed"))
;; By convention, return the command which will run the code:
"./t")

(define-compiler (p423-compile p423-step pass->wrapper pass->unparser parse-LverifyScheme)
(verify-scheme)
(remove-complex-opera*)
(flatten-set!)
(impose-calling-conventions)
(uncover-frame-conflict)
(introduce-allocation-forms)
(iterate
(select-instructions)
(uncover-register-conflict)
(assign-registers)
(break/when everybody-home?)
(assign-frame)
(finalize-frame-locations))
(discard-call-live)
(finalize-locations)
(expose-frame-var)
(expose-basic-blocks)
(flatten-program)
(generate-x86-64 assemble)))


 ;; End library