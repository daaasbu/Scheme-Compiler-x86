<<<<<<< HEAD
(library (Compiler compile)
=======
 (library (Compiler compile)
>>>>>>> a4
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
    (Compiler uncover-frame-conflict)
    (Compiler introduce-allocation-forms)
    (Compiler select-instructions)
    (Compiler uncover-register-conflict)
    (Compiler assign-registers)
    (Compiler everybody-home)
    (Compiler assign-frame)
    (Compiler finalize-frame-locations)
    (Compiler discard-call-live)
    (Compiler finalize-locations)
    (Compiler expose-frame-var)
    (Compiler expose-basic-blocks)
    (Compiler flatten-program)
    (Compiler generate-x86-64)
<<<<<<< HEAD
    )
=======
)

;; Given a thunk that generates assembly code, this will compile the
>>>>>>> a4

;; resulting assembly code and output it to a file named t.s
(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk
    'replace)
  (unless (zero? (system "cc -m64 -o t t.s Framework/runtime.c"))
    (error 'assemble "assembly failed"))
  ;; By convention, return the command which will run the code:
  "./t")

<<<<<<< HEAD
;; Compose the complete Compiler as a pipeline of passes.
(define-compiler (p423-compile p423-step pass->wrapper)
=======
(define-compiler (p423-compile p423-step pass->wrapper pass->unparser parse-LverifyScheme)
>>>>>>> a4
  (verify-scheme)
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
<<<<<<< HEAD
  (generate-x86-64 assemble)) 

=======
   (generate-x86-64 assemble)
>>>>>>> a4
)
) ;; End library

