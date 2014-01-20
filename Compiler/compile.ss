
(library (Compiler compile)
  (export p423-compile p423-step)
<<<<<<< HEAD
  (import
=======
  (import 
>>>>>>> 0160b7f22711de6552f01ba4132edd1a28e61954
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (Framework driver)
    (Framework wrappers)
<<<<<<< HEAD
    (Framework nanopass)
    (Framework helpers)
    ;; Load your passes from the files you wrote them in:
    (Compiler verify-scheme)
    (Compiler generate-x86-64))

;; Given a thunk that generates assembly code, this will compile the
;; resulting assembly code and output it to a file named t.s
(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk
=======
    (Framework match)
    (Framework helpers)
    ;; Load your passes from the files you wrote them in:
    (Compiler verify-scheme)
    (Compiler expose-frame-var)
    (Compiler flatten-program)
    (Compiler generate-x86-64))

;; Given a thunk that generates assembly code, this will compile the 
;; resulting assembly code and output it to a file named t.s
(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk 
>>>>>>> 0160b7f22711de6552f01ba4132edd1a28e61954
    'replace)
  (unless (zero? (system "cc -m64 -o t t.s Framework/runtime.c"))
    (error 'assemble "assembly failed"))
  ;; By convention, return the command which will run the code:
  "./t")

<<<<<<< HEAD
(define-compiler (p423-compile p423-step pass->wrapper pass->unparser parse-LverifyScheme)
  (verify-scheme)
  (generate-x86-64 assemble))
) ;; End library
=======
(define-compiler (p423-compile p423-step pass->wrapper)
  (verify-scheme)
  (expose-frame-var)
  (flatten-program)
  (generate-x86-64 assemble))

)
>>>>>>> 0160b7f22711de6552f01ba4132edd1a28e61954
