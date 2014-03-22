(library (Compiler specify-representation)
	 (export specify-representation)
	 (import
	  (chezscheme)
	  (source-grammar)
	  (Framework helpers)
	  (Framework nanopass))

	 ;; Midterm TODO: Complete this pass definition.

	 (define-pass specify-representation : LverifyScheme (x) -> LspecifyRepresentation ()))
