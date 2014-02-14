(library (Compiler assign-registers)
  (export assign-registers)
  (import (chezscheme)
    (source-grammar)
    (Framework nanopass)
    (Framework match)
    (Framework helpers))

  ;; We represent a conflict graph is an associate list mapping
  ;; variables to its conflict set: a list of variables and registers
  ;; that it is conflicted with.

 (define-pass assign-registers : LuncoverRegisterConflict (x) -> LassignRegisters ()
  (definitions

    ;; The find-used function returns all of the registers that are
    ;; used by the things (variables or registers) in the list conflict*.
    ;; The helper function set-cons, used below, is defined in (Framework helpers).
    (define find-used
      (lambda (conflict* home*)
        (cond
          [(null? conflict*) '()]
          [(register? (car conflict*))
           (set-cons (car conflict*) (find-used (cdr conflict*) home*))]
          [(assq (car conflict*) home*) =>
           (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*) home*)))]
          [else (find-used (cdr conflict*) home*)])))
    ;; The select-register function tries to find a register for variable var
    ;; that has not already been used for the variables that var is conflicted
    ;; with.
    (define select-register
      (lambda (var conflict* home*)
        (let ([used* (find-used conflict* home*)])
          (let ([available* (difference registers used*)])
            (and (not (null? available*)) (car available*))))))
    ;; The rem-conflicts! function removes variable var from the conflict set
    ;; of each of the variables it is conflicted with (conflict*).
    (define rem-conflicts!
      (lambda (conflict-graph var conflict*)
        (for-each
          (lambda (x)
            (when (uvar? x)
              (let ([a (assq x conflict-graph)])
                (set-cdr! a (remq var (cdr a))))))
          conflict*)))
    ;; The find-homes function implements the register allocation algorithm.
    ;; It tries to assign a register (a home) to each variable in the
    ;; list var* such that conflicted variables are never assigned to the
    ;; same home.
    (define find-homes
      (lambda (var* conflict-graph)
        (define k (length registers))
        (define low-degree?
          (lambda (var)
            (< (length (cdr (assq var conflict-graph))) k)))
        (let loop ([var* var*])
          (if (null? var*)
              '()
              (let ([var (or (find low-degree? var*) (car var*))])
                (let ([conflict* (cdr (assq var conflict-graph))]
[var* (remq var var*)])
                  (rem-conflicts! conflict-graph var conflict*)
                  (let ([home* (loop var*)])
                    (let ([reg (select-register var conflict* home*)])
                      (if reg
                          (cons `[,var ,reg] home*)
                          home*))))))))))
  ;; End definitions
  (Body : Body (x) -> Body ()
    [(locals (,uv* ...) (register-conflict ,cfgraph ,[tl])) ;; A parsing of tl is necessary since we need to convert the grammar
     (let ([home* (find-homes uv* cfgraph)])
       (let ([spill* (difference uv* (map car home*))])
         (let ((uv* (map car home*))
               (r* (map cadr home*)))
         (if (null? spill*)
             `(locate ([,uv* ,r*] ...) ,tl)
             (error who "unable to assign registers to ~s" spill*)))))]
    [else (error who "invalid Body ~s" x)])
  (Prog : Prog (x) -> Prog ()
    [(letrec ([,l* (lambda () ,[Body : -> bd*])] ...) ,[Body : -> bd])
     `(letrec ([,l* (lambda () ,bd*)] ...) ,bd)]
    [else (error who "invalid Program ~s" x)]))
)
