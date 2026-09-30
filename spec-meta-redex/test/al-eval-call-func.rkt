#lang racket/base

(require rackunit
         "../common/0.0-prelude.rkt"
         "../al/5-eval.rkt"
         "judgment.rkt")

;; A clause or table row: arg* = exp -- prem*
(define (clause-of args exp prems) (list args exp prems))

;; $id(x) = x
(define id-def `(DEF () (,(clause-of '((EXP (VAR "x"))) '(VAR "x") '())) ()))
;; $sign(i) is "pos", "neg", or otherwise "zero".
(define sign-def
  `(DEF ()
        (,(clause-of '((EXP (VAR "i"))) '(TEXT "pos") '((IF (CMP GT (VAR "i") (INT 0)))))
         ,(clause-of '((EXP (VAR "i"))) '(TEXT "neg") '((IF (CMP LT (VAR "i") (INT 0))))))
        (,(clause-of '((EXP (VAR "i"))) '(TEXT "zero") '()))))
;; $is<X>(x) = x <: X
(define is-def `(DEF ("X") (,(clause-of '((EXP (VAR "x"))) '(SUB (VAR "x") (VAR "X" ())) '())) ()))
;; $apply(FUN f, x) = $f(x)
(define apply-def
  `(DEF () (,(clause-of '((FUN "f") (EXP (VAR "x"))) '(CALL "f" () ((EXP (VAR "x")))) '())) ()))
;; $succ(n) = n + 1
(define succ-def `(DEF () (,(clause-of '((EXP (VAR "n"))) '(BIN ADD (VAR "n") (NAT 1)) '())) ()))
;; $bad(x) = y, with y unbound
(define bad-def `(DEF () (,(clause-of '((EXP (VAR "x"))) '(VAR "y") '())) ()))
;; $down(i) = $down(i - 1) if i > 0, otherwise i
(define down-def
  `(DEF ()
        (,(clause-of '((EXP (VAR "i")))
                     '(CALL "down" () ((EXP (BIN SUB (VAR "i") (INT 1)))))
                     '((IF (CMP GT (VAR "i") (INT 0))))))
        (,(clause-of '((EXP (VAR "i"))) '(VAR "i") '()))))
;; A table: "zero" for 0, "other" otherwise
(define tbl-def
  `(TABLE ((EXP NAT))
          (,(clause-of '((EXP (VAR "n"))) '(TEXT "zero") '((IF (CMP EQ (VAR "n") (NAT 0)))))
           ,(clause-of '((EXP (VAR "n"))) '(TEXT "other") '()))))
;; A table whose row reads x, which only the caller binds
(define peek-def `(TABLE () (,(clause-of '() '(VAR "x") '()))))

(define funcs
  `(("id" ,id-def) ("sign" ,sign-def) ("is" ,is-def) ("apply" ,apply-def) ("succ" ,succ-def)
    ("bad" ,bad-def) ("down" ,down-def) ("tbl" ,tbl-def) ("peek" ,peek-def)
    ("ext" (EXT "ext")) ("rev_" (BUILTIN "rev_" ("X") ((EXP (ITER (VAR "X" ()) STAR)))))))

;; Locally, x, and $loc, which is $succ.
(define C-call
  (ctx-of `(() () ,funcs ()) `(() () (("loc" ,succ-def)) ((("x" ()) (NAT 7))))))
;; Locally, $id returns "local".
(define C-shadow
  (ctx-of `(() () ,funcs ())
          `(() () (("id" (DEF () (,(clause-of '((EXP (VAR "x"))) '(TEXT "local") '())) ()))) ())))
(define C-empty (ctx-of '(() () () ()) '(() () () ())))

(define (call id typs vals [C C-call]) (outputs (call-func ,C ,id ,typs ,vals any)))

;;
;; call-func, through the whole chain
;;

(test-equal (call "id" '() '((NAT 1))) '((OK (NAT 1))))
(test-equal (call "succ" '() '((NAT 1))) '((OK (NAT 2))))
;; Clauses are tried in order, the else clause last.
(test-equal (call "sign" '() '((INT 3))) '((OK (TEXT "pos"))))
(test-equal (call "sign" '() '((INT -3))) '((OK (TEXT "neg"))))
(test-equal (call "sign" '() '((INT 0))) '((OK (TEXT "zero"))))
;; FAIL: no clause takes two arguments, or the output fails
(test-equal (call "sign" '() '((INT 0) (INT 1))) '(FAIL))
(test-equal (call "bad" '() '((NAT 1))) '(FAIL))
;; The type parameters are bound to the type arguments in the callee.
(test-equal (call "is" '(NAT) '((INT 3))) '((OK (BOOL #t))))
(test-equal (call "is" '(NAT) '((INT -3))) '((OK (BOOL #f))))
;; A function argument is looked up in the caller and bound in the callee.
(test-equal (call "apply" '() '((FUNC "succ") (NAT 1))) '((OK (NAT 2))))
(test-equal (call "apply" '() '((FUNC "loc") (NAT 1))) '((OK (NAT 2))))
(test-equal (call "apply" '() '((FUNC "none") (NAT 1))) '(FAIL))
(test-equal (call "down" '() '((INT 3))) '((OK (INT 0))))
;; Tables
(test-equal (call "tbl" '() '((NAT 0))) '((OK (TEXT "zero"))))
(test-equal (call "tbl" '() '((NAT 5))) '((OK (TEXT "other"))))
(test-equal (call "tbl" '() '()) '(FAIL))
;; A table row does not see the caller's local values.
(test-equal (call "peek" '() '()) '(FAIL))
;; A local function is found first.
(test-equal (call "loc" '() '((NAT 1))) '((OK (NAT 2))))
(test-equal (call "id" '() '((NAT 1)) C-shadow) '((OK (TEXT "local"))))
;; No derivation: no such function, other numbers of type arguments than
;; type parameters, or type arguments for a table
(test-equal (call "none" '() '()) '())
(test-equal (call "is" '() '((INT 3))) '())
(test-equal (call "id" '(NAT) '((NAT 1))) '())
(test-equal (call "tbl" '(NAT) '((NAT 0))) '())
;; Externs and builtins are not implemented until Step 9.
(check-exn #rx"call-extern-func: externs are not implemented" (λ () (call "ext" '() '())))
(check-exn #rx"call-builtin-func: externs are not implemented"
           (λ () (call "rev_" '(NAT) '((LIST ())))))

;;
;; The relations below call-func, one at a time
;;

(define (run-eval-clause clause vals)
  (outputs (eval-clause ,C-empty ,C-call ,clause ,vals any)))

(test-equal (run-eval-clause (clause-of '((EXP (VAR "y"))) '(VAR "y") '()) '((NAT 1)))
            '((OK (NAT 1))))
;; The callee does not see the caller's local values.
(test-equal (run-eval-clause (clause-of '() '(VAR "x") '()) '()) '(FAIL))
;; fail: the arguments do not assign, a premise fails, or the output fails
(test-equal (run-eval-clause (clause-of '((EXP (TUP ()))) '(NAT 0) '()) '((NAT 1))) '(FAIL))
(test-equal (run-eval-clause (clause-of '() '(NAT 0) '((IF (BOOL #f)))) '()) '(FAIL))
(test-equal (run-eval-clause (clause-of '() '(VAR "none") '()) '()) '(FAIL))

(test-equal (outputs (eval-clauses ,C-empty ,C-call () () any)) '(FAIL))

(define (run-eval-tblrow tblrow vals)
  (outputs (eval-tblrow ,C-call ,tblrow ,vals any)))

(test-equal (run-eval-tblrow (clause-of '((EXP (VAR "y"))) '(VAR "y") '()) '((NAT 1)))
            '((OK (NAT 1))))
;; fail: the arguments do not assign, a premise fails, or the output fails
(test-equal (run-eval-tblrow (clause-of '((EXP (VAR "y"))) '(VAR "y") '()) '()) '(FAIL))
(test-equal (run-eval-tblrow (clause-of '() '(NAT 0) '((IF (BOOL #f)))) '()) '(FAIL))
(test-equal (run-eval-tblrow (clause-of '() '(VAR "x") '()) '()) '(FAIL))

(test-equal (outputs (eval-tblrows ,C-call () () any)) '(FAIL))
(test-equal (outputs (call-table-func ,C-call ,tbl-def ((NAT 0)) any)) '((OK (TEXT "zero"))))

(test-equal (outputs (call-defined-func ,C-call ,is-def (NAT) ((INT 3)) any)) '((OK (BOOL #t))))
(test-equal (outputs (call-defined-func ,C-call ,is-def (NAT BOOL) ((INT 3)) any)) '())

(test-equal (outputs (call-func-dispatch ,C-call ,succ-def () ((NAT 1)) any)) '((OK (NAT 2))))
(test-equal (outputs (call-func-dispatch ,C-call ,tbl-def () ((NAT 1)) any)) '((OK (TEXT "other"))))

;;
;; Nesting: each shared premise is evaluated once.
;;

;; Each nesting succeeds, so it evaluates every level.
(define (check-not-exponential name J exp-of)
  (define (calls depth)
    (count-calls J (λ () (outputs (eval-exp ,C-call ,(exp-of depth) any)))))
  (define-values (c4 c10) (values (calls 4) (calls 10)))
  (check-true (< c10 (* 10 c4))
              (format "~a: ~a calls at depth 4, ~a at depth 10" name c4 c10))
  (check-equal? (caar (outputs (eval-exp ,C-call ,(exp-of 4) any))) 'OK
                (format "~a at depth 4" name)))

;; $down tries its first clause, which fails at the bottom, then its second.
(check-not-exponential 'clauses 'eval-clause (λ (depth) `(CALL "down" () ((EXP (INT ,depth))))))
;; Nested table calls, each trying its first row, then its second
(check-not-exponential 'tblrows 'eval-tblrow
                       (λ (depth) (for/fold ([exp '(NAT 1)]) ([_ (in-range depth)])
                                    `(LEN (CALL "tbl" () ((EXP ,exp)))))))

;; Every rule appears in some derivation above, except the extern ones.
(check-rules-used eval-clause)
(check-rules-used eval-clause/succ)
(check-rules-used eval-clause/succ-prems)
(check-rules-used eval-clause/succ-exp)
(check-rules-used eval-clauses)
(check-rules-used eval-clauses/cons)
(check-rules-used eval-tblrow)
(check-rules-used eval-tblrow/succ)
(check-rules-used eval-tblrow/succ-prems)
(check-rules-used eval-tblrow/succ-exp)
(check-rules-used eval-tblrows)
(check-rules-used eval-tblrows/cons)
(check-rules-used call-table-func)
(check-rules-used call-defined-func)
(check-rules-used call-func-dispatch #:except ("extern" "builtin"))
(check-rules-used call-func)

(test-results)
