#lang racket/base

(require racket/match
         racket/port
         rackunit
         "../common/0.0-prelude.rkt"
         "../al/5.5-eval-prem.rkt"
         "machine.rkt")

(define coverage (start-coverage ->redex/eval-prem ->ctx/eval-prem))

;; Raises when evaluated, where a premise must not be evaluated
(define DIV0 (term (BIN DIV (NAT 1) (NAT 0))))

;; Succ relates a nat below 5 to the next one, and Small holds for one.
(define G-rels
  (global-of (boot-text #<<EOF
var n : nat

relation Succ: |- nat ':' nat
  hint(input %0)
rule Succ/small:
  |- n ':' $(n + 1)
  -- if $(n < 5)

relation Small: |- nat
  hint(input %0)
rule Small/small:
  |- n
  -- if $(n < 5)
EOF
                        )))

(define-term L-vals
  {TYP () REL () FUNC ()
   VAL ((("x" ()) (NAT 1))
        (("o" (QUEST)) (OPT ((NAT 1))))
        (("n" (QUEST)) (OPT ()))
        (("ns" (STAR)) (LIST ((NAT 1) (NAT 2) (NAT 3))))
        (("ms" (STAR)) (LIST ((NAT 10) (NAT 20) (NAT 30))))
        (("ks" (STAR)) (LIST ((NAT 7))))
        (("ds" (STAR)) (LIST ((NAT 2) (NAT 0))))
        (("es" (STAR)) (LIST ()))
        (("w" (STAR)) (NAT 0)))})

(define (layer-vals L) (list-ref L 7))

;; Runs prem under L-vals. Gives (OK binds), with the bindings of the updated
;; layer that are new or changed, or FAIL.
(define (run-prem prem #:cross-check? [cross-check-on? #t])
  (match (run-in G-rels (term L-vals) prem #:cross-check? cross-check-on?)
    [(list 'OK L_1)
     (define venv (layer-vals (term L-vals)))
     (list 'OK (for/list ([b (in-list (layer-vals L_1))] #:unless (member b venv)) b))]
    [(list 'FAIL _) 'FAIL]))

(define (trace-prem prem)
  (trace-in G-rels (term L-vals) prem))

;;
;; Relation premises
;;

(test-equal (run-prem (term (REL "Succ" ((VAR "x")) ((VAR "y")))))
            (term (OK ((("y" ()) (NAT 2))))))
(test-equal (trace-prem (term (REL "Succ" ((NAT 1)) ((VAR "y")))))
            '("eval-exp/literal/number" "eval-prem/relpr"
              "call-rel" "call-rel-dispatch/defined" "call-defined-rel"
              "eval-rulgroups/cons" "eval-rulgroup" "eval-ruls/cons" "eval-rul/succ"
              "assign-exps/cons" "assign-exp/variable" "assign-exps/cons/tail" "assign-exps/nil"
              "eval-prems/head" "eval-exp/variable" "eval-exp/literal/number"
              "eval-exp/compare/number" "eval-prem/ifpr/true" "eval-prems/head-succ"
              "eval-prems/empty" "eval-exp/variable" "eval-exp/literal/number"
              "eval-exp/binary/number" "eval-rul/succ/output" "in/ok"
              "eval-ruls/cons-succ" "eval-rulgroups/cons-succ" "in/ok"
              "eval-prem/relpr/assign"
              "assign-exps/cons" "assign-exp/variable" "assign-exps/cons/tail" "assign-exps/nil"))
;; The outputs are patterns.
(test-equal (run-prem (term (REL "Succ" ((NAT 3)) ((TUP ())))))
            'FAIL)
(test-equal (run-prem (term (REL "Succ" ((NAT 3)) ((VAR "a") (VAR "b")))))
            'FAIL)
;; The relation fails, or there is none.
(test-equal (run-prem (term (REL "Succ" ((NAT 5)) ((VAR "y"))))) 'FAIL)
(test-equal (trace-prem (term (REL "Nope" () ())))
            '("eval-prem/relpr" "call-rel/fail" "frame/fail"))
;; An input fails, and no later one is evaluated.
(test-equal (trace-prem (term (REL "Succ" ((VAR "y") ,DIV0) ((VAR "z")))))
            '("eval-exp/variable/fail" "frame/fail"))

;;
;; If premises
;;

(test-equal (run-prem (term (IF (BOOL #t)))) (term (OK ())))
(test-equal (run-prem (term (IF (CMP EQ (VAR "x") (NAT 1))))) (term (OK ())))
(test-equal (trace-prem (term (IF (BOOL #t)))) '("eval-exp/literal/boolean" "eval-prem/ifpr/true"))
(test-equal (run-prem (term (IF (BOOL #f)))) 'FAIL)
(test-equal (trace-prem (term (IF (BOOL #f)))) '("eval-exp/literal/boolean" "eval-prem/ifpr/false"))
;; otherwise: not a boolean, or the condition fails
(test-equal (run-prem (term (IF (NAT 1)))) 'FAIL)
(test-equal (trace-prem (term (IF (VAR "o")))) '("eval-exp/variable/fail" "frame/fail"))
(test-equal (trace-prem (term (IF (VAR "x")))) '("eval-exp/variable" "eval-prem/ifpr/fail"))

;;
;; If-hold and if-not-hold premises
;;

(test-equal (run-prem (term (IFHOLD "Small" ((VAR "x"))))) (term (OK ())))
(test-equal (trace-prem (term (IFHOLD "Small" ((NAT 1)))))
            '("eval-exp/literal/number" "eval-prem/ifholdpr/hold"
              "call-rel" "call-rel-dispatch/defined" "call-defined-rel"
              "eval-rulgroups/cons" "eval-rulgroup" "eval-ruls/cons" "eval-rul/succ"
              "assign-exps/cons" "assign-exp/variable" "assign-exps/cons/tail" "assign-exps/nil"
              "eval-prems/head" "eval-exp/variable" "eval-exp/literal/number"
              "eval-exp/compare/number" "eval-prem/ifpr/true" "eval-prems/head-succ"
              "eval-prems/empty" "eval-rul/succ/output" "in/ok"
              "eval-ruls/cons-succ" "eval-rulgroups/cons-succ" "in/ok"
              "eval-prem/ifholdpr/hold/result"))
;; The relation fails, or there is none.
(test-equal (run-prem (term (IFHOLD "Small" ((NAT 5))))) 'FAIL)
(test-equal (trace-prem (term (IFHOLD "Nope" ())))
            '("eval-prem/ifholdpr/hold" "call-rel/fail" "frame/fail"))
;; otherwise: the relation has outputs
(test-equal (run-prem (term (IFHOLD "Succ" ((NAT 1))))) 'FAIL)
(test-equal (car (reverse (trace-prem (term (IFHOLD "Succ" ((NAT 1)))))))
            "eval-prem/ifholdpr/hold/fail")
;; An input fails, and no later one is evaluated.
(test-equal (trace-prem (term (IFHOLD "Small" ((VAR "y") ,DIV0))))
            '("eval-exp/variable/fail" "frame/fail"))

(test-equal (run-prem (term (IFNOTHOLD "Small" ((NAT 5))))) (term (OK ())))
(test-equal (car (reverse (trace-prem (term (IFNOTHOLD "Small" ((NAT 5)))))))
            "eval-prem/ifholdpr/nothold/result")
;; The relation holds, with or without outputs.
(test-equal (run-prem (term (IFNOTHOLD "Small" ((VAR "x"))))) 'FAIL)
(test-equal (car (reverse (trace-prem (term (IFNOTHOLD "Small" ((NAT 1)))))))
            "eval-prem/ifholdpr/nothold/fail")
(test-equal (run-prem (term (IFNOTHOLD "Succ" ((NAT 1))))) 'FAIL)
;; An unknown relation has no derivation in Call_rel, so the premise fails.
(test-equal (run-prem (term (IFNOTHOLD "Nope" ((NAT 1))))) 'FAIL)
(test-equal (trace-prem (term (IFNOTHOLD "Nope" ((NAT 1)))))
            '("eval-exp/literal/number" "eval-prem/ifholdpr/nothold/fail-rel"))
;; The inputs are evaluated first, even for an unknown relation, and an input
;; that fails is not caught.
(check-exn #rx"quotient: undefined for 0"
           (λ () (run-prem (term (IFNOTHOLD "Nope" (,DIV0))))))
(test-equal (run-prem (term (IFNOTHOLD "Small" ((VAR "y"))))) 'FAIL)
(test-equal (trace-prem (term (IFNOTHOLD "Small" ((VAR "y") ,DIV0))))
            '("eval-exp/variable/fail" "frame/fail"))

;;
;; Let premises
;;

(test-equal (run-prem (term (LET (VAR "y") (BIN ADD (VAR "x") (NAT 1)))))
            (term (OK ((("y" ()) (NAT 2))))))
(test-equal (trace-prem (term (LET (VAR "y") (NAT 1))))
            '("eval-exp/literal/number" "eval-prem/letpr" "assign-exp/variable"))
(test-equal (run-prem (term (LET (TUP ((VAR "a") (VAR "b"))) (TUP ((NAT 1) (VAR "x"))))))
            (term (OK ((("a" ()) (NAT 1)) (("b" ()) (NAT 1))))))
;; The right side is evaluated before the left side binds.
(test-equal (run-prem (term (LET (VAR "x") (BIN ADD (VAR "x") (NAT 1)))))
            (term (OK ((("x" ()) (NAT 2))))))
;; The assignment fails, or the right side does.
(test-equal (run-prem (term (LET (TUP ((VAR "a"))) (NAT 1)))) 'FAIL)
(test-equal (trace-prem (term (LET (VAR "y") (VAR "z")))) '("eval-exp/variable/fail" "frame/fail"))

;;
;; Iteration premises - optional
;;

;;; Eval_prem/none: the premise is not evaluated.

(test-equal (run-prem (term (ITER (LET (VAR "y") (VAR "n")) (QUEST (("n" NAT ())) (("y" NAT ()))))))
            (term (OK ((("y" (QUEST)) (OPT ()))))))
(test-equal (trace-prem (term (ITER (IF ,DIV0) (QUEST (("n" NAT ())) (("y" NAT ()) ("z" NAT ()))))))
            '("eval-prem/iterpr-opt/none"))
(test-equal (run-prem (term (ITER (IF ,DIV0) (QUEST (("n" NAT ())) (("y" NAT ()) ("z" NAT ()))))))
            (term (OK ((("y" (QUEST)) (OPT ())) (("z" (QUEST)) (OPT ()))))))

;;; Eval_prem/some: the premise runs in a sub-context, which keeps its own
;;; bindings.

(test-equal (run-prem (term (ITER (LET (VAR "y") (BIN ADD (VAR "o") (NAT 1)))
                                  (QUEST (("o" NAT ())) (("y" NAT ()))))))
            (term (OK ((("y" (QUEST)) (OPT ((NAT 2))))))))
(test-equal (trace-prem (term (ITER (LET (VAR "y") (VAR "o")) (QUEST (("o" NAT ())) (("y" NAT ()))))))
            '("eval-prem/iterpr-opt/some" "eval-exp/variable" "eval-prem/letpr"
              "assign-exp/variable" "eval-prem/iterpr-opt/some/bind"))
;; No variable to bind
(test-equal (run-prem (term (ITER (IF (CMP EQ (VAR "o") (NAT 1))) (QUEST (("o" NAT ())) ()))))
            (term (OK ())))
;; No bound variable: one sub-context
(test-equal (run-prem (term (ITER (LET (VAR "y") (VAR "x")) (QUEST () (("y" NAT ()))))))
            (term (OK ((("y" (QUEST)) (OPT ((NAT 1))))))))
;; The premise fails.
(test-equal (trace-prem (term (ITER (IF (CMP EQ (VAR "o") (NAT 2))) (QUEST (("o" NAT ())) ()))))
            '("eval-prem/iterpr-opt/some" "eval-exp/variable" "eval-exp/literal/number"
              "eval-exp/compare/poly" "eval-prem/ifpr/false" "in/fail" "frame/fail"))
;; otherwise: a variable to bind is unbound after the premise
(test-equal (run-prem (term (ITER (IF (BOOL #t)) (QUEST (("o" NAT ())) (("y" NAT ()))))))
            'FAIL)
(test-equal (trace-prem (term (ITER (IF (BOOL #t)) (QUEST (("o" NAT ())) (("y" NAT ()))))))
            '("eval-prem/iterpr-opt/some" "eval-exp/literal/boolean" "eval-prem/ifpr/true"
              "eval-prem/iterpr-opt/some/fail"))
;; otherwise: a mix of OPT val and OPT eps, an unbound variable, or a value
;; that is not an option, and nothing is evaluated
(test-equal (trace-prem (term (ITER (IF ,DIV0) (QUEST (("o" NAT ()) ("n" NAT ())) ()))))
            '("eval-prem/iterpr-opt/fail"))
(test-equal (run-prem (term (ITER (IF ,DIV0) (QUEST (("q" NAT ())) ())))) 'FAIL)
(test-equal (run-prem (term (ITER (IF ,DIV0) (QUEST (("x" NAT ())) ())))) 'FAIL)

;;
;; Iteration premises - list
;;

;;; Eval_prem/empty: the premise is not evaluated.

(test-equal (run-prem (term (ITER (LET (VAR "y") (VAR "es")) (STAR (("es" NAT ())) (("y" NAT ()))))))
            (term (OK ((("y" (STAR)) (LIST ()))))))
(test-equal (trace-prem (term (ITER (IF ,DIV0) (STAR (("es" NAT ())) ()))))
            '("eval-prem/iterpr-list/empty"))
;; No bound variable: no sub-context, unlike an optional iteration
(test-equal (run-prem (term (ITER (LET (VAR "y") ,DIV0) (STAR () (("y" NAT ()))))))
            (term (OK ((("y" (STAR)) (LIST ()))))))

;;; Eval_prem/list: the premise runs in each sub-context in turn.

(test-equal (run-prem (term (ITER (LET (VAR "y") (BIN MUL (VAR "ns") (NAT 2)))
                                  (STAR (("ns" NAT ())) (("y" NAT ()))))))
            (term (OK ((("y" (STAR)) (LIST ((NAT 2) (NAT 4) (NAT 6))))))))
(test-equal (trace-prem (term (ITER (LET (VAR "y") (VAR "ks")) (STAR (("ks" NAT ())) (("y" NAT ()))))))
            '("eval-prem/iterpr-list/list" "eval-exp/variable" "eval-prem/letpr"
              "assign-exp/variable" "eval-prem/iterpr-list/list/bind"))
;; The bindings of each sub-context are transposed into one list per
;; variable.
(test-equal (run-prem (term (ITER (LET (TUP ((VAR "a") (VAR "b"))) (TUP ((VAR "ms") (VAR "ns"))))
                                  (STAR (("ns" NAT ()) ("ms" NAT ())) (("a" NAT ()) ("b" NAT ()))))))
            (term (OK ((("a" (STAR)) (LIST ((NAT 10) (NAT 20) (NAT 30))))
                       (("b" (STAR)) (LIST ((NAT 1) (NAT 2) (NAT 3))))))))
;; The sub-contexts see the enclosing layer's values.
(test-equal (run-prem (term (ITER (LET (VAR "y") (BIN ADD (VAR "ns") (VAR "x")))
                                  (STAR (("ns" NAT ())) (("y" NAT ()))))))
            (term (OK ((("y" (STAR)) (LIST ((NAT 2) (NAT 3) (NAT 4))))))))
;; No variable to bind
(test-equal (run-prem (term (ITER (IF (CMP LT (VAR "ns") (NAT 5))) (STAR (("ns" NAT ())) ()))))
            (term (OK ())))
;; The first failing sub-context fails the premise, and no later one is
;; evaluated.
(test-equal (trace-prem (term (ITER (IF (CMP EQ (BIN DIV (NAT 1) (VAR "ds")) (NAT 1)))
                                    (STAR (("ds" NAT ())) ()))))
            '("eval-prem/iterpr-list/list" "eval-exp/literal/number" "eval-exp/variable"
              "eval-exp/binary/number" "eval-exp/literal/number" "eval-exp/compare/poly"
              "eval-prem/ifpr/false" "in/fail" "frame/fail"))
;; otherwise: a variable to bind is unbound in some sub-context
(test-equal (run-prem (term (ITER (IF (BOOL #t)) (STAR (("ns" NAT ())) (("y" NAT ()))))))
            'FAIL)
(test-equal (car (reverse (trace-prem (term (ITER (IF (BOOL #t)) (STAR (("ks" NAT ())) (("y" NAT ()))))))))
            "eval-prem/iterpr-list/list/fail")
;; otherwise: an unbound variable, or a value that is not a list, and nothing
;; is evaluated
(test-equal (trace-prem (term (ITER (IF ,DIV0) (STAR (("q" NAT ())) ()))))
            '("eval-prem/iterpr-list/fail"))
(test-equal (run-prem (term (ITER (IF ,DIV0) (STAR (("w" NAT ())) ())))) 'FAIL)
;; Lists of different lengths raise, as $transpose_ does.
(check-exn #rx"cannot transpose"
           (λ () (run-prem (term (ITER (IF (BOOL #t)) (STAR (("ns" NAT ()) ("ks" NAT ())) ()))))))

;;
;; Debug premises
;;

;; Gives the result of running prem, and what it wrote to stderr
(define (run-prem/stderr prem #:cross-check? cross-check-on?)
  (define err (open-output-string))
  (define result
    (parameterize ([current-error-port err])
      (run-prem prem #:cross-check? cross-check-on?)))
  (list result (get-output-string err)))

(test-equal (run-prem/stderr (term (DEBUG (BIN ADD (VAR "x") (NAT 1)))) #:cross-check? #f)
            (list (term (OK ())) "(NAT 2)\n"))
(test-equal (parameterize ([current-error-port (open-output-nowhere)])
              (trace-prem (term (DEBUG (VAR "x")))))
            '("eval-exp/variable" "eval-prem/dbg"))
;; The cross-check applies the rule again, and so writes the value twice.
(test-equal (run-prem/stderr (term (DEBUG (VAR "x"))) #:cross-check? #t)
            (list (term (OK ())) "(NAT 1)\n(NAT 1)\n"))
;; The expression fails, and nothing is written.
(test-equal (run-prem/stderr (term (DEBUG (VAR "y"))) #:cross-check? #f)
            (list 'FAIL ""))

;;
;; Premise sequences
;;

(test-equal (run-prem (term (eval-prems ()))) (term (OK ())))
(test-equal (trace-prem (term (eval-prems ()))) '("eval-prems/empty"))
;; Each premise sees the bindings of the ones before it.
(test-equal (run-prem (term (eval-prems ((LET (VAR "y") (NAT 1))
                                         (LET (VAR "z") (BIN ADD (VAR "y") (VAR "x")))
                                         (IF (CMP EQ (VAR "z") (NAT 2)))))))
            (term (OK ((("y" ()) (NAT 1)) (("z" ()) (NAT 2))))))
(test-equal (trace-prem (term (eval-prems ((IF (BOOL #t)) (LET (VAR "y") (NAT 1))))))
            '("eval-prems/head" "eval-exp/literal/boolean" "eval-prem/ifpr/true"
              "eval-prems/head-succ" "eval-prems/head" "eval-exp/literal/number"
              "eval-prem/letpr" "assign-exp/variable" "eval-prems/head-succ" "eval-prems/empty"))
;; Eval_prems/head-fail: the first failing premise fails the sequence, and no
;; later one is evaluated.
(test-equal (trace-prem (term (eval-prems ((IF (BOOL #t)) (IF (BOOL #f)) (IF ,DIV0)))))
            '("eval-prems/head" "eval-exp/literal/boolean" "eval-prem/ifpr/true"
              "eval-prems/head-succ" "eval-prems/head" "eval-exp/literal/boolean"
              "eval-prem/ifpr/false" "frame/fail"))

(check-coverage coverage)
