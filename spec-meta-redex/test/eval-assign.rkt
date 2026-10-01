#lang racket/base

(require racket/match
         rackunit
         "../common/0.0-prelude.rkt"
         "../al/5.2-eval-assign.rkt"
         "machine.rkt")

(define coverage (start-coverage ->redex/eval-assign ->ctx/eval-assign))

(define DIV0 (term (BIN DIV (NAT 1) (NAT 0))))

;; G has the function "g", and so does the caller's layer, with another
;; definition, and the callee's layer has "h"
(define-term G-funcs {TYP () REL () FUNC (("g" (EXT "g")) ("k" (EXT "k"))) VAL ()})
(define-term L-caller {TYP () REL () FUNC (("g" (EXT "g-local"))) VAL ()})
(define-term L-callee {TYP () REL () FUNC (("h" (EXT "h"))) VAL ()})

(define (layer-with-vals venv)
  `(TYP () REL () FUNC () VAL ,venv))

(define (layer-funcs L) (list-ref L 5))
(define (layer-vals L) (list-ref L 7))

;; Runs form under a layer whose VAL map is venv. Gives (OK venv_1), with the
;; VAL map of the updated layer, or FAIL.
(define (run-assign-form form [venv '()])
  (match (run-in (term G-funcs) (layer-with-vals venv) form)
    [(list 'OK L_1) (list 'OK (layer-vals L_1))]
    [(list 'FAIL _) 'FAIL]))

(define (run-assign exp val [venv '()])
  (run-assign-form `(assign-exp ,exp ,val) venv))

(define (run-assign-exps exps vals [venv '()])
  (run-assign-form `(assign-exps ,exps ,vals) venv))

(define (trace-assign exp val [venv '()])
  (trace-in (term G-funcs) (layer-with-vals venv) `(assign-exp ,exp ,val)))

;; Runs form under L-callee, with L-caller as the caller. Gives
;; (OK fenv_1 venv_1), with the FUNC and VAL maps of the updated callee layer,
;; or FAIL.
(define (run-assign-arg-form form)
  (match (run-in (term G-funcs) (term L-callee) form)
    [(list 'OK L_1) (list 'OK (layer-funcs L_1) (layer-vals L_1))]
    [(list 'FAIL _) 'FAIL]))

(define (run-assign-arg arg val)
  (run-assign-arg-form `(assign-arg ,(term L-caller) ,arg ,val)))

(define (run-assign-args args vals)
  (run-assign-arg-form `(assign-args ,(term L-caller) ,args ,vals)))

;;
;; Variables
;;

(test-equal (run-assign (term (VAR "x")) (term (NAT 1)))
            (term (OK ((("x" ()) (NAT 1))))))
(test-equal (trace-assign (term (VAR "x")) (term (NAT 1))) '("assign-exp/variable"))
;; A bound variable is rebound where it stands.
(test-equal (run-assign (term (VAR "x")) (term (TEXT "a"))
                        (term ((("x" ()) (NAT 0)) (("y" ()) (NAT 2)))))
            (term (OK ((("x" ()) (TEXT "a")) (("y" ()) (NAT 2))))))
;; A variable bound with iterators is another variable.
(test-equal (run-assign (term (VAR "x")) (term (NAT 1)) (term ((("x" (STAR)) (LIST ())))))
            (term (OK ((("x" (STAR)) (LIST ())) (("x" ()) (NAT 1))))))

;;
;; Tuples, cases, and structs
;;

(test-equal (run-assign (term (TUP ((VAR "a") (VAR "b")))) (term (TUP ((NAT 1) (BOOL #t)))))
            (term (OK ((("a" ()) (NAT 1)) (("b" ()) (BOOL #t))))))
(test-equal (trace-assign (term (TUP ((VAR "a") (VAR "b")))) (term (TUP ((NAT 1) (NAT 2)))))
            '("assign-exp/tup"
              "assign-exps/cons" "assign-exp/variable" "assign-exps/cons/tail"
              "assign-exps/cons" "assign-exp/variable" "assign-exps/cons/tail"
              "assign-exps/nil"))
(test-equal (run-assign (term (TUP ())) (term (TUP ()))) (term (OK ())))
(test-equal (run-assign (term (TUP ((TUP ((VAR "a"))) (VAR "b"))))
                        (term (TUP ((TUP ((NAT 1))) (NAT 2)))))
            (term (OK ((("a" ()) (NAT 1)) (("b" ()) (NAT 2))))))
;; Tuples of different lengths
(test-equal (run-assign (term (TUP ((VAR "a")))) (term (TUP ((NAT 1) (NAT 2))))) 'FAIL)
(test-equal (trace-assign (term (TUP ((VAR "a")))) (term (TUP ((NAT 1) (NAT 2)))))
            '("assign-exp/tup"
              "assign-exps/cons" "assign-exp/variable" "assign-exps/cons/tail"
              "assign-exps/fail"))
(test-equal (run-assign (term (TUP ((VAR "a") (VAR "b")))) (term (TUP ((NAT 1))))) 'FAIL)
(test-equal (run-assign (term (TUP ((VAR "a")))) (term (LIST ((NAT 1))))) 'FAIL)
;; The first failing component stops the assignment.
(test-equal (trace-assign (term (TUP ((NAT 1) (VAR "b")))) (term (TUP ((NAT 1) (NAT 2)))))
            '("assign-exp/tup" "assign-exps/cons" "assign-exp/fail" "frame/fail"))

(test-equal (run-assign (term (INJ ((("Some") ()) ((VAR "a")))))
                        (term (INJ ((("Some") ()) ((NAT 1))))))
            (term (OK ((("a" ()) (NAT 1))))))
(test-equal (run-assign (term (INJ ((("None")) ()))) (term (INJ ((("None")) ()))))
            (term (OK ())))
(test-equal (trace-assign (term (INJ ((("None")) ()))) (term (INJ ((("None")) ()))))
            '("assign-exp/inj" "assign-exps/nil"))
;; Another case
(test-equal (run-assign (term (INJ ((("Some") ()) ((VAR "a")))))
                        (term (INJ ((("Just") ()) ((NAT 1))))))
            'FAIL)
(test-equal (trace-assign (term (INJ ((("Some") ()) ((VAR "a")))))
                          (term (INJ ((("Just") ()) ((NAT 1))))))
            '("assign-exp/fail"))
(test-equal (run-assign (term (INJ ((("Some") ()) ((VAR "a"))))) (term (TUP ((NAT 1))))) 'FAIL)

(test-equal (run-assign (term (STR (("A" (VAR "a")) ("B" (VAR "b")))))
                        (term (STR (("A" (NAT 1)) ("B" (NAT 2))))))
            (term (OK ((("a" ()) (NAT 1)) (("b" ()) (NAT 2))))))
(test-equal (trace-assign (term (STR ())) (term (STR ()))) '("assign-exp/str" "assign-exps/nil"))
;; Fields are matched in order, with their atoms equal.
(test-equal (run-assign (term (STR (("A" (VAR "a")) ("B" (VAR "b")))))
                        (term (STR (("B" (NAT 2)) ("A" (NAT 1))))))
            'FAIL)
(test-equal (run-assign (term (STR (("A" (VAR "a")) ("B" (VAR "b")))))
                        (term (STR (("A" (NAT 1)) ("C" (NAT 2))))))
            'FAIL)
(test-equal (run-assign (term (STR (("A" (VAR "a")))))
                        (term (STR (("A" (NAT 1)) ("B" (NAT 2))))))
            'FAIL)
(test-equal (trace-assign (term (STR (("A" (VAR "a")))))
                          (term (STR (("A" (NAT 1)) ("B" (NAT 2))))))
            '("assign-exp/fail"))

;;
;; Options, lists, and cons-lists
;;

(test-equal (run-assign (term (OPT ((VAR "a")))) (term (OPT ((NAT 1)))))
            (term (OK ((("a" ()) (NAT 1))))))
(test-equal (trace-assign (term (OPT ((VAR "a")))) (term (OPT ((NAT 1)))))
            '("assign-exp/opt/opt-some" "assign-exp/variable"))
(test-equal (run-assign (term (OPT ())) (term (OPT ()))) (term (OK ())))
(test-equal (trace-assign (term (OPT ())) (term (OPT ()))) '("assign-exp/opt/opt-none"))
(test-equal (run-assign (term (OPT ((VAR "a")))) (term (OPT ()))) 'FAIL)
(test-equal (run-assign (term (OPT ())) (term (OPT ((NAT 1))))) 'FAIL)
(test-equal (run-assign (term (OPT ((VAR "a")))) (term (NAT 1))) 'FAIL)

(test-equal (run-assign (term (LIST ((VAR "a") (VAR "b")))) (term (LIST ((NAT 1) (NAT 2)))))
            (term (OK ((("a" ()) (NAT 1)) (("b" ()) (NAT 2))))))
(test-equal (trace-assign (term (LIST ())) (term (LIST ()))) '("assign-exp/list" "assign-exps/nil"))
(test-equal (run-assign (term (LIST ((VAR "a")))) (term (LIST ()))) 'FAIL)
(test-equal (run-assign (term (LIST ((VAR "a")))) (term (TUP ((NAT 1))))) 'FAIL)

(test-equal (run-assign (term (CONS (VAR "h") (VAR "t"))) (term (LIST ((NAT 1) (NAT 2)))))
            (term (OK ((("h" ()) (NAT 1)) (("t" ()) (LIST ((NAT 2))))))))
(test-equal (trace-assign (term (CONS (VAR "h") (VAR "t"))) (term (LIST ((NAT 1) (NAT 2)))))
            '("assign-exp/cons" "assign-exp/variable" "assign-exp/cons/tail" "assign-exp/variable"))
(test-equal (run-assign (term (CONS (VAR "h") (VAR "t"))) (term (LIST ((NAT 1)))))
            (term (OK ((("h" ()) (NAT 1)) (("t" ()) (LIST ()))))))
(test-equal (run-assign (term (CONS (VAR "a") (CONS (VAR "b") (VAR "t"))))
                        (term (LIST ((NAT 1) (NAT 2) (NAT 3)))))
            (term (OK ((("a" ()) (NAT 1)) (("b" ()) (NAT 2)) (("t" ()) (LIST ((NAT 3))))))))
;; An empty list, or another value
(test-equal (run-assign (term (CONS (VAR "h") (VAR "t"))) (term (LIST ()))) 'FAIL)
(test-equal (trace-assign (term (CONS (VAR "h") (VAR "t"))) (term (LIST ())))
            '("assign-exp/fail"))
(test-equal (run-assign (term (CONS (VAR "h") (VAR "t"))) (term (OPT ((NAT 1))))) 'FAIL)
;; The head fails, and the tail is not assigned.
(test-equal (trace-assign (term (CONS (TUP ()) (VAR "t"))) (term (LIST ((NAT 1)))))
            '("assign-exp/cons" "assign-exp/fail" "frame/fail"))
;; The tail fails.
(test-equal (run-assign (term (CONS (VAR "h") (LIST ()))) (term (LIST ((NAT 1) (NAT 2))))) 'FAIL)

;;
;; Expressions with no rule
;;

;; The elaborator turns a literal in a pattern into a variable and a check.
(test-equal (run-assign (term (NAT 1)) (term (NAT 1))) 'FAIL)
(test-equal (run-assign (term (BOOL #t)) (term (BOOL #t))) 'FAIL)
(test-equal (run-assign (term (TEXT "a")) (term (TEXT "a"))) 'FAIL)
;; Nothing is evaluated.
(test-equal (trace-assign (term (BIN ADD (VAR "a") ,DIV0)) (term (NAT 2))) '("assign-exp/fail"))
(test-equal (run-assign (term (CALL "f" () ())) (term (NAT 1))) 'FAIL)
(test-equal (run-assign (term (DOT (VAR "a") "A")) (term (NAT 1))) 'FAIL)

;; A repeated variable takes the last value, with no equality check.
(test-equal (run-assign (term (TUP ((VAR "a") (VAR "a")))) (term (TUP ((NAT 1) (NAT 2)))))
            (term (OK ((("a" ()) (NAT 2))))))

;;
;; Iterated expressions
;;

;;; Assign_exp/iter/simple: an iteration on a variable binds it, to any value

(test-equal (run-assign (term (ITER (VAR "x") (STAR (("x" NAT ()))))) (term (LIST ((NAT 1)))))
            (term (OK ((("x" (STAR)) (LIST ((NAT 1))))))))
(test-equal (trace-assign (term (ITER (VAR "x") (STAR (("x" NAT ()))))) (term (LIST ())))
            '("assign-exp/iter/simple"))
(test-equal (run-assign (term (ITER (VAR "x") (STAR (("x" NAT ()))))) (term (NAT 5)))
            (term (OK ((("x" (STAR)) (NAT 5))))))
(test-equal (run-assign (term (ITER (VAR "x") (QUEST (("x" NAT ()))))) (term (TEXT "a")))
            (term (OK ((("x" (QUEST)) (TEXT "a"))))))
(test-equal (run-assign (term (ITER (ITER (VAR "x") (STAR (("x" NAT ()))))
                                    (QUEST (("x" (ITER NAT STAR) (STAR))))))
                        (term (OPT ())))
            (term (OK ((("x" (STAR QUEST)) (OPT ()))))))

;;; Assign_exp/iter/opt-none

(define-term exp-ab (TUP ((VAR "a") (VAR "b"))))
(define-term varis-ab (("a" NAT ()) ("b" NAT ())))

(test-equal (run-assign (term (ITER exp-ab (QUEST varis-ab))) (term (OPT ())))
            (term (OK ((("a" (QUEST)) (OPT ())) (("b" (QUEST)) (OPT ()))))))
(test-equal (trace-assign (term (ITER exp-ab (QUEST varis-ab))) (term (OPT ())))
            '("assign-exp/iter/opt-none"))
(test-equal (run-assign (term (ITER (TUP ()) (QUEST ()))) (term (OPT ()))) (term (OK ())))

;;; Assign_exp/iter/opt-some: the inner bindings stay in the context.

(test-equal (run-assign (term (ITER exp-ab (QUEST varis-ab)))
                        (term (OPT ((TUP ((NAT 1) (NAT 2)))))))
            (term (OK ((("a" ()) (NAT 1)) (("b" ()) (NAT 2))
                       (("a" (QUEST)) (OPT ((NAT 1)))) (("b" (QUEST)) (OPT ((NAT 2))))))))
(test-equal (trace-assign (term (ITER exp-ab (QUEST varis-ab)))
                          (term (OPT ((TUP ((NAT 1) (NAT 2)))))))
            '("assign-exp/iter/opt-some" "assign-exp/tup"
              "assign-exps/cons" "assign-exp/variable" "assign-exps/cons/tail"
              "assign-exps/cons" "assign-exp/variable" "assign-exps/cons/tail"
              "assign-exps/nil" "assign-exp/iter/opt-some/bind"))
(test-equal (run-assign (term (ITER (TUP ()) (QUEST ()))) (term (OPT ((TUP ())))))
            (term (OK ())))
;; The inner assignment fails.
(test-equal (run-assign (term (ITER exp-ab (QUEST varis-ab))) (term (OPT ((NAT 1))))) 'FAIL)
(test-equal (trace-assign (term (ITER exp-ab (QUEST varis-ab))) (term (OPT ((NAT 1)))))
            '("assign-exp/iter/opt-some" "assign-exp/fail" "frame/fail"))
;; A variable that the inner assignment does not bind is found outside it,
;; or fails.
(test-equal (run-assign (term (ITER (TUP ((VAR "a"))) (QUEST (("a" NAT ()) ("c" NAT ())))))
                        (term (OPT ((TUP ((NAT 1))))))
                        (term ((("c" ()) (NAT 9)))))
            (term (OK ((("c" ()) (NAT 9)) (("a" ()) (NAT 1))
                       (("a" (QUEST)) (OPT ((NAT 1)))) (("c" (QUEST)) (OPT ((NAT 9))))))))
(test-equal (run-assign (term (ITER (TUP ((VAR "a"))) (QUEST (("a" NAT ()) ("c" NAT ())))))
                        (term (OPT ((TUP ((NAT 1)))))))
            'FAIL)
(test-equal (trace-assign (term (ITER (TUP ((VAR "a"))) (QUEST (("a" NAT ()) ("c" NAT ())))))
                          (term (OPT ((TUP ((NAT 1)))))))
            '("assign-exp/iter/opt-some" "assign-exp/tup"
              "assign-exps/cons" "assign-exp/variable" "assign-exps/cons/tail"
              "assign-exps/nil" "assign-exp/iter/opt-some/fail"))

;;; Assign_exp/iter/list: each element is assigned under a layer with no
;;; values, and only the iterated variables are bound.

(test-equal (run-assign (term (ITER exp-ab (STAR varis-ab)))
                        (term (LIST ((TUP ((NAT 1) (NAT 2))) (TUP ((NAT 3) (NAT 4))))))
                        (term ((("z" ()) (NAT 0)))))
            (term (OK ((("z" ()) (NAT 0))
                       (("a" (STAR)) (LIST ((NAT 1) (NAT 3))))
                       (("b" (STAR)) (LIST ((NAT 2) (NAT 4))))))))
(test-equal (trace-assign (term (ITER (TUP ((VAR "a"))) (STAR (("a" NAT ())))))
                          (term (LIST ((TUP ((NAT 1))) (TUP ((NAT 3)))))))
            '("assign-exp/iter/list"
              "assign-exp/tup" "assign-exps/cons" "assign-exp/variable"
              "assign-exps/cons/tail" "assign-exps/nil"
              "assign-exp/tup" "assign-exps/cons" "assign-exp/variable"
              "assign-exps/cons/tail" "assign-exps/nil"
              "assign-exp/iter/list/bind"))
(test-equal (run-assign (term (ITER exp-ab (STAR varis-ab))) (term (LIST ())))
            (term (OK ((("a" (STAR)) (LIST ())) (("b" (STAR)) (LIST ()))))))
(test-equal (run-assign (term (ITER (TUP ()) (STAR ()))) (term (LIST ((TUP ()) (TUP ())))))
            (term (OK ())))
;; Nested iterations
(test-equal (run-assign (term (ITER (ITER (TUP ((VAR "a"))) (STAR (("a" NAT ()))))
                                    (STAR (("a" (ITER NAT STAR) (STAR))))))
                        (term (LIST ((LIST ((TUP ((NAT 1))) (TUP ((NAT 2)))))
                                     (LIST ())))))
            (term (OK ((("a" (STAR STAR)) (LIST ((LIST ((NAT 1) (NAT 2))) (LIST ()))))))))
;; An element fails, and no later one is assigned.
(test-equal (run-assign (term (ITER exp-ab (STAR varis-ab)))
                        (term (LIST ((TUP ((NAT 1) (NAT 2))) (NAT 3) (TUP ((NAT 4) (NAT 5)))))))
            'FAIL)
(test-equal (trace-assign (term (ITER (TUP ((VAR "a"))) (STAR (("a" NAT ())))))
                          (term (LIST ((NAT 3) (TUP ((NAT 4)))))))
            '("assign-exp/iter/list" "assign-exp/fail" "in/fail" "frame/fail"))
;; A variable bound only outside the iteration is not found.
(test-equal (run-assign (term (ITER (TUP ((VAR "a"))) (STAR (("a" NAT ()) ("c" NAT ())))))
                        (term (LIST ((TUP ((NAT 1))))))
                        (term ((("c" ()) (NAT 9)))))
            'FAIL)
(test-equal (trace-assign (term (ITER (TUP ((VAR "a"))) (STAR (("a" NAT ()) ("c" NAT ())))))
                          (term (LIST ((TUP ((NAT 1))))))
                          (term ((("c" ()) (NAT 9)))))
            '("assign-exp/iter/list" "assign-exp/tup" "assign-exps/cons" "assign-exp/variable"
              "assign-exps/cons/tail" "assign-exps/nil" "assign-exp/iter/list/fail"))

;;; No rule of Assign_exp/iter applies.

(test-equal (run-assign (term (ITER exp-ab (QUEST varis-ab))) (term (LIST ()))) 'FAIL)
(test-equal (run-assign (term (ITER exp-ab (STAR varis-ab))) (term (OPT ()))) 'FAIL)
(test-equal (run-assign (term (ITER exp-ab (STAR varis-ab))) (term (NAT 1))) 'FAIL)
(test-equal (trace-assign (term (ITER exp-ab (QUEST varis-ab))) (term (TUP ())))
            '("assign-exp/iter/fail"))

;;
;; Sequences of expressions
;;

(test-equal (run-assign-exps (term ()) (term ())) (term (OK ())))
(test-equal (run-assign-exps (term ((VAR "a") (VAR "b"))) (term ((NAT 1) (NAT 2))))
            (term (OK ((("a" ()) (NAT 1)) (("b" ()) (NAT 2))))))
(test-equal (run-assign-exps (term ((VAR "a"))) (term ())) 'FAIL)
(test-equal (run-assign-exps (term ()) (term ((NAT 1)))) 'FAIL)

;;
;; Arguments
;;

(test-equal (run-assign-arg (term (EXP (VAR "a"))) (term (NAT 1)))
            (term (OK (("h" (EXT "h"))) ((("a" ()) (NAT 1))))))
(test-equal (run-assign-arg (term (EXP (NAT 1))) (term (NAT 1))) 'FAIL)

;; Assign_arg/fun looks the function up in the caller's context: in its layer
;; first, then in G.
(test-equal (run-assign-arg (term (FUN "f")) (term (FUNC "g")))
            (term (OK (("h" (EXT "h")) ("f" (EXT "g-local"))) ())))
(test-equal (run-assign-arg (term (FUN "f")) (term (FUNC "k")))
            (term (OK (("h" (EXT "h")) ("f" (EXT "k"))) ())))
(test-equal (run-assign-arg (term (FUN "h")) (term (FUNC "k")))
            (term (OK (("h" (EXT "k"))) ())))
;; The callee's own functions are not the caller's.
(test-equal (run-assign-arg (term (FUN "f")) (term (FUNC "h"))) 'FAIL)
(test-equal (run-assign-arg (term (FUN "f")) (term (FUNC "missing"))) 'FAIL)
(test-equal (run-assign-arg (term (FUN "f")) (term (TEXT "g"))) 'FAIL)

(test-equal (run-assign-args (term ()) (term ())) (term (OK (("h" (EXT "h"))) ())))
(test-equal (run-assign-args (term ((EXP (VAR "a")) (FUN "f"))) (term ((NAT 1) (FUNC "g"))))
            (term (OK (("h" (EXT "h")) ("f" (EXT "g-local"))) ((("a" ()) (NAT 1))))))
(test-equal (trace-in (term G-funcs) (term L-callee)
                      (term (assign-args L-caller ((EXP (VAR "a")) (FUN "f"))
                                         ((NAT 1) (FUNC "g")))))
            '("assign-args/cons" "assign-arg/exp" "assign-exp/variable" "assign-args/cons/tail"
              "assign-args/cons" "assign-arg/fun" "assign-args/cons/tail" "assign-args/nil"))
(test-equal (run-assign-args (term ((EXP (VAR "a")))) (term ())) 'FAIL)
(test-equal (run-assign-args (term ()) (term ((NAT 1)))) 'FAIL)
;; The first failing argument stops the assignment.
(test-equal (trace-in (term G-funcs) (term L-callee)
                      (term (assign-args L-caller ((FUN "f") (EXP (VAR "a")))
                                         ((NAT 0) (NAT 1)))))
            '("assign-args/cons" "assign-arg/fail" "frame/fail"))
(test-equal (trace-in (term G-funcs) (term L-callee)
                      (term (assign-args L-caller ((FUN "f")) ((FUNC "missing")))))
            '("assign-args/cons" "assign-arg/fun/fail" "frame/fail"))

(check-coverage coverage)
