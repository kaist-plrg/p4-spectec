#lang racket/base

(require "../common/0.0-prelude.rkt"
         "../al/5.4-eval-arg.rkt"
         "machine.rkt")

(define coverage (start-coverage ->redex/eval-arg ->ctx/eval-arg))

;; G defines a type alias too, which Eval_targs does not substitute.
(define-term G-typs {TYP (("Y" (DEF () (ALIAS TEXT)))) REL () FUNC () VAL ()})

(define-term L-vals {TYP () REL () FUNC () VAL ((("x" ()) (NAT 1)))})

(define (eval-arg arg)
  (eval-in (term G-typs) (term L-vals) arg))

(define (trace-arg arg)
  (trace-in (term G-typs) (term L-vals) arg))

;; Eval_targs under a layer whose TYP map is tdenv
(define (eval-targs tdenv targs)
  (eval-in (term G-typs) `(TYP ,tdenv REL () FUNC () VAL ()) `(eval-targs ,targs)))

;;
;; Arguments
;;

(test-equal (eval-arg (term (EXP (VAR "x")))) (term (OK (NAT 1))))
(test-equal (eval-arg (term (EXP (BIN ADD (VAR "x") (NAT 1))))) (term (OK (NAT 2))))
(test-equal (trace-arg (term (EXP (VAR "x")))) '("eval-exp/variable" "eval-arg/exp"))
;; otherwise: the expression fails
(test-equal (eval-arg (term (EXP (VAR "y")))) 'FAIL)
(test-equal (trace-arg (term (EXP (VAR "y")))) '("eval-exp/variable/fail" "frame/fail"))

;; A function argument is its name, whether or not the function exists.
(test-equal (eval-arg (term (FUN "f"))) (term (OK (FUNC "f"))))
(test-equal (trace-arg (term (FUN "f"))) '("eval-arg/fun"))

;;
;; Type arguments
;;

(define-term tdenv-X (("X" (DEF () (ALIAS NAT)))))

(test-equal (eval-targs (term ()) (term ())) (term (OK ())))
(test-equal (eval-targs (term ()) (term ((VAR "X" ()) BOOL))) (term (OK ((VAR "X" ()) BOOL))))
(test-equal (eval-targs (term tdenv-X)
                        (term ((VAR "X" ()) (ITER (VAR "X" ()) STAR) (TUP ((VAR "X" ()) INT)))))
            (term (OK (NAT (ITER NAT STAR) (TUP (NAT INT))))))
(test-equal (trace-in (term G-typs) (term {TYP tdenv-X REL () FUNC () VAL ()})
                      (term (eval-targs ((VAR "X" ())))))
            '("eval-targs"))
;; Only the local layer's types are substituted.
(test-equal (eval-targs (term tdenv-X) (term ((VAR "Y" ())))) (term (OK ((VAR "Y" ())))))
;; Parameters and extern types are not substituted.
(test-equal (eval-targs (term (("X" PARAM) ("E" EXT))) (term ((VAR "X" ()) (VAR "E" ()))))
            (term (OK ((VAR "X" ()) (VAR "E" ())))))
;; A local type that is not an alias without parameters: $theta_of_tdenv has
;; no result.
(test-equal (eval-targs (term (("Z" (DEF ("A") (ALIAS NAT))))) (term (NAT))) 'FAIL)
(test-equal (eval-targs (term (("Z" (DEF () (STRUCT ()))))) (term ())) 'FAIL)
(test-equal (trace-in (term G-typs) (term {TYP (("Z" (DEF () (STRUCT ())))) REL () FUNC () VAL ()})
                      (term (eval-targs ())))
            '("eval-targs/fail-theta"))
;; A substituted variable with type arguments: $subst_typ has no result.
(test-equal (eval-targs (term tdenv-X) (term (BOOL (VAR "X" (BOOL))))) 'FAIL)
(test-equal (trace-in (term G-typs) (term {TYP tdenv-X REL () FUNC () VAL ()})
                      (term (eval-targs ((VAR "X" (BOOL))))))
            '("eval-targs/fail-subst"))

(check-coverage coverage)
