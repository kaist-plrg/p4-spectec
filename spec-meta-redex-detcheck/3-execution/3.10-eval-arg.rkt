#lang racket/base
;; Mirrors spec-meta/3-execution/3.08-eval-arg.watsup.

(require redex/reduction-semantics
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.06.1-typ-subst.rkt")
(provide ->term/eval-arg
         ->term+ctx/eval-arg)

;; The rules on a term
(define ->term/eval-arg
  (reduction-relation
   al

   ;;; Argument evaluation rules

   ;; rule Eval_arg/exp
   (--> (EXP val) val
        "eval-arg/exp")))

;; The rules on a term and its context, (r G L K)
(define ->term+ctx/eval-arg
  (reduction-relation
   al

   ;;; Argument evaluation rules

   ;; rule Eval_arg/fun
   (--> ((FUN id) G L K) ((FUNC (resolve-func-param L id)) G L K)
        "eval-arg/fun")

   ;;; Type argument evaluation rules

   ;; rule Eval_targs, for Eval_exp/call
   (--> ((CALL id (targ ...) (val_input ...)) G (name L {TYP tdenv FUNC fnenv VAL venv}) K)
        ((call-func id (typ_input ...) (val_input ...)) G L K)
        (where (OK (typ_input ...)) (subst-typs (theta-of-tdenv tdenv) (targ ...)))
        "eval-targs")
   (--> ((CALL id (targ ...) (val_input ...)) G (name L {TYP tdenv FUNC fnenv VAL venv}) K)
        (ERROR G L K)
        (where ERROR (subst-typs (theta-of-tdenv tdenv) (targ ...)))
        "eval-targs/error")))
