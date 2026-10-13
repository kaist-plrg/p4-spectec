#lang racket/base
;; spec-meta/al/5.4-eval-arg.watsup.
;;
;; An arg evaluates in place, as an exp does. Eval_arg/fail is "frame/fail"
;; on (EXP hole): no other input fails.

(require "../common/0.0-prelude.rkt"
         "../common/2-env.rkt"
         "../common/5.0-eval-typ.rkt"
         "3-context.rkt"
         "4-relation.rkt")
(provide ->redex/eval-arg
         ->ctx/eval-arg)

;; The rules on the redex
(define ->redex/eval-arg
  (reduction-relation/forms
   al

   ;;; Argument evaluation rules

   ;; rule Eval_arg/exp
   (--> (EXP (OK val)) (OK val)
        "eval-arg/exp")

   ;; rule Eval_arg/fun
   (--> (FUN id) (OK (FUNC id))
        "eval-arg/fun")))

;; The rules on the focus triple (r G L)
(define ->ctx/eval-arg
  (reduction-relation/forms
   al

   ;;; Type argument evaluation rules

   ;; rule Eval_targs
   (--> ((eval-targs (targ ...)) G (name L {TYP tdenv REL renv FUNC fenv VAL venv}))
        ((OK (typ_subst ...)) G L)
        (judgment-holds (theta-of-tdenv tdenv theta))
        (judgment-holds (subst-typs theta (targ ...) (typ_subst ...)))
        "eval-targs")
   (--> ((eval-targs (targ ...)) G (name L {TYP tdenv REL renv FUNC fenv VAL venv}))
        (FAIL G L)
        ;; otherwise: the local types are not all aliases
        (side-condition (not (judgment-holds (theta-of-tdenv tdenv any))))
        "eval-targs/fail-theta")
   (--> ((eval-targs (targ ...)) G (name L {TYP tdenv REL renv FUNC fenv VAL venv}))
        (FAIL G L)
        ;; otherwise: some substitution fails
        (judgment-holds (theta-of-tdenv tdenv theta))
        (side-condition (not (judgment-holds (subst-typs theta (targ ...) any))))
        "eval-targs/fail-subst")))
