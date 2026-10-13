#lang racket/base
;; spec-meta/al/5.4-eval-arg.watsup.
;;
;; An arg evaluates in place, as an exp does. Eval_arg/fail is "frame/fail"
;; on (EXP hole): no other input fails.

(require redex/reduction-semantics
         "../common/2-env.rkt"
         "../common/5.0-eval-typ.rkt"
         "3-context.rkt"
         "4-relation.rkt")
(provide ->term/eval-arg
         ->term+ctx/eval-arg)

;; The rules on a term
(define ->term/eval-arg
  (reduction-relation
   al

   ;;; Argument evaluation rules

   ;; rule Eval_arg/exp
   (--> (EXP (OK val)) (OK val)
        "eval-arg/exp")

   ;; rule Eval_arg/fun
   (--> (FUN id) (OK (FUNC id))
        "eval-arg/fun")))

;; The rules on a term and its context (r G L)
(define ->term+ctx/eval-arg
  (reduction-relation
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
