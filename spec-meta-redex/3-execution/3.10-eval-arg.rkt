#lang racket/base
;; Mirrors spec-meta/4-execution/4.08-eval-arg.watsup.
;;
;; An arg evaluates in place, as an exp does. Eval_arg/error has no rule: an
;; argument whose expression errs is stuck, which is ERROR. Eval_targs runs on
;; a call once its arguments are evaluated, so the call is Call_func's.

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

;; The rules on a term and its context, (r G L K), which read G or L, or
;; write L
(define ->term+ctx/eval-arg
  (reduction-relation
   al

   ;;; Argument evaluation rules

   ;; rule Eval_arg/fun: the global function name the parameter stands for
   (--> ((FUN id) G L K) ((FUNC (resolve-func-param L id)) G L K)
        "eval-arg/fun")

   ;;; Type argument evaluation rules

   ;; rule Eval_targs, for Eval_exp/call: the type arguments are substituted
   ;; through the local tdenv. It passes typ_input*, where spec-meta passes
   ;; targ*.
   (--> ((CALL id (targ ...) (val_input ...)) G (name L {TYP tdenv FUNC fnenv VAL venv}) K)
        ((call-func id (typ_input ...) (val_input ...)) G L K)
        (where (OK (typ_input ...)) (subst-typs (theta-of-tdenv tdenv) (targ ...)))
        "eval-targs")))
