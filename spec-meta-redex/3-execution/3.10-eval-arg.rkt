#lang racket/base
;; spec-meta/3-execution/3.09-eval-arg.watsup.
;;
;; An arg evaluates in place, as an exp does. Eval_arg/error is
;; "frame/failure" on (EXP hole): no other input fails. Eval_targs runs on a
;; call once its arguments are evaluated, so the call is Call_func's.

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

;; The rules on a term and its context (r G L)
(define ->term+ctx/eval-arg
  (reduction-relation
   al

   ;;; Argument evaluation rules

   ;; rule Eval_arg/fun: the global function name the parameter stands for
   (--> ((FUN id) G L) ((FUNC (resolve-func-param L id)) G L)
        "eval-arg/fun")

   ;;; Type argument evaluation rules

   ;; rule Eval_targs, for Eval_exp/call: the type arguments are substituted
   ;; through the local tdenv. It passes typ_input*, where spec-meta passes
   ;; targ*.
   (--> ((CALL id (targ ...) (val_input ...)) G (name L {TYP tdenv FUNC fnenv VAL venv}))
        ((call-func id (typ_input ...) (val_input ...)) G L)
        (where (OK (typ_input ...)) (subst-typs (theta-of-tdenv tdenv) (targ ...)))
        "eval-targs")
   (--> ((CALL id (targ ...) (val_input ...)) G (name L {TYP tdenv FUNC fnenv VAL venv}))
        (ERROR G L)
        (where ERROR (subst-typs (theta-of-tdenv tdenv) (targ ...)))
        "eval-targs/error")))
