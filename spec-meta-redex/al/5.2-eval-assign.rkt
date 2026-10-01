#lang racket/base
;; spec-meta/al/5.2-eval-assign.watsup.
;;
;; A successful assignment updates the innermost IN's layer and reduces to OK.
;; The relations have no `otherwise`, so an input that no rule matches has no
;; derivation, and its caller fails: here it reduces to FAIL directly.

(require "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/2-env.rkt"
         "3-context.rkt"
         "4-relation.rkt")
(provide ->redex/eval-assign
         ->ctx/eval-assign)

;; The rules on the redex
(define ->redex/eval-assign
  (reduction-relation
   al

   ;;; Assigning to an expression

   ;; rule Assign_exp/tup
   (--> (assign-exp (TUP (exp ...)) (TUP (val ...)))
        (assign-exps (exp ...) (val ...))
        "assign-exp/tup")

   ;; rule Assign_exp/inj
   (--> (assign-exp (INJ (mixop (exp ...))) (INJ (mixop (val ...))))
        (assign-exps (exp ...) (val ...))
        "assign-exp/inj")

   ;; rule Assign_exp/str
   (--> (assign-exp (STR ((atom exp) ...)) (STR ((atom val) ...)))
        (assign-exps (exp ...) (val ...))
        "assign-exp/str")

   ;; rulegroup Assign_exp/opt
   (--> (assign-exp (OPT (exp)) (OPT (val)))
        (assign-exp exp val)
        "assign-exp/opt/opt-some")
   (--> (assign-exp (OPT ()) (OPT ()))
        OK
        "assign-exp/opt/opt-none")

   ;; rule Assign_exp/list
   (--> (assign-exp (LIST (exp ...)) (LIST (val ...)))
        (assign-exps (exp ...) (val ...))
        "assign-exp/list")

   ;; rule Assign_exp/cons
   (--> (assign-exp (CONS exp_h exp_t) (LIST (val_h val_t ...)))
        (assign-exp/cons (assign-exp exp_h val_h) exp_t (LIST (val_t ...)))
        "assign-exp/cons")
   (--> (assign-exp/cons OK exp_t (LIST (val_t ...)))
        (assign-exp exp_t (LIST (val_t ...)))
        "assign-exp/cons/tail")

   ;; rule Assign_exp/iter/opt-some, until the inner assignment is done
   (--> (assign-exp (ITER exp (QUEST (vari ...))) (OPT (val)))
        (assign-exp/iter/opt-some (assign-exp exp val) (vari ...))
        (where () (is-iter-on-var (ITER exp (QUEST (vari ...)))))
        "assign-exp/iter/opt-some")

   ;; otherwise: no rule applies
   (--> (assign-exp exp val) FAIL
        (side-condition (not (redex-match? al (VAR id) (term exp))))
        (side-condition
         (not (redex-match? al ((TUP (exp ...)) (TUP (val ...))) (term (exp val)))))
        (side-condition
         (not (redex-match? al ((INJ (mixop (exp ...))) (INJ (mixop (val ...))))
                            (term (exp val)))))
        (side-condition
         (not (redex-match? al ((STR ((atom exp) ...)) (STR ((atom val) ...)))
                            (term (exp val)))))
        (side-condition (not (redex-match? al ((OPT (exp)) (OPT (val))) (term (exp val)))))
        (side-condition (not (redex-match? al ((OPT ()) (OPT ())) (term (exp val)))))
        (side-condition
         (not (redex-match? al ((LIST (exp ...)) (LIST (val ...))) (term (exp val)))))
        (side-condition
         (not (redex-match? al ((CONS exp_h exp_t) (LIST (val_h val_t ...))) (term (exp val)))))
        (side-condition (not (redex-match? al (ITER exp iterexp) (term exp))))
        "assign-exp/fail")
   (--> (assign-exp (ITER exp iterexp) val) FAIL
        (where () (is-iter-on-var (ITER exp iterexp)))
        (side-condition
         (not (redex-match? al ((QUEST (vari ...)) (OPT (val ...))) (term (iterexp val)))))
        (side-condition
         (not (redex-match? al ((STAR (vari ...)) (LIST (val ...))) (term (iterexp val)))))
        "assign-exp/iter/fail")

   ;;; Assigning to a sequence of expressions

   ;; rulegroup Assign_exps
   (--> (assign-exps () ())
        OK
        "assign-exps/nil")
   (--> (assign-exps (exp_h exp_t ...) (val_h val_t ...))
        (assign-exps/cons (assign-exp exp_h val_h) (exp_t ...) (val_t ...))
        "assign-exps/cons")
   (--> (assign-exps/cons OK (exp_t ...) (val_t ...))
        (assign-exps (exp_t ...) (val_t ...))
        "assign-exps/cons/tail")
   (--> (assign-exps (exp ...) (val ...)) FAIL
        ;; otherwise: one sequence is longer
        (side-condition (not (eq? (null? (term (exp ...))) (null? (term (val ...))))))
        "assign-exps/fail")

   ;;; Assigning to an argument

   ;; rule Assign_arg/exp
   (--> (assign-arg L_caller (EXP exp) val)
        (assign-exp exp val)
        "assign-arg/exp")

   ;; otherwise: Assign_arg/fun on a value other than a function
   (--> (assign-arg L_caller (FUN id) val) FAIL
        (side-condition (not (redex-match? al (FUNC id) (term val))))
        "assign-arg/fail")

   ;;; Assigning to a sequence of arguments

   ;; rulegroup Assign_args
   (--> (assign-args L_caller () ())
        OK
        "assign-args/nil")
   (--> (assign-args L_caller (arg_h arg_t ...) (val_h val_t ...))
        (assign-args/cons L_caller (assign-arg L_caller arg_h val_h) (arg_t ...) (val_t ...))
        "assign-args/cons")
   (--> (assign-args/cons L_caller OK (arg_t ...) (val_t ...))
        (assign-args L_caller (arg_t ...) (val_t ...))
        "assign-args/cons/tail")
   (--> (assign-args L_caller (arg ...) (val ...)) FAIL
        ;; otherwise: one sequence is longer
        (side-condition (not (eq? (null? (term (arg ...))) (null? (term (val ...))))))
        "assign-args/fail")))

;; The rules on the focus triple (r G L)
(define ->ctx/eval-assign
  (reduction-relation
   al

   ;;; Assigning to an expression

   ;; rule Assign_exp/variable
   (--> ((assign-exp (VAR id) val) G L) (OK G L_1)
        (where L_1 (add-varr L (id ()) val))
        "assign-exp/variable")

   ;; rulegroup Assign_exp/iter

   (--> ((assign-exp (ITER exp iterexp) val) G L) (OK G L_1)
        (where (varr) (is-iter-on-var (ITER exp iterexp)))
        (where L_1 (add-varr L varr val))
        "assign-exp/iter/simple")

   (--> ((assign-exp (ITER exp (QUEST (vari ...))) (OPT ())) G L) (OK G L_1)
        (where () (is-iter-on-var (ITER exp (QUEST (vari ...)))))
        (where (vari_iter ...) ((iter-vari vari QUEST) ...))
        (where (val_bind ...) (repeat- (OPT ()) ,(length (term (vari_iter ...)))))
        (where L_1 (add-varis L (vari_iter ...) (val_bind ...)))
        "assign-exp/iter/opt-none")

   ;; Assign_exp/iter/opt-some, once the inner assignment has updated L.
   ;; It adds (OPT val_sub)*, where spec-meta adds each OPT val_sub on its own.
   (--> ((assign-exp/iter/opt-some OK (vari ...)) G L) (OK G L_1)
        (where (vari_iter ...) ((iter-vari vari QUEST) ...))
        (where ((val_sub) ...) ((find-vari L vari) ...))
        (where L_1 (add-varis L (vari_iter ...) ((OPT (val_sub)) ...)))
        "assign-exp/iter/opt-some/bind")
   (--> ((assign-exp/iter/opt-some OK (vari ...)) G L) (FAIL G L)
        ;; otherwise: some variable is unbound
        (side-condition (not (redex-match? al ((val) ...) (term ((find-vari L vari) ...)))))
        "assign-exp/iter/opt-some/fail")

   ;; Assign_exp/iter/list: each element is assigned under C_local, which has
   ;; no values
   (--> ((assign-exp (ITER exp (STAR (vari ...))) (LIST (val ...))) G L)
        ((assign-exp/iter/list ((IN L_local (assign-exp exp val)) ...) (vari ...)) G L)
        (where () (is-iter-on-var (ITER exp (STAR (vari ...)))))
        (where {TYP tdenv REL renv FUNC fenv VAL venv} L)
        (where venv_local (empty-map))
        (where L_local {TYP tdenv REL renv FUNC fenv VAL venv_local})
        "assign-exp/iter/list")
   (--> ((assign-exp/iter/list ((IN L_sub OK) ...) (vari ...)) G L) (OK G L_1)
        (where (vari_iter ...) ((iter-vari vari STAR) ...))
        (where (((val_sub ...)) ...) ((finds-vari (L_sub ...) vari) ...))
        (where L_1 (add-varis L (vari_iter ...) ((LIST (val_sub ...)) ...)))
        "assign-exp/iter/list/bind")
   (--> ((assign-exp/iter/list ((IN L_sub OK) ...) (vari ...)) G L) (FAIL G L)
        ;; otherwise: some variable is unbound in some element's layer
        (side-condition
         (not (redex-match? al (((val ...)) ...)
                            (term ((finds-vari (L_sub ...) vari) ...)))))
        "assign-exp/iter/list/fail")

   ;;; Assigning to an argument

   ;; rule Assign_arg/fun
   (--> ((assign-arg L_caller (FUN id_callee) (FUNC id_caller)) G L) (OK G L_1)
        (where (funcdef) (find-func G L_caller id_caller))
        (where L_1 (add-func L id_callee funcdef))
        "assign-arg/fun")
   (--> ((assign-arg L_caller (FUN id_callee) (FUNC id_caller)) G L) (FAIL G L)
        ;; otherwise: the caller has no such function
        (where () (find-func G L_caller id_caller))
        "assign-arg/fun/fail")))
