#lang racket/base
;; Mirrors spec-meta/4-execution/4.05-eval-assign.watsup.
;;
;; A successful assignment updates the innermost IN's layer and reduces to OK.
;; The relations' `otherwise` rules, which give ERROR, have no rule here: an
;; input that no rule matches is stuck, which is ERROR (3.03-machine.rkt).

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "../2-runtime/2.1-variable.rkt"
         "3.02-context.rkt"
         "3.03-machine.rkt")
(provide ->term/eval-assign
         ->term+ctx/eval-assign)

;; The rules on a term
(define ->term/eval-assign
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

   ;; rule Assign_exp/cons: the head, then the tail
   (--> (assign-exp (CONS exp_h exp_t) (LIST (val_h val_t ...)))
        (assign-exps (exp_h exp_t) (val_h (LIST (val_t ...))))
        "assign-exp/cons")

   ;; rule Assign_exp/iter/opt-some, until the inner assignment is done
   (--> (assign-exp (ITER exp (QUEST (vari ...))) (OPT (val)))
        (assign-exp/iter/opt-some (assign-exp exp val) (vari ...))
        (where () (is-iter-on-var (ITER exp (QUEST (vari ...)))))
        "assign-exp/iter/opt-some")

   ;;; Assigning to a sequence of expressions

   ;; rulegroup Assign_exps: each exp is assigned its val in place, left to
   ;; right
   (--> (assign-exps (exp ..._n) (val ..._n)) (seq ((assign-exp exp val) ...))
        "assign-exps")

   ;;; Assigning to an argument

   ;; rule Assign_arg/exp
   (--> (assign-arg (EXP exp) val)
        (assign-exp exp val)
        "assign-arg/exp")

   ;;; Assigning to a sequence of arguments

   ;; rulegroup Assign_args: each arg is assigned its val in place, left to
   ;; right
   (--> (assign-args (arg ..._n) (val ..._n)) (seq ((assign-arg arg val) ...))
        "assign-args")))

;; The rules on a term and its context (r G L)
(define ->term+ctx/eval-assign
  (reduction-relation
   al

   ;;; Assigning to an expression

   ;; rule Assign_exp/variable
   (--> ((assign-exp (VAR id) val) G L) (OK G (add-varr L (id ()) val))
        "assign-exp/variable")

   ;; rulegroup Assign_exp/iter

   (--> ((assign-exp (ITER exp iterExp) val) G L) (OK G (add-varr L varr val))
        (where (varr) (is-iter-on-var (ITER exp iterExp)))
        "assign-exp/iter/simple")

   (--> ((assign-exp (ITER exp (QUEST (vari ...))) (OPT ())) G L)
        (OK G (add-varis L ((iter-vari vari QUEST) ...)
                         (repeat- (OPT ()) ,(length (term (vari ...))))))
        (where () (is-iter-on-var (ITER exp (QUEST (vari ...)))))
        "assign-exp/iter/opt-none")

   ;; Assign_exp/iter/opt-some, once the inner assignment has updated L.
   ;; It adds (OPT val_sub)*, where spec-meta adds each OPT val_sub on its own.
   (--> ((assign-exp/iter/opt-some OK (vari ...)) G L)
        (OK G (add-varis L ((iter-vari vari QUEST) ...) ((OPT (val_sub)) ...)))
        (where ((val_sub ...)) (find-varis L (vari ...)))
        "assign-exp/iter/opt-some/bind")

   ;; Assign_exp/iter/list: each element is assigned under C_local, which has
   ;; no values
   (--> ((assign-exp (ITER exp (STAR (vari ...))) (LIST (val ...)))
         G
         (name L {TYP tdenv FUNC fnenv VAL venv}))
        ((assign-exp/iter/list
          ((IN {TYP tdenv FUNC fnenv VAL (empty-map)} (assign-exp exp val)) ...)
          (vari ...))
         G L)
        (where () (is-iter-on-var (ITER exp (STAR (vari ...)))))
        "assign-exp/iter/list")
   (--> ((assign-exp/iter/list ((IN L_sub OK) ...) (vari ...)) G L)
        (OK G (add-varis L ((iter-vari vari STAR) ...) ((LIST (val_sub ...)) ...)))
        (where (((val_sub ...)) ...) ((finds-vari (L_sub ...) vari) ...))
        "assign-exp/iter/list/bind")

   ;;; Assigning to an argument

   ;; rule Assign_arg/fun: the parameter is bound to the global function name
   (--> ((assign-arg (FUN id) (FUNC id_global)) G L)
        (OK G (add-func L id id_global))
        "assign-arg/fun")))
