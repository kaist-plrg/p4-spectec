#lang racket/base
;; spec-meta/al/5.2-eval-assign.watsup.
;;
;; An assignment that does not apply has no derivation.

(require "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/2-env.rkt"
         "3-context.rkt")
(provide Assign_exp
         Assign_exps
         Assign_arg
         Assign_args)

;;; Assigning to an expression

;; ctx |- exp := val : ctx
(define-relation AL-context
  #:mode (Assign_exp I I I O)
  #:contract (Assign_exp ctx exp val ctx)
  [(where C_1 (add_varr C (id ()) val))
   ------------------------------------ "variable"
   (Assign_exp C (VAR id) val C_1)]

  [(Assign_exps C (exp ...) (val ...) C_1)
   -------------------------------------------------- "tup"
   (Assign_exp C (TUP (exp ...)) (TUP (val ...)) C_1)]

  [(Assign_exps C (exp ...) (val ...) C_1)
   ------------------------------------------------------------------ "inj"
   (Assign_exp C (INJ (mixop (exp ...))) (INJ (mixop (val ...))) C_1)]

  [(where ((atom val) ...) (valfield ...))
   (where ((atom exp) ...) (expfield ...))
   (Assign_exps C (exp ...) (val ...) C_1)
   ------------------------------------------------------------ "str"
   (Assign_exp C (STR (expfield ...)) (STR (valfield ...)) C_1)]

  [(Assign_exp C exp val C_1)
   ------------------------------------------ "opt/opt-some"
   (Assign_exp C (OPT (exp)) (OPT (val)) C_1)]

  [---------------------------------- "opt/opt-none"
   (Assign_exp C (OPT ()) (OPT ()) C)]

  [(Assign_exps C (exp ...) (val ...) C_1)
   ---------------------------------------------------- "list"
   (Assign_exp C (LIST (exp ...)) (LIST (val ...)) C_1)]

  [(Assign_exp C exp_h val_h C_1)
   (Assign_exp C_1 exp_t (LIST (val_t ...)) C_2)
   -------------------------------------------------------------- "cons"
   (Assign_exp C (CONS exp_h exp_t) (LIST (val_h val_t ...)) C_2)]

  [(where (varr) (is_iter_on_var (ITER exp iterexp)))
   (where C_1 (add_varr C varr val))
   ----------------------------------------- "iter/simple"
   (Assign_exp C (ITER exp iterexp) val C_1)]

  [(where () (is_iter_on_var (ITER exp iterexp)))
   (where (QUEST (vari ...)) iterexp)
   (where (vari_iter ...) ((iter_vari vari QUEST) ...))
   (where (val_bind ...) (repeat_ (OPT ()) ,(length (term (vari_iter ...)))))
   (where C_1 (add_varis C (vari_iter ...) (val_bind ...)))
   ---------------------------------------------- "iter/opt-none"
   (Assign_exp C (ITER exp iterexp) (OPT ()) C_1)]

  [(where () (is_iter_on_var (ITER exp iterexp)))
   (where (QUEST (vari ...)) iterexp)
   (Assign_exp C exp val C_1)
   (where (vari_iter ...) ((iter_vari vari QUEST) ...))
   (where ((val_sub) ...) ((find_vari C_1 vari) ...))
   (where (val_bind ...) ((OPT (val_sub)) ...))
   (where C_2 (add_varis C_1 (vari_iter ...) (val_bind ...)))
   ------------------------------------------------- "iter/opt-some"
   (Assign_exp C (ITER exp iterexp) (OPT (val)) C_2)]

  [(where () (is_iter_on_var (ITER exp iterexp)))
   (where (STAR (vari ...)) iterexp)
   (where {GLOBAL layer_g LOCAL {TYP tdenv REL renv FUNC fenv VAL _}} C)
   (where C_local {GLOBAL layer_g LOCAL {TYP tdenv REL renv FUNC fenv VAL (empty_map)}})
   (Assign_exp C_local exp val C_sub) ...
   (where (vari_iter ...) ((iter_vari vari STAR) ...))
   (where (((val_sub ...)) ...) ((finds_vari (C_sub ...) vari) ...))
   (where (val_bind ...) ((LIST (val_sub ...)) ...))
   (where C_1 (add_varis C (vari_iter ...) (val_bind ...)))
   ------------------------------------------------------ "iter/list"
   (Assign_exp C (ITER exp iterexp) (LIST (val ...)) C_1)])

;;; Assigning to a sequence of expressions

;; ctx |- exp* := val* : ctx
(define-relation AL-context
  #:mode (Assign_exps I I I O)
  #:contract (Assign_exps ctx (exp ...) (val ...) ctx)
  [----------------------- "nil"
   (Assign_exps C () () C)]

  [(Assign_exp C exp_h val_h C_1)
   (Assign_exps C_1 (exp_t ...) (val_t ...) C_2)
   ------------------------------------------------------- "cons"
   (Assign_exps C (exp_h exp_t ...) (val_h val_t ...) C_2)])

;;; Assigning to an argument

;; ctx '/' ctx |- arg := val : ctx
(define-relation AL-context
  #:mode (Assign_arg I I I I O)
  #:contract (Assign_arg ctx ctx arg val ctx)
  [(Assign_exp C_callee exp val C_callee_1)
   ------------------------------------------------ "exp"
   (Assign_arg C_callee _ (EXP exp) val C_callee_1)]

  [(where (funcdef) (find_func C_caller id_caller))
   (where C_callee_1 (add_func C_callee id_callee funcdef))
   -------------------------------------------------------------------------- "fun"
   (Assign_arg C_callee C_caller (FUN id_callee) (FUNC id_caller) C_callee_1)])

;;; Assigning to a sequence of arguments

;; ctx '/' ctx |- arg* := val* : ctx
(define-relation AL-context
  #:mode (Assign_args I I I I O)
  #:contract (Assign_args ctx ctx (arg ...) (val ...) ctx)
  [-------------------------------------- "nil"
   (Assign_args C_callee _ () () C_callee)]

  [(Assign_arg C_callee C_caller arg_h val_h C_callee_1)
   (Assign_args C_callee_1 C_caller (arg_t ...) (val_t ...) C_callee_2)
   ------------------------------------------------------------------------------ "cons"
   (Assign_args C_callee C_caller (arg_h arg_t ...) (val_h val_t ...) C_callee_2)])
