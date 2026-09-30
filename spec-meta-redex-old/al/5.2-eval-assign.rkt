#lang racket/base
;; spec-meta/al/5.2-eval-assign.watsup.
;;
;; An assignment that does not apply has no derivation.

(require "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/2-env.rkt"
         "3-context.rkt")
(provide assign-exp
         assign-exps
         assign-arg
         assign-args)

;;; Assigning to an expression

;; ctx |- exp := val : ctx
(define-relation al-context
  #:mode (assign-exp I I I O)
  #:contract (assign-exp ctx exp val ctx)
  [(where C_1 (add-varr C (id ()) val))
   ------------------------------------ "variable"
   (assign-exp C (VAR id) val C_1)]

  [(assign-exps C (exp ...) (val ...) C_1)
   -------------------------------------------------- "tup"
   (assign-exp C (TUP (exp ...)) (TUP (val ...)) C_1)]

  [(assign-exps C (exp ...) (val ...) C_1)
   ------------------------------------------------------------------ "inj"
   (assign-exp C (INJ (mixop (exp ...))) (INJ (mixop (val ...))) C_1)]

  [(where ((atom val) ...) (valfield ...))
   (where ((atom exp) ...) (expfield ...))
   (assign-exps C (exp ...) (val ...) C_1)
   ------------------------------------------------------------ "str"
   (assign-exp C (STR (expfield ...)) (STR (valfield ...)) C_1)]

  [(assign-exp C exp val C_1)
   ------------------------------------------ "opt/opt-some"
   (assign-exp C (OPT (exp)) (OPT (val)) C_1)]

  [---------------------------------- "opt/opt-none"
   (assign-exp C (OPT ()) (OPT ()) C)]

  [(assign-exps C (exp ...) (val ...) C_1)
   ---------------------------------------------------- "list"
   (assign-exp C (LIST (exp ...)) (LIST (val ...)) C_1)]

  [(assign-exp C exp_h val_h C_1)
   (assign-exp C_1 exp_t (LIST (val_t ...)) C_2)
   -------------------------------------------------------------- "cons"
   (assign-exp C (CONS exp_h exp_t) (LIST (val_h val_t ...)) C_2)]

  [(where (varr) (is-iter-on-var (ITER exp iterexp)))
   (where C_1 (add-varr C varr val))
   ----------------------------------------- "iter/simple"
   (assign-exp C (ITER exp iterexp) val C_1)]

  [(where () (is-iter-on-var (ITER exp iterexp)))
   (where (QUEST (vari ...)) iterexp)
   (where (vari_iter ...) ((iter-vari vari QUEST) ...))
   (where (val_bind ...) (repeat- (OPT ()) ,(length (term (vari_iter ...)))))
   (where C_1 (add-varis C (vari_iter ...) (val_bind ...)))
   ---------------------------------------------- "iter/opt-none"
   (assign-exp C (ITER exp iterexp) (OPT ()) C_1)]

  [(where () (is-iter-on-var (ITER exp iterexp)))
   (where (QUEST (vari ...)) iterexp)
   (assign-exp C exp val C_1)
   (where (vari_iter ...) ((iter-vari vari QUEST) ...))
   (where ((val_sub) ...) ((find-vari C_1 vari) ...))
   (where (val_bind ...) ((OPT (val_sub)) ...))
   (where C_2 (add-varis C_1 (vari_iter ...) (val_bind ...)))
   ------------------------------------------------- "iter/opt-some"
   (assign-exp C (ITER exp iterexp) (OPT (val)) C_2)]

  [(where () (is-iter-on-var (ITER exp iterexp)))
   (where (STAR (vari ...)) iterexp)
   (where {GLOBAL layer_g LOCAL {TYP tdenv REL renv FUNC fenv VAL _}} C)
   (where C_local {GLOBAL layer_g LOCAL {TYP tdenv REL renv FUNC fenv VAL (empty-map)}})
   (assign-exp C_local exp val C_sub) ...
   (where (vari_iter ...) ((iter-vari vari STAR) ...))
   (where (((val_sub ...)) ...) ((finds-vari (C_sub ...) vari) ...))
   (where (val_bind ...) ((LIST (val_sub ...)) ...))
   (where C_1 (add-varis C (vari_iter ...) (val_bind ...)))
   ------------------------------------------------------ "iter/list"
   (assign-exp C (ITER exp iterexp) (LIST (val ...)) C_1)])

;;; Assigning to a sequence of expressions

;; ctx |- exp* := val* : ctx
(define-relation al-context
  #:mode (assign-exps I I I O)
  #:contract (assign-exps ctx (exp ...) (val ...) ctx)
  [----------------------- "nil"
   (assign-exps C () () C)]

  [(assign-exp C exp_h val_h C_1)
   (assign-exps C_1 (exp_t ...) (val_t ...) C_2)
   ------------------------------------------------------- "cons"
   (assign-exps C (exp_h exp_t ...) (val_h val_t ...) C_2)])

;;; Assigning to an argument

;; ctx '/' ctx |- arg := val : ctx
(define-relation al-context
  #:mode (assign-arg I I I I O)
  #:contract (assign-arg ctx ctx arg val ctx)
  [(assign-exp C_callee exp val C_callee_1)
   ------------------------------------------------ "exp"
   (assign-arg C_callee _ (EXP exp) val C_callee_1)]

  [(where (funcdef) (find-func C_caller id_caller))
   (where C_callee_1 (add-func C_callee id_callee funcdef))
   -------------------------------------------------------------------------- "fun"
   (assign-arg C_callee C_caller (FUN id_callee) (FUNC id_caller) C_callee_1)])

;;; Assigning to a sequence of arguments

;; ctx '/' ctx |- arg* := val* : ctx
(define-relation al-context
  #:mode (assign-args I I I I O)
  #:contract (assign-args ctx ctx (arg ...) (val ...) ctx)
  [-------------------------------------- "nil"
   (assign-args C_callee _ () () C_callee)]

  [(assign-arg C_callee C_caller arg_h val_h C_callee_1)
   (assign-args C_callee_1 C_caller (arg_t ...) (val_t ...) C_callee_2)
   ------------------------------------------------------------------------------ "cons"
   (assign-args C_callee C_caller (arg_h arg_t ...) (val_h val_t ...) C_callee_2)])
