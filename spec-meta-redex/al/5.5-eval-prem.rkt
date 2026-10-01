#lang racket/base
;; spec-meta/al/5.5-eval-prem.watsup.
;;
;; A premise evaluates in place, updates the innermost IN's layer, and reduces
;; to OK. Eval_prem/fail becomes complement rules, and "frame/fail" where a
;; premise's sub-relation fails.

(require "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/2-env.rkt"
         "3-context.rkt"
         "4-relation.rkt")
(provide ->redex/eval-prem
         ->ctx/eval-prem)

;; The rules on the redex
(define ->redex/eval-prem
  (reduction-relation/forms
   al

   ;;; Relation premises

   ;; rule Eval_prem/relpr, once the inputs are evaluated
   (--> (REL id ((OK val) ...) (exp_out ...))
        (eval-prem/relpr (call-rel id (val ...)) (exp_out ...))
        "eval-prem/relpr")
   (--> (eval-prem/relpr (OK (val_out ...)) (exp_out ...))
        (assign-exps (exp_out ...) (val_out ...))
        "eval-prem/relpr/assign")

   ;;; If premises

   ;; rulegroup Eval_prem/ifpr
   (--> (IF (OK (BOOL #t))) OK
        "eval-prem/ifpr/true")
   (--> (IF (OK (BOOL #f))) FAIL
        "eval-prem/ifpr/false")
   (--> (IF (OK val)) FAIL
        ;; otherwise: not a boolean
        (side-condition (not (redex-match? al (BOOL b) (term val))))
        "eval-prem/ifpr/fail")

   ;;; If-hold and if-not-hold premises

   ;; rule Eval_prem/hold, once the inputs are evaluated
   (--> (IFHOLD id ((OK val_input) ...))
        (eval-prem/ifholdpr/hold (call-rel id (val_input ...)))
        "eval-prem/ifholdpr/hold")
   (--> (eval-prem/ifholdpr/hold (OK ())) OK
        "eval-prem/ifholdpr/hold/result")
   (--> (eval-prem/ifholdpr/hold (OK (val_h val_t ...))) FAIL
        ;; otherwise: the relation has outputs
        "eval-prem/ifholdpr/hold/fail")

   ;; rule Eval_prem/nothold, once the relation is called (see ->ctx)
   (--> (eval-prem/ifholdpr/nothold FAIL) OK
        "eval-prem/ifholdpr/nothold/result")
   (--> (eval-prem/ifholdpr/nothold (OK (val_out ...))) FAIL
        ;; otherwise: the relation holds
        "eval-prem/ifholdpr/nothold/fail")

   ;;; Let premises

   ;; rule Eval_prem/letpr
   (--> (LET exp_l (OK val_r)) (assign-exp exp_l val_r)
        "eval-prem/letpr")

   ;;; Debug premises

   ;; rule Eval_prem/dbg
   (--> (DEBUG (OK val)) OK
        (where _ ,(debug (term val)))
        "eval-prem/dbg")

   ;;; Premise sequences

   ;; rulegroup Eval_prems. head-fail and head-succ share the head premise,
   ;; and head-fail is "frame/fail".
   (--> (eval-prems ()) OK
        "eval-prems/empty")
   (--> (eval-prems (prem_h prem_t ...)) (eval-prems/head prem_h (prem_t ...))
        "eval-prems/head")
   (--> (eval-prems/head OK (prem_t ...)) (eval-prems (prem_t ...))
        "eval-prems/head-succ")))

;; The rules on the focus triple (r G L)
(define ->ctx/eval-prem
  (reduction-relation/forms
   al

   ;;; If-not-hold premises

   ;; rule Eval_prem/nothold, once the inputs are evaluated. Call_rel has no
   ;; derivation for an unknown relation, so the relation is looked up here,
   ;; before the FAIL of the call is caught.
   (--> ((IFNOTHOLD id ((OK val_input) ...)) G L)
        ((eval-prem/ifholdpr/nothold (call-rel id (val_input ...))) G L)
        (where (reldef) (find-rel G L id))
        "eval-prem/ifholdpr/nothold")
   (--> ((IFNOTHOLD id ((OK val_input) ...)) G L) (FAIL G L)
        ;; otherwise: no such relation
        (where () (find-rel G L id))
        "eval-prem/ifholdpr/nothold/fail-rel")

   ;;; Iteration premises - optional

   ;; rulegroup Eval_prem/iterpr-opt
   (--> ((ITER prem (QUEST (vari_bound ...) (vari_bind ...))) G L) (OK G L_res)
        (where () (sub-opt L (vari_bound ...)))
        (where (vari_bind_iter ...) ((iter-vari vari_bind QUEST) ...))
        (where (val_bind ...) (repeat- (OPT ()) ,(length (term (vari_bind ...)))))
        (where L_res (add-varis L (vari_bind_iter ...) (val_bind ...)))
        "eval-prem/iterpr-opt/none")
   (--> ((ITER prem (QUEST (vari_bound ...) (vari_bind ...))) G L)
        ((eval-prem/iterpr-opt/some (IN L_sub prem) (vari_bind ...)) G L)
        (where (L_sub) (sub-opt L (vari_bound ...)))
        "eval-prem/iterpr-opt/some")
   (--> ((ITER prem (QUEST (vari_bound ...) (vari_bind ...))) G L) (FAIL G L)
        ;; otherwise: $sub_opt has no result
        (where ⊥ (sub-opt L (vari_bound ...)))
        "eval-prem/iterpr-opt/fail")

   ;; Eval_prem/some, once prem has updated C_sub to C_sub_res
   (--> ((eval-prem/iterpr-opt/some (IN L_sub_res OK) (vari_bind ...)) G L) (OK G L_res)
        (where (vari_bind_iter ...) ((iter-vari vari_bind QUEST) ...))
        (where ((val_bind) ...) ((find-vari L_sub_res vari_bind) ...))
        (where L_res (add-varis L (vari_bind_iter ...) ((OPT (val_bind)) ...)))
        "eval-prem/iterpr-opt/some/bind")
   (--> ((eval-prem/iterpr-opt/some (IN L_sub_res OK) (vari_bind ...)) G L) (FAIL G L)
        ;; otherwise: some variable is unbound
        (side-condition
         (not (redex-match? al ((val) ...) (term ((find-vari L_sub_res vari_bind) ...)))))
        "eval-prem/iterpr-opt/some/fail")

   ;;; Iteration premises - list

   ;; rulegroup Eval_prem/iterpr-list
   (--> ((ITER prem (STAR (vari_bound ...) (vari_bind ...))) G L) (OK G L_res)
        (where () (sub-list L (vari_bound ...)))
        (where (vari_bind_iter ...) ((iter-vari vari_bind STAR) ...))
        (where (val_bind ...) (repeat- (LIST ()) ,(length (term (vari_bind ...)))))
        (where L_res (add-varis L (vari_bind_iter ...) (val_bind ...)))
        "eval-prem/iterpr-list/empty")
   (--> ((ITER prem (STAR (vari_bound ...) (vari_bind ...))) G L)
        ((eval-prem/iterpr-list/list ((IN L_sub prem) ...) (vari_bind ...)) G L)
        (where (L_sub ...) (sub-list L (vari_bound ...)))
        (side-condition (pair? (term (L_sub ...))))
        "eval-prem/iterpr-list/list")
   (--> ((ITER prem (STAR (vari_bound ...) (vari_bind ...))) G L) (FAIL G L)
        ;; otherwise: $sub_list has no result
        (where ⊥ (sub-list L (vari_bound ...)))
        "eval-prem/iterpr-list/fail")

   ;; Eval_prem/list, once prem has updated each C_sub to C_sub_res
   (--> ((eval-prem/iterpr-list/list ((IN L_sub_res OK) ...) (vari_bind ...)) G L) (OK G L_res)
        (where (((val_sub_bind ...)) ...) ((find-varis L_sub_res (vari_bind ...)) ...))
        (where ((val_bind ...) ...) (transpose- ((val_sub_bind ...) ...)))
        (where (val_res_bind ...) ((LIST (val_bind ...)) ...))
        (where (vari_bind_iter ...) ((iter-vari vari_bind STAR) ...))
        (where L_res (add-varis L (vari_bind_iter ...) (val_res_bind ...)))
        "eval-prem/iterpr-list/list/bind")
   (--> ((eval-prem/iterpr-list/list ((IN L_sub_res OK) ...) (vari_bind ...)) G L) (FAIL G L)
        ;; otherwise: some variable is unbound in some sub-context
        (side-condition
         (not (redex-match? al (((val ...)) ...)
                            (term ((find-varis L_sub_res (vari_bind ...)) ...)))))
        "eval-prem/iterpr-list/list/fail")))
