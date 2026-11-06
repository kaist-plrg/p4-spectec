#lang racket/base
;; Mirrors spec-meta/4-execution/4.09-eval-prem.watsup.
;;
;; A premise evaluates in place, updates the innermost IN's layer, and reduces
;; to OK; a premise sequence is a statement sequence (seq). Eval_prem/error
;; has no rule: a premise that errs is stuck, which is ERROR.

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "../2-runtime/2.1-variable.rkt"
         "3.02-context.rkt"
         "3.03-machine.rkt")
(provide ->term/eval-prem
         ->term+ctx/eval-prem)

;; The rules on a term
(define ->term/eval-prem
  (reduction-relation
   al

   ;;; Relation premises

   ;; rulegroup Eval_prem/relpr, once the inputs are evaluated
   (--> (REL id (val ...) (exp_out ...))
        (eval-prem/relpr (call-rel id (val ...)) (exp_out ...))
        "eval-prem/relpr")
   (--> (eval-prem/relpr (OK (val_out ...)) (exp_out ...))
        (assign-exps (exp_out ...) (val_out ...))
        "eval-prem/relpr/succ")

   ;;; If premises

   ;; rulegroup Eval_prem/ifpr
   (--> (IF (BOOL #t)) OK
        "eval-prem/ifpr/succ-true")
   (--> (IF (BOOL #f)) UNMATCH
        "eval-prem/ifpr/succ-false")

   ;;; If-hold and if-not-hold premises

   ;; rulegroup Eval_prem/ifholdpr, once the inputs are evaluated
   (--> (IFHOLD id (val_input ...))
        (eval-prem/ifholdpr (call-rel id (val_input ...)))
        "eval-prem/ifholdpr")
   (--> (eval-prem/ifholdpr (OK ())) OK
        "eval-prem/ifholdpr/succ")

   ;; rulegroup Eval_prem/ifnotholdpr, once the inputs are evaluated
   (--> (IFNOTHOLD id (val_input ...))
        (eval-prem/ifnotholdpr (call-rel id (val_input ...)))
        "eval-prem/ifnotholdpr")
   (--> (eval-prem/ifnotholdpr UNMATCH) OK
        "eval-prem/ifnotholdpr/succ-ok")
   (--> (eval-prem/ifnotholdpr (OK (val_out ...))) UNMATCH
        "eval-prem/ifnotholdpr/succ-unmatch")

   ;;; Let premises

   ;; rulegroup Eval_prem/letpr
   (--> (LET exp_l val_r) (assign-exp exp_l val_r)
        "eval-prem/letpr")

   ;;; Debug premises

   ;; rulegroup Eval_prem/dbg
   (--> (DEBUG val) OK
        (where _ ,(debug (term val)))
        "eval-prem/dbg")))

;; The rules on a term and its context (r G L)
(define ->term+ctx/eval-prem
  (reduction-relation
   al

   ;;; Iteration premises - optional

   ;; rulegroup Eval_prem/iterpr-opt
   (--> ((ITER prem (QUEST (vari_bound ...) (vari_bind ...))) G L)
        (OK G (add-varis L ((iter-vari vari_bind QUEST) ...)
                         (repeat- (OPT ()) ,(length (term (vari_bind ...))))))
        (where (OK ()) (sub-opt L (vari_bound ...)))
        "eval-prem/iterpr-opt/succ-none")
   (--> ((ITER prem (QUEST (vari_bound ...) (vari_bind ...))) G L)
        ((eval-prem/iterpr-opt/some (IN L_sub prem) (vari_bind ...)) G L)
        (where (OK (L_sub)) (sub-opt L (vari_bound ...)))
        "eval-prem/iterpr-opt/some")

   ;; Eval_prem/succ-some, once prem has updated C_sub to C_sub_res
   (--> ((eval-prem/iterpr-opt/some (IN L_sub_res OK) (vari_bind ...)) G L)
        (OK G (add-varis L ((iter-vari vari_bind QUEST) ...) ((OPT (val_bind)) ...)))
        (where ((val_bind ...)) (find-varis L_sub_res (vari_bind ...)))
        "eval-prem/iterpr-opt/succ-some")

   ;;; Iteration premises - list

   ;; rulegroup Eval_prem/iterpr-list
   (--> ((ITER prem (STAR (vari_bound ...) (vari_bind ...))) G L)
        (OK G (add-varis L ((iter-vari vari_bind STAR) ...)
                         (repeat- (LIST ()) ,(length (term (vari_bind ...))))))
        (where (OK ()) (sub-list L (vari_bound ...)))
        "eval-prem/iterpr-list/succ-empty")
   (--> ((ITER prem (STAR (vari_bound ...) (vari_bind ...))) G L)
        ((eval-prem/iterpr-list/list ((IN L_sub prem) ...) (vari_bind ...)) G L)
        (where (OK (L_sub_h L_sub_t ...)) (sub-list L (vari_bound ...)))
        (where (L_sub ...) (L_sub_h L_sub_t ...))
        "eval-prem/iterpr-list/list")

   ;; Eval_prem/succ-list, once prem has updated each C_sub to C_sub_res
   (--> ((eval-prem/iterpr-list/list ((IN L_sub_res OK) ...) (vari_bind ...)) G L)
        (OK G (add-varis L ((iter-vari vari_bind STAR) ...) ((LIST (val_bind ...)) ...)))
        (where (((val_sub_bind ...)) ...) ((find-varis L_sub_res (vari_bind ...)) ...))
        (where ((val_bind ...) ...) (transpose- ((val_sub_bind ...) ...)))
        "eval-prem/iterpr-list/succ-list")))
