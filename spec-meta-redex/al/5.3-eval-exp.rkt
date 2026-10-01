#lang racket/base
;; spec-meta/al/5.3-eval-exp.watsup.
;;
;; An exp evaluates in place: each evaluated subterm is replaced by its result.
;; Eval_exp/fail becomes a complement rule next to each rulegroup it covers.

(require "../common/0.0-prelude.rkt"
         "../common/5.1-eval-ops.rkt"
         "3-context.rkt"
         "4-relation.rkt")
(provide ->redex/eval-exp
         ->ctx/eval-exp)

;; The rules on the redex
(define ->redex/eval-exp
  (reduction-relation
   al

   ;;; Boolean, number, and text evaluation rules

   ;; rulegroup Eval_exp/literal
   (--> (BOOL b) (OK (BOOL b))
        "eval-exp/literal/boolean")
   (--> num (OK num)
        "eval-exp/literal/number")
   (--> (TEXT t) (OK (TEXT t))
        "eval-exp/literal/string")

   ;;; Unary, binary, and comparison evaluation rules

   ;; rulegroup Eval_exp/unary
   (--> (UN NOT (OK (BOOL b))) (OK (BOOL b_res))
        (where b_res ,(not (term b)))
        "eval-exp/unary/boolean")
   (--> (UN numunop (OK num)) (OK val_res)
        (where val_res (unop-number numunop num))
        "eval-exp/unary/number")
   (--> (UN unop (OK val)) FAIL
        ;; otherwise
        (side-condition (not (redex-match? al (NOT (OK (BOOL b))) (term (unop (OK val))))))
        (side-condition (not (redex-match? al (numunop (OK num)) (term (unop (OK val))))))
        "eval-exp/unary/fail")

   ;;; Tuple evaluation rules

   ;; rule Eval_exp/tuple
   (--> (TUP ((OK val) ...)) (OK (TUP (val ...)))
        "eval-exp/tuple")))

;; The rules on the focus triple (r G L)
(define ->ctx/eval-exp
  (reduction-relation
   al

   ;;; Variable evaluation rules

   ;; rule Eval_exp/variable
   (--> ((VAR id) G L) ((OK val) G L)
        (where (val) (find-varr L (id ())))
        "eval-exp/variable")
   (--> ((VAR id) G L) (FAIL G L)
        ;; otherwise
        (where () (find-varr L (id ())))
        "eval-exp/variable/fail")))
