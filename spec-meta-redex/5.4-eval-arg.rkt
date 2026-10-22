#lang racket/base
;; spec-meta/5.4-eval-arg.watsup.
;;
;; An arg evaluates in place, as an exp does. Eval_arg/fail is "frame/fail"
;; on (EXP hole): no other input fails.

(require redex/reduction-semantics
         "4-relation.rkt")
(provide ->term/eval-arg)

;; The rules on a term
(define ->term/eval-arg
  (reduction-relation
   al

   ;;; Argument evaluation rules

   ;; rule Eval_arg/exp
   (--> (EXP val) val
        "eval-arg/exp")

   ;; rule Eval_arg/fun
   (--> (FUN id) (FUNC id)
        "eval-arg/fun")))
