#lang racket/base
;; spec-meta/al/4-relation.watsup.
;;
;; The relations it declares are defined in 5-eval.rkt.

(require "../common/0.0-prelude.rkt"
         "3-context.rkt")
(provide al
         cons-valsres
         cons-ctxsres)

(define-extended-language al al-context
  ;; Result to represent backtracking in evaluation
  (ctxres ::= (OK ctx) FAIL)

  ;; res<ctx*>, for an iterated Eval_prem premise
  (ctxsres ::= (OK (ctx ...)) FAIL))

;; A head prepended to a res<X*>, for the sequence judgments

(define-dec al
  cons-valsres : val valsres -> valsres
  [(cons-valsres val_h (OK (val_t ...))) (OK (val_h val_t ...))]
  [(cons-valsres val_h FAIL) FAIL])

(define-dec al
  cons-ctxsres : ctx ctxsres -> ctxsres
  [(cons-ctxsres C_h (OK (C_t ...))) (OK (C_h C_t ...))]
  [(cons-ctxsres C_h FAIL) FAIL])
