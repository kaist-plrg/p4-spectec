#lang racket/base
;; spec-meta/common/3-context.watsup.

(require redex/reduction-semantics
         "2-env.rkt")
(provide common-context)

(define-extended-language common-context common-env
  ;; Cursor
  (cursor ::= GLOBAL LOCAL))
