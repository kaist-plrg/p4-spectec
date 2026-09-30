#lang racket/base
;; spec-meta/common/3-context.watsup.

(require "0.0-prelude.rkt"
         "2-env.rkt")
(provide common-context)

(define-extended-language common-context common-env
  ;; Cursor
  (cursor ::= GLOBAL LOCAL))
