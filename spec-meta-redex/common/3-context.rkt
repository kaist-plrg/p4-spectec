#lang racket/base
;; spec-meta/common/3-context.watsup.

(require "0-prelude.rkt"
         "2-env.rkt")
(provide Common-context)

(define-extended-language Common-context Common-env
  ;; Cursor
  (cursor ::= GLOBAL LOCAL))
