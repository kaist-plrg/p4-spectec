#lang racket/base
;; spec-meta/2-runtime/2.2-type-def.watsup.

(require redex/reduction-semantics
         "2.1-variable.rkt")
(provide type-def)

(define-extended-language type-def variable
  ;; Runtime type definitions
  (typDef ::= typDefn PARAM))
