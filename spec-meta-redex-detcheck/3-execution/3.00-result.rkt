#lang racket/base
;; Mirrors spec-meta/2-runtime/2.3-result.watsup.

(require redex/reduction-semantics
         "../2-runtime/2.2-type-def.rkt")
(provide result)

(define-extended-language result type-def
  (failure ::= UNMATCH ERROR)
  (unitRes ::= OK ERROR))
