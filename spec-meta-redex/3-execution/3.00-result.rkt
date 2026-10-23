#lang racket/base
;; spec-meta/3-execution/3.00-result.watsup.
;;
;; Redex has no parametric nonterminals, so each ok<X> is written out where
;; it is used.

(require redex/reduction-semantics
         "../2-runtime/2.2-type-def.rkt")
(provide result)

(define-extended-language result type-def
  ;; Recoverable mismatches and execution errors
  (failure ::= UNMATCH ERROR)
  (unitRes ::= OK ERROR))
