#lang racket/base
;; Mirrors spec-meta/4-execution/4.00-env.watsup.

(require redex/reduction-semantics
         "3.00-result.rkt")
(provide env)

(define-extended-language env result
  ;; Value environment
  (venv ::= ((varr val) ...))

  ;; Type definition environment
  (tdenv ::= ((id typDef) ...))

  ;; Relation environment
  (renv ::= ((id relDefn) ...))

  ;; Function environment
  (fenv ::= ((id funcDefn) ...))
  (fnenv ::= ((id id) ...)))
