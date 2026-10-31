#lang racket/base
;; spec-meta/3-execution/3.01-env.watsup.

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
