#lang racket/base
;; spec-meta/common/0-stdlib.watsup.

(require "0.0-prelude.rkt")
(provide stdlib)

(define-language stdlib
  ;; Metavariables for int, nat, bool, and text
  (bool b ::= boolean)
  (int i ::= integer)
  (nat n ::= natural)
  (text t ::= string)

  ;; Sets and maps, as lists and association lists
  (set ::= (any ...))
  (pair ::= (any any))
  (map ::= (pair ...)))
