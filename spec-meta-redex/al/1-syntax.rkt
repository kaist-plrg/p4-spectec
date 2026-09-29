#lang racket/base
;; spec-meta/al/1-syntax.watsup.

(require "../common/0-prelude.rkt"
         "../common/1-syntax.rkt")
(provide AL-syntax)

(define-extended-language AL-syntax Common
  ;; Parameters
  (param ::=
         (EXP typ)
         (FUN id (tparam ...) (param ...) typ))

  ;; Premises
  (iterprem ::= (iter (vari ...) (vari ...)))
  (prem ::=
        (REL id (exp ...) (exp ...))
        (IF exp)
        (IFHOLD id (exp ...))
        (IFNOTHOLD id (exp ...))
        (LET exp exp)
        (ITER prem iterprem)
        (DEBUG exp))

  ;; Definitions
  (rulmatch ::= ((exp ...) (prem ...)))
  (rulpath ::= (id (exp ...) (prem ...)))
  (rulgroup ::= (id rulmatch (rulpath ...)))
  (elsgroup ::= (id rulmatch rulpath))

  (clause ::= ((arg ...) exp (prem ...)))
  (elsclause ::= clause)

  (tblrow ::= ((arg ...) exp (prem ...)))

  (defn ::=
        (EXTTYP id)
        (TYP id (tparam ...) deftyp)
        (EXTREL id (typ ...) (typ ...))
        (REL id (typ ...) (typ ...) (rulgroup ...) ())
        (REL id (typ ...) (typ ...) (rulgroup ...) (elsgroup))
        (EXTFUNC id (tparam ...) (param ...) typ)
        (BUILTINFUNC id (tparam ...) (param ...) typ)
        (TABLEFUNC id (param ...) typ (tblrow ...))
        (FUNC id (tparam ...) (param ...) typ (clause ...) ())
        (FUNC id (tparam ...) (param ...) typ (clause ...) (elsclause)))

  ;; Scripts
  (script ::= (defn ...)))
