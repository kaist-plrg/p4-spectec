#lang racket/base
;; spec-meta/al/2-env.watsup.

(require "../common/0.0-prelude.rkt"
         "../common/4-relation.rkt"
         "1-syntax.rkt")
(provide al-base
         al-env)

;; The common languages and the AL syntax
(define-union-language al-base common-relation al-syntax)

(define-extended-language al-env al-base
  ;; Relation environment
  (externRelDef ::= (EXT id))
  (definedRelDef ::=
                 (DEF (rulgroup ...) ())
                 (DEF (rulgroup ...) (elsgroup)))
  (reldef ::= externRelDef definedRelDef)
  (renv ::= ((id reldef) ...))

  ;; Function environment
  (externFuncDef ::= (EXT id))
  (builtinFuncDef ::= (BUILTIN id (tparam ...) (param ...)))
  (tableFuncDef ::= (TABLE (param ...) (tblrow ...)))
  (definedFuncDef ::=
                  (DEF (tparam ...) (clause ...) ())
                  (DEF (tparam ...) (clause ...) (elsclause)))
  (funcdef ::= externFuncDef builtinFuncDef tableFuncDef definedFuncDef)
  (fenv ::= ((id funcdef) ...)))
