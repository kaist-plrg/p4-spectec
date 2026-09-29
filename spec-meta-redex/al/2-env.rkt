#lang racket/base
;; spec-meta/al/2-env.watsup.

(require "../common/0.0-prelude.rkt"
         "../common/4-relation.rkt"
         "1-syntax.rkt")
(provide AL-base
         AL-env)

;; The common languages and the AL syntax
(define-union-language AL-base Common-relation AL-syntax)

(define-extended-language AL-env AL-base
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
