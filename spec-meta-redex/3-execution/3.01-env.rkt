#lang racket/base
;; spec-meta/3-execution/3.01-env.watsup.
;;
;; typdef is the port's runtime type definition until $load stores
;; definitions as written (typDef of 2.2-type-def.rkt).

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "../2-runtime/2.2-type-def.rkt")
(provide env
         theta-of-tdenv)

(define-extended-language env type-def
  ;; Value environment
  (venv ::= ((varr val) ...))

  ;; Type definition environment
  (typdef ::=
          PARAM
          EXT
          (DEF (tparam ...) deftyp))
  (tdenv ::= ((id typdef) ...))

  ;; Type substitution environment
  (theta ::= ((id typ) ...))

  ;; Relation environment
  (externRelDef ::= (EXT id))
  (definedRelDef ::=
                 (DEF (rulGroup ...) ())
                 (DEF (rulGroup ...) (elsGroup)))
  (reldef ::= externRelDef definedRelDef)
  (renv ::= ((id reldef) ...))

  ;; Function environment
  (externFuncDef ::= (EXT id))
  (builtinFuncDef ::= (BUILTIN id (tparam ...) (param ...)))
  (tableFuncDef ::= (TABLE (param ...) (tblRow ...)))
  (definedFuncDef ::=
                  (DEF (tparam ...) (clause ...) ())
                  (DEF (tparam ...) (clause ...) (elsClause)))
  (funcdef ::= externFuncDef builtinFuncDef tableFuncDef definedFuncDef)
  (fenv ::= ((id funcdef) ...)))

(define-judgment-form env
  #:mode (theta-of-tdenv I O)
  #:contract (theta-of-tdenv tdenv theta)
  [(theta-of-tdenv () (empty-map))]
  [(theta-of-tdenv ((id_t typdef_t) ...) theta_t)
   ----------------------------------------
   (theta-of-tdenv ((id_h (DEF () (ALIAS typ_h))) (id_t typdef_t) ...)
                   (add-map theta_t id_h typ_h))]
  [(theta-of-tdenv ((id_t typdef_t) ...) theta_t)
   ----------------------------------------
   (theta-of-tdenv ((id_h PARAM) (id_t typdef_t) ...) theta_t)]
  [(theta-of-tdenv ((id_t typdef_t) ...) theta_t)
   ----------------------------------------
   (theta-of-tdenv ((id_h EXT) (id_t typdef_t) ...) theta_t)])
