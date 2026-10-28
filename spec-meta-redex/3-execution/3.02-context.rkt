#lang racket/base
;; Mirrors spec-meta/4-execution/4.01-context.watsup.
;;
;; A dec over ctx takes G L if it reads C.GLOBAL, and L alone otherwise. L is
;; the local layer. The global layer never changes after $load, so it stays in
;; a registry, and G is its handle. A ctx result is the new L. $load and
;; $empty_ctx keep ctx. A record update C[ .LOCAL.VAL = x ] is a pattern that
;; rebuilds the layer.

(require racket/match
         redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "../2-runtime/2.1-variable.rkt"
         "3.01-env.rkt")
(provide context
         empty-localLayer
         empty-ctx
         load
         register-global!
         global-ids
         add-varr
         add-varis
         add-func
         find-vari
         find-varr
         find-varis
         finds-vari
         find-typdef
         find-typ
         find-func-param
         resolve-func-param
         find-func
         find-rel
         sub-opt
         sub-list)

(define-extended-language context env
  ;; Context
  (globalLayer ::= {TYP tdenv REL renv FUNC fenv})
  (localLayer L ::= {TYP tdenv FUNC fnenv VAL venv})
  (ctx C ::= {GLOBAL globalLayer LOCAL localLayer})

  ;; The handle of a registered global layer
  (G ::= (GLOBAL natural))

  ;; Sub-contexts for iteration, as their local layers
  (subOptRes ::= (OK ()) (OK (L)) ERROR)
  (subListRes ::= (OK (L ...)) ERROR))

(define-metafunction context
  empty-globalLayer : -> globalLayer
  [(empty-globalLayer) {TYP (empty-map) REL (empty-map) FUNC (empty-map)}])

(define-metafunction context
  empty-localLayer : -> localLayer
  [(empty-localLayer) {TYP (empty-map) FUNC (empty-map) VAL (empty-map)}])

(define-metafunction context
  empty-ctx : -> ctx
  [(empty-ctx) {GLOBAL (empty-globalLayer) LOCAL (empty-localLayer)}])

;;
;; Loading context from a spec
;;

;; The entry that a definition adds to GLOBAL's TYP, REL, or FUNC: the
;; definition as written, one clause per $load clause
(define-metafunction context
  load-defn : defn -> (tdenv renv fenv)
  [(load-defn externTypDefn) (((id externTypDefn)) () ())
   (where (EXTTYP id) externTypDefn)]
  [(load-defn definedTypDefn) (((id definedTypDefn)) () ())
   (where (TYP id (tparam ...) deftyp) definedTypDefn)]
  [(load-defn externRelDefn) (() ((id externRelDefn)) ())
   (where (EXTREL id (typ_input ...) (typ_output ...)) externRelDefn)]
  [(load-defn definedRelDefn) (() ((id definedRelDefn)) ())
   (where (REL id (typ_input ...) (typ_output ...) (rulGroup ...) (elsGroup ...)) definedRelDefn)]
  [(load-defn externFuncDefn) (() () ((id externFuncDefn)))
   (where (EXTFUNC id (tparam ...) (param ...) typ) externFuncDefn)]
  [(load-defn builtinFuncDefn) (() () ((id builtinFuncDefn)))
   (where (BUILTINFUNC id (tparam ...) (param ...) typ) builtinFuncDefn)]
  [(load-defn tableFuncDefn) (() () ((id tableFuncDefn)))
   (where (TABLEFUNC id (param ...) typ (tblRow ...)) tableFuncDefn)]
  [(load-defn definedFuncDefn) (() () ((id definedFuncDefn)))
   (where (FUNC id (tparam ...) (param ...) typ (clause ...) (elsClause ...)) definedFuncDefn)])

;; $load: GLOBAL with the entry of every definition added, in spec order.
;; $adds_map adds its keys in order, as $load's recursion does.
(define-metafunction context
  load : ctx spec -> ctx
  [(load {GLOBAL {TYP tdenv REL renv FUNC fenv} LOCAL L} (defn ...))
   {GLOBAL {TYP (adds-map tdenv (id_t ... ...) (typDef ... ...))
            REL (adds-map renv (id_r ... ...) (relDefn ... ...))
            FUNC (adds-map fenv (id_f ... ...) (funcDefn ... ...))}
    LOCAL L}
   (where ((((id_t typDef) ...) ((id_r relDefn) ...) ((id_f funcDefn) ...)) ...)
          ((load-defn defn) ...))])

;;
;; The global layer, by handle
;;

;; The registered global layers, by number. An entry never changes once
;; added, so a handle denotes the same layer for good, and a metafunction that
;; reads the layer through its handle stays a function of its arguments.
(define global-layers (make-hasheqv))

;; The handle of a global layer, which is checked and added to the registry.
;; Only 3.16-entry.rkt calls this, after $load.
(define (register-global! layer_global)
  (unless (redex-match? context globalLayer layer_global)
    (error 'register-global! "not a global layer: ~e" layer_global))
  (define n (hash-count global-layers))
  (hash-set! global-layers n layer_global)
  (term (GLOBAL ,n)))

;; The map of the global layer G under the label field (TYP, REL, or FUNC).
;; The layer is taken apart in Racket, so neither a term nor a pattern holds
;; it.
(define (global-map G field)
  (match-define (list 'GLOBAL n) G)
  (match-define (list 'TYP tdenv 'REL renv 'FUNC fenv)
    (hash-ref global-layers n (λ () (error 'global-map "no global layer ~a" n))))
  (case field [(TYP) tdenv] [(REL) renv] [(FUNC) fenv]))

;; The value of id in that map, as () or (v)
(define (find-global G field id)
  (map-ref (global-map G field) id))

;; The keys of that map
(define (global-ids G field)
  (map car (global-map G field)))

;;
;; Adders
;;

;;; Value adders

(define-metafunction context
  add-varr : L varr val -> L
  [(add-varr {TYP tdenv FUNC fnenv VAL venv} varr val)
   {TYP tdenv FUNC fnenv VAL (add-map venv varr val)}])

(define-metafunction context
  add-varis : L (vari ...) (val ...) -> L
  [(add-varis {TYP tdenv FUNC fnenv VAL venv} ((id typ (iter ...)) ...) (val ...))
   {TYP tdenv FUNC fnenv VAL (adds-map venv ((id (iter ...)) ...) (val ...))}])

;;; Function adders

(define-metafunction context
  add-func : L id id -> L
  [(add-func {TYP tdenv FUNC fnenv VAL venv} id id_global)
   {TYP tdenv FUNC (add-map fnenv id id_global) VAL venv}])

;;
;; Finders
;;

;;; Value finders

(define-metafunction context
  find-vari : L vari -> () ∨ (val)
  [(find-vari {TYP tdenv FUNC fnenv VAL venv} (id typ (iter ...))) (find-map venv (id (iter ...)))])

(define-metafunction context
  find-varr : L varr -> () ∨ (val)
  [(find-varr {TYP tdenv FUNC fnenv VAL venv} varr) (find-map venv varr)])

(define-metafunction context
  find-varis : L (vari ...) -> () ∨ ((val ...))
  [(find-varis L (vari ...)) ((val ...))
   (where ((val) ...) ((find-vari L vari) ...))]
  ;; otherwise
  [(find-varis L (vari ...)) ()])

(define-metafunction context
  finds-vari : (L ...) vari -> () ∨ ((val ...))
  [(finds-vari (L ...) vari) ((val ...))
   (where ((val) ...) ((find-vari L vari) ...))]
  ;; otherwise
  [(finds-vari (L ...) vari) ()])

;;; Typedef finders

;; $find_typ with the local tdenv on its own, for the callers that hold it
;; apart from L: the definition of id in tdenv, or else in the global layer G
(define-metafunction context
  find-typdef : G tdenv id -> () ∨ (typDef)
  [(find-typdef G tdenv id) (typDef)
   (where (typDef) (find-map tdenv id))]
  ;; otherwise
  [(find-typdef G tdenv id) ,(find-global (term G) 'TYP (term id))])

(define-metafunction context
  find-typ : G L id -> () ∨ (typDef)
  [(find-typ G {TYP tdenv FUNC fnenv VAL venv} id) (find-typdef G tdenv id)])

;;; Function finders

(define-metafunction context
  find-func-param : L id -> () ∨ (id)
  [(find-func-param {TYP tdenv FUNC fnenv VAL venv} id) (find-map fnenv id)])

(define-metafunction context
  resolve-func-param : L id -> id
  [(resolve-func-param L id) id_global
   (where (id_global) (find-func-param L id))]
  ;; otherwise
  [(resolve-func-param L id) id])

(define-metafunction context
  find-func : G id -> () ∨ (funcDefn)
  [(find-func G id) ,(find-global (term G) 'FUNC (term id))])

;;; Relation finders

(define-metafunction context
  find-rel : G id -> () ∨ (relDefn)
  [(find-rel G id) ,(find-global (term G) 'REL (term id))])

;;
;; Sub-context construction for iteration
;;

(define-metafunction context
  sub-opt : L (vari ...) -> subOptRes
  [(sub-opt L ()) ERROR]
  [(sub-opt L (vari_h vari_t ...)) (OK ((add-varis L (vari ...) (val ...))))
   (where (vari ...) (vari_h vari_t ...))
   (where (((OPT (val))) ...) ((find-vari L (iter-vari vari QUEST)) ...))]
  [(sub-opt L (vari_h vari_t ...)) (OK ())
   (where (vari ...) (vari_h vari_t ...))
   (where (((OPT ())) ...) ((find-vari L (iter-vari vari QUEST)) ...))]
  ;; otherwise
  [(sub-opt L (vari ...)) ERROR])

(define-metafunction context
  sub-list : L (vari ...) -> subListRes
  [(sub-list L (vari ...)) (OK ((add-varis L (vari ...) (val_trans ...)) ...))
   (where (((LIST (val ...))) ...) ((find-vari L (iter-vari vari STAR)) ...))
   ;; $forall_((|val*| = |val**[0]|)*)
   (side-condition (let ([lengths (map length (term ((val ...) ...)))])
                     (or (null? lengths) (apply = lengths))))
   (where ((val_trans ...) ...) (transpose- ((val ...) ...)))]
  ;; otherwise
  [(sub-list L (vari ...)) ERROR])
