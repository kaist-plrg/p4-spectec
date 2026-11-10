#lang racket/base
;; Mirrors spec-meta/3-execution/3.01-context.watsup.

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "../2-runtime/2.1-variable.rkt"
         "3.01-env.rkt")
(provide context
         empty-localLayer
         empty-ctx
         load
         global-map
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

  ;; The global layer
  (G ::= any)

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

;; $load: GLOBAL with the entry of every definition added, in spec order
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
;; The global layer
;;

(define-metafunction context
  global-map : G any -> map
  [(global-map {TYP any_typ REL any_rel FUNC any_func} TYP) any_typ]
  [(global-map {TYP any_typ REL any_rel FUNC any_func} REL) any_rel]
  [(global-map {TYP any_typ REL any_rel FUNC any_func} FUNC) any_func])

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

;; $find_typ
(define-metafunction context
  find-typdef : G tdenv id -> () ∨ (typDef)
  [(find-typdef G tdenv id) (typDef)
   (where (typDef) (find-map tdenv id))]
  ;; otherwise
  [(find-typdef G tdenv id) (find-map (global-map G TYP) id)])

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
  [(find-func G id) (find-map (global-map G FUNC) id)])

;;; Relation finders

(define-metafunction context
  find-rel : G id -> () ∨ (relDefn)
  [(find-rel G id) (find-map (global-map G REL) id)])

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
