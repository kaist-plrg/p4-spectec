#lang racket/base
;; spec-meta/al/3-context.watsup.
;;
;; A dec over ctx takes the layers it reads: G L if it reads C.GLOBAL, and L
;; alone otherwise. A ctx result is the new L. $load and $empty_ctx keep ctx.
;; A record update C[ .LOCAL.VAL = x ] is a pattern that rebuilds the layer.

(require "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/2-env.rkt"
         "2-env.rkt")
(provide al-context
         empty-layer
         empty-ctx
         load
         add-vari
         add-varr
         add-varis
         add-varrs
         add-typ
         add-func
         find-vari
         find-varr
         find-varis
         find-varrs
         finds-vari
         find-typ
         find-func
         find-rel
         sub-opt
         sub-list)

(define-extended-language al-context al-env
  ;; Context
  (layer G L ::= {TYP tdenv REL renv FUNC fenv VAL venv})
  (ctx C ::= {GLOBAL layer LOCAL layer}))

(define-dec al-context
  empty-layer : -> layer
  [(empty-layer) {TYP tdenv REL renv FUNC fenv VAL venv}
   (where tdenv (empty-map))
   (where renv (empty-map))
   (where fenv (empty-map))
   (where venv (empty-map))])

(define-dec al-context
  empty-ctx : -> ctx
  [(empty-ctx) {GLOBAL layer_g LOCAL layer_l}
   (where layer_g (empty-layer))
   (where layer_l (empty-layer))])

;;
;; Loading context from a script
;;

;; The entries that a definition adds to GLOBAL's TYP, REL, and FUNC, one
;; clause per $load clause
(define-metafunction al-context
  load-defn : defn -> (tdenv renv fenv)
  [(load-defn (EXTTYP id)) (((id EXT)) () ())]
  [(load-defn (TYP id (tparam ...) deftyp)) (((id (DEF (tparam ...) deftyp))) () ())]
  [(load-defn (EXTREL id (typ_input ...) (typ_output ...))) (() ((id (EXT id))) ())]
  [(load-defn (REL id (typ_input ...) (typ_output ...) (rulgroup ...) (elsgroup ...)))
   (() ((id (DEF (rulgroup ...) (elsgroup ...)))) ())]
  [(load-defn (EXTFUNC id (tparam ...) (param ...) typ)) (() () ((id (EXT id))))]
  [(load-defn (BUILTINFUNC id (tparam ...) (param ...) typ))
   (() () ((id (BUILTIN id (tparam ...) (param ...)))))]
  [(load-defn (TABLEFUNC id (param ...) typ (tblrow ...)))
   (() () ((id (TABLE (param ...) (tblrow ...)))))]
  [(load-defn (FUNC id (tparam ...) (param ...) typ (clause ...) (elsclause ...)))
   (() () ((id (DEF (tparam ...) (clause ...) (elsclause ...)))))])

;; $load: GLOBAL with the entries of every definition added, in script order.
;; $adds_map adds its keys in order, as $load's recursion does.
(define-metafunction al-context
  load : ctx script -> ctx
  [(load {GLOBAL {TYP tdenv REL renv FUNC fenv VAL venv} LOCAL L} (defn ...))
   {GLOBAL {TYP (adds-map tdenv (id_t ... ...) (typdef ... ...))
            REL (adds-map renv (id_r ... ...) (reldef ... ...))
            FUNC (adds-map fenv (id_f ... ...) (funcdef ... ...))
            VAL venv}
    LOCAL L}
   (where ((((id_t typdef) ...) ((id_r reldef) ...) ((id_f funcdef) ...)) ...)
          ((load-defn defn) ...))])

;;
;; Adders
;;

;;; Value adders

(define-dec al-context
  add-vari : L vari val -> L
  [(add-vari {TYP tdenv REL renv FUNC fenv VAL venv} (id typ (iter ...)) val)
   {TYP tdenv REL renv FUNC fenv VAL venv_update}
   (where venv_update (add-map venv (id (iter ...)) val))])

(define-dec al-context
  add-varr : L varr val -> L
  [(add-varr {TYP tdenv REL renv FUNC fenv VAL venv} varr val)
   {TYP tdenv REL renv FUNC fenv VAL venv_update}
   (where venv_update (add-map venv varr val))])

(define-dec al-context
  add-varis : L (vari ...) (val ...) -> L
  [(add-varis {TYP tdenv REL renv FUNC fenv VAL venv} ((id typ (iter ...)) ...) (val ...))
   {TYP tdenv REL renv FUNC fenv VAL venv_update}
   (where venv_update (adds-map venv ((id (iter ...)) ...) (val ...)))])

(define-dec al-context
  add-varrs : L (varr ...) (val ...) -> L
  [(add-varrs {TYP tdenv REL renv FUNC fenv VAL venv} (varr ...) (val ...))
   {TYP tdenv REL renv FUNC fenv VAL venv_update}
   (where venv_update (adds-map venv (varr ...) (val ...)))])

;;; Typedef adders

(define-dec al-context
  add-typ : L id typdef -> L
  [(add-typ {TYP tdenv REL renv FUNC fenv VAL venv} id typdef)
   {TYP tdenv_update REL renv FUNC fenv VAL venv}
   (where tdenv_update (add-map tdenv id typdef))])

;;; Function adders

(define-dec al-context
  add-func : L id funcdef -> L
  [(add-func {TYP tdenv REL renv FUNC fenv VAL venv} id funcdef)
   {TYP tdenv REL renv FUNC fenv_update VAL venv}
   (where fenv_update (add-map fenv id funcdef))])

;;
;; Finders
;;

;;; Value finders

;; No clause for an unbound variable: ⊥, never ().
(define-dec al-context
  find-vari : L vari -> () ∨ (val)
  [(find-vari {TYP tdenv REL renv FUNC fenv VAL venv} (id typ (iter ...))) (val)
   (where (val) (find-map venv (id (iter ...))))])

(define-dec al-context
  find-varr : L varr -> () ∨ (val)
  [(find-varr {TYP tdenv REL renv FUNC fenv VAL venv} varr) (find-map venv varr)])

(define-dec al-context
  find-varis : L (vari ...) -> () ∨ ((val ...))
  [(find-varis L (vari ...)) ((val ...))
   (where ((val) ...) ((find-vari L vari) ...))]
  [(find-varis L (vari ...)) ()
   ;; otherwise: some lookup fails
   (side-condition
    (not (redex-match? al-context ((val) ...) (term ((find-vari L vari) ...)))))])

(define-dec al-context
  find-varrs : L (varr ...) -> () ∨ ((val ...))
  [(find-varrs {TYP tdenv REL renv FUNC fenv VAL venv} (varr ...)) ((val ...))
   (where ((val) ...) ((find-map venv varr) ...))]
  [(find-varrs {TYP tdenv REL renv FUNC fenv VAL venv} (varr ...)) ()
   ;; otherwise: some lookup fails
   (side-condition
    (not (redex-match? al-context ((val) ...) (term ((find-map venv varr) ...)))))])

(define-dec al-context
  finds-vari : (L ...) vari -> () ∨ ((val ...))
  [(finds-vari ({TYP tdenv REL renv FUNC fenv VAL venv} ...) (id typ (iter ...))) ((val ...))
   (where ((val) ...) ((find-map venv (id (iter ...))) ...))]
  [(finds-vari ({TYP tdenv REL renv FUNC fenv VAL venv} ...) (id typ (iter ...))) ()
   ;; otherwise: some lookup fails
   (side-condition
    (not (redex-match? al-context ((val) ...)
                       (term ((find-map venv (id (iter ...))) ...)))))])

;;; Typedef finders

(define-dec al-context
  find-typ : G L id -> () ∨ (typdef)
  [(find-typ G {TYP tdenv_l REL renv_l FUNC fenv_l VAL venv_l} id) (typdef)
   (where (typdef) (find-map tdenv_l id))]
  [(find-typ {TYP tdenv_g REL renv_g FUNC fenv_g VAL venv_g}
             {TYP tdenv_l REL renv_l FUNC fenv_l VAL venv_l}
             id)
   (find-map tdenv_g id)
   (where () (find-map tdenv_l id))])

;;; Function finders

(define-dec al-context
  find-func : G L id -> () ∨ (funcdef)
  [(find-func G {TYP tdenv_l REL renv_l FUNC fenv_l VAL venv_l} id) (funcdef)
   (where (funcdef) (find-map fenv_l id))]
  [(find-func {TYP tdenv_g REL renv_g FUNC fenv_g VAL venv_g}
              {TYP tdenv_l REL renv_l FUNC fenv_l VAL venv_l}
              id)
   (funcdef)
   (where () (find-map fenv_l id))
   (where (funcdef) (find-map fenv_g id))]
  [(find-func {TYP tdenv_g REL renv_g FUNC fenv_g VAL venv_g}
              {TYP tdenv_l REL renv_l FUNC fenv_l VAL venv_l}
              id)
   ()
   ;; otherwise: found in neither layer
   (where () (find-map fenv_l id))
   (where () (find-map fenv_g id))])

;;; Relation finders

(define-dec al-context
  find-rel : G L id -> () ∨ (reldef)
  [(find-rel {TYP tdenv_g REL renv_g FUNC fenv_g VAL venv_g} L id) (find-map renv_g id)])

;;
;; Sub-context construction for iteration
;;

(define-dec al-context
  sub-opt : L (vari ...) -> () ∨ (L)
  [(sub-opt L (vari ...)) (L_1)
   (where (vari_iter ...) ((iter-vari vari QUEST) ...))
   (where (((OPT (val))) ...) ((find-vari L vari_iter) ...))
   (where L_1 (add-varis L (vari ...) (val ...)))]
  [(sub-opt L (vari ...)) ()
   ;; An empty vari* matches both clauses, and watsup takes the first.
   (side-condition (pair? (term (vari ...))))
   (where (vari_iter ...) ((iter-vari vari QUEST) ...))
   (where (((OPT ())) ...) ((find-vari L vari_iter) ...))])

(define-dec al-context
  sub-list : L (vari ...) -> (L ...)
  [(sub-list L (vari ...)) (L_1 ...)
   (where (vari_iter ...) ((iter-vari vari STAR) ...))
   (where (((LIST (val ...))) ...) ((find-vari L vari_iter) ...))
   (where ((val_trans ...) ...) (transpose- ((val ...) ...)))
   (where (L_1 ...) ((add-varis L (vari ...) (val_trans ...)) ...))])
