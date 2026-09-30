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
         load-typdef
         load-reldef
         load-funcdef
         load
         load/shallow
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
  (ctx C ::= {GLOBAL layer LOCAL layer})

  ;; A context of the right shape, with its maps unchecked, for $load
  (layer-shallow ::= {TYP any REL any FUNC any VAL any})
  (ctx-shallow ::= {GLOBAL layer-shallow LOCAL layer-shallow}))

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
;; $load runs as load/shallow, on ctx-shallow, and checks its input and result
;; against ctx once. load/shallow takes defn_h :: defn_t* apart with uncons.

;; (x_h x_t) for a non-empty list, where x_t is the list's own tail
(define (uncons xs)
  (and (pair? xs) (list (car xs) (cdr xs))))

(define-dec al-context
  load-typdef : ctx-shallow id typdef -> ctx-shallow
  [(load-typdef {GLOBAL {TYP any_tdenv REL any_renv FUNC any_fenv VAL any_venv}
                 LOCAL layer-shallow}
                id typdef)
   {GLOBAL {TYP map_update REL any_renv FUNC any_fenv VAL any_venv} LOCAL layer-shallow}
   (where map_update (add-map any_tdenv id typdef))])

(define-dec al-context
  load-reldef : ctx-shallow id reldef -> ctx-shallow
  [(load-reldef {GLOBAL {TYP any_tdenv REL any_renv FUNC any_fenv VAL any_venv}
                 LOCAL layer-shallow}
                id reldef)
   {GLOBAL {TYP any_tdenv REL map_update FUNC any_fenv VAL any_venv} LOCAL layer-shallow}
   (where map_update (add-map any_renv id reldef))])

(define-dec al-context
  load-funcdef : ctx-shallow id funcdef -> ctx-shallow
  [(load-funcdef {GLOBAL {TYP any_tdenv REL any_renv FUNC any_fenv VAL any_venv}
                  LOCAL layer-shallow}
                 id funcdef)
   {GLOBAL {TYP any_tdenv REL any_renv FUNC map_update VAL any_venv} LOCAL layer-shallow}
   (where map_update (add-map any_fenv id funcdef))])

;; load/shallow runs with caching off.
;; Redex still looks calls up in the cache, so nothing else may run it with
;; caching on for a large script: each lookup would then be a deep equal?.
(define-dec al-context
  load : ctx script -> ctx
  [(load C script) C_1
   (where C_1 ,(parameterize ([caching-enabled? #f])
                 (term (load/shallow C script))))])

(define-dec al-context
  load/shallow : ctx-shallow any -> ctx-shallow
  [(load/shallow ctx-shallow any_script)
   (load/shallow ctx-shallow_1 any_t)
   (where ((EXTTYP id) any_t) ,(uncons (term any_script)))
   (where ctx-shallow_1 (load-typdef ctx-shallow id EXT))]
  [(load/shallow ctx-shallow any_script)
   (load/shallow ctx-shallow_1 any_t)
   (where ((TYP id (tparam ...) deftyp) any_t) ,(uncons (term any_script)))
   (where ctx-shallow_1 (load-typdef ctx-shallow id (DEF (tparam ...) deftyp)))]
  [(load/shallow ctx-shallow any_script)
   (load/shallow ctx-shallow_1 any_t)
   (where ((EXTREL id (typ_input ...) (typ_output ...)) any_t) ,(uncons (term any_script)))
   (where ctx-shallow_1 (load-reldef ctx-shallow id (EXT id)))]
  [(load/shallow ctx-shallow any_script)
   (load/shallow ctx-shallow_1 any_t)
   (where ((REL id (typ_input ...) (typ_output ...) (rulgroup ...) (elsgroup ...)) any_t)
          ,(uncons (term any_script)))
   (where ctx-shallow_1 (load-reldef ctx-shallow id (DEF (rulgroup ...) (elsgroup ...))))]
  [(load/shallow ctx-shallow any_script)
   (load/shallow ctx-shallow_1 any_t)
   (where ((EXTFUNC id (tparam ...) (param ...) typ) any_t) ,(uncons (term any_script)))
   (where ctx-shallow_1 (load-funcdef ctx-shallow id (EXT id)))]
  [(load/shallow ctx-shallow any_script)
   (load/shallow ctx-shallow_1 any_t)
   (where ((BUILTINFUNC id (tparam ...) (param ...) typ) any_t) ,(uncons (term any_script)))
   (where ctx-shallow_1 (load-funcdef ctx-shallow id (BUILTIN id (tparam ...) (param ...))))]
  [(load/shallow ctx-shallow any_script)
   (load/shallow ctx-shallow_1 any_t)
   (where ((TABLEFUNC id (param ...) typ (tblrow ...)) any_t) ,(uncons (term any_script)))
   (where ctx-shallow_1 (load-funcdef ctx-shallow id (TABLE (param ...) (tblrow ...))))]
  [(load/shallow ctx-shallow any_script)
   (load/shallow ctx-shallow_1 any_t)
   (where ((FUNC id (tparam ...) (param ...) typ (clause ...) (elsclause ...)) any_t)
          ,(uncons (term any_script)))
   (where ctx-shallow_1
          (load-funcdef ctx-shallow id (DEF (tparam ...) (clause ...) (elsclause ...))))]
  [(load/shallow ctx-shallow ()) ctx-shallow])

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
