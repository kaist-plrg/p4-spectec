#lang racket/base
;; spec-meta/al/3-context.watsup.
;;
;; A record update C[ .LOCAL.VAL = x ] is a pattern that rebuilds the record.

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
  (layer ::= {TYP tdenv REL renv FUNC fenv VAL venv})
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
;; load runs on ctx-shallow and checks the result against `ctx` once.

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

(define-dec al-context
  load : ctx script -> ctx
  [(load C script) C_1
   (where C_1 (load/shallow C script))])

(define-dec al-context
  load/shallow : ctx-shallow (any ...) -> ctx-shallow
  [(load/shallow ctx-shallow ((EXTTYP id) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load-typdef ctx-shallow id EXT))]
  [(load/shallow ctx-shallow ((TYP id (tparam ...) deftyp) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load-typdef ctx-shallow id (DEF (tparam ...) deftyp)))]
  [(load/shallow ctx-shallow ((EXTREL id (typ_input ...) (typ_output ...)) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load-reldef ctx-shallow id (EXT id)))]
  [(load/shallow ctx-shallow
                 ((REL id (typ_input ...) (typ_output ...) (rulgroup ...) (elsgroup ...))
                  any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load-reldef ctx-shallow id (DEF (rulgroup ...) (elsgroup ...))))]
  [(load/shallow ctx-shallow ((EXTFUNC id (tparam ...) (param ...) typ) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load-funcdef ctx-shallow id (EXT id)))]
  [(load/shallow ctx-shallow ((BUILTINFUNC id (tparam ...) (param ...) typ) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load-funcdef ctx-shallow id (BUILTIN id (tparam ...) (param ...))))]
  [(load/shallow ctx-shallow ((TABLEFUNC id (param ...) typ (tblrow ...)) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load-funcdef ctx-shallow id (TABLE (param ...) (tblrow ...))))]
  [(load/shallow ctx-shallow
                 ((FUNC id (tparam ...) (param ...) typ (clause ...) (elsclause ...))
                  any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1
          (load-funcdef ctx-shallow id (DEF (tparam ...) (clause ...) (elsclause ...))))]
  [(load/shallow ctx-shallow ()) ctx-shallow])

;;
;; Adders
;;

;;; Value adders

(define-dec al-context
  add-vari : ctx vari val -> ctx
  [(add-vari {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}}
             (id _ (iter ...)) val)
   {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv_update}}
   (where venv_update (add-map venv (id (iter ...)) val))])

(define-dec al-context
  add-varr : ctx varr val -> ctx
  [(add-varr {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}} varr val)
   {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv_update}}
   (where venv_update (add-map venv varr val))])

(define-dec al-context
  add-varis : ctx (vari ...) (val ...) -> ctx
  [(add-varis {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}}
              ((id _ (iter ...)) ...) (val ...))
   {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv_update}}
   (where venv_update (adds-map venv ((id (iter ...)) ...) (val ...)))])

(define-dec al-context
  add-varrs : ctx (varr ...) (val ...) -> ctx
  [(add-varrs {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}}
              (varr ...) (val ...))
   {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv_update}}
   (where venv_update (adds-map venv (varr ...) (val ...)))])

;;; Typedef adders

(define-dec al-context
  add-typ : ctx id typdef -> ctx
  [(add-typ {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}} id typdef)
   {GLOBAL layer LOCAL {TYP tdenv_update REL renv FUNC fenv VAL venv}}
   (where tdenv_update (add-map tdenv id typdef))])

;;; Function adders

(define-dec al-context
  add-func : ctx id funcdef -> ctx
  [(add-func {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}} id funcdef)
   {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv_update VAL venv}}
   (where fenv_update (add-map fenv id funcdef))])

;;
;; Finders
;;

;;; Value finders

;; No clause for an unbound variable.
(define-dec al-context
  find-vari : ctx vari -> (val ...)
  [(find-vari {GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} (id _ (iter ...)))
   (val)
   (where (val) (find-map venv (id (iter ...))))])

(define-dec al-context
  find-varr : ctx varr -> (val ...)
  [(find-varr {GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} varr)
   (find-map venv varr)])

(define-dec al-context
  find-varis : ctx (vari ...) -> ((val ...) ...)
  [(find-varis C (vari ...)) ((val ...))
   (where ((val) ...) ((find-vari C vari) ...))]
  [(find-varis C (vari ...)) ()
   ;; otherwise
   (side-condition
    (not (redex-match? al-context ((val) ...) (term ((find-vari C vari) ...)))))])

(define-dec al-context
  find-varrs : ctx (varr ...) -> ((val ...) ...)
  [(find-varrs {GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} (varr ...)) ((val ...))
   (where ((val) ...) ((find-map venv varr) ...))]
  [(find-varrs {GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} (varr ...)) ()
   ;; otherwise
   (side-condition
    (not (redex-match? al-context ((val) ...) (term ((find-map venv varr) ...)))))])

(define-dec al-context
  finds-vari : (ctx ...) vari -> ((val ...) ...)
  [(finds-vari ({GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} ...) (id _ (iter ...)))
   ((val ...))
   (where ((val) ...) ((find-map venv (id (iter ...))) ...))]
  [(finds-vari ({GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} ...) (id _ (iter ...)))
   ()
   ;; otherwise
   (side-condition
    (not (redex-match? al-context ((val) ...)
                       (term ((find-map venv (id (iter ...))) ...)))))])

;;; Typedef finders

(define-dec al-context
  find-typ : ctx id -> (typdef ...)
  [(find-typ {GLOBAL _ LOCAL {TYP tdenv_l REL _ FUNC _ VAL _}} id) (typdef)
   (where (typdef) (find-map tdenv_l id))]
  [(find-typ {GLOBAL {TYP tdenv_g REL _ FUNC _ VAL _} LOCAL {TYP tdenv_l REL _ FUNC _ VAL _}}
             id)
   (find-map tdenv_g id)
   (where () (find-map tdenv_l id))])

;;; Function finders

(define-dec al-context
  find-func : ctx id -> (funcdef ...)
  [(find-func {GLOBAL _ LOCAL {TYP _ REL _ FUNC fenv_l VAL _}} id) (funcdef)
   (where (funcdef) (find-map fenv_l id))]
  [(find-func {GLOBAL {TYP _ REL _ FUNC fenv_g VAL _} LOCAL {TYP _ REL _ FUNC fenv_l VAL _}}
              id)
   (funcdef)
   (where () (find-map fenv_l id))
   (where (funcdef) (find-map fenv_g id))]
  [(find-func {GLOBAL {TYP _ REL _ FUNC fenv_g VAL _} LOCAL {TYP _ REL _ FUNC fenv_l VAL _}}
              id)
   ()
   ;; otherwise: found in neither layer
   (where () (find-map fenv_l id))
   (where () (find-map fenv_g id))])

;;; Relation finders

(define-dec al-context
  find-rel : ctx id -> (reldef ...)
  [(find-rel {GLOBAL {TYP _ REL renv_g FUNC _ VAL _} LOCAL _} id) (find-map renv_g id)])

;;
;; Sub-context construction for iteration
;;

(define-dec al-context
  sub-opt : ctx (vari ...) -> (ctx ...)
  [(sub-opt C (vari ...)) (C_1)
   (where (vari_iter ...) ((iter-vari vari QUEST) ...))
   (where (((OPT (val))) ...) ((find-vari C vari_iter) ...))
   (where C_1 (add-varis C (vari ...) (val ...)))]
  [(sub-opt C (vari ...)) ()
   ;; An empty vari* matches both clauses, and watsup takes the first.
   (side-condition (pair? (term (vari ...))))
   (where (vari_iter ...) ((iter-vari vari QUEST) ...))
   (where (((OPT ())) ...) ((find-vari C vari_iter) ...))])

(define-dec al-context
  sub-list : ctx (vari ...) -> (ctx ...)
  [(sub-list C (vari ...)) (C_1 ...)
   (where (vari_iter ...) ((iter-vari vari STAR) ...))
   (where (((LIST (val ...))) ...) ((find-vari C vari_iter) ...))
   (where ((val_trans ...) ...) (transpose- ((val ...) ...)))
   (where (C_1 ...) ((add-varis C (vari ...) (val_trans ...)) ...))])
