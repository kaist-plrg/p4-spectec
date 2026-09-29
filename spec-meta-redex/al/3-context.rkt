#lang racket/base
;; spec-meta/al/3-context.watsup.
;;
;; A record update C[ .LOCAL.VAL = x ] is a pattern that rebuilds the record.

(require "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/2-env.rkt"
         "2-env.rkt")
(provide AL-context
         empty_layer
         empty_ctx
         load_typdef
         load_reldef
         load_funcdef
         load
         load/shallow
         add_vari
         add_varr
         add_varis
         add_varrs
         add_typ
         add_func
         find_vari
         find_varr
         find_varis
         find_varrs
         finds_vari
         find_typ
         find_func
         find_rel
         sub_opt
         sub_list)

(define-extended-language AL-context AL-env
  ;; Context
  (layer ::= {TYP tdenv REL renv FUNC fenv VAL venv})
  (ctx C ::= {GLOBAL layer LOCAL layer})

  ;; A context of the right shape, with its maps unchecked, for $load
  (layer-shallow ::= {TYP any REL any FUNC any VAL any})
  (ctx-shallow ::= {GLOBAL layer-shallow LOCAL layer-shallow}))

(define-dec AL-context
  empty_layer : -> layer
  [(empty_layer) {TYP tdenv REL renv FUNC fenv VAL venv}
   (where tdenv (empty_map))
   (where renv (empty_map))
   (where fenv (empty_map))
   (where venv (empty_map))])

(define-dec AL-context
  empty_ctx : -> ctx
  [(empty_ctx) {GLOBAL layer_g LOCAL layer_l}
   (where layer_g (empty_layer))
   (where layer_l (empty_layer))])

;;
;; Loading context from a script
;;
;; load runs on ctx-shallow and checks the result against `ctx` once.

(define-dec AL-context
  load_typdef : ctx-shallow id typdef -> ctx-shallow
  [(load_typdef {GLOBAL {TYP any_tdenv REL any_renv FUNC any_fenv VAL any_venv}
                 LOCAL layer-shallow}
                id typdef)
   {GLOBAL {TYP map_update REL any_renv FUNC any_fenv VAL any_venv} LOCAL layer-shallow}
   (where map_update (add_map any_tdenv id typdef))])

(define-dec AL-context
  load_reldef : ctx-shallow id reldef -> ctx-shallow
  [(load_reldef {GLOBAL {TYP any_tdenv REL any_renv FUNC any_fenv VAL any_venv}
                 LOCAL layer-shallow}
                id reldef)
   {GLOBAL {TYP any_tdenv REL map_update FUNC any_fenv VAL any_venv} LOCAL layer-shallow}
   (where map_update (add_map any_renv id reldef))])

(define-dec AL-context
  load_funcdef : ctx-shallow id funcdef -> ctx-shallow
  [(load_funcdef {GLOBAL {TYP any_tdenv REL any_renv FUNC any_fenv VAL any_venv}
                  LOCAL layer-shallow}
                 id funcdef)
   {GLOBAL {TYP any_tdenv REL any_renv FUNC map_update VAL any_venv} LOCAL layer-shallow}
   (where map_update (add_map any_fenv id funcdef))])

(define-dec AL-context
  load : ctx script -> ctx
  [(load C script) C_1
   (where C_1 (load/shallow C script))])

(define-dec AL-context
  load/shallow : ctx-shallow (any ...) -> ctx-shallow
  [(load/shallow ctx-shallow ((EXTTYP id) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load_typdef ctx-shallow id EXT))]
  [(load/shallow ctx-shallow ((TYP id (tparam ...) deftyp) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load_typdef ctx-shallow id (DEF (tparam ...) deftyp)))]
  [(load/shallow ctx-shallow ((EXTREL id (typ_input ...) (typ_output ...)) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load_reldef ctx-shallow id (EXT id)))]
  [(load/shallow ctx-shallow
                 ((REL id (typ_input ...) (typ_output ...) (rulgroup ...) (elsgroup ...))
                  any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load_reldef ctx-shallow id (DEF (rulgroup ...) (elsgroup ...))))]
  [(load/shallow ctx-shallow ((EXTFUNC id (tparam ...) (param ...) typ) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load_funcdef ctx-shallow id (EXT id)))]
  [(load/shallow ctx-shallow ((BUILTINFUNC id (tparam ...) (param ...) typ) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load_funcdef ctx-shallow id (BUILTIN id (tparam ...) (param ...))))]
  [(load/shallow ctx-shallow ((TABLEFUNC id (param ...) typ (tblrow ...)) any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1 (load_funcdef ctx-shallow id (TABLE (param ...) (tblrow ...))))]
  [(load/shallow ctx-shallow
                 ((FUNC id (tparam ...) (param ...) typ (clause ...) (elsclause ...))
                  any_t ...))
   (load/shallow ctx-shallow_1 (any_t ...))
   (where ctx-shallow_1
          (load_funcdef ctx-shallow id (DEF (tparam ...) (clause ...) (elsclause ...))))]
  [(load/shallow ctx-shallow ()) ctx-shallow])

;;
;; Adders
;;

;;; Value adders

(define-dec AL-context
  add_vari : ctx vari val -> ctx
  [(add_vari {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}}
             (id _ (iter ...)) val)
   {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv_update}}
   (where venv_update (add_map venv (id (iter ...)) val))])

(define-dec AL-context
  add_varr : ctx varr val -> ctx
  [(add_varr {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}} varr val)
   {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv_update}}
   (where venv_update (add_map venv varr val))])

(define-dec AL-context
  add_varis : ctx (vari ...) (val ...) -> ctx
  [(add_varis {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}}
              ((id _ (iter ...)) ...) (val ...))
   {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv_update}}
   (where venv_update (adds_map venv ((id (iter ...)) ...) (val ...)))])

(define-dec AL-context
  add_varrs : ctx (varr ...) (val ...) -> ctx
  [(add_varrs {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}}
              (varr ...) (val ...))
   {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv_update}}
   (where venv_update (adds_map venv (varr ...) (val ...)))])

;;; Typedef adders

(define-dec AL-context
  add_typ : ctx id typdef -> ctx
  [(add_typ {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}} id typdef)
   {GLOBAL layer LOCAL {TYP tdenv_update REL renv FUNC fenv VAL venv}}
   (where tdenv_update (add_map tdenv id typdef))])

;;; Function adders

(define-dec AL-context
  add_func : ctx id funcdef -> ctx
  [(add_func {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}} id funcdef)
   {GLOBAL layer LOCAL {TYP tdenv REL renv FUNC fenv_update VAL venv}}
   (where fenv_update (add_map fenv id funcdef))])

;;
;; Finders
;;

;;; Value finders

;; No clause for an unbound variable.
(define-dec AL-context
  find_vari : ctx vari -> (val ...)
  [(find_vari {GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} (id _ (iter ...)))
   (val)
   (where (val) (find_map venv (id (iter ...))))])

(define-dec AL-context
  find_varr : ctx varr -> (val ...)
  [(find_varr {GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} varr)
   (find_map venv varr)])

(define-dec AL-context
  find_varis : ctx (vari ...) -> ((val ...) ...)
  [(find_varis C (vari ...)) ((val ...))
   (where ((val) ...) ((find_vari C vari) ...))]
  [(find_varis C (vari ...)) ()
   ;; otherwise
   (side-condition
    (not (redex-match? AL-context ((val) ...) (term ((find_vari C vari) ...)))))])

(define-dec AL-context
  find_varrs : ctx (varr ...) -> ((val ...) ...)
  [(find_varrs {GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} (varr ...)) ((val ...))
   (where ((val) ...) ((find_map venv varr) ...))]
  [(find_varrs {GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} (varr ...)) ()
   ;; otherwise
   (side-condition
    (not (redex-match? AL-context ((val) ...) (term ((find_map venv varr) ...)))))])

(define-dec AL-context
  finds_vari : (ctx ...) vari -> ((val ...) ...)
  [(finds_vari ({GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} ...) (id _ (iter ...)))
   ((val ...))
   (where ((val) ...) ((find_map venv (id (iter ...))) ...))]
  [(finds_vari ({GLOBAL _ LOCAL {TYP _ REL _ FUNC _ VAL venv}} ...) (id _ (iter ...)))
   ()
   ;; otherwise
   (side-condition
    (not (redex-match? AL-context ((val) ...)
                       (term ((find_map venv (id (iter ...))) ...)))))])

;;; Typedef finders

(define-dec AL-context
  find_typ : ctx id -> (typdef ...)
  [(find_typ {GLOBAL _ LOCAL {TYP tdenv_l REL _ FUNC _ VAL _}} id) (typdef)
   (where (typdef) (find_map tdenv_l id))]
  [(find_typ {GLOBAL {TYP tdenv_g REL _ FUNC _ VAL _} LOCAL {TYP tdenv_l REL _ FUNC _ VAL _}}
             id)
   (find_map tdenv_g id)
   (where () (find_map tdenv_l id))])

;;; Function finders

(define-dec AL-context
  find_func : ctx id -> (funcdef ...)
  [(find_func {GLOBAL _ LOCAL {TYP _ REL _ FUNC fenv_l VAL _}} id) (funcdef)
   (where (funcdef) (find_map fenv_l id))]
  [(find_func {GLOBAL {TYP _ REL _ FUNC fenv_g VAL _} LOCAL {TYP _ REL _ FUNC fenv_l VAL _}}
              id)
   (funcdef)
   (where () (find_map fenv_l id))
   (where (funcdef) (find_map fenv_g id))]
  [(find_func {GLOBAL {TYP _ REL _ FUNC fenv_g VAL _} LOCAL {TYP _ REL _ FUNC fenv_l VAL _}}
              id)
   ()
   ;; otherwise: found in neither layer
   (where () (find_map fenv_l id))
   (where () (find_map fenv_g id))])

;;; Relation finders

(define-dec AL-context
  find_rel : ctx id -> (reldef ...)
  [(find_rel {GLOBAL {TYP _ REL renv_g FUNC _ VAL _} LOCAL _} id) (find_map renv_g id)])

;;
;; Sub-context construction for iteration
;;

(define-dec AL-context
  sub_opt : ctx (vari ...) -> (ctx ...)
  [(sub_opt C (vari ...)) (C_1)
   (where (vari_iter ...) ((iter_vari vari QUEST) ...))
   (where (((OPT (val))) ...) ((find_vari C vari_iter) ...))
   (where C_1 (add_varis C (vari ...) (val ...)))]
  [(sub_opt C (vari ...)) ()
   ;; An empty vari* matches both clauses, and watsup takes the first.
   (side-condition (pair? (term (vari ...))))
   (where (vari_iter ...) ((iter_vari vari QUEST) ...))
   (where (((OPT ())) ...) ((find_vari C vari_iter) ...))])

(define-dec AL-context
  sub_list : ctx (vari ...) -> (ctx ...)
  [(sub_list C (vari ...)) (C_1 ...)
   (where (vari_iter ...) ((iter_vari vari STAR) ...))
   (where (((LIST (val ...))) ...) ((find_vari C vari_iter) ...))
   (where ((val_trans ...) ...) (transpose_ ((val ...) ...)))
   (where (C_1 ...) ((add_varis C (vari ...) (val_trans ...)) ...))])
