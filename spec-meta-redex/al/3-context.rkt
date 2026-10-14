#lang racket/base
;; spec-meta/al/3-context.watsup.
;;
;; A dec over ctx takes G L if it reads C.GLOBAL, and L alone otherwise. L is
;; the local layer. The global layer never changes after $load, so it stays in
;; a registry, and G is its handle. A ctx result is the new L. $load and
;; $empty_ctx keep ctx. A record update C[ .LOCAL.VAL = x ] is a pattern that
;; rebuilds the layer.

(require racket/match
         redex/reduction-semantics
         "../common/0.1-stdlib.rkt"
         "../common/2-env.rkt"
         "2-env.rkt")
(provide al-context
         empty-layer
         empty-ctx
         load
         register-global!
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
         find-typdef
         find-typ
         find-func
         find-rel
         sub-opt
         sub-list)

(define-extended-language al-context al-env
  ;; Context
  (layer L ::= {TYP tdenv REL renv FUNC fenv VAL venv})
  (ctx C ::= {GLOBAL layer LOCAL layer})

  ;; The handle of a registered global layer
  (G ::= (GLOBAL natural)))

(define-metafunction al-context
  empty-layer : -> layer
  [(empty-layer) {TYP (empty-map) REL (empty-map) FUNC (empty-map) VAL (empty-map)}])

(define-metafunction al-context
  empty-ctx : -> ctx
  [(empty-ctx) {GLOBAL (empty-layer) LOCAL (empty-layer)}])

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
;; The global layer, by handle
;;

;; The registered global layers, by number. An entry never changes once
;; added, so a handle denotes the same layer for good, and a metafunction that
;; reads the layer through its handle stays a function of its arguments.
(define global-layers (make-hasheqv))

;; The handle of a global layer, which is checked and added to the registry.
;; Only al/7-entry.rkt calls this, after $load.
(define (register-global! layer_global)
  (unless (redex-match? al-context layer layer_global)
    (error 'register-global! "not a layer: ~e" layer_global))
  (define n (hash-count global-layers))
  (hash-set! global-layers n layer_global)
  (term (GLOBAL ,n)))

;; The value of id in the map of the global layer G under the label field
;; (TYP, REL, or FUNC), as () or (v). The layer is taken apart in Racket, so
;; neither a term nor a pattern holds it.
(define (find-global G field id)
  (match-define (list 'GLOBAL n) G)
  (match-define (list 'TYP tdenv 'REL renv 'FUNC fenv 'VAL _)
    (hash-ref global-layers n (λ () (error 'find-global "no global layer ~a" n))))
  (map-ref (case field [(TYP) tdenv] [(REL) renv] [(FUNC) fenv]) id))

;;
;; Adders
;;

;;; Value adders

(define-metafunction al-context
  add-vari : L vari val -> L
  [(add-vari {TYP tdenv REL renv FUNC fenv VAL venv} (id typ (iter ...)) val)
   {TYP tdenv REL renv FUNC fenv VAL (add-map venv (id (iter ...)) val)}])

(define-metafunction al-context
  add-varr : L varr val -> L
  [(add-varr {TYP tdenv REL renv FUNC fenv VAL venv} varr val)
   {TYP tdenv REL renv FUNC fenv VAL (add-map venv varr val)}])

(define-metafunction al-context
  add-varis : L (vari ...) (val ...) -> L
  [(add-varis {TYP tdenv REL renv FUNC fenv VAL venv} ((id typ (iter ...)) ...) (val ...))
   {TYP tdenv REL renv FUNC fenv VAL (adds-map venv ((id (iter ...)) ...) (val ...))}])

(define-metafunction al-context
  add-varrs : L (varr ...) (val ...) -> L
  [(add-varrs {TYP tdenv REL renv FUNC fenv VAL venv} (varr ...) (val ...))
   {TYP tdenv REL renv FUNC fenv VAL (adds-map venv (varr ...) (val ...))}])

;;; Typedef adders

(define-metafunction al-context
  add-typ : L id typdef -> L
  [(add-typ {TYP tdenv REL renv FUNC fenv VAL venv} id typdef)
   {TYP (add-map tdenv id typdef) REL renv FUNC fenv VAL venv}])

;;; Function adders

(define-metafunction al-context
  add-func : L id funcdef -> L
  [(add-func {TYP tdenv REL renv FUNC fenv VAL venv} id funcdef)
   {TYP tdenv REL renv FUNC (add-map fenv id funcdef) VAL venv}])

;;
;; Finders
;;

;;; Value finders

(define-judgment-form al-context
  #:mode (find-vari I I O)
  #:contract (find-vari L vari val)
  [(where (val) (find-map venv (id (iter ...))))
   ----------------------------------------
   (find-vari {TYP tdenv REL renv FUNC fenv VAL venv} (id typ (iter ...)) val)])

(define-metafunction al-context
  find-varr : L varr -> () ∨ (val)
  [(find-varr {TYP tdenv REL renv FUNC fenv VAL venv} varr) (find-map venv varr)])

(define-metafunction al-context
  find-varis : L (vari ...) -> () ∨ ((val ...))
  [(find-varis {TYP tdenv REL renv FUNC fenv VAL venv} ((id typ (iter ...)) ...)) ((val ...))
   (where ((val) ...) ((find-map venv (id (iter ...))) ...))]
  [(find-varis {TYP tdenv REL renv FUNC fenv VAL venv} ((id typ (iter ...)) ...)) ()
   ;; otherwise: some lookup fails
   (side-condition
    (not (redex-match? al-context ((val) ...)
                       (term ((find-map venv (id (iter ...))) ...)))))])

(define-metafunction al-context
  find-varrs : L (varr ...) -> () ∨ ((val ...))
  [(find-varrs {TYP tdenv REL renv FUNC fenv VAL venv} (varr ...)) ((val ...))
   (where ((val) ...) ((find-map venv varr) ...))]
  [(find-varrs {TYP tdenv REL renv FUNC fenv VAL venv} (varr ...)) ()
   ;; otherwise: some lookup fails
   (side-condition
    (not (redex-match? al-context ((val) ...) (term ((find-map venv varr) ...)))))])

(define-metafunction al-context
  finds-vari : (L ...) vari -> () ∨ ((val ...))
  [(finds-vari ({TYP tdenv REL renv FUNC fenv VAL venv} ...) (id typ (iter ...))) ((val ...))
   (where ((val) ...) ((find-map venv (id (iter ...))) ...))]
  [(finds-vari ({TYP tdenv REL renv FUNC fenv VAL venv} ...) (id typ (iter ...))) ()
   ;; otherwise: some lookup fails
   (side-condition
    (not (redex-match? al-context ((val) ...)
                       (term ((find-map venv (id (iter ...))) ...)))))])

;;; Typedef finders

;; The definition of id in tdenv, or else in the global layer G: the value of
;; id in $extend_tdenv(C.GLOBAL.TYP, tdenv)
(define-metafunction al-context
  find-typdef : G tdenv id -> () ∨ (typdef)
  [(find-typdef G tdenv id) (typdef)
   (where (typdef) (find-map tdenv id))]
  [(find-typdef G tdenv id) ,(find-global (term G) 'TYP (term id))
   (where () (find-map tdenv id))])

(define-metafunction al-context
  find-typ : G L id -> () ∨ (typdef)
  [(find-typ G {TYP tdenv_l REL renv_l FUNC fenv_l VAL venv_l} id) (find-typdef G tdenv_l id)])

;;; Function finders

(define-metafunction al-context
  find-func : G L id -> () ∨ (funcdef)
  [(find-func G {TYP tdenv_l REL renv_l FUNC fenv_l VAL venv_l} id) (funcdef)
   (where (funcdef) (find-map fenv_l id))]
  [(find-func G {TYP tdenv_l REL renv_l FUNC fenv_l VAL venv_l} id)
   ,(find-global (term G) 'FUNC (term id))
   (where () (find-map fenv_l id))])

;;; Relation finders

(define-metafunction al-context
  find-rel : G L id -> () ∨ (reldef)
  [(find-rel G L id) ,(find-global (term G) 'REL (term id))])

;;
;; Sub-context construction for iteration
;;

(define-judgment-form al-context
  #:mode (sub-opt I I O)
  #:contract (sub-opt L (vari ...) (L ...))
  [(where (vari_iter ...) ((iter-vari vari QUEST) ...))
   (find-vari L vari_iter (OPT (val))) ...
   ----------------------------------------
   (sub-opt L (vari ...) ((add-varis L (vari ...) (val ...))))]
  [;; An empty vari* is the first rule's, as watsup takes the first clause.
   (side-condition ,(pair? (term (vari ...))))
   (where (vari_iter ...) ((iter-vari vari QUEST) ...))
   (find-vari L vari_iter (OPT ())) ...
   ----------------------------------------
   (sub-opt L (vari ...) ())])

(define-judgment-form al-context
  #:mode (sub-list I I O)
  #:contract (sub-list L (vari ...) (L ...))
  [(where (vari_iter ...) ((iter-vari vari STAR) ...))
   (find-vari L vari_iter (LIST (val ...))) ...
   (where ((val_trans ...) ...) (transpose- ((val ...) ...)))
   ----------------------------------------
   (sub-list L (vari ...) ((add-varis L (vari ...) (val_trans ...)) ...))])
