#lang racket/base
;; spec-meta/3-execution/3.05.1-typ-subst.watsup, 3.05.3-typ-match.watsup, and
;; 3.05.4-typ-cast.watsup.

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.05-eval-ops.rkt")
(provide subst-typ
         subst-typs
         upcast
         downcast
         subtyp)

;; Type substitution

(define-judgment-form al
  #:mode (subst-typ I I O)
  #:contract (subst-typ theta typ typ)
  [(subst-typ () typ typ)]
  [(subst-typ-inner theta typ typ_subst)
   ----------------------------------------
   (subst-typ (name theta ((id_h typ_h) (id_t typ_t) ...)) typ typ_subst)])

;; $subst_typ'
(define-judgment-form al
  #:mode (subst-typ-inner I I O)
  #:contract (subst-typ-inner theta typ typ)
  [(subst-typ-inner theta NAT NAT)]
  [(subst-typ-inner theta INT INT)]
  [(subst-typ-inner theta TEXT TEXT)]
  [(subst-typ-inner theta BOOL BOOL)]
  [(where (typ) (find-map theta id))
   ----------------------------------------
   (subst-typ-inner theta (VAR id ()) typ)]
  [(where () (find-map theta id))
   (subst-typ-inner theta targ typ_subst) ...
   ----------------------------------------
   (subst-typ-inner theta (VAR id (targ ...)) (VAR id (typ_subst ...)))]
  [(subst-typ-inner theta typ typ_subst) ...
   ----------------------------------------
   (subst-typ-inner theta (TUP (typ ...)) (TUP (typ_subst ...)))]
  [(subst-typ-inner theta typ typ_subst)
   ----------------------------------------
   (subst-typ-inner theta (ITER typ iter) (ITER typ_subst iter))]
  [(subst-typ-inner theta (FUNC (tparam ...) (typ ...) typ_ret) (FUNC (tparam ...) (typ ...) typ_ret))])

;; $subst_typ on each type, for a caller that iterates it. Not a watsup def:
;; judgment-holds takes no ellipsis.
(define-judgment-form al
  #:mode (subst-typs I I O)
  #:contract (subst-typs theta (typ ...) (typ ...))
  [(subst-typ theta typ typ_subst) ...
   ----------------------------------------
   (subst-typs theta (typ ...) (typ_subst ...))])

;;
;; Type casts
;;

;;; Upcasts

(define-metafunction al
  upcast : G L typ val -> valres
  [(upcast G L INT (INT i)) (INT i)]
  [(upcast G L INT (NAT n)) (INT n)]
  [(upcast G L INT val) FAIL]
  [(upcast G L (VAR id (targ ..._n)) val) (upcast G L typ_subst val)
   (where ((DEF (tparam ..._n) (ALIAS typ))) (find-typ G L id))
   (where theta ((tparam targ) ...))
   (judgment-holds (subst-typ theta typ typ_subst))]
  [(upcast G L (TUP (typ ..._n)) (TUP (val ..._n))) (TUP (val_upcast ...))
   (where (val_upcast ...) ((upcast G L typ val) ...))]
  [(upcast G L (TUP (typ ...)) val) FAIL
   (where #f (is-tup val))]
  [(upcast G L (ITER typ QUEST) (OPT (val ...))) (OPT (val_upcast ...))
   (where (val_upcast ...) ((upcast G L typ val) ...))]
  [(upcast G L (ITER typ STAR) (LIST (val ...))) (LIST (val_upcast ...))
   (where (val_upcast ...) ((upcast G L typ val) ...))]
  ;; otherwise
  [(upcast G L typ val) val])

;;; Downcasts

(define-metafunction al
  downcast : G L typ val -> valres
  [(downcast G L NAT (NAT n)) (NAT n)]
  [(downcast G L NAT (INT n)) (NAT n)]
  [(downcast G L NAT val) FAIL
   (side-condition (not (redex-match? al num (term val))))]
  [(downcast G L (VAR id (targ ..._n)) val) (downcast G L typ_subst val)
   (where ((DEF (tparam ..._n) (ALIAS typ))) (find-typ G L id))
   (where theta ((tparam targ) ...))
   (judgment-holds (subst-typ theta typ typ_subst))]
  [(downcast G L (TUP (typ ..._n)) (TUP (val ..._n))) (TUP (val_downcast ...))
   (where (val_downcast ...) ((downcast G L typ val) ...))]
  [(downcast G L (TUP (typ ...)) val) FAIL
   (where #f (is-tup val))]
  [(downcast G L (ITER typ QUEST) (OPT (val ...))) (OPT (val_downcast ...))
   (where (val_downcast ...) ((downcast G L typ val) ...))]
  [(downcast G L (ITER typ STAR) (LIST (val ...))) (LIST (val_downcast ...))
   (where (val_downcast ...) ((downcast G L typ val) ...))]
  ;; otherwise
  [(downcast G L typ val) val])

;;
;; Value-type matching
;;

;; $subtyp takes G and the local tdenv, where spec-meta takes their extension
;; $extend_tdenv(C.GLOBAL.TYP, C.LOCAL.TYP): find-typdef finds in G and tdenv
;; what $find_map finds in the extension.
(define-metafunction al
  subtyp : G tdenv typ val -> bool
  [(subtyp G tdenv BOOL (BOOL b)) #t]
  [(subtyp G tdenv NAT (NAT n)) #t]
  [(subtyp G tdenv NAT (INT i)) ,(>= (term i) 0)]
  [(subtyp G tdenv INT num) #t]
  [(subtyp G tdenv TEXT (TEXT t)) #t]
  [(subtyp G tdenv (VAR id (targ ...)) (EXT json)) #t
   (where (EXT) (find-typdef G tdenv id))]
  [(subtyp G tdenv (VAR id (targ ..._n)) val) (subtyp G tdenv typ_subst val)
   (where ((DEF (tparam ..._n) (ALIAS typ))) (find-typdef G tdenv id))
   (where theta ((tparam targ) ...))
   (judgment-holds (subst-typ theta typ typ_subst))]
  [(subtyp G tdenv (VAR id (targ ..._n)) (INJ (mixop (val ..._m))))
   (forall- ((subtyp G tdenv typ_case_found_subst val) ...))
   (where ((DEF (tparam ..._n) (VARIANT (typCase ...)))) (find-typdef G tdenv id))
   (where theta ((tparam targ) ...))
   (where (mixop (typ_case_found ..._m)) ,(assoc (term mixop) (term (typCase ...))))
   (judgment-holds (subst-typs theta (typ_case_found ...) (typ_case_found_subst ...)))]
  [(subtyp G tdenv (VAR id (targ ..._n)) (STR ((atom val_field) ...)))
   (forall- ((subtyp G tdenv typ_field_subst val_field) ...))
   (where ((DEF (tparam ..._n) (STRUCT ((atom typ_field) ...)))) (find-typdef G tdenv id))
   (where theta ((tparam targ) ...))
   (judgment-holds (subst-typs theta (typ_field ...) (typ_field_subst ...)))]
  [(subtyp G tdenv (TUP (typ ..._n)) (TUP (val ..._n))) (forall- ((subtyp G tdenv typ val) ...))]
  [(subtyp G tdenv (ITER typ QUEST) (OPT (val))) (subtyp G tdenv typ val)]
  [(subtyp G tdenv (ITER typ QUEST) (OPT ())) #t]
  [(subtyp G tdenv (ITER typ STAR) (LIST (val ...))) (forall- ((subtyp G tdenv typ val) ...))]
  ;; otherwise
  [(subtyp G tdenv typ val) #f])
