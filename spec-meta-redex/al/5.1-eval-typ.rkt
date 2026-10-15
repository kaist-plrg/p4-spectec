#lang racket/base
;; spec-meta/al/5.1-eval-typ.watsup.

(require redex/reduction-semantics
         "../common/0.0-stdlib.rkt"
         "../common/4.0-eval-typ.rkt"
         "../common/4.1-eval-ops.rkt"
         "3-context.rkt")
(provide upcast
         downcast
         subtyp)

;;
;; Type casts
;;

;;; Upcasts

(define-metafunction al-context
  upcast : G L typ val -> valres
  [(upcast G L INT (INT i)) (OK (INT i))]
  [(upcast G L INT (NAT n)) (OK (INT n))]
  [(upcast G L INT val) FAIL
   (side-condition (not (redex-match? al-context num (term val))))]
  [(upcast G L (VAR id (targ ..._n)) val) (upcast G L typ_subst val)
   (where ((DEF (tparam ..._n) (ALIAS typ))) (find-typ G L id))
   (where theta ((tparam targ) ...))
   (judgment-holds (subst-typ theta typ typ_subst))]
  [(upcast G L (TUP (typ ..._n)) (TUP (val ..._n))) (OK (TUP (val_upcast ...)))
   (where ((OK val_upcast) ...) ((upcast G L typ val) ...))]
  [(upcast G L (TUP (typ ...)) val) FAIL
   (where #f (is-tup val))]
  [(upcast G L (ITER typ QUEST) (OPT (val ...))) (OK (OPT (val_upcast ...)))
   (where ((OK val_upcast) ...) ((upcast G L typ val) ...))]
  [(upcast G L (ITER typ STAR) (LIST (val ...))) (OK (LIST (val_upcast ...)))
   (where ((OK val_upcast) ...) ((upcast G L typ val) ...))]
  ;; otherwise
  [(upcast G L typ val) (OK val)])

;;; Downcasts

(define-metafunction al-context
  downcast : G L typ val -> valres
  [(downcast G L NAT (NAT n)) (OK (NAT n))]
  [(downcast G L NAT (INT n)) (OK (NAT n))]
  [(downcast G L NAT val) FAIL
   (side-condition (not (redex-match? al-context num (term val))))]
  [(downcast G L (VAR id (targ ..._n)) val) (downcast G L typ_subst val)
   (where ((DEF (tparam ..._n) (ALIAS typ))) (find-typ G L id))
   (where theta ((tparam targ) ...))
   (judgment-holds (subst-typ theta typ typ_subst))]
  [(downcast G L (TUP (typ ..._n)) (TUP (val ..._n))) (OK (TUP (val_downcast ...)))
   (where ((OK val_downcast) ...) ((downcast G L typ val) ...))]
  [(downcast G L (TUP (typ ...)) val) FAIL
   (where #f (is-tup val))]
  [(downcast G L (ITER typ QUEST) (OPT (val ...))) (OK (OPT (val_downcast ...)))
   (where ((OK val_downcast) ...) ((downcast G L typ val) ...))]
  [(downcast G L (ITER typ STAR) (LIST (val ...))) (OK (LIST (val_downcast ...)))
   (where ((OK val_downcast) ...) ((downcast G L typ val) ...))]
  ;; otherwise
  [(downcast G L typ val) (OK val)])

;;
;; Value-type matching
;;

;; $subtyp takes G and the local tdenv, where spec-meta takes their extension
;; $extend_tdenv(C.GLOBAL.TYP, C.LOCAL.TYP): find-typdef finds in G and tdenv
;; what $find_map finds in the extension.
(define-metafunction al-context
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
   (where ((DEF (tparam ..._n) (VARIANT (typcase ...)))) (find-typdef G tdenv id))
   (where theta ((tparam targ) ...))
   (where (mixop (typ_case_found ..._m)) ,(assoc (term mixop) (term (typcase ...))))
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
