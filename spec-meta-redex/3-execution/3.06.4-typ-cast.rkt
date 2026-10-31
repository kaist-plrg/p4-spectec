#lang racket/base
;; spec-meta/3-execution/3.05.4-typ-cast.watsup.
;;
;; A cast takes G and L for ctx, and passes L's tdenv to subtyp.

(require redex/reduction-semantics
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.06.1-typ-subst.rkt"
         "3.06.3-typ-match.rkt")
(provide upcast
         downcast)

;;; Upcasts

(define-metafunction al
  upcast : G L typ val -> castRes
  [(upcast G L INT (INT i)) (OK (INT i))]
  [(upcast G L INT (NAT n)) (OK (INT n))]
  [(upcast G L (VAR id (targ ..._n)) val) (upcast G L typ_subst val)
   (where ((TYP id_def (tparam ..._n) (ALIAS typ))) (find-typ G L id))
   (where (OK typ_subst) (subst-typ ((tparam targ) ...) typ))]
  [(upcast G (name L {TYP tdenv FUNC fnenv VAL venv}) (VAR id (targ ...)) val) (OK val)
   (where ((TYP id_def (tparam ...) (VARIANT (typCase ...)))) (find-typ G L id))
   (where (OK #t) (subtyp G tdenv (VAR id (targ ...)) val))]
  [(upcast G (name L {TYP tdenv FUNC fnenv VAL venv}) (VAR id (targ ...)) val) (OK val)
   (where ((TYP id_def (tparam ...) (STRUCT (typField ...)))) (find-typ G L id))
   (where (OK #t) (subtyp G tdenv (VAR id (targ ...)) val))]
  [(upcast G L (VAR id ()) (EXT json)) (OK (EXT json))
   (where ((EXTTYP id_def)) (find-typ G L id))]
  [(upcast G L (TUP (typ ..._n)) (TUP (val ..._n))) (OK (TUP (val_cast ...)))
   (where ((OK val_cast) ...) ((upcast G L typ val) ...))]
  [(upcast G L (ITER typ QUEST) (OPT ())) (OK (OPT ()))]
  [(upcast G L (ITER typ QUEST) (OPT (val))) (OK (OPT (val_cast)))
   (where (OK val_cast) (upcast G L typ val))]
  [(upcast G L (ITER typ STAR) (LIST (val ...))) (OK (LIST (val_cast ...)))
   (where ((OK val_cast) ...) ((upcast G L typ val) ...))]
  [(upcast G L BOOL (BOOL b)) (OK (BOOL b))]
  [(upcast G L TEXT (TEXT t)) (OK (TEXT t))]
  [(upcast G L NAT (NAT n)) (OK (NAT n))]
  [(upcast G {TYP tdenv FUNC fnenv VAL venv} (FUNC (tparam ...) (typ ...) typ_ret) (FUNC id)) (OK (FUNC id))
   (where (OK #t) (subtyp G tdenv (FUNC (tparam ...) (typ ...) typ_ret) (FUNC id)))]
  ;; otherwise
  [(upcast G L typ val) ERROR])

;;; Downcasts

(define-metafunction al
  downcast : G L typ val -> castRes
  [(downcast G L NAT (NAT n)) (OK (NAT n))]
  [(downcast G L NAT (INT n)) (OK (NAT n))]
  [(downcast G L (VAR id (targ ..._n)) val) (downcast G L typ_subst val)
   (where ((TYP id_def (tparam ..._n) (ALIAS typ))) (find-typ G L id))
   (where (OK typ_subst) (subst-typ ((tparam targ) ...) typ))]
  [(downcast G (name L {TYP tdenv FUNC fnenv VAL venv}) (VAR id (targ ...)) val) (OK val)
   (where ((TYP id_def (tparam ...) (VARIANT (typCase ...)))) (find-typ G L id))
   (where (OK #t) (subtyp G tdenv (VAR id (targ ...)) val))]
  [(downcast G (name L {TYP tdenv FUNC fnenv VAL venv}) (VAR id (targ ...)) val) (OK val)
   (where ((TYP id_def (tparam ...) (STRUCT (typField ...)))) (find-typ G L id))
   (where (OK #t) (subtyp G tdenv (VAR id (targ ...)) val))]
  [(downcast G L (VAR id ()) (EXT json)) (OK (EXT json))
   (where ((EXTTYP id_def)) (find-typ G L id))]
  [(downcast G L (TUP (typ ..._n)) (TUP (val ..._n))) (OK (TUP (val_cast ...)))
   (where ((OK val_cast) ...) ((downcast G L typ val) ...))]
  [(downcast G L (ITER typ QUEST) (OPT ())) (OK (OPT ()))]
  [(downcast G L (ITER typ QUEST) (OPT (val))) (OK (OPT (val_cast)))
   (where (OK val_cast) (downcast G L typ val))]
  [(downcast G L (ITER typ STAR) (LIST (val ...))) (OK (LIST (val_cast ...)))
   (where ((OK val_cast) ...) ((downcast G L typ val) ...))]
  [(downcast G L BOOL (BOOL b)) (OK (BOOL b))]
  [(downcast G L TEXT (TEXT t)) (OK (TEXT t))]
  [(downcast G L INT num) (OK num)]
  [(downcast G {TYP tdenv FUNC fnenv VAL venv} (FUNC (tparam ...) (typ ...) typ_ret) (FUNC id)) (OK (FUNC id))
   (where (OK #t) (subtyp G tdenv (FUNC (tparam ...) (typ ...) typ_ret) (FUNC id)))]
  ;; otherwise
  [(downcast G L typ val) ERROR])
