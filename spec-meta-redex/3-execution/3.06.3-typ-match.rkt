#lang racket/base
;; spec-meta/3-execution/3.05.3-typ-match.watsup.
;;
;; $subtyp takes G and the local tdenv where spec-meta takes fenv and the
;; extension $adds_map(C.GLOBAL.TYP, ...): find-typdef finds in G and tdenv
;; what $find_map finds in the extension, and fenv is read through G.

(require redex/reduction-semantics
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.06.1-typ-subst.rkt"
         "3.06.2-typ-equiv.rkt")
(provide subtyp)

;;; Values

(define-metafunction al
  subtyp : G tdenv typ val -> subtypRes
  [(subtyp G tdenv BOOL (BOOL b)) (OK #t)]
  [(subtyp G tdenv NAT (NAT n)) (OK #t)]
  [(subtyp G tdenv NAT (INT i)) (OK ,(>= (term i) 0))]
  [(subtyp G tdenv INT num) (OK #t)]
  [(subtyp G tdenv TEXT (TEXT t)) (OK #t)]
  [(subtyp G tdenv (VAR id (targ ...)) val) (subtyp-var G tdenv id (targ ...) val)]
  [(subtyp G tdenv (TUP (typ ...)) (TUP (val ...))) (subtyps G tdenv (typ ...) (val ...))]
  [(subtyp G tdenv (ITER typ QUEST) (OPT (val))) (subtyp G tdenv typ val)]
  [(subtyp G tdenv (ITER typ QUEST) (OPT ())) (OK #t)]
  [(subtyp G tdenv (ITER typ STAR) (LIST (val ...))) (subtyp-list G tdenv typ (val ...))]
  [(subtyp G tdenv (FUNC (tparam ...) (typ ...) typ_ret) (FUNC id))
   (subtyp-func G tdenv (FUNC (tparam ...) (typ ...) typ_ret) id)]
  ;; otherwise
  [(subtyp G tdenv typ val) (OK #f)])

;;; Named types and type definitions

(define-metafunction al
  subtyp-var : G tdenv id (targ ...) val -> subtypRes
  [(subtyp-var G tdenv id (targ ...) val) (subtyp-def G tdenv typDef (targ ...) val)
   (where (typDef) (find-typdef G tdenv id))]
  ;; otherwise
  [(subtyp-var G tdenv id (targ ...) val) ERROR])

(define-metafunction al
  subtyp-def : G tdenv typDef (targ ...) val -> subtypRes
  [(subtyp-def G tdenv (EXTTYP id) () val) (subtyp-extern val)]
  [(subtyp-def G tdenv (TYP id (tparam ..._n) (ALIAS typ)) (targ ..._n) val) (subtyp G tdenv typ_subst val)
   (where (OK typ_subst) (subst-typ ((tparam targ) ...) typ))]
  [(subtyp-def G tdenv (TYP id (tparam ..._n) (VARIANT (typCase ...))) (targ ..._n) val)
   (subtyp-variant G tdenv ((tparam targ) ...) (typCase ...) val)]
  [(subtyp-def G tdenv (TYP id (tparam ..._n) (STRUCT (typField ...))) (targ ..._n) val)
   (subtyp-struct G tdenv ((tparam targ) ...) (typField ...) val)]
  ;; otherwise
  [(subtyp-def G tdenv typDef (targ ...) val) ERROR])

;;; Extern values

(define-metafunction al
  subtyp-extern : val -> subtypRes
  [(subtyp-extern (EXT json)) (OK #t)]
  ;; otherwise
  [(subtyp-extern val) (OK #f)])

;;; Variants and structs

(define-metafunction al
  subtyp-variant : G tdenv theta (typCase ...) val -> subtypRes
  [(subtyp-variant G tdenv theta (typCase_1 ... (mixop (typ_case ...)) typCase_2 ...) (INJ (mixop (val ...))))
   (subtyp-case G tdenv theta (typ_case ...) (val ...))]
  ;; otherwise
  [(subtyp-variant G tdenv theta (typCase ...) val) (OK #f)])

(define-metafunction al
  subtyp-struct : G tdenv theta (typField ...) val -> subtypRes
  [(subtyp-struct G tdenv theta ((atom typ_field) ...) (STR ((atom val_field) ...)))
   (subtyp-case G tdenv theta (typ_field ...) (val_field ...))]
  ;; otherwise
  [(subtyp-struct G tdenv theta (typField ...) val) (OK #f)])

(define-metafunction al
  subtyp-case : G tdenv theta (typ ...) (val ...) -> subtypRes
  [(subtyp-case G tdenv theta (typ ...) (val ...)) (subtyps G tdenv (typ_subst ...) (val ...))
   (where (OK (typ_subst ...)) (subst-typs theta (typ ...)))]
  ;; otherwise
  [(subtyp-case G tdenv theta (typ ...) (val ...)) ERROR])

;;; Sequences and lists

(define-metafunction al
  subtyps : G tdenv (typ ...) (val ...) -> subtypRes
  [(subtyps G tdenv () ()) (OK #t)]
  [(subtyps G tdenv (typ_h typ_t ...) (val_h val_t ...)) (subtyps G tdenv (typ_t ...) (val_t ...))
   (where (OK #t) (subtyp G tdenv typ_h val_h))]
  [(subtyps G tdenv (typ_h typ_t ...) (val_h val_t ...)) (OK #f)
   (where (OK #f) (subtyp G tdenv typ_h val_h))]
  [(subtyps G tdenv (typ_h typ_t ...) (val_h val_t ...)) ERROR
   (where ERROR (subtyp G tdenv typ_h val_h))]
  [(subtyps G tdenv () (val_h val_t ...)) (OK #f)]
  [(subtyps G tdenv (typ_h typ_t ...) ()) (OK #f)])

(define-metafunction al
  subtyp-list : G tdenv typ (val ...) -> subtypRes
  [(subtyp-list G tdenv typ ()) (OK #t)]
  [(subtyp-list G tdenv typ (val_h val_t ...)) (subtyp-list G tdenv typ (val_t ...))
   (where (OK #t) (subtyp G tdenv typ val_h))]
  [(subtyp-list G tdenv typ (val_h val_t ...)) (OK #f)
   (where (OK #f) (subtyp G tdenv typ val_h))]
  [(subtyp-list G tdenv typ (val_h val_t ...)) ERROR
   (where ERROR (subtyp G tdenv typ val_h))])

;;; Function signatures and values

(define-metafunction al
  param-typ : param -> typ
  [(param-typ (EXP typ)) typ]
  [(param-typ (FUN id (tparam ...) (param ...) typ)) (FUNC (tparam ...) ((param-typ param) ...) typ)])

(define-metafunction al
  func-typ : funcDefn -> typ
  [(func-typ (EXTFUNC id (tparam ...) (param ...) typ)) (FUNC (tparam ...) ((param-typ param) ...) typ)]
  [(func-typ (BUILTINFUNC id (tparam ...) (param ...) typ)) (FUNC (tparam ...) ((param-typ param) ...) typ)]
  [(func-typ (TABLEFUNC id (param ...) typ (tblRow ...))) (FUNC () ((param-typ param) ...) typ)]
  [(func-typ (FUNC id (tparam ...) (param ...) typ (clause ...) (elsClause ...)))
   (FUNC (tparam ...) ((param-typ param) ...) typ)])

(define-metafunction al
  subtyp-func : G tdenv typ id -> subtypRes
  [(subtyp-func G tdenv typ id) (equiv-func-typ G tdenv typ (func-typ funcDefn))
   (where (funcDefn) (find-func G id))]
  ;; otherwise
  [(subtyp-func G tdenv typ id) ERROR])
