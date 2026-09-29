#lang racket/base
;; spec-meta/al/5.1-eval-typ.watsup.
;;
;; A clause whose premise results decide between it and `otherwise` computes
;; them once, and a helper named after the clause branches on them.

(require "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/5.0-eval-typ.rkt"
         "../common/5.1-eval-ops.rkt"
         "3-context.rkt")
(provide upcast
         downcast
         subtyp
         subtyps)

;;
;; Type casts
;;

;;; Upcasts

(define-dec AL-context
  upcast : ctx typ val -> valres
  [(upcast C INT (INT i)) (OK (INT i))]
  [(upcast C INT (NAT n)) (OK (INT n))]
  [(upcast C INT val) FAIL
   (side-condition (not (redex-match? AL-context num (term val))))]
  [(upcast C (VAR id (targ ...)) val) (upcast/var C (typdef ...) (targ ...) val)
   (where (typdef ...) (find_typ C id))]
  [(upcast C (TUP (typ ..._n)) (TUP (val ..._n))) (upcast/tup (TUP (val ...)) (valres ...))
   (where (valres ...) ((upcast C typ val) ...))]
  [(upcast C (TUP (typ ...)) val) FAIL
   (where #f (is_tup val))]
  [(upcast C (ITER typ QUEST) (OPT (val ...))) (upcast/opt (OPT (val ...)) (valres ...))
   (where (valres ...) ((upcast C typ val) ...))]
  [(upcast C (ITER typ STAR) (LIST (val ...))) (upcast/list (LIST (val ...)) (valres ...))
   (where (valres ...) ((upcast C typ val) ...))]
  ;; otherwise, for the inputs no clause above applies to
  [(upcast C typ val) (OK val)
   (side-condition (memq (term typ) '(BOOL NAT TEXT FUNC)))]
  [(upcast C (TUP (typ ...)) (TUP (val ...))) (OK (TUP (val ...)))
   (side-condition (not (= (length (term (typ ...))) (length (term (val ...))))))]
  [(upcast C (ITER typ QUEST) val) (OK val)
   (side-condition (not (redex-match? AL-context (OPT (val ...)) (term val))))]
  [(upcast C (ITER typ STAR) val) (OK val)
   (side-condition (not (redex-match? AL-context (LIST (val ...)) (term val))))])

;; The VAR clause of $upcast, given the type definition found for id.
(define-dec AL-context
  upcast/var : ctx (typdef ...) (targ ...) val -> valres
  [(upcast/var C ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val) (upcast C typ_1 val)
   (where theta ((tparam targ) ...))
   (where typ_1 (subst_typ theta typ))]
  [(upcast/var C ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val) (OK val)
   ;; otherwise: the substitution fails
   (where theta ((tparam targ) ...))
   (side-condition (not (redex-match? AL-context typ (term (subst_typ theta typ)))))]
  [(upcast/var C (typdef ...) (targ ...) val) (OK val)
   ;; otherwise: id is not an alias with as many parameters as targ*
   (side-condition
    (not (redex-match? AL-context (((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n))
                       (term ((typdef ...) (targ ...))))))])

;; The TUP clause of $upcast, given the upcasts of the components.
(define-dec AL-context
  upcast/tup : val (valres ...) -> valres
  [(upcast/tup val ((OK val_upcast) ...)) (OK (TUP (val_upcast ...)))]
  [(upcast/tup val (valres ...)) (OK val)
   ;; otherwise: some component upcast fails
   (side-condition (not (redex-match? AL-context ((OK val) ...) (term (valres ...)))))])

;; The OPT clause of $upcast, given the upcast of the component, if any.
(define-dec AL-context
  upcast/opt : val (valres ...) -> valres
  [(upcast/opt val ((OK val_upcast) ...)) (OK (OPT (val_upcast ...)))]
  [(upcast/opt val (valres ...)) (OK val)
   ;; otherwise: the component upcast fails
   (side-condition (not (redex-match? AL-context ((OK val) ...) (term (valres ...)))))])

;; The LIST clause of $upcast, given the upcasts of the elements.
(define-dec AL-context
  upcast/list : val (valres ...) -> valres
  [(upcast/list val ((OK val_upcast) ...)) (OK (LIST (val_upcast ...)))]
  [(upcast/list val (valres ...)) (OK val)
   ;; otherwise: some element upcast fails
   (side-condition (not (redex-match? AL-context ((OK val) ...) (term (valres ...)))))])

;;; Downcasts

(define-dec AL-context
  downcast : ctx typ val -> valres
  [(downcast C NAT (NAT n)) (OK (NAT n))]
  [(downcast C NAT (INT i)) (OK (NAT n))
   (where n i)]
  [(downcast C NAT val) FAIL
   (side-condition (not (redex-match? AL-context num (term val))))]
  [(downcast C (VAR id (targ ...)) val) (downcast/var C (typdef ...) (targ ...) val)
   (where (typdef ...) (find_typ C id))]
  [(downcast C (TUP (typ ..._n)) (TUP (val ..._n)))
   (downcast/tup (TUP (val ...)) (valres ...))
   (where (valres ...) ((downcast C typ val) ...))]
  [(downcast C (TUP (typ ...)) val) FAIL
   (where #f (is_tup val))]
  [(downcast C (ITER typ QUEST) (OPT (val ...))) (downcast/opt (OPT (val ...)) (valres ...))
   (where (valres ...) ((downcast C typ val) ...))]
  [(downcast C (ITER typ STAR) (LIST (val ...))) (downcast/list (LIST (val ...)) (valres ...))
   (where (valres ...) ((downcast C typ val) ...))]
  ;; otherwise, for the inputs no clause above applies to
  [(downcast C NAT (INT i)) (OK (INT i))
   (side-condition (negative? (term i)))]
  [(downcast C typ val) (OK val)
   (side-condition (memq (term typ) '(BOOL INT TEXT FUNC)))]
  [(downcast C (TUP (typ ...)) (TUP (val ...))) (OK (TUP (val ...)))
   (side-condition (not (= (length (term (typ ...))) (length (term (val ...))))))]
  [(downcast C (ITER typ QUEST) val) (OK val)
   (side-condition (not (redex-match? AL-context (OPT (val ...)) (term val))))]
  [(downcast C (ITER typ STAR) val) (OK val)
   (side-condition (not (redex-match? AL-context (LIST (val ...)) (term val))))])

;; The VAR clause of $downcast, given the type definition found for id.
(define-dec AL-context
  downcast/var : ctx (typdef ...) (targ ...) val -> valres
  [(downcast/var C ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val) (downcast C typ_1 val)
   (where theta ((tparam targ) ...))
   (where typ_1 (subst_typ theta typ))]
  [(downcast/var C ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val) (OK val)
   ;; otherwise: the substitution fails
   (where theta ((tparam targ) ...))
   (side-condition (not (redex-match? AL-context typ (term (subst_typ theta typ)))))]
  [(downcast/var C (typdef ...) (targ ...) val) (OK val)
   ;; otherwise: id is not an alias with as many parameters as targ*
   (side-condition
    (not (redex-match? AL-context (((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n))
                       (term ((typdef ...) (targ ...))))))])

;; The TUP clause of $downcast, given the downcasts of the components.
(define-dec AL-context
  downcast/tup : val (valres ...) -> valres
  [(downcast/tup val ((OK val_downcast) ...)) (OK (TUP (val_downcast ...)))]
  [(downcast/tup val (valres ...)) (OK val)
   ;; otherwise: some component downcast fails
   (side-condition (not (redex-match? AL-context ((OK val) ...) (term (valres ...)))))])

;; The OPT clause of $downcast, given the downcast of the component, if any.
(define-dec AL-context
  downcast/opt : val (valres ...) -> valres
  [(downcast/opt val ((OK val_downcast) ...)) (OK (OPT (val_downcast ...)))]
  [(downcast/opt val (valres ...)) (OK val)
   ;; otherwise: the component downcast fails
   (side-condition (not (redex-match? AL-context ((OK val) ...) (term (valres ...)))))])

;; The LIST clause of $downcast, given the downcasts of the elements.
(define-dec AL-context
  downcast/list : val (valres ...) -> valres
  [(downcast/list val ((OK val_downcast) ...)) (OK (LIST (val_downcast ...)))]
  [(downcast/list val (valres ...)) (OK val)
   ;; otherwise: some element downcast fails
   (side-condition (not (redex-match? AL-context ((OK val) ...) (term (valres ...)))))])

;;
;; Value-type matching
;;

(define-dec AL-context
  subtyp : tdenv typ val -> bool
  [(subtyp tdenv BOOL (BOOL b)) #t]
  [(subtyp tdenv NAT (NAT n)) #t]
  [(subtyp tdenv NAT (INT i)) ,(>= (term i) 0)]
  [(subtyp tdenv INT num) #t]
  [(subtyp tdenv TEXT (TEXT t)) #t]
  [(subtyp tdenv (VAR id (targ ...)) val) (subtyp/var tdenv (typdef ...) (targ ...) val)
   (where (typdef ...) (find_map tdenv id))]
  [(subtyp tdenv (TUP (typ ..._n)) (TUP (val ..._n))) (forall_ (b ...))
   (where (b ...) ((subtyp tdenv typ val) ...))]
  [(subtyp tdenv (ITER typ QUEST) (OPT (val))) (subtyp tdenv typ val)]
  [(subtyp tdenv (ITER typ QUEST) (OPT ())) #t]
  [(subtyp tdenv (ITER typ STAR) (LIST (val ...))) (forall_ (b ...))
   (where (b ...) ((subtyp tdenv typ val) ...))]
  [(subtyp tdenv typ val) #f
   ;; otherwise: no pattern above matches
   (side-condition (not (redex-match? AL-context (BOOL (BOOL b)) (term (typ val)))))
   (side-condition (not (redex-match? AL-context (NAT (NAT n)) (term (typ val)))))
   (side-condition (not (redex-match? AL-context (NAT (INT i)) (term (typ val)))))
   (side-condition (not (redex-match? AL-context (INT num) (term (typ val)))))
   (side-condition (not (redex-match? AL-context (TEXT (TEXT t)) (term (typ val)))))
   (side-condition (not (redex-match? AL-context ((VAR id (targ ...)) val) (term (typ val)))))
   (side-condition
    (not (redex-match? AL-context ((TUP (typ ..._n)) (TUP (val ..._n))) (term (typ val)))))
   (side-condition
    (not (redex-match? AL-context ((ITER typ QUEST) (OPT (val))) (term (typ val)))))
   (side-condition
    (not (redex-match? AL-context ((ITER typ QUEST) (OPT ())) (term (typ val)))))
   (side-condition
    (not (redex-match? AL-context ((ITER typ STAR) (LIST (val ...))) (term (typ val)))))])

;; The VAR clauses of $subtyp, given the type definition found for id. They
;; are disjoint by its kind.
(define-dec AL-context
  subtyp/var : tdenv (typdef ...) (targ ...) val -> bool
  [(subtyp/var tdenv (EXT) (targ ...) (EXT json)) #t]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val)
   (subtyp tdenv typ_subst val)
   (where theta ((tparam targ) ...))
   (where typ_subst (subst_typ theta typ))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (VARIANT ((mixop_case (typ_case ...)) ...))))
               (targ ..._n) (INJ (mixop (val ..._m))))
   (forall_ (b ...))
   (where theta ((tparam targ) ...))
   (side-condition (member (term mixop) (term (mixop_case ...))))
   (where ((typ_case_found ...)) (assoc_ mixop ((mixop_case (typ_case ...)) ...)))
   (where (typ_case_found_subst ..._m) ((subst_typ theta typ_case_found) ...))
   (where (b ...) ((subtyp tdenv typ_case_found_subst val) ...))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (STRUCT ((atom typ_field) ...))))
               (targ ..._n) (STR ((atom val_field) ...)))
   (forall_ (b ...))
   (where theta ((tparam targ) ...))
   (where (typ_field_subst ...) ((subst_typ theta typ_field) ...))
   (where (b ...) ((subtyp tdenv typ_field_subst val_field) ...))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val) #f
   ;; otherwise: the substitution fails
   (where theta ((tparam targ) ...))
   (side-condition (not (redex-match? AL-context typ (term (subst_typ theta typ)))))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (VARIANT ((mixop_case (typ_case ...)) ...))))
               (targ ..._n) (INJ (mixop (val ...))))
   #f
   ;; otherwise: mixop is not a case
   (side-condition (not (member (term mixop) (term (mixop_case ...)))))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (VARIANT ((mixop_case (typ_case ...)) ...))))
               (targ ..._n) (INJ (mixop (val ...))))
   #f
   ;; otherwise: the case's types fail to substitute, or are not as many as val*
   (where theta ((tparam targ) ...))
   (side-condition (member (term mixop) (term (mixop_case ...))))
   (where ((typ_case_found ...)) (assoc_ mixop ((mixop_case (typ_case ...)) ...)))
   (side-condition
    (not (redex-match? AL-context ((typ ..._m) (val ..._m))
                       (term (((subst_typ theta typ_case_found) ...) (val ...))))))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (STRUCT ((atom typ_field) ...))))
               (targ ..._n) (STR ((atom val_field) ...)))
   #f
   ;; otherwise: a field's type fails to substitute
   (where theta ((tparam targ) ...))
   (side-condition
    (not (redex-match? AL-context (typ ...) (term ((subst_typ theta typ_field) ...)))))]
  [(subtyp/var tdenv (typdef ...) (targ ...) val) #f
   ;; otherwise: no pattern above matches
   (side-condition
    (not (redex-match? AL-context ((EXT) (targ ...) (EXT json))
                       (term ((typdef ...) (targ ...) val)))))
   (side-condition
    (not (redex-match? AL-context (((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val)
                       (term ((typdef ...) (targ ...) val)))))
   (side-condition
    (not (redex-match? AL-context (((DEF (tparam ..._n) (VARIANT (typcase ...))))
                                   (targ ..._n) (INJ valcase))
                       (term ((typdef ...) (targ ...) val)))))
   (side-condition
    (not (redex-match? AL-context (((DEF (tparam ..._n) (STRUCT ((atom typ) ...))))
                                   (targ ..._n) (STR ((atom val) ...)))
                       (term ((typdef ...) (targ ...) val)))))])

(define-dec AL-context
  subtyps : tdenv (typ ...) (val ...) -> bool
  [(subtyps tdenv (typ ..._n) (val ..._n)) (forall_ (b ...))
   (where (b ...) ((subtyp tdenv typ val) ...))]
  [(subtyps tdenv (typ ...) (val ...)) #f
   ;; otherwise: the lengths differ
   (side-condition (not (= (length (term (typ ...))) (length (term (val ...))))))])
