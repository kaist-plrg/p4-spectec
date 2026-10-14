#lang racket/base
;; spec-meta/al/5.1-eval-typ.watsup.
;;
;; A clause whose premise results decide between it and `otherwise` computes
;; them once, and a helper branches on them.

(require redex/reduction-semantics
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

(define-metafunction al-context
  upcast : G L typ val -> valres
  [(upcast G L INT (INT i)) (OK (INT i))]
  [(upcast G L INT (NAT n)) (OK (INT n))]
  [(upcast G L INT val) FAIL
   (side-condition (not (redex-match? al-context num (term val))))]
  [(upcast G L (VAR id (targ ...)) val) (upcast/var G L (find-typ G L id) (targ ...) val)]
  [(upcast G L (TUP (typ ..._n)) (TUP (val ..._n)))
   (cast-components (TUP (val ...)) ((upcast G L typ val) ...))]
  [(upcast G L (TUP (typ ...)) val) FAIL
   (where #f (is-tup val))]
  [(upcast G L (ITER typ QUEST) (OPT (val ...)))
   (cast-components (OPT (val ...)) ((upcast G L typ val) ...))]
  [(upcast G L (ITER typ STAR) (LIST (val ...)))
   (cast-components (LIST (val ...)) ((upcast G L typ val) ...))]
  ;; otherwise, for the inputs no clause above applies to
  [(upcast G L BOOL val) (OK val)]
  [(upcast G L NAT val) (OK val)]
  [(upcast G L TEXT val) (OK val)]
  [(upcast G L FUNC val) (OK val)]
  [(upcast G L (TUP (typ ...)) (TUP (val ...))) (OK (TUP (val ...)))
   (side-condition (not (= (length (term (typ ...))) (length (term (val ...))))))]
  [(upcast G L (ITER typ QUEST) val) (OK val)
   (side-condition (not (redex-match? al-context (OPT (val ...)) (term val))))]
  [(upcast G L (ITER typ STAR) val) (OK val)
   (side-condition (not (redex-match? al-context (LIST (val ...)) (term val))))])

;; The VAR clause of $upcast, given the type definition found for id.
(define-metafunction al-context
  upcast/var : G L (typdef ...) (targ ...) val -> valres
  [(upcast/var G L ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val) (upcast G L typ_1 val)
   (where theta ((tparam targ) ...))
   (judgment-holds (subst-typ theta typ typ_1))]
  [(upcast/var G L ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val) (OK val)
   ;; otherwise: the substitution fails
   (where theta ((tparam targ) ...))
   (side-condition (not (judgment-holds (subst-typ theta typ any))))]
  [(upcast/var G L (typdef ...) (targ ...) val) (OK val)
   ;; otherwise: id is not an alias with as many parameters as targ*
   (side-condition
    (not (redex-match? al-context (((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n))
                       (term ((typdef ...) (targ ...))))))])

;;; Downcasts

(define-metafunction al-context
  downcast : G L typ val -> valres
  [(downcast G L NAT (NAT n)) (OK (NAT n))]
  [(downcast G L NAT (INT n)) (OK (NAT n))]
  [(downcast G L NAT val) FAIL
   (side-condition (not (redex-match? al-context num (term val))))]
  [(downcast G L (VAR id (targ ...)) val) (downcast/var G L (find-typ G L id) (targ ...) val)]
  [(downcast G L (TUP (typ ..._n)) (TUP (val ..._n)))
   (cast-components (TUP (val ...)) ((downcast G L typ val) ...))]
  [(downcast G L (TUP (typ ...)) val) FAIL
   (where #f (is-tup val))]
  [(downcast G L (ITER typ QUEST) (OPT (val ...)))
   (cast-components (OPT (val ...)) ((downcast G L typ val) ...))]
  [(downcast G L (ITER typ STAR) (LIST (val ...)))
   (cast-components (LIST (val ...)) ((downcast G L typ val) ...))]
  ;; otherwise, for the inputs no clause above applies to
  [(downcast G L NAT (INT i)) (OK (INT i))
   (side-condition (negative? (term i)))]
  [(downcast G L BOOL val) (OK val)]
  [(downcast G L INT val) (OK val)]
  [(downcast G L TEXT val) (OK val)]
  [(downcast G L FUNC val) (OK val)]
  [(downcast G L (TUP (typ ...)) (TUP (val ...))) (OK (TUP (val ...)))
   (side-condition (not (= (length (term (typ ...))) (length (term (val ...))))))]
  [(downcast G L (ITER typ QUEST) val) (OK val)
   (side-condition (not (redex-match? al-context (OPT (val ...)) (term val))))]
  [(downcast G L (ITER typ STAR) val) (OK val)
   (side-condition (not (redex-match? al-context (LIST (val ...)) (term val))))])

;; The VAR clause of $downcast, given the type definition found for id.
(define-metafunction al-context
  downcast/var : G L (typdef ...) (targ ...) val -> valres
  [(downcast/var G L ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val) (downcast G L typ_1 val)
   (where theta ((tparam targ) ...))
   (judgment-holds (subst-typ theta typ typ_1))]
  [(downcast/var G L ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val) (OK val)
   ;; otherwise: the substitution fails
   (where theta ((tparam targ) ...))
   (side-condition (not (judgment-holds (subst-typ theta typ any))))]
  [(downcast/var G L (typdef ...) (targ ...) val) (OK val)
   ;; otherwise: id is not an alias with as many parameters as targ*
   (side-condition
    (not (redex-match? al-context (((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n))
                       (term ((typdef ...) (targ ...))))))])

;;; Components

;; The TUP, OPT, and LIST clauses of $upcast and $downcast, given the casts of
;; the components: the value of the cast components if every cast succeeds,
;; and the value itself otherwise.
(define-metafunction al-context
  cast-components : val (valres ...) -> valres
  [(cast-components (TUP (val ...)) ((OK val_cast) ...)) (OK (TUP (val_cast ...)))]
  [(cast-components (OPT (val ...)) ((OK val_cast) ...)) (OK (OPT (val_cast ...)))]
  [(cast-components (LIST (val ...)) ((OK val_cast) ...)) (OK (LIST (val_cast ...)))]
  [(cast-components val (valres ...)) (OK val)
   ;; otherwise: some cast fails
   (side-condition (not (redex-match? al-context ((OK val) ...) (term (valres ...)))))])

;;
;; Value-type matching
;;

(define-metafunction al-context
  subtyp : tdenv typ val -> bool
  [(subtyp tdenv BOOL (BOOL b)) #t]
  [(subtyp tdenv NAT (NAT n)) #t]
  [(subtyp tdenv NAT (INT i)) ,(>= (term i) 0)]
  [(subtyp tdenv INT num) #t]
  [(subtyp tdenv TEXT (TEXT t)) #t]
  [(subtyp tdenv (VAR id (targ ...)) val) (subtyp/var tdenv (find-map tdenv id) (targ ...) val)]
  [(subtyp tdenv (TUP (typ ..._n)) (TUP (val ..._n))) (forall- ((subtyp tdenv typ val) ...))]
  [(subtyp tdenv (ITER typ QUEST) (OPT (val))) (subtyp tdenv typ val)]
  [(subtyp tdenv (ITER typ QUEST) (OPT ())) #t]
  [(subtyp tdenv (ITER typ STAR) (LIST (val ...))) (forall- ((subtyp tdenv typ val) ...))]
  [(subtyp tdenv typ val) #f
   ;; otherwise: no pattern above matches
   (side-condition (not (redex-match? al-context (BOOL (BOOL b)) (term (typ val)))))
   (side-condition (not (redex-match? al-context (NAT (NAT n)) (term (typ val)))))
   (side-condition (not (redex-match? al-context (NAT (INT i)) (term (typ val)))))
   (side-condition (not (redex-match? al-context (INT num) (term (typ val)))))
   (side-condition (not (redex-match? al-context (TEXT (TEXT t)) (term (typ val)))))
   (side-condition (not (redex-match? al-context ((VAR id (targ ...)) val) (term (typ val)))))
   (side-condition
    (not (redex-match? al-context ((TUP (typ ..._n)) (TUP (val ..._n))) (term (typ val)))))
   (side-condition
    (not (redex-match? al-context ((ITER typ QUEST) (OPT (val))) (term (typ val)))))
   (side-condition
    (not (redex-match? al-context ((ITER typ QUEST) (OPT ())) (term (typ val)))))
   (side-condition
    (not (redex-match? al-context ((ITER typ STAR) (LIST (val ...))) (term (typ val)))))])

;; The VAR clauses of $subtyp, given the type definition found for id. They
;; are disjoint by its kind.
(define-metafunction al-context
  subtyp/var : tdenv (typdef ...) (targ ...) val -> bool
  [(subtyp/var tdenv (EXT) (targ ...) (EXT json)) #t]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val)
   (subtyp tdenv typ_subst val)
   (where theta ((tparam targ) ...))
   (judgment-holds (subst-typ theta typ typ_subst))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (VARIANT (typcase ...))))
               (targ ..._n) (INJ (mixop (val ..._m))))
   (forall- ((subtyp tdenv typ_case_found_subst val) ...))
   (where theta ((tparam targ) ...))
   (where (mixop (typ_case_found ..._m)) ,(assoc (term mixop) (term (typcase ...))))
   (judgment-holds (subst-typs theta (typ_case_found ...) (typ_case_found_subst ...)))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (STRUCT ((atom typ_field) ...))))
               (targ ..._n) (STR ((atom val_field) ...)))
   (forall- ((subtyp tdenv typ_field_subst val_field) ...))
   (where theta ((tparam targ) ...))
   (judgment-holds (subst-typs theta (typ_field ...) (typ_field_subst ...)))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val) #f
   ;; otherwise: the substitution fails
   (where theta ((tparam targ) ...))
   (side-condition (not (judgment-holds (subst-typ theta typ any))))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (VARIANT (typcase ...))))
               (targ ..._n) (INJ (mixop (val ...))))
   #f
   ;; otherwise: mixop is not a case
   (where #f ,(assoc (term mixop) (term (typcase ...))))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (VARIANT (typcase ...))))
               (targ ..._n) (INJ (mixop (val ...))))
   #f
   ;; otherwise: the case's types are not as many as val*
   (where (mixop (typ_case_found ...)) ,(assoc (term mixop) (term (typcase ...))))
   (side-condition
    (not (= (length (term (typ_case_found ...))) (length (term (val ...))))))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (VARIANT (typcase ...))))
               (targ ..._n) (INJ (mixop (val ..._m))))
   #f
   ;; otherwise: the case's types fail to substitute
   (where theta ((tparam targ) ...))
   (where (mixop (typ_case_found ..._m)) ,(assoc (term mixop) (term (typcase ...))))
   (side-condition (not (judgment-holds (subst-typs theta (typ_case_found ...) any))))]
  [(subtyp/var tdenv ((DEF (tparam ..._n) (STRUCT ((atom typ_field) ...))))
               (targ ..._n) (STR ((atom val_field) ...)))
   #f
   ;; otherwise: a field's type fails to substitute
   (where theta ((tparam targ) ...))
   (side-condition (not (judgment-holds (subst-typs theta (typ_field ...) any))))]
  [(subtyp/var tdenv (typdef ...) (targ ...) val) #f
   ;; otherwise: no pattern above matches
   (side-condition
    (not (redex-match? al-context ((EXT) (targ ...) (EXT json))
                       (term ((typdef ...) (targ ...) val)))))
   (side-condition
    (not (redex-match? al-context (((DEF (tparam ..._n) (ALIAS typ))) (targ ..._n) val)
                       (term ((typdef ...) (targ ...) val)))))
   (side-condition
    (not (redex-match? al-context (((DEF (tparam ..._n) (VARIANT (typcase ...))))
                                   (targ ..._n) (INJ valcase))
                       (term ((typdef ...) (targ ...) val)))))
   (side-condition
    (not (redex-match? al-context (((DEF (tparam ..._n) (STRUCT ((atom typ) ...))))
                                   (targ ..._n) (STR ((atom val) ...)))
                       (term ((typdef ...) (targ ...) val)))))])

(define-metafunction al-context
  subtyps : tdenv (typ ...) (val ...) -> bool
  [(subtyps tdenv (typ ..._n) (val ..._n)) (forall- ((subtyp tdenv typ val) ...))]
  [(subtyps tdenv (typ ...) (val ...)) #f
   ;; otherwise: the lengths differ
   (side-condition (not (= (length (term (typ ...))) (length (term (val ...))))))])
