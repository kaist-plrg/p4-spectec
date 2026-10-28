#lang racket/base
;; Mirrors spec-meta/2-runtime/2.5-typ-equiv.watsup.
;;
;; The functions take G and the local tdenv where spec-meta takes their
;; extension $adds_map(C.GLOBAL.TYP, ...): find-typdef finds in G and tdenv
;; what $find_map finds in the extension, and global-ids gives the names the
;; extension holds besides tdenv's.

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.06.1-typ-subst.rkt")
(provide expand-typ
         equiv-typ
         equiv-typs
         equiv-func-typ)

;;; Outer alias expansion

(define-metafunction al
  expand-typ : G tdenv typ -> typRes
  [(expand-typ G tdenv (VAR id (targ ..._n))) (expand-typ G tdenv typ_subst)
   (where ((TYP id_def (tparam ..._n) (ALIAS typ))) (find-typdef G tdenv id))
   (where (OK typ_subst) (subst-typ ((tparam targ) ...) typ))]
  [(expand-typ G tdenv (VAR id ())) (OK (VAR id ()))
   (where (PARAM) (find-typdef G tdenv id))]
  [(expand-typ G tdenv (VAR id ())) (OK (VAR id ()))
   (where ((EXTTYP id_def)) (find-typdef G tdenv id))]
  [(expand-typ G tdenv (VAR id (targ ..._n))) (OK (VAR id (targ ...)))
   (where ((TYP id_def (tparam ..._n) (STRUCT (typField ...)))) (find-typdef G tdenv id))]
  [(expand-typ G tdenv (VAR id (targ ..._n))) (OK (VAR id (targ ...)))
   (where ((TYP id_def (tparam ..._n) (VARIANT (typCase ...)))) (find-typdef G tdenv id))]
  [(expand-typ G tdenv BOOL) (OK BOOL)]
  [(expand-typ G tdenv NAT) (OK NAT)]
  [(expand-typ G tdenv INT) (OK INT)]
  [(expand-typ G tdenv TEXT) (OK TEXT)]
  [(expand-typ G tdenv (TUP (typ ...))) (OK (TUP (typ ...)))]
  [(expand-typ G tdenv (ITER typ iter)) (OK (ITER typ iter))]
  [(expand-typ G tdenv (FUNC (tparam ...) (typ ...) typ_ret)) (OK (FUNC (tparam ...) (typ ...) typ_ret))]
  ;; otherwise
  [(expand-typ G tdenv typ) ERROR])

;;; Structural equivalence

(define-metafunction al
  equiv-typ : G tdenv typ typ -> equivRes
  [(equiv-typ G tdenv typ_l typ_r) (equiv-expanded-typ G tdenv typ_l_expand typ_r_expand)
   (where (OK typ_l_expand) (expand-typ G tdenv typ_l))
   (where (OK typ_r_expand) (expand-typ G tdenv typ_r))]
  ;; otherwise
  [(equiv-typ G tdenv typ_l typ_r) ERROR])

(define-metafunction al
  equiv-typs : G tdenv (typ ...) (typ ...) -> equivRes
  [(equiv-typs G tdenv () ()) (OK #t)]
  [(equiv-typs G tdenv (typ_l typ_l_t ...) (typ_r typ_r_t ...)) (equiv-typs G tdenv (typ_l_t ...) (typ_r_t ...))
   (where (OK #t) (equiv-typ G tdenv typ_l typ_r))]
  [(equiv-typs G tdenv (typ_l typ_l_t ...) (typ_r typ_r_t ...)) (OK #f)
   (where (OK #f) (equiv-typ G tdenv typ_l typ_r))]
  [(equiv-typs G tdenv (typ_l typ_l_t ...) (typ_r typ_r_t ...)) ERROR
   (where ERROR (equiv-typ G tdenv typ_l typ_r))]
  [(equiv-typs G tdenv () (typ_r typ_r_t ...)) (OK #f)]
  [(equiv-typs G tdenv (typ_l typ_l_t ...) ()) (OK #f)])

;; $equiv_typ': native structural equivalence excludes nested function types;
;; only function-value membership invokes signature alpha-equivalence
(define-metafunction al
  equiv-expanded-typ : G tdenv typ typ -> equivRes
  [(equiv-expanded-typ G tdenv BOOL BOOL) (OK #t)]
  [(equiv-expanded-typ G tdenv NAT NAT) (OK #t)]
  [(equiv-expanded-typ G tdenv INT INT) (OK #t)]
  [(equiv-expanded-typ G tdenv TEXT TEXT) (OK #t)]
  [(equiv-expanded-typ G tdenv (VAR id (targ_l ...)) (VAR id (targ_r ...)))
   (equiv-typs G tdenv (targ_l ...) (targ_r ...))]
  [(equiv-expanded-typ G tdenv (TUP (typ_l ...)) (TUP (typ_r ...)))
   (equiv-typs G tdenv (typ_l ...) (typ_r ...))]
  [(equiv-expanded-typ G tdenv (ITER typ_l iter) (ITER typ_r iter))
   (equiv-typ G tdenv typ_l typ_r)]
  ;; otherwise
  [(equiv-expanded-typ G tdenv typ_l typ_r) (OK #f)])

;;; Function signature alpha-equivalence

(define-metafunction al
  equiv-func-typ : G tdenv typ typ -> equivRes
  [(equiv-func-typ G tdenv
                   (FUNC (tparam_l ..._n) (typ_l ..._m) typ_ret_l)
                   (FUNC (tparam_r ..._n) (typ_r ..._m) typ_ret_r))
   (equiv-typs G tdenv_fresh (typ_l_subst ... typ_ret_l_subst) (typ_r_subst ... typ_ret_r_subst))
   (where ((id typDef) ...) tdenv)
   (where (id_used ...) ,(append (global-ids (term G) 'TYP)
                                 (term (id ...))
                                 (term (ids-typ (FUNC (tparam_l ...) (typ_l ...) typ_ret_l)))
                                 (term (ids-typ (FUNC (tparam_r ...) (typ_r ...) typ_ret_r)))))
   (where (tparam_fresh ...) (fresh-ids (tparam_l ...) (id_used ...)))
   (where theta_l ((tparam_l (VAR tparam_fresh ())) ...))
   (where theta_r ((tparam_r (VAR tparam_fresh ())) ...))
   (where (OK (typ_l_subst ...)) (subst-typs theta_l (typ_l ...)))
   (where (OK (typ_r_subst ...)) (subst-typs theta_r (typ_r ...)))
   (where (OK typ_ret_l_subst) (subst-typ theta_l typ_ret_l))
   (where (OK typ_ret_r_subst) (subst-typ theta_r typ_ret_r))
   (where tdenv_fresh (adds-map tdenv (tparam_fresh ...) (repeat- PARAM ,(length (term (tparam_fresh ...))))))]
  [(equiv-func-typ G tdenv
                   (FUNC (tparam_l ...) (typ_l ...) typ_ret_l)
                   (FUNC (tparam_r ...) (typ_r ...) typ_ret_r))
   (OK #f)
   (side-condition (not (and (= (length (term (tparam_l ...))) (length (term (tparam_r ...))))
                             (= (length (term (typ_l ...))) (length (term (typ_r ...)))))))]
  ;; otherwise
  [(equiv-func-typ G tdenv typ_l typ_r) ERROR])
