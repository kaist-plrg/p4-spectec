#lang racket/base
;; spec-meta/common/5.0-eval-typ.watsup.

(require "0-prelude.rkt"
         "0-stdlib.rkt"
         "4-relation.rkt")
(provide subst_typ
         subst_type_inner)

;; Type substitution

(define-dec Common-relation
  subst_typ : theta typ -> typ
  [(subst_typ () typ) typ]
  [(subst_typ theta typ) (subst_type_inner theta typ)
   ;; otherwise
   (side-condition (not (null? (term theta))))])

(define-dec Common-relation
  subst_type_inner : theta typ -> typ
  [(subst_type_inner theta NAT) NAT]
  [(subst_type_inner theta INT) INT]
  [(subst_type_inner theta TEXT) TEXT]
  [(subst_type_inner theta BOOL) BOOL]
  [(subst_type_inner theta (VAR id ())) typ
   (where (typ) (find_map theta id))]
  [(subst_type_inner theta (VAR id (targ ...))) (VAR id (typ_subst ...))
   (where () (find_map theta id))
   (where (typ_subst ...) ((subst_type_inner theta targ) ...))]
  [(subst_type_inner theta (TUP (typ ...))) (TUP (typ_subst ...))
   (where (typ_subst ...) ((subst_type_inner theta typ) ...))]
  [(subst_type_inner theta (ITER typ iter)) (ITER typ_subst iter)
   (where typ_subst (subst_type_inner theta typ))]
  [(subst_type_inner theta FUNC) FUNC])
