#lang racket/base
;; spec-meta/common/5.0-eval-typ.watsup.

(require "0.0-prelude.rkt"
         "0.1-stdlib.rkt"
         "4-relation.rkt")
(provide subst-typ
         subst-typ-inner)

;; Type substitution

(define-dec common-relation
  subst-typ : theta typ -> typ
  [(subst-typ () typ) typ]
  [(subst-typ theta typ) (subst-typ-inner theta typ)
   ;; otherwise
   (side-condition (not (null? (term theta))))])

;; $subst_typ'
(define-dec common-relation
  subst-typ-inner : theta typ -> typ
  [(subst-typ-inner theta NAT) NAT]
  [(subst-typ-inner theta INT) INT]
  [(subst-typ-inner theta TEXT) TEXT]
  [(subst-typ-inner theta BOOL) BOOL]
  [(subst-typ-inner theta (VAR id ())) typ
   (where (typ) (find-map theta id))]
  [(subst-typ-inner theta (VAR id (targ ...))) (VAR id (typ_subst ...))
   (where () (find-map theta id))
   (where (typ_subst ...) ((subst-typ-inner theta targ) ...))]
  [(subst-typ-inner theta (TUP (typ ...))) (TUP (typ_subst ...))
   (where (typ_subst ...) ((subst-typ-inner theta typ) ...))]
  [(subst-typ-inner theta (ITER typ iter)) (ITER typ_subst iter)
   (where typ_subst (subst-typ-inner theta typ))]
  [(subst-typ-inner theta FUNC) FUNC])
