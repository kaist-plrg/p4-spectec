#lang racket/base
;; spec-meta/common/5.0-eval-typ.watsup.

(require "0.0-prelude.rkt"
         "0.1-stdlib.rkt"
         "4-relation.rkt")
(provide subst-typ
         subst-type-inner)

;; Type substitution

(define-dec common-relation
  subst-typ : theta typ -> typ
  [(subst-typ () typ) typ]
  [(subst-typ theta typ) (subst-type-inner theta typ)
   ;; otherwise
   (side-condition (not (null? (term theta))))])

(define-dec common-relation
  subst-type-inner : theta typ -> typ
  [(subst-type-inner theta NAT) NAT]
  [(subst-type-inner theta INT) INT]
  [(subst-type-inner theta TEXT) TEXT]
  [(subst-type-inner theta BOOL) BOOL]
  [(subst-type-inner theta (VAR id ())) typ
   (where (typ) (find-map theta id))]
  [(subst-type-inner theta (VAR id (targ ...))) (VAR id (typ_subst ...))
   (where () (find-map theta id))
   (where (typ_subst ...) ((subst-type-inner theta targ) ...))]
  [(subst-type-inner theta (TUP (typ ...))) (TUP (typ_subst ...))
   (where (typ_subst ...) ((subst-type-inner theta typ) ...))]
  [(subst-type-inner theta (ITER typ iter)) (ITER typ_subst iter)
   (where typ_subst (subst-type-inner theta typ))]
  [(subst-type-inner theta FUNC) FUNC])
