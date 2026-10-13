#lang racket/base
;; spec-meta/common/5.0-eval-typ.watsup.

(require redex/reduction-semantics
         "0.1-stdlib.rkt"
         "4-relation.rkt")
(provide subst-typ
         subst-typ-inner
         subst-typs)

;; Type substitution

(define-judgment-form common-relation
  #:mode (subst-typ I I O)
  #:contract (subst-typ theta typ typ)
  [(subst-typ () typ typ)]
  [(side-condition ,(pair? (term theta)))
   (subst-typ-inner theta typ typ_subst)
   ----------------------------------------
   (subst-typ theta typ typ_subst)])

;; $subst_typ'
(define-judgment-form common-relation
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
  [(subst-typ-inner theta FUNC FUNC)])

;; $subst_typ on each type, for a caller that iterates it. Not a watsup def:
;; judgment-holds takes no ellipsis.
(define-judgment-form common-relation
  #:mode (subst-typs I I O)
  #:contract (subst-typs theta (typ ...) (typ ...))
  [(subst-typ theta typ typ_subst) ...
   ----------------------------------------
   (subst-typs theta (typ ...) (typ_subst ...))])
