#lang racket/base
;; spec-meta/3-execution/3.05.1-typ-subst.watsup.

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "3.03-machine.rkt")
(provide theta-of-tdenv
         ids-typ
         ids-typs
         fresh-ids
         subst-typ
         subst-typs)

;;; Substitution environments

(define-metafunction al
  theta-of-tdenv : tdenv -> theta
  [(theta-of-tdenv ()) (empty-map)]
  [(theta-of-tdenv ((id_h (TYP id () (ALIAS typ_h))) (id_t typDef_t) ...))
   (add-map (theta-of-tdenv ((id_t typDef_t) ...)) id_h typ_h)]
  [(theta-of-tdenv ((id_h PARAM) (id_t typDef_t) ...))
   (theta-of-tdenv ((id_t typDef_t) ...))]
  [(theta-of-tdenv ((id_h (EXTTYP id)) (id_t typDef_t) ...))
   (theta-of-tdenv ((id_t typDef_t) ...))]
  [(theta-of-tdenv ((id_h (TYP id (tparam_h tparam_t ...) (ALIAS typ))) (id_t typDef_t) ...))
   (theta-of-tdenv ((id_t typDef_t) ...))]
  [(theta-of-tdenv ((id_h (TYP id (tparam ...) (STRUCT (typField ...)))) (id_t typDef_t) ...))
   (theta-of-tdenv ((id_t typDef_t) ...))]
  [(theta-of-tdenv ((id_h (TYP id (tparam ...) (VARIANT (typCase ...)))) (id_t typDef_t) ...))
   (theta-of-tdenv ((id_t typDef_t) ...))])

;;; Type names and fresh binders

(define-metafunction al
  ids-typ : typ -> (id ...)
  [(ids-typ (VAR id (typ ...))) (id id_arg ...)
   (where (id_arg ...) (ids-typs (typ ...)))]
  [(ids-typ (TUP (typ ...))) (ids-typs (typ ...))]
  [(ids-typ (ITER typ iter)) (ids-typ typ)]
  [(ids-typ (FUNC (tparam ...) (typ ...) typ_ret)) (tparam ... id_param ... id_ret ...)
   (where (id_param ...) (ids-typs (typ ...)))
   (where (id_ret ...) (ids-typ typ_ret))]
  ;; otherwise
  [(ids-typ typ) ()])

(define-metafunction al
  ids-typs : (typ ...) -> (id ...)
  [(ids-typs (typ ...)) (id ... ...)
   (where ((id ...) ...) ((ids-typ typ) ...))])

(define-metafunction al
  fresh-id : id (id ...) -> id
  [(fresh-id id (id_used ...)) id
   (side-condition (not (member (term id) (term (id_used ...)))))]
  ;; otherwise
  [(fresh-id id (id_used ...)) (fresh-id ,(string-append (term id) "'") (id_used ...))])

(define-metafunction al
  fresh-ids : (id ...) (id ...) -> (id ...)
  [(fresh-ids () (id_used ...)) ()]
  [(fresh-ids (id_h id_t ...) (id_used ...)) (id_fresh id_fresh_t ...)
   (where id_fresh (fresh-id id_h (id_used ...)))
   (where (id_fresh_t ...) (fresh-ids (id_t ...) (id_fresh id_used ...)))])

;;; Substitution in types and type sequences

(define-metafunction al
  subst-typ : theta typ -> typRes
  [(subst-typ theta NAT) (OK NAT)]
  [(subst-typ theta INT) (OK INT)]
  [(subst-typ theta TEXT) (OK TEXT)]
  [(subst-typ theta BOOL) (OK BOOL)]
  [(subst-typ theta (VAR id ())) (OK typ)
   (where (typ) (find-map theta id))]
  [(subst-typ theta (VAR id (targ ...))) (OK (VAR id (targ_subst ...)))
   (where () (find-map theta id))
   (where (OK (targ_subst ...)) (subst-typs theta (targ ...)))]
  [(subst-typ theta (TUP (typ ...))) (OK (TUP (typ_subst ...)))
   (where (OK (typ_subst ...)) (subst-typs theta (typ ...)))]
  [(subst-typ theta (ITER typ iter)) (OK (ITER typ_subst iter))
   (where (OK typ_subst) (subst-typ theta typ))]
  ;; The binders are renamed apart from every name in play before the
  ;; substitution goes under them.
  [(subst-typ theta (FUNC (tparam ...) (typ ...) typ_ret))
   (OK (FUNC (tparam_fresh ...) (typ_subst ...) typ_ret_subst))
   (where ((id typ_arg) ...) theta)
   (where (id_used ...) ,(append (term (id ...))
                                 (term (ids-typs (typ_arg ...)))
                                 (term (ids-typ (FUNC (tparam ...) (typ ...) typ_ret)))))
   (where (tparam_fresh ...) (fresh-ids (tparam ...) (id_used ...)))
   (where theta_fresh ((tparam (VAR tparam_fresh ())) ...))
   (where (OK (typ_fresh ...)) (subst-typs theta_fresh (typ ...)))
   (where (OK (typ_subst ...)) (subst-typs theta (typ_fresh ...)))
   (where (OK typ_ret_fresh) (subst-typ theta_fresh typ_ret))
   (where (OK typ_ret_subst) (subst-typ theta typ_ret_fresh))]
  ;; otherwise: a parameterized use of a type parameter, or a failed
  ;; substitution
  [(subst-typ theta typ) ERROR])

(define-metafunction al
  subst-typs : theta (typ ...) -> typsRes
  [(subst-typs theta (typ ...)) (OK (typ_subst ...))
   (where ((OK typ_subst) ...) ((subst-typ theta typ) ...))]
  ;; otherwise
  [(subst-typs theta (typ ...)) ERROR])
