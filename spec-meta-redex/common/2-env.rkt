#lang racket/base
;; spec-meta/common/2-env.watsup.

(require "0.0-prelude.rkt"
         "0.1-stdlib.rkt"
         "1-syntax.rkt")
(provide common-env
         extend-tdenv
         theta-of-tdenv
         is-iter-on-var
         iter-vari
         iter-varr)

(define-extended-language common-env common
  ;; Value environment
  (varr ::= (id (iter ...)))
  (venv ::= ((varr val) ...))

  ;; Type definition environment
  (typdef ::=
          PARAM
          EXT
          (DEF (tparam ...) deftyp))
  (tdenv ::= ((id typdef) ...))

  ;; Type substitution environment
  (theta ::= ((id typ) ...)))

(define-dec common-env
  extend-tdenv : tdenv tdenv -> tdenv
  [(extend-tdenv tdenv_global ()) tdenv_global]
  [(extend-tdenv tdenv_global ((id typdef) ...))
   (adds-map tdenv_global (id ...) (typdef ...))
   ;; otherwise
   (side-condition (not (null? (term ((id typdef) ...)))))])

(define-dec common-env
  theta-of-tdenv : tdenv -> theta
  [(theta-of-tdenv ()) (empty-map)]
  [(theta-of-tdenv ((id_h (DEF () (ALIAS typ_h))) (id_t typdef_t) ...))
   (add-map theta_t id_h typ_h)
   (where theta_t (theta-of-tdenv ((id_t typdef_t) ...)))]
  [(theta-of-tdenv ((id_h typdef_h) (id_t typdef_t) ...))
   (theta-of-tdenv ((id_t typdef_t) ...))
   (side-condition (or (equal? (term typdef_h) 'PARAM)
                       (equal? (term typdef_h) 'EXT)))])

;;
;; Iteration helper
;;

(define-dec common-env
  is-iter-on-var : exp -> () ∨ (varr)
  [(is-iter-on-var (VAR id)) ((id ()))]
  [(is-iter-on-var (ITER exp iterexp))
   (is-iter-on-var/iter (varr ...) iterexp)
   (where (varr ...) (is-iter-on-var exp))]
  [(is-iter-on-var exp) ()
   ;; otherwise
   (side-condition (not (redex-match? common-env (VAR id) (term exp))))
   (side-condition (not (redex-match? common-env (ITER exp iterexp) (term exp))))])

;; The ITER clause of $is_iter_on_var, given the result of its premise on the
;; inner exp.
(define-dec common-env
  is-iter-on-var/iter : (varr ...) iterexp -> () ∨ (varr)
  [(is-iter-on-var/iter ((id (iter ...))) (iter_outer ((id _ (iter ...)))))
   ((id (iter ... iter_outer)))]
  [(is-iter-on-var/iter (varr ...) iterexp) ()
   ;; otherwise
   (side-condition
    (not (redex-match? common-env
                       (((id (iter ...))) (iter_outer ((id _ (iter ...)))))
                       (term ((varr ...) iterexp)))))])

(define-dec common-env
  iter-vari : vari iter -> vari
  [(iter-vari (id typ (iter ...)) iter_add) (id typ (iter ... iter_add))])

(define-dec common-env
  iter-varr : varr iter -> varr
  [(iter-varr (id (iter ...)) iter_add) (id (iter ... iter_add))])
