#lang racket/base
;; spec-meta/common/2-env.watsup.

(require "0-prelude.rkt"
         "0-stdlib.rkt"
         "1-syntax.rkt")
(provide Common-env
         extend_tdenv
         theta_of_tdenv
         is_iter_on_var
         iter_vari
         iter_varr)

(define-extended-language Common-env Common
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

(define-dec Common-env
  extend_tdenv : tdenv tdenv -> tdenv
  [(extend_tdenv tdenv_global ()) tdenv_global]
  [(extend_tdenv tdenv_global ((id typdef) ...))
   (adds_map tdenv_global (id ...) (typdef ...))
   ;; otherwise
   (side-condition (not (null? (term ((id typdef) ...)))))])

(define-dec Common-env
  theta_of_tdenv : tdenv -> theta
  [(theta_of_tdenv ()) (empty_map)]
  [(theta_of_tdenv ((id_h (DEF () (ALIAS typ_h))) (id_t typdef_t) ...))
   (add_map theta_t id_h typ_h)
   (where theta_t (theta_of_tdenv ((id_t typdef_t) ...)))]
  [(theta_of_tdenv ((id_h typdef_h) (id_t typdef_t) ...))
   (theta_of_tdenv ((id_t typdef_t) ...))
   (side-condition (or (equal? (term typdef_h) 'PARAM)
                       (equal? (term typdef_h) 'EXT)))])

;;
;; Iteration helper
;;

(define-dec Common-env
  is_iter_on_var : exp -> (varr ...)
  [(is_iter_on_var (VAR id)) ((id ()))]
  [(is_iter_on_var (ITER exp iterexp))
   (is_iter_on_var/iter (varr ...) iterexp)
   (where (varr ...) (is_iter_on_var exp))]
  [(is_iter_on_var exp) ()
   ;; otherwise
   (side-condition (not (redex-match? Common-env (VAR id) (term exp))))
   (side-condition (not (redex-match? Common-env (ITER exp iterexp) (term exp))))])

;; The ITER clause of $is_iter_on_var, given the result of its premise on the
;; inner exp.
(define-dec Common-env
  is_iter_on_var/iter : (varr ...) iterexp -> (varr ...)
  [(is_iter_on_var/iter ((id (iter ...))) (iter_outer ((id _ (iter ...)))))
   ((id (iter ... iter_outer)))]
  [(is_iter_on_var/iter (varr ...) iterexp) ()
   ;; otherwise
   (side-condition
    (not (redex-match? Common-env
                       (((id (iter ...))) (iter_outer ((id _ (iter ...)))))
                       (term ((varr ...) iterexp)))))])

(define-dec Common-env
  iter_vari : vari iter -> vari
  [(iter_vari (id typ (iter ...)) iter_add) (id typ (iter ... iter_add))])

(define-dec Common-env
  iter_varr : varr iter -> varr
  [(iter_varr (id (iter ...)) iter_add) (id (iter ... iter_add))])
