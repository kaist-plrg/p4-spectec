#lang racket/base
;; spec-meta/common/2-env.watsup.

(require redex/reduction-semantics
         "0.0-stdlib.rkt"
         "1-syntax.rkt")
(provide common-env
         theta-of-tdenv
         is-iter-on-var
         iter-vari)

(define-extended-language common-env common-syntax
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

(define-judgment-form common-env
  #:mode (theta-of-tdenv I O)
  #:contract (theta-of-tdenv tdenv theta)
  [(theta-of-tdenv () (empty-map))]
  [(theta-of-tdenv ((id_t typdef_t) ...) theta_t)
   ----------------------------------------
   (theta-of-tdenv ((id_h (DEF () (ALIAS typ_h))) (id_t typdef_t) ...)
                   (add-map theta_t id_h typ_h))]
  [(theta-of-tdenv ((id_t typdef_t) ...) theta_t)
   ----------------------------------------
   (theta-of-tdenv ((id_h PARAM) (id_t typdef_t) ...) theta_t)]
  [(theta-of-tdenv ((id_t typdef_t) ...) theta_t)
   ----------------------------------------
   (theta-of-tdenv ((id_h EXT) (id_t typdef_t) ...) theta_t)])

;;
;; Iteration helper
;;

(define-metafunction common-env
  is-iter-on-var : exp -> () ∨ (varr)
  [(is-iter-on-var (VAR id)) ((id ()))]
  [(is-iter-on-var (ITER exp (iter_outer ((id _ (iter ...)))))) ((id (iter ... iter_outer)))
   (where ((id (iter ...))) (is-iter-on-var exp))]
  ;; otherwise
  [(is-iter-on-var exp) ()])

(define-metafunction common-env
  iter-vari : vari iter -> vari
  [(iter-vari (id typ (iter ...)) iter_add) (id typ (iter ... iter_add))])
