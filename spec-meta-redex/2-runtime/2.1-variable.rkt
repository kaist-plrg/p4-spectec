#lang racket/base
;; spec-meta/2-runtime/2.1-variable.watsup.

(require redex/reduction-semantics
         "2.0-value.rkt")
(provide variable
         is-iter-on-var
         iter-vari)

(define-extended-language variable value
  ;; Runtime variables
  (varr ::= (id (iter ...))))

;;
;; Iteration helpers
;;

(define-metafunction variable
  is-iter-on-var : exp -> () ∨ (varr)
  [(is-iter-on-var (VAR id)) ((id ()))]
  [(is-iter-on-var (ITER exp (iter_outer ((id _ (iter ...)))))) ((id (iter ... iter_outer)))
   (where ((id (iter ...))) (is-iter-on-var exp))]
  ;; otherwise
  [(is-iter-on-var exp) ()])

(define-metafunction variable
  iter-vari : vari iter -> vari
  [(iter-vari (id typ (iter ...)) iter_add) (id typ (iter ... iter_add))])
