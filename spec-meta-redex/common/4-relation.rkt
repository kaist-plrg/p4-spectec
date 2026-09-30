#lang racket/base
;; spec-meta/common/4-relation.watsup.
;;
;; The three extern relations are reduction rules on machine forms (in al/).
;; Their rules call the host procedures here, which are impure, so only a
;; reduction rule may call them, never a metafunction.

(require "0.0-prelude.rkt"
         "3-context.rkt")
(provide common-relation
         host-call-extern-func
         host-call-builtin-func
         host-call-extern-rel)

;; Redex has no parametric nonterminals, so each res<X> is written out.
(define-extended-language common-relation common-context
  ;; Result to represent backtracking in evaluation
  (unitres ::= OK FAIL)
  (valres ::= (OK val) FAIL)
  (valsres ::= (OK (val ...)) FAIL))

;; Until the host is reachable, every extern call raises.
(define (unreachable who)
  (error who "the host is not reachable yet"))

;;; Extern meta-function invocation

;; |- id `< typ* `> `( val* `) : res<val>, as a valres
(define (host-call-extern-func id typs vals)
  (unreachable 'host-call-extern-func))

;;; Builtin meta-function invocation

;; |- id '@' `< typ* `> `( val* `) : res<val>, as a valres
(define (host-call-builtin-func id typs vals)
  (unreachable 'host-call-builtin-func))

;;; Extern relations

;; |- id val* : res<val*>, as a valsres
(define (host-call-extern-rel id vals)
  (unreachable 'host-call-extern-rel))
