#lang racket/base
;; spec-meta/common/4-relation.watsup.
;;
;; The three extern relations are reduction rules on machine forms (in al/).
;; Their rules call the host procedures here, which go to the OCaml host over
;; the extern wire. They are impure, so only a reduction rule may call them,
;; never a metafunction.

(require json
         "0.0-prelude.rkt"
         "0.2-extern-json.rkt"
         "0.3-extern-ffi.rkt"
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

;; Sends a request to the OCaml host, and gives its response as a jsexpr.
(define (host-request request)
  (string->jsexpr (host-eval (jsexpr->string request))))

;;; Extern meta-function invocation

;; |- id `< typ* `> `( val* `) : res<val>, as a valres
(define (host-call-extern-func id typs vals)
  (response->valres (host-request (extern-func-request id typs vals))))

;;; Builtin meta-function invocation

;; |- id '@' `< typ* `> `( val* `) : res<val>, as a valres
(define (host-call-builtin-func id typs vals)
  (response->valres (host-request (builtin-request id typs vals))))

;;; Extern relations

;; |- id val* : res<val*>, as a valsres
(define (host-call-extern-rel id vals)
  (response->valsres (host-request (extern-rel-request id vals))))
