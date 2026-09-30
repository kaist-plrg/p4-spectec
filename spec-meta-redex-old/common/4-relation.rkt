#lang racket/base
;; spec-meta/common/4-relation.watsup.

(require "0.0-prelude.rkt"
         "3-context.rkt")
(provide common-relation
         call-extern-func
         call-builtin-func
         call-extern-rel)

;; Redex has no parametric nonterminals, so each res<X> is written out.
(define-extended-language common-relation common-context
  ;; Result to represent backtracking in evaluation
  (unitres ::= OK FAIL)
  (valres ::= (OK val) FAIL)
  (valsres ::= (OK (val ...)) FAIL))

;; Until the host is reachable (Step 9), every extern call raises.
(define (unreachable name)
  (error name "externs are not implemented yet"))

;;; Extern meta-function invocation

;; |- id `< typ* `> `( val* `) : res<val>
(define-relation common-relation
  #:mode (call-extern-func I I I O)
  #:contract (call-extern-func id (typ ...) (val ...) valres)
  [(where valres ,(unreachable 'call-extern-func))
   ------------------------------------------------ "stub"
   (call-extern-func id (typ ...) (val ...) valres)])

;;; Builtin meta-function invocation

;; |- id '@' `< typ* `> `( val* `) : res<val>
(define-relation common-relation
  #:mode (call-builtin-func I I I O)
  #:contract (call-builtin-func id (typ ...) (val ...) valres)
  [(where valres ,(unreachable 'call-builtin-func))
   ------------------------------------------------- "stub"
   (call-builtin-func id (typ ...) (val ...) valres)])

;;; Extern relations

;; |- id val* : res<val*>
(define-relation common-relation
  #:mode (call-extern-rel I I O)
  #:contract (call-extern-rel id (val ...) valsres)
  [(where valsres ,(unreachable 'call-extern-rel))
   ----------------------------------------------- "stub"
   (call-extern-rel id (val ...) valsres)])
