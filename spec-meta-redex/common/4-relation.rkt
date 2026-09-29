#lang racket/base
;; spec-meta/common/4-relation.watsup.

(require "0-prelude.rkt"
         "3-context.rkt")
(provide Common-relation
         Call_extern_func
         Call_builtin_func
         Call_extern_rel)

;; Redex has no parametric nonterminals, so each res<X> is written out.
(define-extended-language Common-relation Common-context
  ;; Result to represent backtracking in evaluation
  (unitres ::= OK FAIL)
  (valres ::= (OK val) FAIL)
  (valsres ::= (OK (val ...)) FAIL))

;; Until the host is reachable (Step 9), every extern call raises.
(define (unreachable name)
  (error name "externs are not implemented yet"))

;;; Extern meta-function invocation

;; |- id `< typ* `> `( val* `) : res<val>
(define-relation Common-relation
  #:mode (Call_extern_func I I I O)
  #:contract (Call_extern_func id (typ ...) (val ...) valres)
  [(where valres ,(unreachable 'Call_extern_func))
   ------------------------------------------------ "stub"
   (Call_extern_func id (typ ...) (val ...) valres)])

;;; Builtin meta-function invocation

;; |- id '@' `< typ* `> `( val* `) : res<val>
(define-relation Common-relation
  #:mode (Call_builtin_func I I I O)
  #:contract (Call_builtin_func id (typ ...) (val ...) valres)
  [(where valres ,(unreachable 'Call_builtin_func))
   ------------------------------------------------- "stub"
   (Call_builtin_func id (typ ...) (val ...) valres)])

;;; Extern relations

;; |- id val* : res<val*>
(define-relation Common-relation
  #:mode (Call_extern_rel I I O)
  #:contract (Call_extern_rel id (val ...) valsres)
  [(where valsres ,(unreachable 'Call_extern_rel))
   ----------------------------------------------- "stub"
   (Call_extern_rel id (val ...) valsres)])
