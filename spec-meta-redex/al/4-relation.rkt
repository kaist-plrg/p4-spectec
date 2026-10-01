#lang racket/base
;; spec-meta/al/4-relation.watsup, as the terms of the machine.
;;
;; An AL phrase evaluates in place, and every other relation has a machine
;; form. Frames give their evaluation positions. watsup's res<ctx> is
;; (IN L OK) or FAIL: premises update the innermost IN's layer in place.

(require "../common/0.0-prelude.rkt"
         "3-context.rkt")
(provide al)

(define-extended-language al al-context
  ;; Configurations
  (conf ::= (G e))

  ;; Results
  (typsres ::= (OK (typ ...)) FAIL)
  (res ::= unitres valres valsres typsres)

  ;; Machine terms
  (e ::=
     res
     (IN L e)
     ;; Eval_exp
     exp
     (UN unop e)
     (TUP ((OK val) ... e exp ...)))

  ;; Finished subterms
  (done ::= res (IN L OK))

  ;; Frames, one level each, through which FAIL passes. Redex has no empty
  ;; nonterminals, so Fr-catch comes with the first frame that catches FAIL.
  (Fr-pass ::=
           ;; Eval_exp
           (UN unop hole)
           (TUP ((OK val) ... hole exp ...)))
  (Fr ::= Fr-pass)

  ;; Frames within one local context, and across local contexts
  (F ::= hole (in-hole Fr F))
  (E ::= F (in-hole F (IN L E))))
