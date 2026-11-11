#lang racket/base
;; Mirrors spec-meta/3-execution/3.11-eval-call-rel.watsup.

(require redex/reduction-semantics
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.04-extern-host.rkt")
(provide ->term/eval-call-rel
         ->term+ctx/eval-call-rel)

(define-metafunction al
  ruls-of-rulgroup : rulGroup -> (rul ...)
  [(ruls-of-rulgroup (id rulMatch (rulPath ...))) ((rulMatch rulPath) ...)])

(define-metafunction al
  ruls-of-rulgroups : (rulGroup ...) -> (rul ...)
  [(ruls-of-rulgroups (rulGroup ...)) (rul ... ...)
   (where ((rul ...) ...) ((ruls-of-rulgroup rulGroup) ...))])

(define-metafunction al
  rul-of-elsgroup : elsGroup -> rul
  [(rul-of-elsgroup (id rulMatch rulPath)) (rulMatch rulPath)])

;; The rules on a term
(define ->term/eval-call-rel
  (reduction-relation
   al

   ;;; Rule path evaluation

   ;; rule Eval_rul/succ
   (--> (eval-rul (((exp ...) (prem_match ...)) (id (exp_out ...) (prem_path ...))) (val ...))
        (seq ((assign-exps (exp ...) (val ...)) prem_match ... prem_path ... (eval-exps (exp_out ...))))
        "eval-rul")

   ;;; Rule path sequence evaluation

   ;; rulegroup Eval_ruls
   (--> (eval-ruls (UNMATCH ...)) UNMATCH
        "eval-ruls/nil")
   ;; (--> (eval-ruls (UNMATCH ... (OK (val_out ...)) (IN L (eval-rul rul (val ...))) ...)) (OK (val_out ...))
   ;;      "eval-ruls/succ")
   (--> (eval-ruls (UNMATCH ... ERROR (IN L (eval-rul rul (val ...))) ...)) ERROR
        "eval-ruls/error")

   ;; Determinism check (Eval_ruls_on_ok): replaces "eval-ruls/succ".
   ;;
   (--> (eval-ruls (UNMATCH ... (OK (val_out ...)) (IN L (eval-rul rul (val ...))) ...))
        (eval-ruls-on-ok (OK (val_out ...)) ((IN L (eval-rul rul (val ...))) ...))
        "eval-ruls/succ")
   ;;
   ;; rulegroup Eval_ruls_on_ok, once the path at hand is evaluated
   (--> (eval-ruls-on-ok (OK (val_out ...)) (UNMATCH ...)) (OK (val_out ...))
        "eval-ruls-on-ok/nil")
   (--> (eval-ruls-on-ok (OK (val_out ...)) (UNMATCH ... valsres (IN L (eval-rul rul (val ...))) ...)) ERROR
        (side-condition (not (equal? (term valsres) 'UNMATCH)))
        "eval-ruls-on-ok/error")

   ;;; Defined relations

   ;; rulegroup Call_defined_rel
   (--> (call-defined-rel (REL id (typ_input ...) (typ_output ...) (rulGroup ...) (elsGroup ...)) (val ...))
        (IN L_local
            (call-defined-rel/ruls (eval-ruls ((IN L_local (eval-rul rul (val ...))) ...))
                                   (elsGroup ...) (val ...)))
        (where (rul ...) (ruls-of-rulgroups (rulGroup ...)))
        (where L_local (empty-localLayer))
        "call-defined-rel")
   (--> (call-defined-rel/ruls valsres (elsGroup ...) (val ...)) valsres
        (side-condition (not (equal? (term valsres) 'UNMATCH)))
        "call-defined-rel/result")
   (--> (call-defined-rel/ruls UNMATCH (elsGroup) (val ...))
        (eval-rul (rul-of-elsgroup elsGroup) (val ...))
        "call-defined-rel/unmatch")
   (--> (call-defined-rel/ruls UNMATCH () (val ...)) UNMATCH
        "call-defined-rel/unmatch/none")

   ;;; Extern relations

   ;; extern relation Call_extern_rel
   (--> (call-extern-rel id (val ...)) valsres
        (where valsres ,(host-call-extern-rel (term id) (term (val ...))))
        "call-extern-rel")))

;; The rules on a term and its context, (r G L K)
(define ->term+ctx/eval-call-rel
  (reduction-relation
   al

   ;;; Relation call

   ;; rulegroup Call_rel
   (--> ((call-rel id (val ...)) G L K) ((call-extern-rel id_rel (val ...)) G L K)
        (where ((EXTREL id_rel (typ_input ...) (typ_output ...))) (find-rel G id))
        "call-rel/extern")
   (--> ((call-rel id (val ...)) G L K) ((call-defined-rel definedRelDefn (val ...)) G L K)
        (where (definedRelDefn) (find-rel G id))
        "call-rel/defined")
   (--> ((call-rel id (val ...)) G L K) (ERROR G L K)
        ;; otherwise
        (where () (find-rel G id))
        "call-rel/error")))
