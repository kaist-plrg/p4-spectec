#lang racket/base
;; Mirrors spec-meta/4-execution/4.11-eval-call-rel.watsup.
;;
;; A relation runs in (IN G L_local ...), and each rule path in a copy of it, so
;; a path leaves no bindings to the next. The paths of every group, in order,
;; are evaluated until one succeeds (Eval_ruls), which gives the result;
;; unlike spec-meta, the later paths are not checked to be unmatched. The
;; else group runs only when every path is unmatched. The `/error` rules have
;; no rules here: a call that errs is stuck, which is ERROR.

(require redex/reduction-semantics
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.04-extern-host.rkt")
(provide ->term/eval-call-rel
         ->term+ctx/eval-call-rel)

;; The rule paths of a rule group, each with the group's match
(define-metafunction al
  ruls-of-rulgroup : rulGroup -> (rul ...)
  [(ruls-of-rulgroup (id rulMatch (rulPath ...))) ((rulMatch rulPath) ...)])

;; The rule paths of rule groups, in order
(define-metafunction al
  ruls-of-rulgroups : (rulGroup ...) -> (rul ...)
  [(ruls-of-rulgroups (rulGroup ...)) (rul ... ...)
   (where ((rul ...) ...) ((ruls-of-rulgroup rulGroup) ...))])

;; The rule path of an otherwise group
(define-metafunction al
  rul-of-elsgroup : elsGroup -> rul
  [(rul-of-elsgroup (id rulMatch rulPath)) (rulMatch rulPath)])

;; The rules on a term
(define ->term/eval-call-rel
  (reduction-relation
   al

   ;;; Rule path evaluation

   ;; rule Eval_rul/succ: the inputs are assigned, the premises hold, and the
   ;; outputs are evaluated, as a statement sequence
   (--> (eval-rul (((exp ...) (prem_match ...)) (id (exp_out ...) (prem_path ...))) (val ...))
        (seq ((assign-exps (exp ...) (val ...)) prem_match ... prem_path ... (eval-exps (exp_out ...))))
        "eval-rul")

   ;;; Rule path sequence evaluation

   ;; rulegroup Eval_ruls, once the path at hand is evaluated. An unmatched
   ;; path is passed over by the frame (Eval_ruls/unmatch).
   (--> (eval-ruls (UNMATCH ...)) UNMATCH
        "eval-ruls/nil")
   (--> (eval-ruls (UNMATCH ... (OK (val_out ...)) (IN G L (eval-rul rul (val ...))) ...))
        (OK (val_out ...))
        "eval-ruls/succ")

   ;;; Defined relations

   ;; rulegroup Call_defined_rel, once the paths are evaluated (a rule under
   ;; the IN starts it): the result, or the else group in the relation's
   ;; layer when all are unmatched
   (--> (call-defined-rel/ruls (OK (val_out ...)) (elsGroup ...) (val ...)) (OK (val_out ...))
        "call-defined-rel/result")
   (--> (call-defined-rel/ruls UNMATCH (elsGroup) (val ...))
        (eval-rul (rul-of-elsgroup elsGroup) (val ...))
        "call-defined-rel/unmatch")
   (--> (call-defined-rel/ruls UNMATCH () (val ...)) UNMATCH
        "call-defined-rel/unmatch/none")

   ;;; Extern relations, which reach the host

   ;; extern relation Call_extern_rel. An error reply is not a valsres, so
   ;; the call is stuck.
   (--> (call-extern-rel id (val ...)) valsres
        (where valsres ,(host-call-extern-rel (term id) (term (val ...))))
        "call-extern-rel")))

;; The rules on a term under its innermost IN, (IN G L (in-hole F r)), which
;; read G or L, or write L
(define ->term+ctx/eval-call-rel
  (reduction-relation
   al

   ;;; Defined relations

   ;; rulegroup Call_defined_rel: every path runs in a copy of the relation's
   ;; local layer
   (--> (IN G L (in-hole F (call-defined-rel (REL id (typ_input ...) (typ_output ...) (rulGroup ...) (elsGroup ...))
                                             (val ...))))
        (IN G L (in-hole F (IN G L_local
                               (call-defined-rel/ruls (eval-ruls ((IN G L_local (eval-rul rul (val ...))) ...))
                                                      (elsGroup ...) (val ...)))))
        (where (rul ...) (ruls-of-rulgroups (rulGroup ...)))
        (where L_local (empty-localLayer))
        "call-defined-rel")

   ;;; Relation call

   ;; rulegroup Call_rel
   (--> (IN G L (in-hole F (call-rel id (val ...))))
        (IN G L (in-hole F (call-extern-rel id_rel (val ...))))
        (where ((EXTREL id_rel (typ_input ...) (typ_output ...))) (find-rel G id))
        "call-rel/extern")
   (--> (IN G L (in-hole F (call-rel id (val ...))))
        (IN G L (in-hole F (call-defined-rel definedRelDefn (val ...))))
        (where (definedRelDefn) (find-rel G id))
        "call-rel/defined")))
