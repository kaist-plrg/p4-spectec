#lang racket/base
;; spec-meta/3-execution/3.12-eval-call-rel.watsup.
;;
;; A relation runs in (IN L_local ...), and each rule path in a copy of it, so
;; a path leaves no bindings to the next. The paths of every group, in order,
;; are evaluated until one succeeds or errors (Eval_ruls), and the paths after
;; a success must all be unmatched (Eval_ruls_on_ok): a second success is an
;; error. The else group runs only when every path is unmatched. The `/error`
;; rules become complement rules, and "frame/failure" where a path's
;; statement fails.

(require redex/reduction-semantics
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.04.2-extern-ffi.rkt")
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
   (--> (eval-ruls (UNMATCH ... (OK (val_out ...)) (IN L (eval-rul rul (val ...))) ...))
        (eval-ruls-on-ok (OK (val_out ...)) ((IN L (eval-rul rul (val ...))) ...))
        "eval-ruls/succ")
   (--> (eval-ruls (UNMATCH ... ERROR (IN L (eval-rul rul (val ...))) ...)) ERROR
        "eval-ruls/error")

   ;; rulegroup Eval_ruls_on_ok, once the path at hand is evaluated
   (--> (eval-ruls-on-ok (OK (val_out ...)) (UNMATCH ...)) (OK (val_out ...))
        "eval-ruls-on-ok/nil")
   (--> (eval-ruls-on-ok (OK (val_out ...)) (UNMATCH ... valsres (IN L (eval-rul rul (val ...))) ...)) ERROR
        (side-condition (not (equal? (term valsres) 'UNMATCH)))
        "eval-ruls-on-ok/error")

   ;;; Defined relations

   ;; rulegroup Call_defined_rel: every path runs in a copy of the relation's
   ;; layer
   (--> (call-defined-rel (REL id (typ_input ...) (typ_output ...) (rulGroup ...) (elsGroup ...)) (val ...))
        (IN L_local
            (call-defined-rel/ruls (eval-ruls ((IN L_local (eval-rul rul (val ...))) ...))
                                   (elsGroup ...) (val ...)))
        (where (rul ...) (ruls-of-rulgroups (rulGroup ...)))
        (where L_local (empty-localLayer))
        "call-defined-rel")
   ;; once the paths are evaluated: the result, or the else group in the
   ;; relation's layer when all are unmatched
   (--> (call-defined-rel/ruls valsres (elsGroup ...) (val ...)) valsres
        (side-condition (not (equal? (term valsres) 'UNMATCH)))
        "call-defined-rel/result")
   (--> (call-defined-rel/ruls UNMATCH (elsGroup) (val ...))
        (eval-rul (rul-of-elsgroup elsGroup) (val ...))
        "call-defined-rel/unmatch")
   (--> (call-defined-rel/ruls UNMATCH () (val ...)) UNMATCH
        "call-defined-rel/unmatch/none")

   ;;; Extern relations, which reach the host

   ;; extern relation Call_extern_rel
   (--> (call-extern-rel id (val ...)) valsres
        (where valsres ,(host-call-extern-rel (term id) (term (val ...))))
        "call-extern-rel")))

;; The rules on a term and its context (r G L)
(define ->term+ctx/eval-call-rel
  (reduction-relation
   al

   ;;; Relation call

   ;; rulegroup Call_rel
   (--> ((call-rel id (val ...)) G L) ((call-extern-rel id_rel (val ...)) G L)
        (where ((EXTREL id_rel (typ_input ...) (typ_output ...))) (find-rel G id))
        "call-rel/extern")
   (--> ((call-rel id (val ...)) G L) ((call-defined-rel definedRelDefn (val ...)) G L)
        (where (definedRelDefn) (find-rel G id))
        "call-rel/defined")
   (--> ((call-rel id (val ...)) G L) (ERROR G L)
        ;; otherwise: no such relation
        (where () (find-rel G id))
        "call-rel/error")))
