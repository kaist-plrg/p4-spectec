#lang racket/base
;; spec-meta/3-execution/3.12-eval-call-rel.watsup.
;;
;; A relation runs in (IN L_local ...), and each rule path in a copy of it, so
;; a path leaves no bindings to the next. Every path of every group is
;; evaluated, and the call is the one success among them; it is FAIL when
;; several succeed ($det_valsres), and the else group runs only when none
;; did. Eval_rul/fail is "frame/fail". Call_rel has no `otherwise`, so an
;; unknown relation reduces to FAIL directly, as Eval_prem/fail does for the
;; premise.
;;
;; Deviation from the spec: the spec checks determinism per group (Eval_ruls)
;; and then across groups (Eval_rulgroups), so a group with two matching
;; paths FAILs and a second group's single match is accepted. Here all paths
;; of all groups are checked together, and the else group runs only when
;; none matched.

(require redex/reduction-semantics
         "3.02-context.rkt"
         "3.03-machine.rkt")
(provide ->term/eval-call-rel
         ->term+ctx/eval-call-rel)

;; Every (rulMatch, rulPath) pair of the groups, in order: the attempts of
;; a relation call
(define-metafunction al
  rul-attempts : (rulGroup ...) -> ((rulMatch rulPath) ...)
  [(rul-attempts ()) ()]
  [(rul-attempts ((id rulMatch (rulPath ...)) rulGroup ...))
   ((rulMatch rulPath) ... any ...)
   (where (any ...) (rul-attempts (rulGroup ...)))])

;; The rules on a term
(define ->term/eval-call-rel
  (reduction-relation
   al

   ;;; Rule path evaluation

   ;; rule Eval_rul/succ: the inputs are assigned, the premises hold, and the
   ;; outputs are evaluated, as a statement sequence
   (--> (eval-rul ((exp ...) (prem_match ...)) (id (exp_out ...) (prem_path ...)) (val ...))
        (seq ((assign-exps (exp ...) (val ...)) prem_match ... prem_path ... (eval-exps (exp_out ...))))
        "eval-rul")

   ;;; Defined relations

   ;; rules Call_defined_rel/succ and Call_defined_rel/fail, with Eval_ruls
   ;; and Eval_rulgroups flat over the groups: every path runs in a copy of
   ;; the relation's layer
   (--> (call-defined-rel definedRelDef (val ...))
        (IN L_local
            (call-defined-rel/ruls ((IN L_local (eval-rul rulMatch rulPath (val ...))) ...)
                                   (elsGroup ...) (val ...)))
        (where (DEF (rulGroup ...) (elsGroup ...)) definedRelDef)
        (where ((rulMatch rulPath) ...) (rul-attempts (rulGroup ...)))
        (where L_local (empty-layer))
        "call-defined-rel")
   ;; once every path is evaluated, with $det_valsres: the one success, or
   ;; the else group in the relation's layer when none, or FAIL when several
   ;; (a nondeterministic call)
   (--> (call-defined-rel/ruls (FAIL ... (OK (val_rel ...)) FAIL ...) (elsGroup ...) (val ...))
        (OK (val_rel ...))
        "call-defined-rel/succ")
   (--> (call-defined-rel/ruls (FAIL ...) ((id rulMatch rulPath)) (val ...))
        (eval-rul rulMatch rulPath (val ...))
        "call-defined-rel/fail")
   (--> (call-defined-rel/ruls (FAIL ...) () (val ...)) FAIL
        "call-defined-rel/fail/none")
   (--> (call-defined-rel/ruls (valsres_1 ... (OK (val_1 ...)) valsres_2 ... (OK (val_2 ...)) valsres_3 ...)
                               (elsGroup ...) (val ...))
        FAIL
        "call-defined-rel/nondet")

   ;;; Relation dispatch

   ;; rulegroup Call_rel_dispatch
   (--> (call-rel-dispatch definedRelDef (val ...)) (call-defined-rel definedRelDef (val ...))
        "call-rel-dispatch/defined")
   (--> (call-rel-dispatch (EXT id) (val ...)) (call-extern-rel id (val ...))
        "call-rel-dispatch/ext")

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

   ;; rule Call_rel
   (--> ((call-rel id (val ...)) G L) ((call-rel-dispatch reldef (val ...)) G L)
        (where (reldef) (find-rel G L id))
        "call-rel")
   (--> ((call-rel id (val ...)) G L) (FAIL G L)
        ;; otherwise: no such relation
        (where () (find-rel G L id))
        "call-rel/fail")))
