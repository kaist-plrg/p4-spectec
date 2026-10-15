#lang racket/base
;; spec-meta/al/5.7-eval-call-rel.watsup.
;;
;; A relation runs in (IN L_local ...), and each rule path in a copy of it, so
;; a failed path leaves no bindings. Eval_rul/fail is "frame/fail". Call_rel
;; has no `otherwise`, so an unknown relation reduces to FAIL directly, as
;; Eval_prem/fail does for the premise.

(require redex/reduction-semantics
         "../common/3-relation.rkt"
         "3-context.rkt"
         "4-relation.rkt")
(provide ->term/eval-call-rel
         ->term+ctx/eval-call-rel)

;; $elsgroup_as_rulgroup
(define-metafunction al
  elsgroup-as-rulgroup : elsgroup -> rulgroup
  [(elsgroup-as-rulgroup (id rulmatch rulpath)) (id rulmatch (rulpath))])

;; The rules on a term
(define ->term/eval-call-rel
  (reduction-relation
   al

   ;;; Rule path evaluation

   ;; rule Eval_rul/succ
   (--> (eval-rul ((exp ...) (prem_match ...)) (id (exp_out ...) (prem_path ...)) (val ...))
        (eval-rul/succ (assign-exps (exp ...) (val ...))
                       (eval-prems (prem_match ... prem_path ...))
                       (exp_out ...))
        "eval-rul/succ")
   (--> (eval-rul/succ OK OK ((OK val_out) ...)) (OK (val_out ...))
        "eval-rul/succ/output")

   ;;; Rule path sequence evaluation

   ;; rulegroup Eval_ruls. cons-succ and cons-fail share their premise.
   (--> (eval-ruls rulmatch () (val ...)) FAIL
        "eval-ruls/nil")
   (--> (eval-ruls/cons (OK (val_out ...)) rulmatch (rulpath_t ...) (val ...)) (OK (val_out ...))
        "eval-ruls/cons-succ")
   (--> (eval-ruls/cons FAIL rulmatch (rulpath_t ...) (val ...))
        (eval-ruls rulmatch (rulpath_t ...) (val ...))
        "eval-ruls/cons-fail")

   ;;; Rule group evaluation

   ;; rules Eval_rulgroup/succ and Eval_rulgroup/fail: the group's result is
   ;; that of Eval_ruls
   (--> (eval-rulgroup (id rulmatch (rulpath ...)) (val ...))
        (eval-ruls rulmatch (rulpath ...) (val ...))
        "eval-rulgroup")

   ;;; Rule group sequence evaluation

   ;; rulegroup Eval_rulgroups. cons-succ and cons-fail share their premise.
   (--> (eval-rulgroups () (val ...)) FAIL
        "eval-rulgroups/nil")
   (--> (eval-rulgroups (rulgroup_h rulgroup_t ...) (val ...))
        (eval-rulgroups/cons (eval-rulgroup rulgroup_h (val ...)) (rulgroup_t ...) (val ...))
        "eval-rulgroups/cons")
   (--> (eval-rulgroups/cons (OK (val_out ...)) (rulgroup_t ...) (val ...)) (OK (val_out ...))
        "eval-rulgroups/cons-succ")
   (--> (eval-rulgroups/cons FAIL (rulgroup_t ...) (val ...))
        (eval-rulgroups (rulgroup_t ...) (val ...))
        "eval-rulgroups/cons-fail")

   ;;; Defined relations

   ;; rule Call_defined_rel
   (--> (call-defined-rel definedRelDef (val ...))
        (IN (empty-layer)
            (eval-rulgroups (rulgroup ... (elsgroup-as-rulgroup elsgroup) ...) (val ...)))
        (where (DEF (rulgroup ...) (elsgroup ...)) definedRelDef)
        "call-defined-rel")

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

   ;;; Rule path sequence evaluation

   ;; Eval_ruls/cons-succ and cons-fail: the head path runs in a copy of the
   ;; relation's layer
   (--> ((eval-ruls rulmatch (rulpath_h rulpath_t ...) (val ...)) G L)
        ((eval-ruls/cons (IN L (eval-rul rulmatch rulpath_h (val ...)))
                         rulmatch (rulpath_t ...) (val ...))
         G L)
        "eval-ruls/cons")

   ;;; Relation call

   ;; rule Call_rel
   (--> ((call-rel id (val ...)) G L) ((call-rel-dispatch reldef (val ...)) G L)
        (where (reldef) (find-rel G L id))
        "call-rel")
   (--> ((call-rel id (val ...)) G L) (FAIL G L)
        ;; otherwise: no such relation
        (where () (find-rel G L id))
        "call-rel/fail")))
