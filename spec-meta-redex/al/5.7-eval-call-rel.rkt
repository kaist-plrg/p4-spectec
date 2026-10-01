#lang racket/base
;; spec-meta/al/5.7-eval-call-rel.watsup.
;;
;; A relation runs in (IN L_local ...), and each rule path in a copy of it, so
;; a failed path leaves no bindings. Eval_rul/fail is "frame/fail". Call_rel
;; has no `otherwise`, so an unknown relation reduces to FAIL directly, as
;; Eval_prem/fail does for the premise.

(require "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/4-relation.rkt"
         "3-context.rkt"
         "4-relation.rkt")
(provide ->redex/eval-call-rel
         ->ctx/eval-call-rel
         elsgroup-as-rulgroup)

;; $elsgroup_as_rulgroup
(define-dec al
  elsgroup-as-rulgroup : elsgroup -> rulgroup
  [(elsgroup-as-rulgroup (id rulmatch rulpath)) (id rulmatch (rulpath))])

;; The rules on the redex
(define ->redex/eval-call-rel
  (reduction-relation/forms
   al

   ;;; Rule path evaluation

   ;; rule Eval_rul/succ
   (--> (eval-rul rulmatch rulpath (val ...))
        (eval-rul/succ (assign-exps (exp ...) (val ...)) (eval-prems (prem ...)) (exp_out ...))
        (where (id (exp_out ...) (prem_path ...)) rulpath)
        (where ((exp ...) (prem_match ...)) rulmatch)
        (where (prem ...) (prem_match ... prem_path ...))
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
   (--> (eval-rulgroup rulgroup (val ...)) (eval-ruls rulmatch (rulpath ...) (val ...))
        (where (id rulmatch (rulpath ...)) rulgroup)
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
        (IN L_local (eval-rulgroups (rulgroup_all ...) (val ...)))
        (where (DEF (rulgroup ...) (elsgroup ...)) definedRelDef)
        (where (rulgroup_els ...) ((elsgroup-as-rulgroup elsgroup) ...))
        (where (rulgroup_else ...) (opt-as-seq- (rulgroup_els ...)))
        (where (rulgroup_all ...) (rulgroup ... rulgroup_else ...))
        (where L_local (empty-layer))
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

;; The rules on the focus triple (r G L)
(define ->ctx/eval-call-rel
  (reduction-relation/forms
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
