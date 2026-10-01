#lang racket/base
;; spec-meta/al/5.6-eval-call-func.watsup.
;;
;; A callee runs in (IN L_callee ...), and each clause in a copy of it, so a
;; failed clause leaves no bindings. Eval_clause/fail and Eval_tblrow/fail are
;; "frame/fail". Call_func, Call_func_dispatch, and Call_defined_func have no
;; `otherwise`, so an input that no rule matches reduces to FAIL directly, as
;; Eval_exp/fail does for the call.

(require "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/4-relation.rkt"
         "3-context.rkt"
         "4-relation.rkt")
(provide ->redex/eval-call-func
         ->ctx/eval-call-func)

;; The rules on the redex
(define ->redex/eval-call-func
  (reduction-relation/forms
   al

   ;;; Clause invocation

   ;; rule Eval_clause/succ
   (--> (eval-clause L_caller clause (val ...))
        (eval-clause/succ (assign-args L_caller (arg ...) (val ...)) (eval-prems (prem ...)) exp_out)
        (where ((arg ...) exp_out (prem ...)) clause)
        "eval-clause/succ")
   (--> (eval-clause/succ OK OK (OK val_out)) (OK val_out)
        "eval-clause/succ/output")

   ;; rulegroup Eval_clauses. cons-succ and cons-fail share their premise.
   (--> (eval-clauses L_caller () (val ...)) FAIL
        "eval-clauses/nil")
   (--> (eval-clauses/cons (OK val_res) L_caller (clause_t ...) (val ...)) (OK val_res)
        "eval-clauses/cons-succ")
   (--> (eval-clauses/cons FAIL L_caller (clause_t ...) (val ...))
        (eval-clauses L_caller (clause_t ...) (val ...))
        "eval-clauses/cons-fail")

   ;;; Table meta-function invocation

   ;; rule Eval_tblrow/succ (see ->ctx), once the output is evaluated
   (--> (eval-tblrow/succ OK OK (OK val_out)) (OK val_out)
        "eval-tblrow/succ/output")

   ;; rulegroup Eval_tblrows. cons-succ and cons-fail share their premise.
   (--> (eval-tblrows () (val ...)) FAIL
        "eval-tblrows/nil")
   (--> (eval-tblrows (tblrow_h tblrow_t ...) (val ...))
        (eval-tblrows/cons (eval-tblrow tblrow_h (val ...)) (tblrow_t ...) (val ...))
        "eval-tblrows/cons")
   (--> (eval-tblrows/cons (OK val_out) (tblrow_t ...) (val ...)) (OK val_out)
        "eval-tblrows/cons-succ")
   (--> (eval-tblrows/cons FAIL (tblrow_t ...) (val ...)) (eval-tblrows (tblrow_t ...) (val ...))
        "eval-tblrows/cons-fail")

   ;; rule Call_table_func
   (--> (call-table-func tableFuncDef (val ...)) (eval-tblrows (tblrow ...) (val ...))
        (where (TABLE _ (tblrow ...)) tableFuncDef)
        "call-table-func")

   ;;; Defined meta-function invocation

   ;; rule Call_defined_func (see ->ctx)
   (--> (call-defined-func definedFuncDef (typ ...) (val ...)) FAIL
        ;; otherwise: as many types as type parameters
        (where (DEF (tparam ...) (clause ...) (elsclause ...)) definedFuncDef)
        (side-condition (not (= (length (term (tparam ...))) (length (term (typ ...))))))
        "call-defined-func/fail")

   ;;; Meta-function dispatch

   ;; rulegroup Call_func_dispatch
   (--> (call-func-dispatch (EXT id) (typ ...) (val ...)) (call-extern-func id (typ ...) (val ...))
        "call-func-dispatch/extern")
   (--> (call-func-dispatch builtinFuncDef (typ ...) (val ...))
        (call-builtin-func id (typ ...) (val ...))
        (where (BUILTIN id _ _) builtinFuncDef)
        "call-func-dispatch/builtin")
   (--> (call-func-dispatch tableFuncDef () (val ...)) (call-table-func tableFuncDef (val ...))
        "call-func-dispatch/table")
   (--> (call-func-dispatch tableFuncDef (typ_h typ_t ...) (val ...)) FAIL
        ;; otherwise: a table function takes no types
        "call-func-dispatch/table/fail")
   (--> (call-func-dispatch definedFuncDef (typ ...) (val ...))
        (call-defined-func definedFuncDef (typ ...) (val ...))
        "call-func-dispatch/defined")

   ;;; Extern meta-function invocation, which reaches the host

   ;; extern relation Call_extern_func
   (--> (call-extern-func id (typ ...) (val ...)) valres
        (where valres ,(host-call-extern-func (term id) (term (typ ...)) (term (val ...))))
        "call-extern-func")

   ;;; Builtin meta-function invocation, which reaches the host

   ;; extern relation Call_builtin_func
   (--> (call-builtin-func id (typ ...) (val ...)) valres
        (where valres ,(host-call-builtin-func (term id) (term (typ ...)) (term (val ...))))
        "call-builtin-func")))

;; The rules on the focus triple (r G L)
(define ->ctx/eval-call-func
  (reduction-relation/forms
   al

   ;;; Clause invocation

   ;; Eval_clauses/cons-succ and cons-fail: the head clause runs in a copy of
   ;; the callee's layer
   (--> ((eval-clauses L_caller (clause_h clause_t ...) (val ...)) G L)
        ((eval-clauses/cons (IN L (eval-clause L_caller clause_h (val ...)))
                            L_caller (clause_t ...) (val ...))
         G L)
        "eval-clauses/cons")

   ;;; Table meta-function invocation

   ;; rule Eval_tblrow/succ: the row runs in a layer of its own, with the
   ;; caller's layer L
   (--> ((eval-tblrow tblrow (val ...)) G L)
        ((IN L_callee (eval-tblrow/succ (assign-args L (arg ...) (val ...))
                                        (eval-prems (prem ...))
                                        exp_out))
         G L)
        (where ((arg ...) exp_out (prem ...)) tblrow)
        (where L_callee (empty-layer))
        "eval-tblrow/succ")

   ;;; Defined meta-function invocation

   ;; rule Call_defined_func: the callee's layer binds the type parameters,
   ;; and L is the caller's
   (--> ((call-defined-func definedFuncDef (typ ..._n) (val ...)) G L)
        ((IN L_callee (eval-clauses L (clause_all ...) (val ...))) G L)
        (where (DEF (tparam ..._n) (clause ...) (elsclause ...)) definedFuncDef)
        (where tdenv ((tparam (DEF () (ALIAS typ))) ...))
        (where {TYP tdenv_empty REL renv FUNC fenv VAL venv} (empty-layer))
        (where L_callee {TYP tdenv REL renv FUNC fenv VAL venv})
        (where (clause_else ...) (opt-as-seq- (elsclause ...)))
        (where (clause_all ...) (clause ... clause_else ...))
        "call-defined-func")

   ;;; Meta-function call

   ;; rule Call_func
   (--> ((call-func id (typ ...) (val ...)) G L) ((call-func-dispatch funcdef (typ ...) (val ...)) G L)
        (where (funcdef) (find-func G L id))
        "call-func")
   (--> ((call-func id (typ ...) (val ...)) G L) (FAIL G L)
        ;; otherwise: no such function
        (where () (find-func G L id))
        "call-func/fail")))
