#lang racket/base
;; spec-meta/5.6-eval-call-func.watsup.
;;
;; A callee runs in (IN L_callee ...), and each clause in a copy of it, so a
;; clause leaves no bindings to the next. Every clause is evaluated, and the
;; call is the one success among them; it is FAIL when several succeed
;; ($det_valres), and the `otherwise` clause runs only when none did. Table
;; rows are still tried in order. Eval_clause/fail and Eval_tblrow/fail are
;; "frame/fail". Call_func and Call_func_dispatch have no `otherwise`, so an
;; input that no rule matches reduces to FAIL directly, as Eval_exp/fail does
;; for the call.

(require redex/reduction-semantics
         "3-context.rkt"
         "4-relation.rkt")
(provide ->term/eval-call-func
         ->term+ctx/eval-call-func)

;; The rules on a term
(define ->term/eval-call-func
  (reduction-relation
   al

   ;;; Clause invocation

   ;; rule Eval_clause/succ
   (--> (eval-clause L_caller ((arg ...) exp_out (prem ...)) (val ...))
        (eval-clause/succ (assign-args L_caller (arg ...) (val ...)) (eval-prems (prem ...)) exp_out)
        "eval-clause/succ")
   (--> (eval-clause/succ OK OK (OK val_out)) (OK val_out)
        "eval-clause/succ/output")

   ;;; Table meta-function invocation

   ;; rule Eval_tblrow/succ (see ->term+ctx), once the output is evaluated
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
   (--> (call-table-func (TABLE (param ...) (tblrow ...)) (val ...))
        (eval-tblrows (tblrow ...) (val ...))
        "call-table-func")

   ;;; Defined meta-function invocation

   ;; rules Call_defined_func/succ and Call_defined_func/fail (see
   ;; ->term+ctx), once every clause is evaluated, with Eval_clauses's
   ;; $det_valres: the one success, or the `otherwise` clause in the callee's
   ;; layer when none, or FAIL when several (a nondeterministic call)
   (--> (call-defined-func/clauses (FAIL ... (OK val_func) FAIL ...) L_caller (elsclause ...) (val ...))
        (OK val_func)
        "call-defined-func/succ")
   (--> (call-defined-func/clauses (FAIL ...) L_caller (elsclause) (val ...))
        (eval-clause L_caller elsclause (val ...))
        "call-defined-func/fail")
   (--> (call-defined-func/clauses (FAIL ...) L_caller () (val ...)) FAIL
        "call-defined-func/fail/none")
   (--> (call-defined-func/clauses (valres_1 ... (OK val_1) valres_2 ... (OK val_2) valres_3 ...)
                                   L_caller (elsclause ...) (val ...))
        FAIL
        "call-defined-func/nondet")
   (--> (call-defined-func definedFuncDef (typ ...) (val ...)) FAIL
        ;; otherwise: as many types as type parameters
        (where (DEF (tparam ...) (clause ...) (elsclause ...)) definedFuncDef)
        (side-condition (not (= (length (term (tparam ...))) (length (term (typ ...))))))
        "call-defined-func/typs/fail")

   ;;; Meta-function dispatch

   ;; rulegroup Call_func_dispatch
   (--> (call-func-dispatch (EXT id) (typ ...) (val ...)) (call-extern-func id (typ ...) (val ...))
        "call-func-dispatch/extern")
   (--> (call-func-dispatch (BUILTIN id (tparam ...) (param ...)) (typ ...) (val ...))
        (call-builtin-func id (typ ...) (val ...))
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

;; The rules on a term and its context (r G L)
(define ->term+ctx/eval-call-func
  (reduction-relation
   al

   ;;; Table meta-function invocation

   ;; rule Eval_tblrow/succ: the row runs in a layer of its own, with the
   ;; caller's layer L
   (--> ((eval-tblrow ((arg ...) exp_out (prem ...)) (val ...)) G L)
        ((IN (empty-layer) (eval-tblrow/succ (assign-args L (arg ...) (val ...))
                                             (eval-prems (prem ...))
                                             exp_out))
         G L)
        "eval-tblrow/succ")

   ;;; Defined meta-function invocation

   ;; rules Call_defined_func/succ and Call_defined_func/fail, with
   ;; Eval_clauses: the callee's layer binds the type parameters, every
   ;; clause runs in a copy of it, and L is the caller's
   (--> ((call-defined-func definedFuncDef (typ ..._n) (val ...)) G L)
        ((IN L_callee
             (call-defined-func/clauses ((IN L_callee (eval-clause L clause (val ...))) ...)
                                        L (elsclause ...) (val ...)))
         G L)
        (where (DEF (tparam ..._n) (clause ...) (elsclause ...)) definedFuncDef)
        (where {TYP tdenv_empty REL renv FUNC fenv VAL venv} (empty-layer))
        (where L_callee {TYP ((tparam (DEF () (ALIAS typ))) ...) REL renv FUNC fenv VAL venv})
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
