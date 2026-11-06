#lang racket/base
;; Mirrors spec-meta/4-execution/4.10-eval-call-func.watsup.
;;
;; A callee runs in (IN G L_callee ...), and each clause in a copy of it, so a
;; clause leaves no bindings to the next. The clauses are evaluated in order
;; until one succeeds (Eval_clauses), which gives the result; unlike
;; spec-meta, the later clauses are not checked to be unmatched. The
;; `otherwise` clause runs only when every clause is unmatched.
;; Table rows are tried in order, and the first row that succeeds gives the
;; result. The `/error` rules have no rules here: a call that errs is stuck,
;; which is ERROR.

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.04-extern-host.rkt")
(provide ->term/eval-call-func
         ->term+ctx/eval-call-func)

;; The rules on a term
(define ->term/eval-call-func
  (reduction-relation
   al

   ;;; Clause invocation

   ;; rule Eval_clause/succ: the arguments are assigned, the premises hold,
   ;; and the output is evaluated, as a statement sequence
   (--> (eval-clause ((arg ...) exp_out (prem ...)) (val ...))
        (seq ((assign-args (arg ...) (val ...)) prem ... exp_out))
        "eval-clause")

   ;; rulegroup Eval_clauses, once the clause at hand is evaluated. An
   ;; unmatched clause is passed over by the frame (Eval_clauses/unmatch).
   (--> (eval-clauses (UNMATCH ...)) UNMATCH
        "eval-clauses/nil")
   (--> (eval-clauses (UNMATCH ... val_out (IN G L (eval-clause clause (val ...))) ...)) val_out
        "eval-clauses/succ")

   ;;; Table meta-function invocation

   ;; rulegroup Eval_tblrows
   (--> (eval-tblrows () (val ...)) UNMATCH
        "eval-tblrows/nil")
   (--> (eval-tblrows (tblRow_h tblRow_t ...) (val ...))
        (eval-tblrows/cons (eval-tblrow tblRow_h (val ...)) (tblRow_t ...) (val ...))
        "eval-tblrows/cons")
   (--> (eval-tblrows/cons UNMATCH (tblRow_t ...) (val ...)) (eval-tblrows (tblRow_t ...) (val ...))
        "eval-tblrows/unmatch")
   (--> (eval-tblrows/cons val_out (tblRow_t ...) (val ...)) val_out
        "eval-tblrows/succ")

   ;; rule Call_table_func
   (--> (call-table-func (TABLEFUNC id (param ...) typ (tblRow ...)) (val ...))
        (eval-tblrows (tblRow ...) (val ...))
        "call-table-func")

   ;;; Defined meta-function invocation

   ;; rulegroup Call_defined_func, once the clauses are evaluated (a rule
   ;; under the IN starts it): the result, or the `otherwise` clause in the
   ;; callee's layer when all are unmatched
   (--> (call-defined-func/clauses val_out (elsClause ...) (val ...)) val_out
        "call-defined-func/result")
   (--> (call-defined-func/clauses UNMATCH (elsClause) (val ...))
        (eval-clause elsClause (val ...))
        "call-defined-func/unmatch")
   (--> (call-defined-func/clauses UNMATCH () (val ...)) UNMATCH
        "call-defined-func/unmatch/none")

   ;;; Extern meta-function invocation, which reaches the host

   ;; extern relation Call_extern_func. An error reply is not a valres, so
   ;; the call is stuck.
   (--> (call-extern-func id (typ ...) (val ...)) valres
        (where valres ,(host-call-extern-func (term id) (term (typ ...)) (term (val ...))))
        "call-extern-func")

   ;;; Builtin meta-function invocation, which reaches the host

   ;; extern relation Call_builtin_func, likewise
   (--> (call-builtin-func id (typ ...) (val ...)) valres
        (where valres ,(host-call-builtin-func (term id) (term (typ ...)) (term (val ...))))
        "call-builtin-func")))

;; The rules on a term under its innermost IN, (IN G L (in-hole F r)), which
;; read G or L, or write L
(define ->term+ctx/eval-call-func
  (reduction-relation
   al

   ;;; Table meta-function invocation

   ;; rule Eval_tblrow/succ: the row runs in a local layer of its own, as a
   ;; statement sequence
   (--> (IN G L (in-hole F (eval-tblrow ((arg ...) exp_out (prem ...)) (val ...))))
        (IN G L (in-hole F (IN G (empty-localLayer) (seq ((assign-args (arg ...) (val ...)) prem ... exp_out)))))
        "eval-tblrow")

   ;;; Defined meta-function invocation

   ;; rulegroup Call_defined_func: the callee's local layer binds the type
   ;; parameters, and every clause runs in a copy of it
   (--> (IN G L (in-hole F (call-defined-func (FUNC id (tparam ..._n) (param ...) typ_ret (clause ...) (elsClause ...))
                                              (typ ..._n) (val ...))))
        (IN G L (in-hole F (IN G L_callee
                               (call-defined-func/clauses (eval-clauses ((IN G L_callee (eval-clause clause (val ...))) ...))
                                                          (elsClause ...) (val ...)))))
        (where L_callee {TYP ((tparam (TYP tparam () (ALIAS typ))) ...) FUNC (empty-map) VAL (empty-map)})
        "call-defined-func")

   ;;; Meta-function call

   ;; rulegroup Call_func: the name is resolved through the local function
   ;; environment, and its definition found in the global layer
   (--> (IN G L (in-hole F (call-func id (typ ...) (val ...))))
        (IN G L (in-hole F (call-extern-func id_func (typ ...) (val ...))))
        (where ((EXTFUNC id_func (tparam ...) (param ...) typ_ret)) (find-func G (resolve-func-param L id)))
        "call-func/extern")
   (--> (IN G L (in-hole F (call-func id (typ ...) (val ...))))
        (IN G L (in-hole F (call-builtin-func id_func (typ ...) (val ...))))
        (where ((BUILTINFUNC id_func (tparam ...) (param ...) typ_ret)) (find-func G (resolve-func-param L id)))
        "call-func/builtin")
   (--> (IN G L (in-hole F (call-func id () (val ...))))
        (IN G L (in-hole F (call-table-func tableFuncDefn (val ...))))
        (where (tableFuncDefn) (find-func G (resolve-func-param L id)))
        "call-func/table")
   (--> (IN G L (in-hole F (call-func id (typ ...) (val ...))))
        (IN G L (in-hole F (call-defined-func definedFuncDefn (typ ...) (val ...))))
        (where (definedFuncDefn) (find-func G (resolve-func-param L id)))
        "call-func/defined")))
