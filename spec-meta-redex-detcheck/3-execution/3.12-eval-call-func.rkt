#lang racket/base
;; Mirrors spec-meta/3-execution/3.10-eval-call-func.watsup.

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

   ;; rule Eval_clause/succ
   (--> (eval-clause ((arg ...) exp_out (prem ...)) (val ...))
        (seq ((assign-args (arg ...) (val ...)) prem ... exp_out))
        "eval-clause")

   ;; rulegroup Eval_clauses, once the clause at hand is evaluated
   (--> (eval-clauses (UNMATCH ...)) UNMATCH
        "eval-clauses/nil")
   ;; (--> (eval-clauses (UNMATCH ... val_out (IN L (eval-clause clause (val ...))) ...)) val_out
   ;;      "eval-clauses/succ")
   (--> (eval-clauses (UNMATCH ... ERROR (IN L (eval-clause clause (val ...))) ...)) ERROR
        "eval-clauses/error")

   ;; Determinism check (Eval_clauses_on_ok): replaces "eval-clauses/succ".
   ;;
   (--> (eval-clauses (UNMATCH ... val_out (IN L (eval-clause clause (val ...))) ...))
        (eval-clauses-on-ok val_out ((IN L (eval-clause clause (val ...))) ...))
        "eval-clauses/succ")
   ;;
   ;; rulegroup Eval_clauses_on_ok, once the clause at hand is evaluated
   (--> (eval-clauses-on-ok val_out (UNMATCH ...)) val_out
        "eval-clauses-on-ok/nil")
   (--> (eval-clauses-on-ok val_out (UNMATCH ... valres (IN L (eval-clause clause (val ...))) ...)) ERROR
        (side-condition (not (equal? (term valres) 'UNMATCH)))
        "eval-clauses-on-ok/error")

   ;;; Table meta-function invocation

   ;; rule Eval_tblrow/succ
   (--> (eval-tblrow ((arg ...) exp_out (prem ...)) (val ...))
        (IN (empty-localLayer) (seq ((assign-args (arg ...) (val ...)) prem ... exp_out)))
        "eval-tblrow")

   ;; rulegroup Eval_tblrows
   (--> (eval-tblrows () (val ...)) UNMATCH
        "eval-tblrows/nil")
   (--> (eval-tblrows (tblRow_h tblRow_t ...) (val ...))
        (eval-tblrows/cons (eval-tblrow tblRow_h (val ...)) (tblRow_t ...) (val ...))
        "eval-tblrows/cons")
   (--> (eval-tblrows/cons UNMATCH (tblRow_t ...) (val ...)) (eval-tblrows (tblRow_t ...) (val ...))
        "eval-tblrows/unmatch")
   (--> (eval-tblrows/cons valres (tblRow_t ...) (val ...)) valres
        (side-condition (not (equal? (term valres) 'UNMATCH)))
        "eval-tblrows/cons-result")

   ;; rule Call_table_func
   (--> (call-table-func (TABLEFUNC id (param ...) typ (tblRow ...)) (val ...))
        (eval-tblrows (tblRow ...) (val ...))
        "call-table-func")

   ;;; Defined meta-function invocation

   ;; rulegroup Call_defined_func
   (--> (call-defined-func (FUNC id (tparam ..._n) (param ...) typ_ret (clause ...) (elsClause ...))
                           (typ ..._n) (val ...))
        (IN L_callee
            (call-defined-func/clauses (eval-clauses ((IN L_callee (eval-clause clause (val ...))) ...))
                                       (elsClause ...) (val ...)))
        (where L_callee {TYP ((tparam (TYP tparam () (ALIAS typ))) ...) FUNC (empty-map) VAL (empty-map)})
        "call-defined-func")
   (--> (call-defined-func (FUNC id (tparam ...) (param ...) typ_ret (clause ...) (elsClause ...))
                           (typ ...) (val ...))
        ERROR
        ;; otherwise
        (side-condition (not (= (length (term (tparam ...))) (length (term (typ ...))))))
        "call-defined-func/error")
   (--> (call-defined-func/clauses valres (elsClause ...) (val ...)) valres
        (side-condition (not (equal? (term valres) 'UNMATCH)))
        "call-defined-func/result")
   (--> (call-defined-func/clauses UNMATCH (elsClause) (val ...))
        (eval-clause elsClause (val ...))
        "call-defined-func/unmatch")
   (--> (call-defined-func/clauses UNMATCH () (val ...)) UNMATCH
        "call-defined-func/unmatch/none")

   ;;; Extern meta-function invocation

   ;; extern relation Call_extern_func
   (--> (call-extern-func id (typ ...) (val ...)) valres
        (where valres ,(host-call-extern-func (term id) (term (typ ...)) (term (val ...))))
        "call-extern-func")

   ;;; Builtin meta-function invocation

   ;; extern relation Call_builtin_func
   (--> (call-builtin-func id (typ ...) (val ...)) valres
        (where valres ,(host-call-builtin-func (term id) (term (typ ...)) (term (val ...))))
        "call-builtin-func")))

;; The rules on a term and its context, (r G L K)
(define ->term+ctx/eval-call-func
  (reduction-relation
   al

   ;;; Meta-function call

   ;; rulegroup Call_func
   (--> ((call-func id (typ ...) (val ...)) G L K) ((call-extern-func id_func (typ ...) (val ...)) G L K)
        (where ((EXTFUNC id_func (tparam ...) (param ...) typ_ret)) (find-func G (resolve-func-param L id)))
        "call-func/extern")
   (--> ((call-func id (typ ...) (val ...)) G L K) ((call-builtin-func id_func (typ ...) (val ...)) G L K)
        (where ((BUILTINFUNC id_func (tparam ...) (param ...) typ_ret)) (find-func G (resolve-func-param L id)))
        "call-func/builtin")
   (--> ((call-func id () (val ...)) G L K) ((call-table-func tableFuncDefn (val ...)) G L K)
        (where (tableFuncDefn) (find-func G (resolve-func-param L id)))
        "call-func/table")
   (--> ((call-func id (typ ...) (val ...)) G L K) ((call-defined-func definedFuncDefn (typ ...) (val ...)) G L K)
        (where (definedFuncDefn) (find-func G (resolve-func-param L id)))
        "call-func/defined")
   (--> ((call-func id (typ ...) (val ...)) G L K) (ERROR G L K)
        (where () (find-func G (resolve-func-param L id)))
        "call-func/error")
   (--> ((call-func id (typ_h typ_t ...) (val ...)) G L K) (ERROR G L K)
        (where (tableFuncDefn) (find-func G (resolve-func-param L id)))
        "call-func/table/error")))
