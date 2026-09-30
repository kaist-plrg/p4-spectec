;; spec-meta/al/5.6-eval-call-func.watsup, included in 5-eval.rkt.

;;
;; Clause invocation
;;

;; ctx '/' ctx |- clause '@' `( val* `) : res<val>
(define-relation al
  #:mode (eval-clause I I I I O)
  #:contract (eval-clause ctx ctx clause (val ...) valres)
  [(where ((arg ...) exp_out (prem ...)) clause)
   (where (C_1 ...)
          ,(judgment-holds (assign-args C C_caller (arg ...) (val ...) C_out) C_out))
   (eval-clause/succ (C_1 ...) (prem ...) exp_out valres)
   ------------------------------------------------ "succ"
   (eval-clause C C_caller clause (val ...) valres)])

;; rule Eval_clause/succ, given the outputs of Assign_args
(define-relation al
  #:mode (eval-clause/succ I I I O)
  #:contract (eval-clause/succ (ctx ...) (prem ...) exp valres)
  [(eval-prems C_1 (prem ...) ctxres)
   (eval-clause/succ-prems exp_out ctxres valres)
   ------------------------------------------------- "succ"
   (eval-clause/succ (C_1) (prem ...) exp_out valres)]
  [--------------------------------------------- "fail"
   (eval-clause/succ () (prem ...) exp_out FAIL)])

;; rule Eval_clause/succ, given the premises
(define-relation al
  #:mode (eval-clause/succ-prems I I O)
  #:contract (eval-clause/succ-prems exp ctxres valres)
  [(eval-exp C_2 exp_out valres_out)
   (eval-clause/succ-exp valres_out valres)
   ------------------------------------------------ "succ"
   (eval-clause/succ-prems exp_out (OK C_2) valres)]
  [------------------------------------------ "fail"
   (eval-clause/succ-prems exp_out FAIL FAIL)])

;; rule Eval_clause/succ, given the output expression
(define-relation al
  #:mode (eval-clause/succ-exp I O)
  #:contract (eval-clause/succ-exp valres valres)
  [------------------------------------------------ "succ"
   (eval-clause/succ-exp (OK val_out) (OK val_out))]
  [-------------------------------- "fail"
   (eval-clause/succ-exp FAIL FAIL)])

;; ctx '/' ctx |- clause* '@' `( val* `) : res<val>
(define-relation al
  #:mode (eval-clauses I I I I O)
  #:contract (eval-clauses ctx ctx (clause ...) (val ...) valres)
  [------------------------------------------- "nil"
   (eval-clauses C C_caller () (val ...) FAIL)]
  [(eval-clause C C_caller clause_h (val ...) valres_h)
   (eval-clauses/cons C C_caller (clause_t ...) (val ...) valres_h valres)
   --------------------------------------------------------------- "cons"
   (eval-clauses C C_caller (clause_h clause_t ...) (val ...) valres)])

;; rules Eval_clauses/cons-succ and cons-fail, given the head
(define-relation al
  #:mode (eval-clauses/cons I I I I I O)
  #:contract (eval-clauses/cons ctx ctx (clause ...) (val ...) valres valres)
  [----------------------------------------------------------------------------- "cons-succ"
   (eval-clauses/cons C C_caller (clause_t ...) (val ...) (OK val_res) (OK val_res))]
  [(eval-clauses C C_caller (clause_t ...) (val ...) valres)
   ------------------------------------------------------------------ "cons-fail"
   (eval-clauses/cons C C_caller (clause_t ...) (val ...) FAIL valres)])

;;
;; Table meta-function invocation
;;

;;; Table row evaluation

;; ctx |- tblrow '@' `( val* `) : res<val>
(define-relation al
  #:mode (eval-tblrow I I I O)
  #:contract (eval-tblrow ctx tblrow (val ...) valres)
  [(where ((arg ...) exp_out (prem ...)) tblrow)
   (where {GLOBAL layer_g LOCAL _} C)
   (where layer_l (empty-layer))
   (where C_callee {GLOBAL layer_g LOCAL layer_l})
   (where (C_callee_1 ...)
          ,(judgment-holds (assign-args C_callee C (arg ...) (val ...) C_out) C_out))
   (eval-tblrow/succ (C_callee_1 ...) (prem ...) exp_out valres)
   ---------------------------------------- "succ"
   (eval-tblrow C tblrow (val ...) valres)])

;; rule Eval_tblrow/succ, given the outputs of Assign_args
(define-relation al
  #:mode (eval-tblrow/succ I I I O)
  #:contract (eval-tblrow/succ (ctx ...) (prem ...) exp valres)
  [(eval-prems C_callee_1 (prem ...) ctxres)
   (eval-tblrow/succ-prems exp_out ctxres valres)
   -------------------------------------------------------- "succ"
   (eval-tblrow/succ (C_callee_1) (prem ...) exp_out valres)]
  [--------------------------------------------- "fail"
   (eval-tblrow/succ () (prem ...) exp_out FAIL)])

;; rule Eval_tblrow/succ, given the premises
(define-relation al
  #:mode (eval-tblrow/succ-prems I I O)
  #:contract (eval-tblrow/succ-prems exp ctxres valres)
  [(eval-exp C_callee_2 exp_out valres_out)
   (eval-tblrow/succ-exp valres_out valres)
   ------------------------------------------------------- "succ"
   (eval-tblrow/succ-prems exp_out (OK C_callee_2) valres)]
  [------------------------------------------ "fail"
   (eval-tblrow/succ-prems exp_out FAIL FAIL)])

;; rule Eval_tblrow/succ, given the output expression
(define-relation al
  #:mode (eval-tblrow/succ-exp I O)
  #:contract (eval-tblrow/succ-exp valres valres)
  [------------------------------------------------ "succ"
   (eval-tblrow/succ-exp (OK val_out) (OK val_out))]
  [-------------------------------- "fail"
   (eval-tblrow/succ-exp FAIL FAIL)])

;;; Table row sequence evaluation

;; ctx |- tblrow* '@' `( val* `) : res<val>
(define-relation al
  #:mode (eval-tblrows I I I O)
  #:contract (eval-tblrows ctx (tblrow ...) (val ...) valres)
  [---------------------------------- "nil"
   (eval-tblrows C () (val ...) FAIL)]
  [(eval-tblrow C tblrow_h (val ...) valres_h)
   (eval-tblrows/cons C (tblrow_t ...) (val ...) valres_h valres)
   ------------------------------------------------------ "cons"
   (eval-tblrows C (tblrow_h tblrow_t ...) (val ...) valres)])

;; rules Eval_tblrows/cons-succ and cons-fail, given the head
(define-relation al
  #:mode (eval-tblrows/cons I I I I O)
  #:contract (eval-tblrows/cons ctx (tblrow ...) (val ...) valres valres)
  [-------------------------------------------------------------------- "cons-succ"
   (eval-tblrows/cons C (tblrow_t ...) (val ...) (OK val_out) (OK val_out))]
  [(eval-tblrows C (tblrow_t ...) (val ...) valres)
   --------------------------------------------------------- "cons-fail"
   (eval-tblrows/cons C (tblrow_t ...) (val ...) FAIL valres)])

;;; Table meta-function evaluation

;; ctx |- tableFuncDef '@' `( val* `) : res<val>
(define-relation al
  #:mode (call-table-func I I I O)
  #:contract (call-table-func ctx tableFuncDef (val ...) valres)
  [(where (TABLE _ (tblrow ...)) tableFuncDef)
   (eval-tblrows C (tblrow ...) (val ...) valres)
   ------------------------------------------------- "call-table-func"
   (call-table-func C tableFuncDef (val ...) valres)])

;;
;; Defined meta-function invocation
;;

;; ctx |- definedFuncDef '@' `< typ* `> `( val* `) : res<val>
(define-relation al
  #:mode (call-defined-func I I I I O)
  #:contract (call-defined-func ctx definedFuncDef (typ ...) (val ...) valres)
  [(where (DEF (tparam ...) (clause ...) (elsclause ...)) definedFuncDef)
   (side-condition ,(= (length (term (tparam ...))) (length (term (typ ...)))))
   (where tdenv ((tparam (DEF () (ALIAS typ))) ...))
   (where {GLOBAL layer_g LOCAL _} C)
   (where {TYP _ REL renv FUNC fenv VAL venv} (empty-layer))
   (where C_callee {GLOBAL layer_g LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}})
   (where (clause_else ...) (opt-as-seq- (elsclause ...)))
   (where (clause_all ...) (clause ... clause_else ...))
   (eval-clauses C_callee C (clause_all ...) (val ...) valres)
   --------------------------------------------------------------- "call-defined-func"
   (call-defined-func C definedFuncDef (typ ...) (val ...) valres)])

;;
;; Meta-function dispatch
;;

;; ctx |- funcdef targ* val* : res<val>
(define-relation al
  #:mode (call-func-dispatch I I I I O)
  #:contract (call-func-dispatch ctx funcdef (typ ...) (val ...) valres)
  [(call-extern-func id (typ ...) (val ...) valres)
   ---------------------------------------------------------- "extern"
   (call-func-dispatch C (EXT id) (typ ...) (val ...) valres)]
  [(where (BUILTIN id _ _) builtinFuncDef)
   (call-builtin-func id (typ ...) (val ...) valres)
   ---------------------------------------------------------------- "builtin"
   (call-func-dispatch C builtinFuncDef (typ ...) (val ...) valres)]
  [(call-table-func C tableFuncDef (val ...) valres)
   ------------------------------------------------------- "table"
   (call-func-dispatch C tableFuncDef () (val ...) valres)]
  [(call-defined-func C definedFuncDef (typ ...) (val ...) valres)
   ---------------------------------------------------------------- "defined"
   (call-func-dispatch C definedFuncDef (typ ...) (val ...) valres)])

;;
;; Meta-function call
;;

;; ctx |- id targ* val* : res<val>
(define-relation al
  #:mode (call-func I I I I O)
  #:contract (call-func ctx id (typ ...) (val ...) valres)
  [(where (funcdef) (find-func C id))
   (call-func-dispatch C funcdef (typ ...) (val ...) valres)
   ---------------------------------------------- "call-func"
   (call-func C id (typ ...) (val ...) valres)])
