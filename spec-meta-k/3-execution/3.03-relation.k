// Relations: their declarations and the extern relations.
//
// Mirrors spec-meta/3-execution/3.03-relation.watsup. A relation of the
// spec is a `<k>` computation here; this module declares the computations
// and the modules that follow give their rules. The context `ctx` is the
// configuration, and a `failure` outcome is a signal in flight
// (3.00-result.k).

requires "3.04.2-extern-ffi.k"

module RELATION
  imports EXTERN-FFI

  // ==========================================================================
  // Extern meta-function and relation invocation
  //
  //   extern relation Call_extern_func:  |- id `< typ* `> `( val* `) : funcRes
  //   extern relation Call_builtin_func: |- id `< typ* `> `( val* `) : funcRes
  //   extern relation Call_extern_rel:   |- id `( val* `) : relRes
  // ==========================================================================

  syntax KItem ::= callExternFunc(Id, TypList, ValList)
                 | callBuiltinFunc(Id, TypList, ValList)
                 | callExternRel(Id, ValList)

  // ==========================================================================
  // Evaluation relations
  //
  //   relation Eval_exp:       ctx |- exp : expRes
  //   relation Eval_exps:      ctx |- exp* : expsRes
  //   relation Eval_exp_maybe: ctx? |- exp : expOptRes
  //   relation Eval_exp_each:  ctx* |- exp : expsRes
  //
  // An `Exp` or `ExpList` at the head of `<k>` evaluates in place by the
  // strictness of 1.0-syntax.k (3.08-eval-exp.k); the optional and list
  // contexts are `optSub`/`listSub` there.
  //
  //   relation Eval_path_acc: ctx |- val path : pathRes
  //   relation Eval_path_upd: ctx |- val path := val : pathRes
  // ==========================================================================

  syntax Exp ::= path(Val, Path)
               | upd(Val, Path, Exp)        [strict(3)]

  //   relation Eval_targs: ctx |- targ* : typsRes    -- within `callE` (3.10)
  //   relation Eval_arg:   ctx |- arg : argRes       -- an `Arg` at the head of <k>
  //   relation Eval_args:  ctx |- arg* : argsRes     -- an `ArgList`
  //
  //   relation Eval_prem:       ctx |- prem : premRes    -- a `Prem` at the head of <k>
  //   relation Eval_prems:      ctx |- prem* : premRes   -- a `PremList`
  //   relation Eval_prem_maybe: ctx? |- prem : premOptRes   -- `iterPrOpt` (3.11)
  //   relation Eval_prem_each:  ctx* |- prem : premIterRes -- `premRows` (3.11)

  // ==========================================================================
  // Assignment relations (3.06)
  //
  //   relation Assign_exp:  ctx |- exp := val : assignRes
  //   relation Assign_exps: ctx |- exp* := val* : assignRes
  //   relation Assign_arg:  ctx |- arg := val : assignRes
  //   relation Assign_args: ctx |- arg* := val* : assignRes
  //
  // A statement: `.K` with the bindings added to `<local>`, or `error`. The
  // values of `assignExps` may still be a computation (a relation's outputs).
  // ==========================================================================

  syntax KItem ::= assignExp(Exp, Val)
                 | assignExps(ExpList, KItem) [strict(2)]
                 | assignArg(Arg, Val)
                 | assignArgs(ArgList, ValList)

  // ==========================================================================
  // Call relations (3.11, 3.12)
  //
  //   relation Eval_clause:        ctx |- clause `( val* `) : funcRes
  //   relation Eval_clauses:       ctx |- clause* `( val* `) : funcRes
  //   relation Eval_clauses_on_ok: ctx |- clause* `( val* `) : unitRes
  //   relation Eval_tblrow:        ctx |- tblRow `( val* `) : funcRes
  //   relation Eval_tblrows:       ctx |- tblRow* `( val* `) : funcRes
  //   relation Call_table_func:    ctx |- tableFuncDefn `( val* `) : funcRes
  //   relation Call_defined_func:  ctx |- definedFuncDefn `< typ* `> `( val* `) : funcRes
  //   relation Call_func:          ctx |- id `< targ* `> `( val* `) : funcRes
  //
  //   syntax rul = rulMatch -> rulPath
  //   relation Eval_rul:          ctx |- rul `( val* `) : relRes
  //   relation Eval_ruls:         ctx |- rul* `( val* `) : relRes
  //   relation Eval_ruls_on_ok:   ctx |- rul* `( val* `) : unitRes
  //   relation Call_defined_rel:  ctx |- definedRelDefn `( val* `) : relRes
  //   relation Call_rel:          ctx |- id `( val* `) : relRes
  // ==========================================================================

  syntax KItem ::= evalClause(Clause, ValList)
                 | evalClauses(ClauseList, ValList)
                 | evalClausesOnOk(ClauseList, ValList)
                 | evalTblRow(TblRow, ValList)
                 | evalTblRows(TblRowList, ValList)
                 | callTableFunc(TableFuncDefn, ValList)
                 | callDefinedFunc(DefinedFuncDefn, TArgList, ValList)
                 | callFunc(Id, TArgList, ValList)

  syntax Rul ::= rul(RulMatch, RulPath)     [symbol(rul)]
  syntax RulList ::= List{Rul, ","}         [symbol(rulList), terminator-symbol(.rulList)]

  syntax KItem ::= evalRul(Rul, ValList)
                 | evalRuls(RulList, ValList)
                 | evalRulsOnOk(RulList, ValList)
                 | callDefinedRel(DefinedRelDefn, ValList)
                 | callRel(Id, ValList)

  // ==========================================================================
  // Call_extern_func / Call_builtin_func / Call_extern_rel: to the host
  // ==========================================================================

  rule <k> callExternFunc(X:Id, Ts:TypList, Vs:ValList)
        => externReq(JSON2String(jsonOfExternFuncRequest(X, Ts, Vs)),
                     externFuncK())
       ...
       </k>

  rule <k> callBuiltinFunc(X:Id, Ts:TypList, Vs:ValList)
        => externReq(JSON2String(jsonOfRequest(X, Ts, Vs)), builtinK())
       ...
       </k>

  rule <k> callExternRel(X:Id, Vs:ValList)
        => externReq(JSON2String(jsonOfExternRelRequest(X, Vs)),
                     externRelK())
       ...
       </k>

endmodule
