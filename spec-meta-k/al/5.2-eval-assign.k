// Assignment (pattern matching a value against an expression).
//
// Mirrors spec-meta/al/5.2-eval-assign.watsup.  `Assign_exp`, `Assign_exps`,
// `Assign_arg`, `Assign_args` are `relation`s in watsup, so they
// are ported as K-cell rewrite rules rather than functions.
//
// watsup's `ctx` result becomes an *implicit* `<local>` mutation plus an
// explicit outcome marker:
//
//   Assign_exp:  ctx |- exp := val : ctx      -->  assignExp(Exp, Val) : Outcome, mutates <local>
//   Assign_exps: ctx |- exp* := val* : ctx    -->  assignExps(ExpList, ValList) : Outcome
//   Assign_arg:  ctx/ctx |- arg := val : ctx  -->  assignArg(Arg, Val) : Outcome, reads <caller>
//   Assign_args: ctx/ctx |- arg* := val* : ctx -> assignArgs(ArgList, ValList) : Outcome

requires "3-context.k"
requires "../common/3-relation.k"

module AL-ASSIGN
  imports AL-CONTEXT
  imports COMMON-RELATION
  imports BOOL

  // ==========================================================================
  // Outcome marker
  //
  // watsup relations that "produce a ctx" either hold (in which case `<local>`
  // now reflects the new bindings) or do not hold at all. `assignOK()` and
  // `FAIL` are the two possible results left on `<k>` after a task completes.
  // `FAIL` itself is declared in common/3-relation.k, since the extern codec
  // produces it too and must not depend on this file.
  // ==========================================================================

  syntax Outcome ::= assignOK()             [symbol(assignOK)]

  // ==========================================================================
  // Assign_exp: ctx |- exp := val : ctx
  // ==========================================================================

  syntax KItem ::= assignExp(Exp, Val)

  //   rule Assign_exp/variable: C |- VAR id := val : C'
  rule <k> assignExp(varE(X:Id), V:Val) => assignOK() ... </k>
       <lvenv> M:Map => M[varr(X, .IterList) <- V] </lvenv>

  //   rule Assign_exp/tup: C |- TUP exp* := TUP val* : C'
  rule <k> assignExp(tupE(Es:ExpList), tupV(Vs:ValList)) => assignExps(Es, Vs) ... </k>

  //   rule Assign_exp/inj: C |- INJ (mixop exp*) := INJ (mixop val*) : C'
  rule <k> assignExp(injE(expCase(M:MixOp, Es:ExpList)),
                     injV(valCase(M:MixOp, Vs:ValList)))
        => assignExps(Es, Vs)
       ...
       </k>

  //   rule Assign_exp/str: C |- STR expfield* := STR valfield* : C'
  rule <k> assignExp(strE(Fs:ExpFieldList), strV(Gs:ValFieldList))
        => assignFields(Fs, Gs)
       ...
       </k>

  //   rulegroup Assign_exp/opt
  rule <k> assignExp(optE(someExp(E:Exp)), optV(someVal(V:Val))) => assignExp(E, V) ... </k>
  rule <k> assignExp(optE(noExp()), optV(noVal())) => assignOK() ... </k>

  //   rule Assign_exp/list: C |- LIST exp* := LIST val* : C'
  rule <k> assignExp(listE(Es:ExpList), listV(Vs:ValList)) => assignExps(Es, Vs) ... </k>

  //   rule Assign_exp/cons: C |- CONS exp_h exp_t := LIST (val_h :: val_t*) : C''
  rule <k> assignExp(consE(Eh:Exp, Et:Exp), listV(Vh:Val, Vt:ValList))
        => assignExp(Eh, Vh) ~> assignThen(assignExp(Et, listV(Vt)))
       ...
       </k>

  //   rulegroup Assign_exp/iter
  rule <k> assignExp(iterE(E:Exp, IE:IterExp), V:Val)
        => assignIter(isIterOnVar(iterE(E, IE)), E, IE, V)
       ...
       </k>

  rule <k> assignExp(_:Exp, _:Val) => FAIL ... </k> [owise]

  // Sequencing helper: run the second task only if the first succeeded.
  syntax KItem ::= assignThen(KItem)
  rule <k> assignOK() ~> assignThen(Next:KItem) => Next ... </k>
  rule <k> FAIL ~> assignThen(_:KItem) => FAIL ... </k>

  // ==========================================================================
  // Assigning to an iterated expression
  // ==========================================================================

  syntax KItem ::= assignIter(VarrOpt, Exp, IterExp, Val)

  //   rule Assign_exp/simple
  rule <k> assignIter(someVarr(R:Varr), _:Exp, _:IterExp, V:Val) => assignOK() ... </k>
       <lvenv> M:Map => M[R <- V] </lvenv>

  //   rule Assign_exp/opt-none
  rule <k> assignIter(noVarr(), _:Exp, iterExp(quest(), Vs:VariList), optV(noVal()))
        => assignOK()
       ...
       </k>
       <lvenv> M:Map
            => addsMap(M, iterVaris(Vs, quest()),
                       repeatVal(optV(noVal()), lengthVaris(Vs)))
       </lvenv>

  //   rule Assign_exp/opt-some
  rule <k> assignIter(noVarr(), E:Exp, iterExp(quest(), Vs:VariList), optV(someVal(V:Val)))
        => assignExp(E, V) ~> assignThen(bindQuestFromLocal(Vs))
       ...
       </k>

  syntax KItem ::= bindQuestFromLocal(VariList)
  rule <k> bindQuestFromLocal(Vs:VariList) => assignOK() ... </k>
       <lvenv> M:Map
            => addsMap(M, iterVaris(Vs, quest()), someVals(findVaris(Vs, M)))
       </lvenv>

  //   rule Assign_exp/list
  //
  // Each element is assigned in a scratch environment (the caller's <lvenv>
  // cleared), one at a time; the per-element bindings for `vari*` are
  // collected into `LIST`s and added to the *original* <lvenv>, restored via
  // <saves> before the collected lists are bound.
  rule <k> assignIter(noVarr(), E:Exp, iterExp(star(), Vs:VariList), listV(Ws:ValList))
        => pushSave() ~> clearLocalVal()
           ~> assignEach(E, Ws, Vs, .List)
       ...
       </k>

  // `pushSave()` snapshots <local> onto <saves>; `clearLocalVal()` empties
  // <lvenv> only (tdenv/fenv are untouched by `.LOCAL.VAL = ...`).
  syntax KItem ::= "pushSave" "(" ")"
                 | "clearLocalVal" "(" ")"

  rule <k> pushSave() => .K ... </k>
       <ltdenv> T:Map </ltdenv>
       <lfenv> F:Map </lfenv>
       <lvenv> V:Map </lvenv>
       <saves> .List => ListItem(localSave(T, F, V)) ... </saves>

  rule <k> clearLocalVal() => .K ... </k>
       <lvenv> _ => .Map </lvenv>

  // Assign `exp` against every element of `val*` in turn, in the cleared
  // scratch environment, collecting each element's bindings for `vari*` (as a
  // `List` of `ValList`s, one per element, in order) before restoring <local>
  // and merging the collected columns in as `LIST`-valued bindings.
  syntax KItem ::= assignEach(Exp, ValList, VariList, List)

  rule <k> assignEach(_:Exp, .ValList, Vs:VariList, Acc:List)
        => popSaveMergeLists(Vs, Acc)
       ...
       </k>

  rule <k> assignEach(E:Exp, (V:Val, Rest:ValList), Vs:VariList, Acc:List)
        => assignExp(E, V) ~> assignThen(collectThenContinue(E, Rest, Vs, Acc))
       ...
       </k>

  syntax KItem ::= collectThenContinue(Exp, ValList, VariList, List)
  rule <k> collectThenContinue(E:Exp, Rest:ValList, Vs:VariList, Acc:List)
        => clearLocalVal() ~> assignEach(E, Rest, Vs, Acc ListItem(findVaris(Vs, M)))
       ...
       </k>
       <lvenv> M:Map </lvenv>

  // Restore <local> from <saves>, then bind each `vari` in `Vs` to the `LIST`
  // formed from its column across `Acc` (one entry per element assigned).
  syntax KItem ::= popSaveMergeLists(VariList, List)
  rule <k> popSaveMergeLists(Vs:VariList, Acc:List) => assignOK() ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => addsMap(V, iterVaris(Vs, star()), transposeToLists(Vs, Acc)) </lvenv>
       <saves> ListItem(localSave(T:Map, F:Map, V:Map)) => .List ... </saves>

  // ==========================================================================
  // Assign_exps: ctx |- exp* := val* : ctx
  // ==========================================================================

  syntax KItem ::= assignExps(ExpList, ValList)

  rule <k> assignExps(.ExpList, .ValList) => assignOK() ... </k>
  rule <k> assignExps((E:Exp, Es:ExpList), (V:Val, Vs:ValList))
        => assignExp(E, V) ~> assignThen(assignExps(Es, Vs))
       ...
       </k>
  rule <k> assignExps(_:ExpList, _:ValList) => FAIL ... </k> [owise]

  // Struct fields, matched positionally with their atoms compared.
  syntax KItem ::= assignFields(ExpFieldList, ValFieldList)

  rule <k> assignFields(.ExpFieldList, .ValFieldList) => assignOK() ... </k>
  rule <k> assignFields((expField(A:Atom, E:Exp), Fs:ExpFieldList),
                        (valField(A:Atom, V:Val), Gs:ValFieldList))
        => assignExp(E, V) ~> assignThen(assignFields(Fs, Gs))
       ...
       </k>
  rule <k> assignFields(_:ExpFieldList, _:ValFieldList) => FAIL ... </k> [owise]

  // ==========================================================================
  // Assign_arg / Assign_args: ctx_callee '/' ctx_caller |- arg(*) := val(*)
  //
  // `<local>` holds `ctx_callee`; `<caller>`'s `cfenv` holds `ctx_caller`'s
  // local function environment (all `Assign_arg/fun` needs — see 0-config.k).
  // The caller cell must be populated by whoever starts an `assignArgs` task
  // (Call_defined_func / Eval_tblrow, in 4-call-func.k) and is read-only here.
  // ==========================================================================

  syntax KItem ::= assignArg(Arg, Val)
                 | assignArgs(ArgList, ValList)

  //   rule Assign_arg/exp
  rule <k> assignArg(expA(E:Exp), V:Val) => assignExp(E, V) ... </k>

  //   rule Assign_arg/fun: looks up id_caller in the caller's fenv (local then
  //   global; global is shared and read directly from <gfenv>)
  syntax KItem ::= findFuncCaller(Id, Id)   // (callee id to bind, caller id to look up)

  rule <k> assignArg(funA(X:Id), funcV(Y:Id)) => findFuncCaller(X, Y) ... </k>

  rule <k> assignArg(_:Arg, _:Val) => FAIL ... </k> [owise]

  rule <k> findFuncCaller(X:Id, Y:Id) => assignOK() ... </k>
       <cfenv> CF:Map </cfenv>
       <lfenv> M:Map => M[X <- {CF[Y]}:>FuncDef] </lfenv>
    requires Y in_keys(CF)
  rule <k> findFuncCaller(X:Id, Y:Id) => assignOK() ... </k>
       <cfenv> CF:Map </cfenv>
       <gfenv> GF:Map </gfenv>
       <lfenv> M:Map => M[X <- {GF[Y]}:>FuncDef] </lfenv>
    requires notBool (Y in_keys(CF)) andBool (Y in_keys(GF))
  rule <k> findFuncCaller(_:Id, Y:Id) => FAIL ... </k>
       <cfenv> CF:Map </cfenv>
       <gfenv> GF:Map </gfenv>
    requires notBool (Y in_keys(CF)) andBool notBool (Y in_keys(GF))

  //   rulegroup Assign_args
  rule <k> assignArgs(.ArgList, .ValList) => assignOK() ... </k>
  rule <k> assignArgs((A:Arg, As:ArgList), (V:Val, Vs:ValList))
        => assignArg(A, V) ~> assignThen(assignArgs(As, Vs))
       ...
       </k>
  rule <k> assignArgs(_:ArgList, _:ValList) => FAIL ... </k> [owise]

  // ==========================================================================
  // Value-level helpers
  //
  // These are ordinary functions (not judgments), matching stdlib/env
  // functions such as `$adds_map`, `$repeat_<val>`, `$iter_vari`,
  // `$find_vari`, `$finds_vari` instantiated on the cell-based `Map`.
  // ==========================================================================

  // `$adds_map<varr, val>`: add each key/value pair, in order. Partial on
  // mismatched lengths, as `$adds_map` is.
  syntax Map ::= addsMap(Map, VarrList, ValList)
                                            [function, symbol(addsMap)]

  rule addsMap(M:Map, .VarrList, .ValList) => M
  rule addsMap(M:Map, (R:Varr, Rs:VarrList), (V:Val, Vs:ValList))
    => addsMap(M[R <- V], Rs, Vs)

  // `$repeat_<val>(val, n)` is `repeatVal` from common/0-stdlib.k.

  // `vari*` at one more `iter`, and its length.
  syntax VarrList ::= iterVaris(VariList, Iter)
                                            [function, total, symbol(iterVaris)]
  rule iterVaris(.VariList, _:Iter) => .VarrList
  rule iterVaris((V:Vari, Vs:VariList), I:Iter)
    => varrOfVari(iterVari(V, I)), iterVaris(Vs, I)

  syntax Int ::= lengthVaris(VariList)      [function, total, symbol(lengthVaris)]
  rule lengthVaris(.VariList) => 0
  rule lengthVaris(_:Vari, Vs:VariList) => 1 +Int lengthVaris(Vs)

  // `$find_vari`/`$finds_vari` against an explicit `Map` (rather than
  // `<local>`), used where the environment being searched is a scratch value
  // already extracted from a cell (e.g. after popping a save). Defined only
  // when every lookup succeeds, matching watsup's total-or-nothing `val*?`.
  syntax ValList ::= findVaris(VariList, Map)
                                            [function, symbol(findVaris)]
  rule findVaris(.VariList, _:Map) => .ValList
  rule findVaris((V:Vari, Vs:VariList), M:Map) => {M[varrOfVari(V)]}:>Val, findVaris(Vs, M)
    requires varrOfVari(V) in_keys(M)

  // `(OPT val_sub)*`: wrap every element as `optV(someVal(_))`.
  syntax ValList ::= someVals(ValList)      [function, total, symbol(someVals)]
  rule someVals(.ValList) => .ValList
  rule someVals(V:Val, Vs:ValList) => optV(someVal(V)), someVals(Vs)

  // Transpose the per-element binding columns in `Acc` (a `List` of
  // `ValList`s, one per element, each in the same order as `vari*`) into one
  // `ValList` of `LIST val_sub*`, one per `vari`, in the same order as `Vs`.
  // `transposeToLists` walks `Vs` while `transposeToLists0` walks the position
  // within each row, so the two stay in lock-step by construction.
  syntax ValList ::= transposeToLists(VariList, List)
                                            [function, symbol(transposeToLists)]
  rule transposeToLists(Vs:VariList, Acc:List) => transposeToLists0(Vs, 0, Acc)

  syntax ValList ::= transposeToLists0(VariList, Int, List)
                                            [function, total, symbol(transposeToLists0)]
  rule transposeToLists0(.VariList, _:Int, _:List) => .ValList
  rule transposeToLists0((_:Vari, Vs:VariList), I:Int, Acc:List)
    => listV(columnAt(I, Acc)), transposeToLists0(Vs, I +Int 1, Acc)

  // `columnAt(i, Acc)`: the `i`-th element of every `ValList` in `Acc`, in
  // order.
  syntax ValList ::= columnAt(Int, List)    [function, symbol(columnAt)]
  rule columnAt(_:Int, .List) => .ValList
  rule columnAt(I:Int, ListItem(Vs:ValList) Rest:List)
    => nthVal(Vs, I), columnAt(I, Rest)

  syntax Val ::= nthVal(ValList, Int)       [function, symbol(nthVal)]
  rule nthVal((V:Val, _:ValList), 0) => V
  rule nthVal((_:Val, Vs:ValList), N:Int) => nthVal(Vs, N -Int 1) requires N >Int 0

endmodule
