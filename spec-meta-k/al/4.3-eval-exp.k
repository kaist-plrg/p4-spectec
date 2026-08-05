// Expression, path, and argument evaluation.
//
// Mirrors spec-meta/al/5.3-eval-exp.watsup (Eval_exp, Eval_path,
// Eval_path_upd) and spec-meta/al/5.4-eval-arg.watsup (Eval_arg, Eval_targs) —
// there is no separate 5.4-eval-arg.k, because `Eval_arg` is two rules that
// are mutually recursive with `Eval_exp` and so cannot be split into their own
// K module.

requires "4.2-eval-assign.k"
requires "4.1-eval-typ.k"

module AL-EVAL-EXP
  imports AL-ASSIGN
  imports AL-TYP

  // ==========================================================================
  // Eval_exp: ctx |- exp : res<val>
  // ==========================================================================

  syntax KItem ::= evalExp(Exp)

  //   rulegroup Eval_exp/literal
  rule <k> evalExp(boolE(B:Bool)) => boolV(B) ... </k>
  rule <k> evalExp(N:Num) => N ... </k>
  rule <k> evalExp(textE(S:String)) => textV(S) ... </k>

  //   rule Eval_exp/variable
  rule <k> evalExp(varE(X:Id)) => {M[varr(X, .IterList)]}:>Val ... </k>
       <lvenv> M:Map </lvenv>
    requires varr(X, .IterList) in_keys(M)
  rule <k> evalExp(varE(X:Id)) => FAIL ... </k>
       <lvenv> M:Map </lvenv>
    requires notBool (varr(X, .IterList) in_keys(M))

  //   rulegroup Eval_exp/unary
  syntax KItem ::= unaryK(UnOp)
  rule <k> evalExp(unE(O:UnOp, E:Exp)) => evalExp(E) ~> unaryK(O) ... </k>

  rule <k> boolV(B:Bool) ~> unaryK(notOp()) => boolV(notBool B) ... </k>
  rule <k> N:Num ~> unaryK(O:NumUnOp) => unopNumber(O, N) ... </k>
  rule <k> FAIL ~> unaryK(_:UnOp) => FAIL ... </k>
  rule <k> _V:Val ~> unaryK(_:UnOp) => FAIL ... </k> [owise]

  //   rulegroup Eval_exp/binary and /compare
  syntax KItem ::= binaryK1(BinOp, Exp) | binaryK2(BinOp, Val)
                 | cmpK1(CmpOp, Exp) | cmpK2(CmpOp, Val)

  rule <k> evalExp(binE(O:BinOp, L:Exp, R:Exp)) => evalExp(L) ~> binaryK1(O, R) ... </k>
  rule <k> V:Val ~> binaryK1(O:BinOp, R:Exp) => evalExp(R) ~> binaryK2(O, V) ... </k>
  rule <k> FAIL ~> binaryK1(_:BinOp, _:Exp) => FAIL ... </k>

  rule <k> boolV(Br:Bool) ~> binaryK2(O:BoolBinOp, boolV(Bl:Bool))
        => boolV(binopBool(O, Bl, Br))
       ...
       </k>
  rule <k> Nr:Num ~> binaryK2(O:NumBinOp, Nl:Num) => binopNumber(O, Nl, Nr) ... </k>
  rule <k> FAIL ~> binaryK2(_:BinOp, _:Val) => FAIL ... </k>
  rule <k> _:Val ~> binaryK2(_:BinOp, _:Val) => FAIL ... </k> [owise]

  rule <k> evalExp(cmpE(O:CmpOp, L:Exp, R:Exp)) => evalExp(L) ~> cmpK1(O, R) ... </k>
  rule <k> V:Val ~> cmpK1(O:CmpOp, R:Exp) => evalExp(R) ~> cmpK2(O, V) ... </k>
  rule <k> FAIL ~> cmpK1(_:CmpOp, _:Exp) => FAIL ... </k>

  rule <k> Vr:Val ~> cmpK2(O:PolyCmpOp, Vl:Val) => boolV(cmpopPoly(O, Vl, Vr)) ... </k>
  rule <k> Nr:Num ~> cmpK2(O:NumCmpOp, Nl:Num) => boolV(cmpopNumber(O, Nl, Nr)) ... </k>
  rule <k> FAIL ~> cmpK2(_:CmpOp, _:Val) => FAIL ... </k>

  //   rule Eval_exp/upcast, /downcast, /subtype
  syntax KItem ::= upCastK(Typ) | downCastK(Typ) | subK(Typ)

  rule <k> evalExp(upCastE(T:Typ, E:Exp)) => evalExp(E) ~> upCastK(T) ... </k>
  rule <k> V:Val ~> upCastK(T:Typ) => valOfRes(upcast(L, G, T, V)) ... </k>
       <ltdenv> L:Map </ltdenv>
       <gtdenv> G:Map </gtdenv>
  rule <k> FAIL ~> upCastK(_:Typ) => FAIL ... </k>

  rule <k> evalExp(downCastE(T:Typ, E:Exp)) => evalExp(E) ~> downCastK(T) ... </k>
  rule <k> V:Val ~> downCastK(T:Typ) => valOfRes(downcast(L, G, T, V)) ... </k>
       <ltdenv> L:Map </ltdenv>
       <gtdenv> G:Map </gtdenv>
  rule <k> FAIL ~> downCastK(_:Typ) => FAIL ... </k>

  rule <k> evalExp(subE(E:Exp, T:Typ)) => evalExp(E) ~> subK(T) ... </k>
  rule <k> V:Val ~> subK(T:Typ) => boolV(subtyp(extendTdenv(G, L), T, V)) ... </k>
       <ltdenv> L:Map </ltdenv>
       <gtdenv> G:Map </gtdenv>
  rule <k> FAIL ~> subK(_:Typ) => FAIL ... </k>

  syntax KItem ::= valOfRes(ValRes)
  rule <k> valOfRes(okV(V:Val)) => V ... </k>
  rule <k> valOfRes(failV()) => FAIL ... </k>

  //   rulegroup Eval_exp/match
  syntax KItem ::= matchK(Pattern)
  rule <k> evalExp(matchE(E:Exp, P:Pattern)) => evalExp(E) ~> matchK(P) ... </k>
  rule <k> FAIL ~> matchK(_:Pattern) => FAIL ... </k>

  rule <k> injV(valCase(M':MixOp, _:ValList)) ~> matchK(injPat(M:MixOp))
        => boolV(eqMixOp(M, M'))
       ...
       </k>
  rule <k> listV(Vs:ValList) ~> matchK(consPat()) => boolV(lengthVals(Vs) >Int 0) ... </k>
  rule <k> listV(Vs:ValList) ~> matchK(fixedPat(N:Int))
        => boolV(lengthVals(Vs) ==Int N)
       ...
       </k>
  rule <k> listV(Vs:ValList) ~> matchK(nilPat()) => boolV(lengthVals(Vs) ==Int 0) ... </k>
  rule <k> optV(someVal(_:Val)) ~> matchK(somePat()) => boolV(true) ... </k>
  rule <k> optV(noVal()) ~> matchK(somePat()) => boolV(false) ... </k>
  rule <k> optV(someVal(_:Val)) ~> matchK(nonePat()) => boolV(false) ... </k>
  rule <k> optV(noVal()) ~> matchK(nonePat()) => boolV(true) ... </k>
  rule <k> _:Val ~> matchK(_:Pattern) => FAIL ... </k> [owise]

  syntax Int ::= lengthVals(ValList)        [function, total, symbol(lengthVals)]
  rule lengthVals(.ValList) => 0
  rule lengthVals(_:Val, Vs:ValList) => 1 +Int lengthVals(Vs)

  //   rule Eval_exp/tuple
  rule <k> evalExp(tupE(Es:ExpList)) => evalExps(Es) ~> tupK() ... </k>
  syntax KItem ::= "tupK" "(" ")"
  rule <k> Vs:ValList ~> tupK() => tupV(Vs) ... </k>
  rule <k> FAIL ~> tupK() => FAIL ... </k>

  //   rule Eval_exp/case
  syntax KItem ::= caseK(MixOp)
  rule <k> evalExp(injE(expCase(M:MixOp, Es:ExpList))) => evalExps(Es) ~> caseK(M) ... </k>
  rule <k> Vs:ValList ~> caseK(M:MixOp) => injV(valCase(M, Vs)) ... </k>
  rule <k> FAIL ~> caseK(_:MixOp) => FAIL ... </k>

  //   rule Eval_exp/struct
  syntax KItem ::= strFieldK(AtomList)
  rule <k> evalExp(strE(Fs:ExpFieldList))
        => evalExps(expsOfFields(Fs)) ~> strFieldK(atomsOfFields(Fs))
       ...
       </k>
  rule <k> Vs:ValList ~> strFieldK(As:AtomList) => strV(zipFields(As, Vs)) ... </k>
  rule <k> FAIL ~> strFieldK(_:AtomList) => FAIL ... </k>

  syntax AtomList ::= atomsOfFields(ExpFieldList)
                                            [function, total, symbol(atomsOfFields)]
  syntax ExpList ::= expsOfFields(ExpFieldList)
                                            [function, total, symbol(expsOfFields)]

  rule atomsOfFields(.ExpFieldList) => .AtomList
  rule atomsOfFields(expField(A:Atom, _:Exp), Fs:ExpFieldList) => A, atomsOfFields(Fs)
  rule expsOfFields(.ExpFieldList) => .ExpList
  rule expsOfFields(expField(_:Atom, E:Exp), Fs:ExpFieldList) => E, expsOfFields(Fs)

  syntax ValFieldList ::= zipFields(AtomList, ValList)
                                            [function, symbol(zipFields)]
  rule zipFields(.AtomList, .ValList) => .ValFieldList
  rule zipFields((A:Atom, As:AtomList), (V:Val, Vs:ValList))
    => valField(A, V), zipFields(As, Vs)

  //   rulegroup Eval_exp/opt
  rule <k> evalExp(optE(someExp(E:Exp))) => evalExp(E) ~> someK() ... </k>
  syntax KItem ::= "someK" "(" ")"
  rule <k> V:Val ~> someK() => optV(someVal(V)) ... </k>
  rule <k> FAIL ~> someK() => FAIL ... </k>
  rule <k> evalExp(optE(noExp())) => optV(noVal()) ... </k>

  //   rule Eval_exp/list
  rule <k> evalExp(listE(Es:ExpList)) => evalExps(Es) ~> listK() ... </k>
  syntax KItem ::= "listK" "(" ")"
  rule <k> Vs:ValList ~> listK() => listV(Vs) ... </k>
  rule <k> FAIL ~> listK() => FAIL ... </k>

  //   rule Eval_exp/cons
  syntax KItem ::= consK1(Exp) | consK2(Val)
  rule <k> evalExp(consE(Eh:Exp, Et:Exp)) => evalExp(Eh) ~> consK1(Et) ... </k>
  rule <k> V:Val ~> consK1(Et:Exp) => evalExp(Et) ~> consK2(V) ... </k>
  rule <k> FAIL ~> consK1(_:Exp) => FAIL ... </k>
  rule <k> listV(Vs:ValList) ~> consK2(V:Val) => listV(V, Vs) ... </k>
  rule <k> FAIL ~> consK2(_:Val) => FAIL ... </k>
  rule <k> _:Val ~> consK2(_:Val) => FAIL ... </k> [owise]

  //   rulegroup Eval_exp/concat
  syntax KItem ::= catK1(Exp) | catK2(Val)
  rule <k> evalExp(catE(L:Exp, R:Exp)) => evalExp(L) ~> catK1(R) ... </k>
  rule <k> V:Val ~> catK1(R:Exp) => evalExp(R) ~> catK2(V) ... </k>
  rule <k> FAIL ~> catK1(_:Exp) => FAIL ... </k>
  rule <k> textV(Sr:String) ~> catK2(textV(Sl:String)) => textV(Sl +String Sr) ... </k>
  rule <k> listV(Vr:ValList) ~> catK2(listV(Vl:ValList)) => listV(appendVal(Vl, Vr)) ... </k>
  rule <k> FAIL ~> catK2(_:Val) => FAIL ... </k>
  rule <k> _:Val ~> catK2(_:Val) => FAIL ... </k> [owise]

  //   rule Eval_exp/mem
  syntax KItem ::= memK1(Exp) | memK2(Val)
  rule <k> evalExp(memE(Ee:Exp, Es:Exp)) => evalExp(Ee) ~> memK1(Es) ... </k>
  rule <k> V:Val ~> memK1(Es:Exp) => evalExp(Es) ~> memK2(V) ... </k>
  rule <k> FAIL ~> memK1(_:Exp) => FAIL ... </k>
  rule <k> listV(Vs:ValList) ~> memK2(V:Val) => boolV(memVal(V, Vs)) ... </k>
  rule <k> FAIL ~> memK2(_:Val) => FAIL ... </k>
  rule <k> _:Val ~> memK2(_:Val) => FAIL ... </k> [owise]

  syntax Bool ::= memVal(Val, ValList)      [function, total, symbol(memVal)]
  rule memVal(_:Val, .ValList) => false
  rule memVal(V:Val, (W:Val, Vs:ValList)) => V ==K W orBool memVal(V, Vs)

  //   rulegroup Eval_exp/len
  syntax KItem ::= "lenK" "(" ")"
  rule <k> evalExp(lenE(E:Exp)) => evalExp(E) ~> lenK() ... </k>
  rule <k> textV(S:String) ~> lenK() => natN(lengthString(S)) ... </k>
  rule <k> listV(Vs:ValList) ~> lenK() => natN(lengthVals(Vs)) ... </k>
  rule <k> FAIL ~> lenK() => FAIL ... </k>
  rule <k> _:Val ~> lenK() => FAIL ... </k> [owise]

  //   rule Eval_exp/dot, /idx, /slice — all go through Eval_path
  syntax KItem ::= dotBaseK(Atom) | idxBaseK(Exp) | sliceBaseK(Exp, Exp)

  rule <k> evalExp(dotE(E:Exp, A:Atom)) => evalExp(E) ~> dotBaseK(A) ... </k>
  rule <k> V:Val ~> dotBaseK(A:Atom) => evalPath(V, dotPath(rootPath(), A)) ... </k>
  rule <k> FAIL ~> dotBaseK(_:Atom) => FAIL ... </k>

  rule <k> evalExp(idxE(Eb:Exp, Ei:Exp)) => evalExp(Eb) ~> idxBaseK(Ei) ... </k>
  rule <k> V:Val ~> idxBaseK(Ei:Exp) => evalPath(V, idxPath(rootPath(), Ei)) ... </k>
  rule <k> FAIL ~> idxBaseK(_:Exp) => FAIL ... </k>

  rule <k> evalExp(sliceE(Eb:Exp, Ei:Exp, En:Exp))
        => evalExp(Eb) ~> sliceBaseK(Ei, En)
       ...
       </k>
  rule <k> V:Val ~> sliceBaseK(Ei:Exp, En:Exp)
        => evalPath(V, slicePath(rootPath(), Ei, En))
       ...
       </k>
  rule <k> FAIL ~> sliceBaseK(_:Exp, _:Exp) => FAIL ... </k>

  //   rule Eval_exp/upd
  syntax KItem ::= updBaseK(Path, Exp) | updFieldK(Path, Val)
  rule <k> evalExp(updE(Eb:Exp, P:Path, Ef:Exp)) => evalExp(Eb) ~> updBaseK(P, Ef) ... </k>
  rule <k> V:Val ~> updBaseK(P:Path, Ef:Exp) => evalExp(Ef) ~> updFieldK(P, V) ... </k>
  rule <k> FAIL ~> updBaseK(_:Path, _:Exp) => FAIL ... </k>
  rule <k> Vf:Val ~> updFieldK(P:Path, Vb:Val) => evalPathUpd(Vb, P, Vf) ... </k>
  rule <k> FAIL ~> updFieldK(_:Path, _:Val) => FAIL ... </k>

  //   rule Eval_exp/call
  //
  //   rule Eval_targs: C |- targ* : targ_subst*
  //     -- if theta = $theta_of_tdenv(C.LOCAL.TYP)
  //     -- if (targ_subst = $subst_typ(theta, targ))*
  //
  // `Eval_targs` never fails, so it is evaluated eagerly (reading <ltdenv>)
  // rather than heated as its own task.
  syntax KItem ::= callArgsK(Id, TypList)
  rule <k> evalExp(callE(X:Id, Ts:TypList, As:ArgList))
        => evalArgs(As) ~> callArgsK(X, substTypsMap(thetaOfTdenv(L), Ts))
       ...
       </k>
       <ltdenv> L:Map </ltdenv>

  rule <k> Vs:ValList ~> callArgsK(X:Id, Ts:TypList) => callFunc(X, Ts, Vs) ... </k>
  rule <k> FAIL ~> callArgsK(_:Id, _:TypList) => FAIL ... </k>

  //   rulegroup Eval_exp/iter
  syntax KItem ::= evalIter(VarrOpt, Exp, IterExp)
  rule <k> evalExp(iterE(E:Exp, IE:IterExp)) => evalIter(isIterOnVar(iterE(E, IE)), E, IE) ... </k>

  //   rule Eval_exp/simple
  rule <k> evalIter(someVarr(R:Varr), _:Exp, _:IterExp) => {M[R]}:>Val ... </k>
       <lvenv> M:Map </lvenv>
    requires R in_keys(M)
  rule <k> evalIter(someVarr(R:Varr), _:Exp, _:IterExp) => FAIL ... </k>
       <lvenv> M:Map </lvenv>
    requires notBool (R in_keys(M))

  // ==========================================================================
  // Sub-contexts for iteration
  //
  //   dec $sub_opt(ctx, vari*) : ctx?
  //   dec $sub_list(ctx, vari*) : ctx*
  //
  // `$sub_opt` looks each `vari` up at one more `QUEST` in <lvenv>, expecting
  // an `OPT`; if every one is `OPT val`, the (single) sub-context binds each
  // `vari` to its `val`, and if every one is `OPT eps` there is no
  // sub-context. `$sub_list` looks each `vari` up at one more `STAR`,
  // expecting a `LIST`, and transposes the lists into one row of bindings per
  // element — there are as many sub-contexts as the (common) list length.
  //
  // Neither builds a `ctx` value: a cell design instead computes the *rows* to
  // bind (a `ValList` for the single `$sub_opt` case, or `ValListList` — one
  // row per element — for `$sub_list`), and the caller drives the iteration by
  // installing one row at a time into a save/cleared `<local>`.
  // ==========================================================================

  // `$sub_opt`: `noRowOpt()` if `$sub_opt` is `eps` (skip the body);
  // `someRowOpt(vals)` if it applies, with `vals` the value to bind each
  // `vari` in `Vs` to, in order.
  syntax SubOptRow ::= noRowOpt()           [symbol(noRowOpt)]
                     | someRowOpt(ValList)  [symbol(someRowOpt)]

  syntax KItem ::= subOptRow(VariList)
  rule <k> subOptRow(Vs:VariList) => classifySubOpt(findOptsOrFail(iterVaris(Vs, quest()), M)) ... </k>
       <lvenv> M:Map </lvenv>

  // Look each `varr` up; FAIL (stuck) is impossible here since `$sub_opt`'s
  // precondition requires the lookup to exist under normal use, but to stay
  // total this yields `noValList()` when any lookup is missing, and the
  // classifier below treats that the same as "no rule applies" would in
  // watsup: it gets stuck, matching no clause. In practice every `vari_iter`
  // used by AL programs is always bound (the elaborator guarantees it).
  syntax ValListOpt ::= noValListOpt()      [symbol(noValListOpt)]
                      | someValListOpt(ValList)
                                            [symbol(someValListOpt)]

  syntax ValListOpt ::= findOptsOrFail(VarrList, Map)
                                            [function, total, symbol(findOptsOrFail)]
  rule findOptsOrFail(.VarrList, _:Map) => someValListOpt(.ValList)
  rule findOptsOrFail((R:Varr, Rs:VarrList), M:Map)
    => consOptRow({M[R]}:>Val, findOptsOrFail(Rs, M))
    requires R in_keys(M)
  rule findOptsOrFail((R:Varr, _:VarrList), M:Map) => noValListOpt()
    requires notBool (R in_keys(M))

  syntax ValListOpt ::= consOptRow(Val, ValListOpt)
                                            [function, total, symbol(consOptRow)]
  rule consOptRow(V:Val, someValListOpt(Vs:ValList)) => someValListOpt(V, Vs)
  rule consOptRow(_:Val, noValListOpt()) => noValListOpt()

  syntax SubOptRow ::= classifySubOpt(ValListOpt)
                                            [function, symbol(classifySubOpt)]
  // every lookup was `OPT val`
  rule classifySubOpt(someValListOpt(Vs:ValList)) => someRowOpt(unwrapSomeOpts(Vs))
    requires allSomeOpts(Vs)
  // every lookup was `OPT eps`
  rule classifySubOpt(someValListOpt(Vs:ValList)) => noRowOpt()
    requires allNoneOpts(Vs)

  syntax Bool ::= allSomeOpts(ValList)      [function, total, symbol(allSomeOpts)]
                | allNoneOpts(ValList)      [function, total, symbol(allNoneOpts)]
  rule allSomeOpts(.ValList) => true
  rule allSomeOpts(optV(someVal(_:Val)), Vs:ValList) => allSomeOpts(Vs)
  rule allSomeOpts(_:Val, _:ValList) => false [owise]
  rule allNoneOpts(.ValList) => true
  rule allNoneOpts(optV(noVal()), Vs:ValList) => allNoneOpts(Vs)
  rule allNoneOpts(_:Val, _:ValList) => false [owise]

  syntax ValList ::= unwrapSomeOpts(ValList)
                                            [function, symbol(unwrapSomeOpts)]
  rule unwrapSomeOpts(.ValList) => .ValList
  rule unwrapSomeOpts(optV(someVal(V:Val)), Vs:ValList) => V, unwrapSomeOpts(Vs)

  // `$sub_list`: the rows to bind, one per element, or `.ValListList` if no
  // rows apply (`$sub_list(C, vari*) = eps`, e.g. an empty `LIST`).
  syntax KItem ::= subListRows(VariList)
  rule <k> subListRows(Vs:VariList) => transposeVal(unwrapLists(findLists(iterVaris(Vs, star()), M))) ... </k>
       <lvenv> M:Map </lvenv>

  syntax ValList ::= findLists(VarrList, Map)
                                            [function, symbol(findLists)]
  rule findLists(.VarrList, _:Map) => .ValList
  rule findLists((R:Varr, Rs:VarrList), M:Map) => {M[R]}:>Val, findLists(Rs, M)
    requires R in_keys(M)

  syntax ValListList ::= unwrapLists(ValList)
                                            [function, symbol(unwrapLists)]
  rule unwrapLists(.ValList) => .ValListList
  rule unwrapLists(listV(Vs:ValList), Ws:ValList) => Vs ; unwrapLists(Ws)

  // ==========================================================================
  // Eval_exp/opt and /list, using the rows above
  // ==========================================================================

  //   rule Eval_exp/opt
  rule <k> evalIter(noVarr(), E:Exp, iterExp(quest(), Vs:VariList))
        => subOptRow(Vs) ~> evalOptSub(Vs, E)
       ...
       </k>

  syntax KItem ::= evalOptSub(VariList, Exp)
  rule <k> noRowOpt() ~> evalOptSub(_:VariList, _:Exp) => optV(noVal()) ... </k>
  rule <k> someRowOpt(Row:ValList) ~> evalOptSub(Vs:VariList, E:Exp)
        => pushSave() ~> bindRow(Vs, Row) ~> evalExp(E) ~> popSaveThen(someK())
       ...
       </k>

  // Bind each `vari` in `Vs` (unmarked, i.e. plain `varr`s) to the
  // corresponding element of `Row` in the (already-cleared-if-needed) local
  // value environment.
  syntax KItem ::= bindRow(VariList, ValList)
  rule <k> bindRow(.VariList, .ValList) => .K ... </k>
  rule <k> bindRow((V:Vari, Vs:VariList), (Val:Val, Vals:ValList)) => bindRow(Vs, Vals) ... </k>
       <lvenv> M:Map => M[varrOfVari(V) <- Val] </lvenv>

  // Pop <saves> and continue with `Next` (a wrapper like `someK()`/`listK()`
  // that turns the value or FAIL left on `<k>` into the final shape).
  syntax KItem ::= popSaveThen(KItem)
  rule <k> V:Val ~> popSaveThen(Next:KItem) => popSaveRestore() ~> V ~> Next ... </k>
  rule <k> FAIL ~> popSaveThen(_:KItem) => popSaveRestore() ~> FAIL ... </k>

  syntax KItem ::= "popSaveRestore" "(" ")"
  rule <k> popSaveRestore() => .K ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <saves> ListItem(localSave(T:Map, F:Map, V:Map)) => .List ... </saves>

  //   rule Eval_exp/list
  rule <k> evalIter(noVarr(), E:Exp, iterExp(star(), Vs:VariList))
        => subListRows(Vs) ~> evalListSub(Vs, E)
       ...
       </k>

  syntax KItem ::= evalListSub(VariList, Exp)
  rule <k> .ValListList ~> evalListSub(_:VariList, _:Exp) => listV(.ValList) ... </k>
  rule <k> Rows:ValListList ~> evalListSub(Vs:VariList, E:Exp)
        => pushSave() ~> evalEachRow(Vs, E, Rows) ~> popSaveRestoreList()
       ...
       </k>
    requires Rows =/=K .ValListList

  // `evalEachRow` leaves a `ValList` (the per-element results, or `FAIL`) on
  // `<k>`, not a bare `Val` — `popSaveThen` is for the single-value case
  // (Eval_exp/opt), so list iteration needs its own pop-and-wrap.
  syntax KItem ::= "popSaveRestoreList" "(" ")"
  rule <k> Vs:ValList ~> popSaveRestoreList() => popSaveRestore() ~> listV(Vs) ... </k>
  rule <k> FAIL ~> popSaveRestoreList() => popSaveRestore() ~> FAIL ... </k>

  // Evaluate `E` once per row of `Rows`.  Each element's sub-context is the
  // *base* `<local>` (the one at the top of `<saves>`, unmodified since it was
  // pushed) plus that row's bindings — watsup's `$sub_list` adds onto `C`, it
  // does not clear it (unlike `Assign_exp/list`, which does). `peekRestore()`
  // resets `<local>` to the saved base without popping it, so each row starts
  // fresh from the same point; the save is popped only once, after every row
  // has run.
  syntax KItem ::= evalEachRow(VariList, Exp, ValListList)
  rule <k> evalEachRow(_:VariList, _:Exp, .ValListList) => .ValList ... </k>
  rule <k> evalEachRow(Vs:VariList, E:Exp, (Row:ValList ; Rows:ValListList))
        => peekRestore() ~> bindRow(Vs, Row) ~> evalExp(E)
           ~> collectRow(Vs, E, Rows)
       ...
       </k>

  syntax KItem ::= "peekRestore" "(" ")"
  rule <k> peekRestore() => .K ... </k>
       <ltdenv> _ => T </ltdenv>
       <lfenv> _ => F </lfenv>
       <lvenv> _ => V </lvenv>
       <saves> ListItem(localSave(T:Map, F:Map, V:Map)) ... </saves>

  syntax KItem ::= collectRow(VariList, Exp, ValListList)
                 | prependVal(Val)
  rule <k> V:Val ~> collectRow(Vs:VariList, E:Exp, Rows:ValListList)
        => evalEachRow(Vs, E, Rows) ~> prependVal(V)
       ...
       </k>
  rule <k> FAIL ~> collectRow(_:VariList, _:Exp, _:ValListList) => FAIL ... </k>

  rule <k> Vs:ValList ~> prependVal(V:Val) => V, Vs ... </k>
  rule <k> FAIL ~> prependVal(_:Val) => FAIL ... </k>

  // ==========================================================================
  // Eval_exp on a sequence: evaluate every element, short-circuiting on FAIL.
  // ==========================================================================

  syntax KItem ::= evalExps(ExpList)
  rule <k> evalExps(.ExpList) => .ValList ... </k>
  rule <k> evalExps((E:Exp, Es:ExpList)) => evalExp(E) ~> collectExps(Es) ... </k>

  syntax KItem ::= collectExps(ExpList)
  rule <k> V:Val ~> collectExps(Es:ExpList) => evalExps(Es) ~> prependVal(V) ... </k>
  rule <k> FAIL ~> collectExps(_:ExpList) => FAIL ... </k>

  // ==========================================================================
  // Eval_path: ctx |- val path : res<val>
  // ==========================================================================

  syntax KItem ::= evalPath(Val, Path)

  //   rule Eval_path/root
  rule <k> evalPath(V:Val, rootPath()) => V ... </k>

  //   rule Eval_path/dot
  syntax KItem ::= pathDotK(Atom)
  rule <k> evalPath(V:Val, dotPath(P:Path, A:Atom)) => evalPath(V, P) ~> pathDotK(A) ... </k>
  rule <k> strV(Fs:ValFieldList) ~> pathDotK(A:Atom) => valOptToKItem(assocVal(A, Fs)) ... </k>
  rule <k> FAIL ~> pathDotK(_:Atom) => FAIL ... </k>
  rule <k> _:Val ~> pathDotK(_:Atom) => FAIL ... </k> [owise]

  syntax KItem ::= valOptToKItem(ValOpt)
  rule <k> valOptToKItem(someVal(V:Val)) => V ... </k>
  rule <k> valOptToKItem(noVal()) => FAIL ... </k>

  //   rulegroup Eval_path/idx
  syntax KItem ::= pathIdxBaseK(Path, Exp) | pathIdxK(Val)
  rule <k> evalPath(V:Val, idxPath(P:Path, Ei:Exp))
        => evalPath(V, P) ~> pathIdxWaitK(Ei)
       ...
       </k>

  syntax KItem ::= pathIdxWaitK(Exp)
  rule <k> Vb:Val ~> pathIdxWaitK(Ei:Exp) => evalExp(Ei) ~> pathIdxK(Vb) ... </k>
  rule <k> FAIL ~> pathIdxWaitK(_:Exp) => FAIL ... </k>

  rule <k> natN(N:Int) ~> pathIdxK(textV(S:String)) => textV(substrString(S, N, N +Int 1)) ... </k>
    requires N >=Int 0 andBool N <Int lengthString(S)
  rule <k> natN(N:Int) ~> pathIdxK(listV(Vs:ValList)) => nthVal(Vs, N) ... </k>
    requires N >=Int 0 andBool N <Int lengthVals(Vs)
  rule <k> FAIL ~> pathIdxK(_:Val) => FAIL ... </k>
  rule <k> _:Val ~> pathIdxK(_:Val) => FAIL ... </k> [owise]

  // `nthVal` is defined in 4.2-eval-assign.k.

  //   rulegroup Eval_path/slice
  syntax KItem ::= pathSliceWaitK(Exp, Exp) | pathSliceK1(Val, Exp) | pathSliceK2(Val, Int)
  rule <k> evalPath(V:Val, slicePath(P:Path, Ei:Exp, En:Exp))
        => evalPath(V, P) ~> pathSliceWaitK(Ei, En)
       ...
       </k>
  rule <k> Vb:Val ~> pathSliceWaitK(Ei:Exp, En:Exp) => evalExp(Ei) ~> pathSliceK1(Vb, En) ... </k>
  rule <k> FAIL ~> pathSliceWaitK(_:Exp, _:Exp) => FAIL ... </k>
  rule <k> natN(I:Int) ~> pathSliceK1(Vb:Val, En:Exp) => evalExp(En) ~> pathSliceK2(Vb, I) ... </k>
  rule <k> FAIL ~> pathSliceK1(_:Val, _:Exp) => FAIL ... </k>

  rule <k> natN(N:Int) ~> pathSliceK2(textV(S:String), I:Int)
        => textV(substrString(S, I, I +Int N))
       ...
       </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthString(S)
  rule <k> natN(N:Int) ~> pathSliceK2(listV(Vs:ValList), I:Int)
        => listV(sliceVal(Vs, I, N))
       ...
       </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthVals(Vs)
  rule <k> FAIL ~> pathSliceK2(_:Val, _:Int) => FAIL ... </k>
  rule <k> _:Val ~> pathSliceK2(_:Val, _:Int) => FAIL ... </k> [owise]

  syntax ValList ::= sliceVal(ValList, Int, Int)
                                            [function, symbol(sliceVal)]
  rule sliceVal(_:ValList, _:Int, 0) => .ValList
  rule sliceVal((V:Val, Vs:ValList), 0, N:Int) => V, sliceVal(Vs, 0, N -Int 1)
    requires N >Int 0
  rule sliceVal((_:Val, Vs:ValList), I:Int, N:Int) => sliceVal(Vs, I -Int 1, N)
    requires I >Int 0

  //   rule Eval_path/fail: no rule above matched, so evalPath is stuck only if
  //   none of the shape-specific rules applied; the FAIL threading above
  //   already covers every case Eval_path itself distinguishes.

  // ==========================================================================
  // Eval_path_upd: ctx |- val path := val : res<val>
  //
  // Rebuilds the value bottom-up: the sub-value at `path` is fetched with
  // `Eval_path`, updated at its last step, and written back by recursing on
  // the prefix.
  // ==========================================================================

  syntax KItem ::= evalPathUpd(Val, Path, Val)

  //   rule Eval_path_upd/root
  rule <k> evalPathUpd(_:Val, rootPath(), Vn:Val) => Vn ... </k>

  //   rulegroup Eval_path_upd/idx
  syntax KItem ::= updIdxWaitK(Val, Path, Exp, Val) | updIdxK(Val, Path, Val)
  rule <k> evalPathUpd(Vb:Val, idxPath(P:Path, Ei:Exp), Vn:Val)
        => evalPath(Vb, P) ~> updIdxWaitK(Vb, P, Ei, Vn)
       ...
       </k>
  rule <k> Vcur:Val ~> updIdxWaitK(Vb:Val, P:Path, Ei:Exp, Vn:Val)
        => evalExp(Ei) ~> updIdxK2(Vb, P, Vcur, Vn)
       ...
       </k>
  rule <k> FAIL ~> updIdxWaitK(_:Val, _:Path, _:Exp, _:Val) => FAIL ... </k>

  syntax KItem ::= updIdxK2(Val, Path, Val, Val)
  // Text index update replaces one character (`-- if |t_n| = 1`).
  rule <k> natN(N:Int) ~> updIdxK2(Vb:Val, P:Path, textV(S:String), textV(Sn:String))
        => evalPathUpd(Vb, P, textV(replaceAtString(S, N, Sn)))
       ...
       </k>
    requires lengthString(Sn) ==Int 1 andBool N >=Int 0 andBool N <Int lengthString(S)
  rule <k> natN(N:Int) ~> updIdxK2(Vb:Val, P:Path, listV(Vs:ValList), Vn:Val)
        => evalPathUpd(Vb, P, listV(updateNthVal(Vs, N, Vn)))
       ...
       </k>
    requires N >=Int 0 andBool N <Int lengthVals(Vs)
  rule <k> FAIL ~> updIdxK2(_:Val, _:Path, _:Val, _:Val) => FAIL ... </k>
  rule <k> _:Val ~> updIdxK2(_:Val, _:Path, _:Val, _:Val) => FAIL ... </k> [owise]

  syntax ValList ::= updateNthVal(ValList, Int, Val)
                                            [function, symbol(updateNthVal)]
  rule updateNthVal((_:Val, Vs:ValList), 0, Vn:Val) => Vn, Vs
  rule updateNthVal((V:Val, Vs:ValList), N:Int, Vn:Val)
    => V, updateNthVal(Vs, N -Int 1, Vn)
    requires N >Int 0

  syntax String ::= replaceAtString(String, Int, String)
                                            [function, symbol(replaceAtString)]
  rule replaceAtString(S:String, N:Int, Sn:String)
    => substrString(S, 0, N) +String Sn +String substrString(S, N +Int 1, lengthString(S))

  //   rulegroup Eval_path_upd/slice
  syntax KItem ::= updSliceWaitK(Val, Path, Exp, Exp, Val) | updSliceK1(Val, Path, Val, Exp, Val)
                 | updSliceK2(Val, Path, Val, Int, Val)
  rule <k> evalPathUpd(Vb:Val, slicePath(P:Path, Ei:Exp, En:Exp), Vn:Val)
        => evalPath(Vb, P) ~> updSliceWaitK(Vb, P, Ei, En, Vn)
       ...
       </k>
  rule <k> Vcur:Val ~> updSliceWaitK(Vb:Val, P:Path, Ei:Exp, En:Exp, Vn:Val)
        => evalExp(Ei) ~> updSliceK1(Vb, P, Vcur, En, Vn)
       ...
       </k>
  rule <k> FAIL ~> updSliceWaitK(_:Val, _:Path, _:Exp, _:Exp, _:Val) => FAIL ... </k>
  rule <k> natN(I:Int) ~> updSliceK1(Vb:Val, P:Path, Vcur:Val, En:Exp, Vn:Val)
        => evalExp(En) ~> updSliceK2(Vb, P, Vcur, I, Vn)
       ...
       </k>
  rule <k> FAIL ~> updSliceK1(_:Val, _:Path, _:Val, _:Exp, _:Val) => FAIL ... </k>

  rule <k> natN(N:Int) ~> updSliceK2(Vb:Val, P:Path, textV(S:String), I:Int, textV(Sn:String))
        => evalPathUpd(Vb, P,
             textV(substrString(S, 0, I) +String Sn
                   +String substrString(S, I +Int N, lengthString(S))))
       ...
       </k>
    requires lengthString(Sn) ==Int N andBool I >=Int 0 andBool N >=Int 0
       andBool I +Int N <=Int lengthString(S)
  rule <k> natN(N:Int) ~> updSliceK2(Vb:Val, P:Path, listV(Vs:ValList), I:Int, listV(Ws:ValList))
        => evalPathUpd(Vb, P, listV(replaceSliceVal(Vs, I, N, Ws)))
       ...
       </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthVals(Vs)
  rule <k> FAIL ~> updSliceK2(_:Val, _:Path, _:Val, _:Int, _:Val) => FAIL ... </k>
  rule <k> _:Val ~> updSliceK2(_:Val, _:Path, _:Val, _:Int, _:Val) => FAIL ... </k> [owise]

  syntax ValList ::= replaceSliceVal(ValList, Int, Int, ValList)
                                            [function, symbol(replaceSliceVal)]
  rule replaceSliceVal(Vs:ValList, I:Int, N:Int, Ws:ValList)
    => appendVal(sliceVal(Vs, 0, I),
                 appendVal(Ws, sliceVal(Vs, I +Int N, lengthVals(Vs) -Int (I +Int N))))

  //   rule Eval_path_upd/dot
  syntax KItem ::= updDotWaitK(Val, Path, Atom, Val)
  rule <k> evalPathUpd(Vb:Val, dotPath(P:Path, A:Atom), Vn:Val)
        => evalPath(Vb, P) ~> updDotWaitK(Vb, P, A, Vn)
       ...
       </k>
  rule <k> strV(Fs:ValFieldList) ~> updDotWaitK(Vb:Val, P:Path, A:Atom, Vn:Val)
        => evalPathUpd(Vb, P, strV(updateField(Fs, A, Vn)))
       ...
       </k>
  rule <k> FAIL ~> updDotWaitK(_:Val, _:Path, _:Atom, _:Val) => FAIL ... </k>
  rule <k> _:Val ~> updDotWaitK(_:Val, _:Path, _:Atom, _:Val) => FAIL ... </k> [owise]

  // `$ite<val>(atom = atom_field, val_n, val_field)` over every field.
  syntax ValFieldList ::= updateField(ValFieldList, Atom, Val)
                                            [function, total, symbol(updateField)]
  rule updateField(.ValFieldList, _:Atom, _:Val) => .ValFieldList
  rule updateField((valField(A':Atom, V:Val), Fs:ValFieldList), A:Atom, Vn:Val)
    => valField(A', iteVal(A ==String A', Vn, V)), updateField(Fs, A, Vn)

  // ==========================================================================
  // Eval_arg / Eval_targs
  //
  //   rule Eval_arg/exp, /fun, /fail
  //   rule Eval_targs
  // ==========================================================================

  syntax KItem ::= evalArg(Arg)
                 | evalArgs(ArgList)

  rule <k> evalArg(expA(E:Exp)) => evalExp(E) ... </k>
  rule <k> evalArg(funA(X:Id)) => funcV(X) ... </k>

  rule <k> evalArgs(.ArgList) => .ValList ... </k>
  rule <k> evalArgs((A:Arg, As:ArgList)) => evalArg(A) ~> collectArgs(As) ... </k>

  syntax KItem ::= collectArgs(ArgList)
  rule <k> V:Val ~> collectArgs(As:ArgList) => evalArgs(As) ~> prependVal(V) ... </k>
  rule <k> FAIL ~> collectArgs(_:ArgList) => FAIL ... </k>

  // ==========================================================================
  // Call_func forward declaration.
  //
  // `Eval_exp/call` needs `Call_func`, defined by cell rules in 5-call.k
  // together with the rest of function/relation invocation (they are
  // mutually recursive with clause and premise evaluation, which is why they
  // live in one file). Only the task shape is declared here so this module
  // compiles standalone; 5-call.k adds the rules that resolve it.
  // ==========================================================================

  syntax KItem ::= callFunc(Id, TypList, ValList)

endmodule
