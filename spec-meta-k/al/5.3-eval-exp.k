// Expression, path, and argument evaluation.
//
// Mirrors spec-meta/al/5.3-eval-exp.watsup (Eval_exp, Eval_path,
// Eval_path_upd) and spec-meta/al/5.4-eval-arg.watsup (Eval_arg, Eval_targs).

requires "5.1-eval-typ.k"
requires "5.2-eval-assign.k"

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
  syntax KItem ::= unaryAwaitOperandK(UnOp)
  rule <k> evalExp(unE(O:UnOp, E:Exp)) => evalExp(E) ~> unaryAwaitOperandK(O) ... </k>

  rule <k> boolV(B:Bool) ~> unaryAwaitOperandK(notOp()) => boolV(notBool B) ... </k>
  rule <k> N:Num ~> unaryAwaitOperandK(O:NumUnOp) => unopNumber(O, N) ... </k>
  rule <k> FAIL ~> unaryAwaitOperandK(_:UnOp) => FAIL ... </k>
  rule <k> _V:Val ~> unaryAwaitOperandK(_:UnOp) => FAIL ... </k> [owise]

  //   rulegroup Eval_exp/binary and /compare
  syntax KItem ::= binaryAwaitLeftK(BinOp, Exp) | binaryAwaitRightK(BinOp, Val)
                 | cmpAwaitLeftK(CmpOp, Exp) | cmpAwaitRightK(CmpOp, Val)

  rule <k> evalExp(binE(O:BinOp, L:Exp, R:Exp)) => evalExp(L) ~> binaryAwaitLeftK(O, R) ... </k>
  rule <k> V:Val ~> binaryAwaitLeftK(O:BinOp, R:Exp) => evalExp(R) ~> binaryAwaitRightK(O, V) ... </k>
  rule <k> FAIL ~> binaryAwaitLeftK(_:BinOp, _:Exp) => FAIL ... </k>

  rule <k> boolV(Br:Bool) ~> binaryAwaitRightK(O:BoolBinOp, boolV(Bl:Bool))
        => boolV(binopBool(O, Bl, Br))
       ...
       </k>
  rule <k> Nr:Num ~> binaryAwaitRightK(O:NumBinOp, Nl:Num) => binopNumber(O, Nl, Nr) ... </k>
  rule <k> FAIL ~> binaryAwaitRightK(_:BinOp, _:Val) => FAIL ... </k>
  rule <k> _:Val ~> binaryAwaitRightK(_:BinOp, _:Val) => FAIL ... </k> [owise]

  rule <k> evalExp(cmpE(O:CmpOp, L:Exp, R:Exp)) => evalExp(L) ~> cmpAwaitLeftK(O, R) ... </k>
  rule <k> V:Val ~> cmpAwaitLeftK(O:CmpOp, R:Exp) => evalExp(R) ~> cmpAwaitRightK(O, V) ... </k>
  rule <k> FAIL ~> cmpAwaitLeftK(_:CmpOp, _:Exp) => FAIL ... </k>

  rule <k> Vr:Val ~> cmpAwaitRightK(O:PolyCmpOp, Vl:Val) => boolV(cmpopPoly(O, Vl, Vr)) ... </k>
  rule <k> Nr:Num ~> cmpAwaitRightK(O:NumCmpOp, Nl:Num) => boolV(cmpopNumber(O, Nl, Nr)) ... </k>
  rule <k> FAIL ~> cmpAwaitRightK(_:CmpOp, _:Val) => FAIL ... </k>

  //   rule Eval_exp/upcast, /downcast, /subtype
  syntax KItem ::= upCastAwaitOperandK(Typ) | downCastAwaitOperandK(Typ)
                 | subAwaitOperandK(Typ)

  rule <k> evalExp(upCastE(T:Typ, E:Exp)) => evalExp(E) ~> upCastAwaitOperandK(T) ... </k>
  rule <k> V:Val ~> upCastAwaitOperandK(T:Typ) => valOfRes(upcast(L, G, T, V)) ... </k>
       <ltdenv> L:Map </ltdenv>
       <gtdenv> G:Map </gtdenv>
  rule <k> FAIL ~> upCastAwaitOperandK(_:Typ) => FAIL ... </k>

  rule <k> evalExp(downCastE(T:Typ, E:Exp)) => evalExp(E) ~> downCastAwaitOperandK(T) ... </k>
  rule <k> V:Val ~> downCastAwaitOperandK(T:Typ) => valOfRes(downcast(L, G, T, V)) ... </k>
       <ltdenv> L:Map </ltdenv>
       <gtdenv> G:Map </gtdenv>
  rule <k> FAIL ~> downCastAwaitOperandK(_:Typ) => FAIL ... </k>

  rule <k> evalExp(subE(E:Exp, T:Typ)) => evalExp(E) ~> subAwaitOperandK(T) ... </k>
  rule <k> V:Val ~> subAwaitOperandK(T:Typ) => boolV(subtyp(updateMap(G, L), T, V)) ... </k>
       <ltdenv> L:Map </ltdenv>
       <gtdenv> G:Map </gtdenv>
  rule <k> FAIL ~> subAwaitOperandK(_:Typ) => FAIL ... </k>

  syntax KItem ::= valOfRes(ValRes)
  rule <k> valOfRes(okV(V:Val)) => V ... </k>
  rule <k> valOfRes(failV()) => FAIL ... </k>

  //   rulegroup Eval_exp/match
  syntax KItem ::= matchAwaitValueK(Pattern)
  rule <k> evalExp(matchE(E:Exp, P:Pattern)) => evalExp(E) ~> matchAwaitValueK(P) ... </k>
  rule <k> FAIL ~> matchAwaitValueK(_:Pattern) => FAIL ... </k>

  rule <k> injV(valCase(M':MixOp, _:ValList)) ~> matchAwaitValueK(injPat(M:MixOp))
        => boolV(eqMixOp(M, M'))
       ...
       </k>
  rule <k> listV(Vs:ValList) ~> matchAwaitValueK(consPat()) => boolV(lengthVals(Vs) >Int 0) ... </k>
  rule <k> listV(Vs:ValList) ~> matchAwaitValueK(fixedPat(N:Int))
        => boolV(lengthVals(Vs) ==Int N)
       ...
       </k>
  rule <k> listV(Vs:ValList) ~> matchAwaitValueK(nilPat()) => boolV(lengthVals(Vs) ==Int 0) ... </k>
  rule <k> optV(someVal(_:Val)) ~> matchAwaitValueK(somePat()) => boolV(true) ... </k>
  rule <k> optV(noVal()) ~> matchAwaitValueK(somePat()) => boolV(false) ... </k>
  rule <k> optV(someVal(_:Val)) ~> matchAwaitValueK(nonePat()) => boolV(false) ... </k>
  rule <k> optV(noVal()) ~> matchAwaitValueK(nonePat()) => boolV(true) ... </k>
  rule <k> _:Val ~> matchAwaitValueK(_:Pattern) => FAIL ... </k> [owise]

  syntax Int ::= lengthVals(ValList)        [function, total, symbol(lengthVals)]
  rule lengthVals(.ValList) => 0
  rule lengthVals(_:Val, Vs:ValList) => 1 +Int lengthVals(Vs)

  //   rule Eval_exp/tuple
  rule <k> evalExp(tupE(Es:ExpList)) => evalExps(Es) ~> tupAwaitElementsK() ... </k>
  syntax KItem ::= "tupAwaitElementsK" "(" ")"
  rule <k> Vs:ValList ~> tupAwaitElementsK() => tupV(Vs) ... </k>
  rule <k> FAIL ~> tupAwaitElementsK() => FAIL ... </k>

  //   rule Eval_exp/case
  syntax KItem ::= caseAwaitArgsK(MixOp)
  rule <k> evalExp(injE(expCase(M:MixOp, Es:ExpList))) => evalExps(Es) ~> caseAwaitArgsK(M) ... </k>
  rule <k> Vs:ValList ~> caseAwaitArgsK(M:MixOp) => injV(valCase(M, Vs)) ... </k>
  rule <k> FAIL ~> caseAwaitArgsK(_:MixOp) => FAIL ... </k>

  //   rule Eval_exp/struct
  syntax KItem ::= strAwaitFieldsK(AtomList)
  rule <k> evalExp(strE(Fs:ExpFieldList))
        => evalExps(expsOfFields(Fs)) ~> strAwaitFieldsK(atomsOfFields(Fs))
       ...
       </k>
  rule <k> Vs:ValList ~> strAwaitFieldsK(As:AtomList) => strV(zipFields(As, Vs)) ... </k>
  rule <k> FAIL ~> strAwaitFieldsK(_:AtomList) => FAIL ... </k>

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
  rule <k> evalExp(optE(someExp(E:Exp))) => evalExp(E) ~> optAwaitValueK() ... </k>
  syntax KItem ::= "optAwaitValueK" "(" ")"
  rule <k> V:Val ~> optAwaitValueK() => optV(someVal(V)) ... </k>
  rule <k> FAIL ~> optAwaitValueK() => FAIL ... </k>
  rule <k> evalExp(optE(noExp())) => optV(noVal()) ... </k>

  //   rule Eval_exp/list
  rule <k> evalExp(listE(Es:ExpList)) => evalExps(Es) ~> listAwaitElementsK() ... </k>
  syntax KItem ::= "listAwaitElementsK" "(" ")"
  rule <k> Vs:ValList ~> listAwaitElementsK() => listV(Vs) ... </k>
  rule <k> FAIL ~> listAwaitElementsK() => FAIL ... </k>

  //   rule Eval_exp/cons
  syntax KItem ::= consAwaitHeadK(Exp) | consAwaitTailK(Val)
  rule <k> evalExp(consE(Eh:Exp, Et:Exp)) => evalExp(Eh) ~> consAwaitHeadK(Et) ... </k>
  rule <k> V:Val ~> consAwaitHeadK(Et:Exp) => evalExp(Et) ~> consAwaitTailK(V) ... </k>
  rule <k> FAIL ~> consAwaitHeadK(_:Exp) => FAIL ... </k>
  rule <k> listV(Vs:ValList) ~> consAwaitTailK(V:Val) => listV(V, Vs) ... </k>
  rule <k> FAIL ~> consAwaitTailK(_:Val) => FAIL ... </k>
  rule <k> _:Val ~> consAwaitTailK(_:Val) => FAIL ... </k> [owise]

  //   rulegroup Eval_exp/concat
  syntax KItem ::= catAwaitLeftK(Exp) | catAwaitRightK(Val)
  rule <k> evalExp(catE(L:Exp, R:Exp)) => evalExp(L) ~> catAwaitLeftK(R) ... </k>
  rule <k> V:Val ~> catAwaitLeftK(R:Exp) => evalExp(R) ~> catAwaitRightK(V) ... </k>
  rule <k> FAIL ~> catAwaitLeftK(_:Exp) => FAIL ... </k>
  rule <k> textV(Sr:String) ~> catAwaitRightK(textV(Sl:String)) => textV(Sl +String Sr) ... </k>
  rule <k> listV(Vr:ValList) ~> catAwaitRightK(listV(Vl:ValList)) => listV(appendVal(Vl, Vr)) ... </k>
  rule <k> FAIL ~> catAwaitRightK(_:Val) => FAIL ... </k>
  rule <k> _:Val ~> catAwaitRightK(_:Val) => FAIL ... </k> [owise]

  //   rule Eval_exp/mem
  syntax KItem ::= memAwaitElementK(Exp) | memAwaitCollectionK(Val)
  rule <k> evalExp(memE(Ee:Exp, Es:Exp)) => evalExp(Ee) ~> memAwaitElementK(Es) ... </k>
  rule <k> V:Val ~> memAwaitElementK(Es:Exp) => evalExp(Es) ~> memAwaitCollectionK(V) ... </k>
  rule <k> FAIL ~> memAwaitElementK(_:Exp) => FAIL ... </k>
  rule <k> listV(Vs:ValList) ~> memAwaitCollectionK(V:Val) => boolV(memVal(V, Vs)) ... </k>
  rule <k> FAIL ~> memAwaitCollectionK(_:Val) => FAIL ... </k>
  rule <k> _:Val ~> memAwaitCollectionK(_:Val) => FAIL ... </k> [owise]

  syntax Bool ::= memVal(Val, ValList)      [function, total, symbol(memVal)]
  rule memVal(_:Val, .ValList) => false
  rule memVal(V:Val, (W:Val, Vs:ValList)) => V ==K W orBool memVal(V, Vs)

  //   rulegroup Eval_exp/len
  syntax KItem ::= "lenAwaitOperandK" "(" ")"
  rule <k> evalExp(lenE(E:Exp)) => evalExp(E) ~> lenAwaitOperandK() ... </k>
  rule <k> textV(S:String) ~> lenAwaitOperandK() => natN(lengthString(S)) ... </k>
  rule <k> listV(Vs:ValList) ~> lenAwaitOperandK() => natN(lengthVals(Vs)) ... </k>
  rule <k> FAIL ~> lenAwaitOperandK() => FAIL ... </k>
  rule <k> _:Val ~> lenAwaitOperandK() => FAIL ... </k> [owise]

  //   rule Eval_exp/dot, /idx, /slice — all go through Eval_path
  syntax KItem ::= dotAwaitBaseK(Atom) | idxAwaitBaseK(Exp) | sliceAwaitBaseK(Exp, Exp)

  rule <k> evalExp(dotE(E:Exp, A:Atom)) => evalExp(E) ~> dotAwaitBaseK(A) ... </k>
  rule <k> V:Val ~> dotAwaitBaseK(A:Atom) => evalPath(V, dotPath(rootPath(), A)) ... </k>
  rule <k> FAIL ~> dotAwaitBaseK(_:Atom) => FAIL ... </k>

  rule <k> evalExp(idxE(Eb:Exp, Ei:Exp)) => evalExp(Eb) ~> idxAwaitBaseK(Ei) ... </k>
  rule <k> V:Val ~> idxAwaitBaseK(Ei:Exp) => evalPath(V, idxPath(rootPath(), Ei)) ... </k>
  rule <k> FAIL ~> idxAwaitBaseK(_:Exp) => FAIL ... </k>

  rule <k> evalExp(sliceE(Eb:Exp, Ei:Exp, En:Exp))
        => evalExp(Eb) ~> sliceAwaitBaseK(Ei, En)
       ...
       </k>
  rule <k> V:Val ~> sliceAwaitBaseK(Ei:Exp, En:Exp)
        => evalPath(V, slicePath(rootPath(), Ei, En))
       ...
       </k>
  rule <k> FAIL ~> sliceAwaitBaseK(_:Exp, _:Exp) => FAIL ... </k>

  //   rule Eval_exp/upd
  syntax KItem ::= updAwaitBaseK(Path, Exp) | updAwaitFieldK(Path, Val)
  rule <k> evalExp(updE(Eb:Exp, P:Path, Ef:Exp)) => evalExp(Eb) ~> updAwaitBaseK(P, Ef) ... </k>
  rule <k> V:Val ~> updAwaitBaseK(P:Path, Ef:Exp) => evalExp(Ef) ~> updAwaitFieldK(P, V) ... </k>
  rule <k> FAIL ~> updAwaitBaseK(_:Path, _:Exp) => FAIL ... </k>
  rule <k> Vf:Val ~> updAwaitFieldK(P:Path, Vb:Val) => evalPathUpd(Vb, P, Vf) ... </k>
  rule <k> FAIL ~> updAwaitFieldK(_:Path, _:Val) => FAIL ... </k>

  //   rule Eval_exp/call
  //
  //   rule Eval_targs: C |- targ* : targ_subst*
  //     -- if theta = $theta_of_tdenv(C.LOCAL.TYP)
  //     -- if (targ_subst = $subst_typ(theta, targ))*
  syntax KItem ::= callAwaitArgsK(Id, TypList)
  rule <k> evalExp(callE(X:Id, Ts:TypList, As:ArgList))
        => evalArgs(As) ~> callAwaitArgsK(X, substTypsMap(thetaOfTdenv(L), Ts))
       ...
       </k>
       <ltdenv> L:Map </ltdenv>

  rule <k> Vs:ValList ~> callAwaitArgsK(X:Id, Ts:TypList) => callFunc(X, Ts, Vs) ... </k>
  rule <k> FAIL ~> callAwaitArgsK(_:Id, _:TypList) => FAIL ... </k>

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

  //   rule Eval_exp/opt
  rule <k> evalIter(noVarr(), E:Exp, iterExp(quest(), Vs:VariList))
        => subOptRow(Vs) ~> evalOptSub(Vs, E)
       ...
       </k>

  syntax KItem ::= evalOptSub(VariList, Exp)
  rule <k> noRowOpt() ~> evalOptSub(_:VariList, _:Exp) => optV(noVal()) ... </k>
  rule <k> someRowOpt(Row:ValList) ~> evalOptSub(Vs:VariList, E:Exp)
        => pushLocalSave() ~> bindRow(Vs, Row) ~> evalExp(E) ~> popLocalSaveThen(optAwaitValueK())
       ...
       </k>

  // Bind each `vari` in `Vs` (unmarked, i.e. plain `varr`s) to the
  // corresponding element of `Row` in the (already-cleared-if-needed) local
  // value environment.
  syntax KItem ::= bindRow(VariList, ValList)
  rule <k> bindRow(.VariList, .ValList) => .K ... </k>
  rule <k> bindRow((V:Vari, Vs:VariList), (Val:Val, Vals:ValList)) => bindRow(Vs, Vals) ... </k>
       <lvenv> M:Map => M[varrOfVari(V) <- Val] </lvenv>

  //   rule Eval_exp/list
  rule <k> evalIter(noVarr(), E:Exp, iterExp(star(), Vs:VariList))
        => subListRows(Vs) ~> evalListSub(Vs, E)
       ...
       </k>

  syntax KItem ::= evalListSub(VariList, Exp)
  rule <k> .ValListList ~> evalListSub(_:VariList, _:Exp) => listV(.ValList) ... </k>
  rule <k> Rows:ValListList ~> evalListSub(Vs:VariList, E:Exp)
        => pushLocalSave() ~> evalEachRow(Vs, E, Rows) ~> popLocalSaveList()
       ...
       </k>
    requires Rows =/=K .ValListList

  syntax KItem ::= evalEachRow(VariList, Exp, ValListList)
  rule <k> evalEachRow(_:VariList, _:Exp, .ValListList) => .ValList ... </k>
  rule <k> evalEachRow(Vs:VariList, E:Exp, (Row:ValList ; Rows:ValListList))
        => peekLocalSave() ~> bindRow(Vs, Row) ~> evalExp(E)
           ~> collectRow(Vs, E, Rows)
       ...
       </k>

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
  syntax KItem ::= pathDotAwaitBaseK(Atom)
  rule <k> evalPath(V:Val, dotPath(P:Path, A:Atom)) => evalPath(V, P) ~> pathDotAwaitBaseK(A) ... </k>
  rule <k> strV(Fs:ValFieldList) ~> pathDotAwaitBaseK(A:Atom) => valOptToKItem(assocVal(A, Fs)) ... </k>
  rule <k> FAIL ~> pathDotAwaitBaseK(_:Atom) => FAIL ... </k>
  rule <k> _:Val ~> pathDotAwaitBaseK(_:Atom) => FAIL ... </k> [owise]

  syntax KItem ::= valOptToKItem(ValOpt)
  rule <k> valOptToKItem(someVal(V:Val)) => V ... </k>
  rule <k> valOptToKItem(noVal()) => FAIL ... </k>

  //   rulegroup Eval_path/idx
  syntax KItem ::= pathIdxBaseK(Path, Exp) | pathIdxAwaitIndexK(Val)
  rule <k> evalPath(V:Val, idxPath(P:Path, Ei:Exp))
        => evalPath(V, P) ~> pathIdxAwaitBaseK(Ei)
       ...
       </k>

  syntax KItem ::= pathIdxAwaitBaseK(Exp)
  rule <k> Vb:Val ~> pathIdxAwaitBaseK(Ei:Exp) => evalExp(Ei) ~> pathIdxAwaitIndexK(Vb) ... </k>
  rule <k> FAIL ~> pathIdxAwaitBaseK(_:Exp) => FAIL ... </k>

  rule <k> natN(N:Int) ~> pathIdxAwaitIndexK(textV(S:String)) => textV(substrString(S, N, N +Int 1)) ... </k>
    requires N >=Int 0 andBool N <Int lengthString(S)
  rule <k> natN(N:Int) ~> pathIdxAwaitIndexK(listV(Vs:ValList)) => nthVal(Vs, N) ... </k>
    requires N >=Int 0 andBool N <Int lengthVals(Vs)
  rule <k> FAIL ~> pathIdxAwaitIndexK(_:Val) => FAIL ... </k>
  rule <k> _:Val ~> pathIdxAwaitIndexK(_:Val) => FAIL ... </k> [owise]

  //   rulegroup Eval_path/slice
  syntax KItem ::= pathSliceAwaitBaseK(Exp, Exp) | pathSliceAwaitIndexK(Val, Exp)
                 | pathSliceAwaitLengthK(Val, Int)
  rule <k> evalPath(V:Val, slicePath(P:Path, Ei:Exp, En:Exp))
        => evalPath(V, P) ~> pathSliceAwaitBaseK(Ei, En)
       ...
       </k>
  rule <k> Vb:Val ~> pathSliceAwaitBaseK(Ei:Exp, En:Exp) => evalExp(Ei) ~> pathSliceAwaitIndexK(Vb, En) ... </k>
  rule <k> FAIL ~> pathSliceAwaitBaseK(_:Exp, _:Exp) => FAIL ... </k>
  rule <k> natN(I:Int) ~> pathSliceAwaitIndexK(Vb:Val, En:Exp) => evalExp(En) ~> pathSliceAwaitLengthK(Vb, I) ... </k>
  rule <k> FAIL ~> pathSliceAwaitIndexK(_:Val, _:Exp) => FAIL ... </k>

  rule <k> natN(N:Int) ~> pathSliceAwaitLengthK(textV(S:String), I:Int)
        => textV(substrString(S, I, I +Int N))
       ...
       </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthString(S)
  rule <k> natN(N:Int) ~> pathSliceAwaitLengthK(listV(Vs:ValList), I:Int)
        => listV(sliceVal(Vs, I, N))
       ...
       </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthVals(Vs)
  rule <k> FAIL ~> pathSliceAwaitLengthK(_:Val, _:Int) => FAIL ... </k>
  rule <k> _:Val ~> pathSliceAwaitLengthK(_:Val, _:Int) => FAIL ... </k> [owise]

  syntax ValList ::= sliceVal(ValList, Int, Int)
                                            [function, symbol(sliceVal)]
  rule sliceVal(_:ValList, _:Int, 0) => .ValList
  rule sliceVal((V:Val, Vs:ValList), 0, N:Int) => V, sliceVal(Vs, 0, N -Int 1)
    requires N >Int 0
  rule sliceVal((_:Val, Vs:ValList), I:Int, N:Int) => sliceVal(Vs, I -Int 1, N)
    requires I >Int 0

  // ==========================================================================
  // Eval_path_upd: ctx |- val path := val : res<val>
  // ==========================================================================

  syntax KItem ::= evalPathUpd(Val, Path, Val)

  //   rule Eval_path_upd/root
  rule <k> evalPathUpd(_:Val, rootPath(), Vn:Val) => Vn ... </k>

  //   rulegroup Eval_path_upd/idx
  syntax KItem ::= updIdxAwaitCurrentK(Val, Path, Exp, Val) | updIdxK(Val, Path, Val)
  rule <k> evalPathUpd(Vb:Val, idxPath(P:Path, Ei:Exp), Vn:Val)
        => evalPath(Vb, P) ~> updIdxAwaitCurrentK(Vb, P, Ei, Vn)
       ...
       </k>
  rule <k> Vcur:Val ~> updIdxAwaitCurrentK(Vb:Val, P:Path, Ei:Exp, Vn:Val)
        => evalExp(Ei) ~> updIdxAwaitIndexK(Vb, P, Vcur, Vn)
       ...
       </k>
  rule <k> FAIL ~> updIdxAwaitCurrentK(_:Val, _:Path, _:Exp, _:Val) => FAIL ... </k>

  syntax KItem ::= updIdxAwaitIndexK(Val, Path, Val, Val)
  // Text index update replaces one character (`-- if |t_n| = 1`).
  rule <k> natN(N:Int) ~> updIdxAwaitIndexK(Vb:Val, P:Path, textV(S:String), textV(Sn:String))
        => evalPathUpd(Vb, P, textV(replaceAtString(S, N, Sn)))
       ...
       </k>
    requires lengthString(Sn) ==Int 1 andBool N >=Int 0 andBool N <Int lengthString(S)
  rule <k> natN(N:Int) ~> updIdxAwaitIndexK(Vb:Val, P:Path, listV(Vs:ValList), Vn:Val)
        => evalPathUpd(Vb, P, listV(updateNthVal(Vs, N, Vn)))
       ...
       </k>
    requires N >=Int 0 andBool N <Int lengthVals(Vs)
  rule <k> FAIL ~> updIdxAwaitIndexK(_:Val, _:Path, _:Val, _:Val) => FAIL ... </k>
  rule <k> _:Val ~> updIdxAwaitIndexK(_:Val, _:Path, _:Val, _:Val) => FAIL ... </k> [owise]

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
  syntax KItem ::= updSliceAwaitCurrentK(Val, Path, Exp, Exp, Val)
                 | updSliceAwaitIndexK(Val, Path, Val, Exp, Val)
                 | updSliceAwaitLengthK(Val, Path, Val, Int, Val)
  rule <k> evalPathUpd(Vb:Val, slicePath(P:Path, Ei:Exp, En:Exp), Vn:Val)
        => evalPath(Vb, P) ~> updSliceAwaitCurrentK(Vb, P, Ei, En, Vn)
       ...
       </k>
  rule <k> Vcur:Val ~> updSliceAwaitCurrentK(Vb:Val, P:Path, Ei:Exp, En:Exp, Vn:Val)
        => evalExp(Ei) ~> updSliceAwaitIndexK(Vb, P, Vcur, En, Vn)
       ...
       </k>
  rule <k> FAIL ~> updSliceAwaitCurrentK(_:Val, _:Path, _:Exp, _:Exp, _:Val) => FAIL ... </k>
  rule <k> natN(I:Int) ~> updSliceAwaitIndexK(Vb:Val, P:Path, Vcur:Val, En:Exp, Vn:Val)
        => evalExp(En) ~> updSliceAwaitLengthK(Vb, P, Vcur, I, Vn)
       ...
       </k>
  rule <k> FAIL ~> updSliceAwaitIndexK(_:Val, _:Path, _:Val, _:Exp, _:Val) => FAIL ... </k>

  rule <k> natN(N:Int) ~> updSliceAwaitLengthK(Vb:Val, P:Path, textV(S:String), I:Int, textV(Sn:String))
        => evalPathUpd(Vb, P,
             textV(substrString(S, 0, I) +String Sn
                   +String substrString(S, I +Int N, lengthString(S))))
       ...
       </k>
    requires lengthString(Sn) ==Int N andBool I >=Int 0 andBool N >=Int 0
       andBool I +Int N <=Int lengthString(S)
  rule <k> natN(N:Int) ~> updSliceAwaitLengthK(Vb:Val, P:Path, listV(Vs:ValList), I:Int, listV(Ws:ValList))
        => evalPathUpd(Vb, P, listV(replaceSliceVal(Vs, I, N, Ws)))
       ...
       </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthVals(Vs)
  rule <k> FAIL ~> updSliceAwaitLengthK(_:Val, _:Path, _:Val, _:Int, _:Val) => FAIL ... </k>
  rule <k> _:Val ~> updSliceAwaitLengthK(_:Val, _:Path, _:Val, _:Int, _:Val) => FAIL ... </k> [owise]

  syntax ValList ::= replaceSliceVal(ValList, Int, Int, ValList)
                                            [function, symbol(replaceSliceVal)]
  rule replaceSliceVal(Vs:ValList, I:Int, N:Int, Ws:ValList)
    => appendVal(sliceVal(Vs, 0, I),
                 appendVal(Ws, sliceVal(Vs, I +Int N, lengthVals(Vs) -Int (I +Int N))))

  //   rule Eval_path_upd/dot
  syntax KItem ::= updDotAwaitCurrentK(Val, Path, Atom, Val)
  rule <k> evalPathUpd(Vb:Val, dotPath(P:Path, A:Atom), Vn:Val)
        => evalPath(Vb, P) ~> updDotAwaitCurrentK(Vb, P, A, Vn)
       ...
       </k>
  rule <k> strV(Fs:ValFieldList) ~> updDotAwaitCurrentK(Vb:Val, P:Path, A:Atom, Vn:Val)
        => evalPathUpd(Vb, P, strV(updateField(Fs, A, Vn)))
       ...
       </k>
  rule <k> FAIL ~> updDotAwaitCurrentK(_:Val, _:Path, _:Atom, _:Val) => FAIL ... </k>
  rule <k> _:Val ~> updDotAwaitCurrentK(_:Val, _:Path, _:Atom, _:Val) => FAIL ... </k> [owise]

  // Update every field selected by its atom.
  syntax ValFieldList ::= updateField(ValFieldList, Atom, Val)
                                            [function, total, symbol(updateField)]
  rule updateField(.ValFieldList, _:Atom, _:Val) => .ValFieldList
  rule updateField((valField(A':Atom, V:Val), Fs:ValFieldList), A:Atom, Vn:Val)
    => valField(A', #if A ==String A' #then Vn #else V #fi), updateField(Fs, A, Vn)

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
  // ==========================================================================

  syntax KItem ::= callFunc(Id, TypList, ValList)

endmodule
