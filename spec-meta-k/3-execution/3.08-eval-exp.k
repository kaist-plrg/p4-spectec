// Expression, path, and argument evaluation.
//
// Mirrors spec-meta/3-execution/3.07-eval-exp.watsup, 3.08-eval-path.watsup and
// 3.09-eval-arg.watsup.
// An expression at the head of `<k>` evaluates its operands by the
// strictness attributes of 1.0-syntax.k; the rules here compute on values.

requires "3.05-eval-ops.k"
requires "3.06.4-typ-cast.k"
requires "3.07-eval-assign.k"

module EVAL-EXP
  imports ASSIGN
  imports EVAL-OPS
  imports TYP-CAST

  // ==========================================================================
  // Eval_exp: ctx |- exp : res<val>
  // ==========================================================================

  //   rulegroup Eval_exp/literal
  rule <k> boolE(B:Bool) => boolV(B) ... </k>
  rule <k> textE(S:String) => textV(S) ... </k>

  //   rule Eval_exp/variable
  rule <k> varE(X:Id) => V ... </k>
       <lvenv> ... varr(X, .IterList) |-> V:Val ... </lvenv>
  rule <k> varE(_:Id) => error ... </k> [owise]

  //   rulegroup Eval_exp/unary
  rule <k> unE(notOp(), boolV(B:Bool)) => boolV(notBool B) ... </k>
  rule <k> unE(O:NumUnOp, N:Num) => numUnop(O, N) ... </k>
  rule <k> unE(_:UnOp, _:Val) => error ... </k> [owise]

  //   rulegroup Eval_exp/binary and /compare
  rule <k> binE(O:BoolBinOp, boolV(Bl:Bool), boolV(Br:Bool)) => boolV(boolBinop(O, Bl, Br)) ... </k>
  rule <k> binE(O:NumBinOp, Nl:Num, Nr:Num) => unres(numBinop(O, Nl, Nr)) ... </k>
  rule <k> binE(_:BinOp, _:Val, _:Val) => error ... </k> [owise]

  rule <k> cmpE(O:PolyCmpOp, Vl:Val, Vr:Val) => boolV(polyCmpop(O, Vl, Vr)) ... </k>
  rule <k> cmpE(O:NumCmpOp, Nl:Num, Nr:Num) => unresBool(numCmpop(O, Nl, Nr)) ... </k>
  rule <k> cmpE(_:CmpOp, _:Val, _:Val) => error ... </k> [owise]

  //   rule Eval_exp/upcast, /downcast, /subtype
  rule <k> upCastE(T:Typ, V:Val) => unres(upcast(G, L, FE, T, V)) ... </k>
       <gtdenv> G:Map </gtdenv>
       <ltdenv> L:Map </ltdenv>
       <gfenv> FE:Map </gfenv>
  rule <k> downCastE(T:Typ, V:Val) => unres(downcast(G, L, FE, T, V)) ... </k>
       <gtdenv> G:Map </gtdenv>
       <ltdenv> L:Map </ltdenv>
       <gfenv> FE:Map </gfenv>
  rule <k> subE(V:Val, T:Typ) => unresBool(subtyp(FE, updateMap(G, L), T, V)) ... </k>
       <gtdenv> G:Map </gtdenv>
       <ltdenv> L:Map </ltdenv>
       <gfenv> FE:Map </gfenv>

  //   rulegroup Eval_exp/match
  rule <k> matchE(V:Val, P:Pat) => boolV(matches(V, P)) ... </k>

  //   rule Eval_exp/cons
  rule <k> consE(V:Val, listV(Vs:ValList)) => listV(V, Vs) ... </k>
  rule <k> consE(_:Val, _:Val) => error ... </k> [owise]

  //   rulegroup Eval_exp/concat
  rule <k> catE(textV(Sl:String), textV(Sr:String)) => textV(Sl +String Sr) ... </k>
  rule <k> catE(listV(Vl:ValList), listV(Vr:ValList)) => listV(appendVal(Vl, Vr)) ... </k>
  rule <k> catE(_:Val, _:Val) => error ... </k> [owise]

  //   rule Eval_exp/mem
  rule <k> memE(V:Val, listV(Vs:ValList)) => boolV(memVal(V, Vs)) ... </k>
  rule <k> memE(_:Val, _:Val) => error ... </k> [owise]

  syntax Bool ::= memVal(Val, ValList)      [function, total, symbol(memVal)]
  rule memVal(_:Val, .ValList) => false
  rule memVal(V:Val, (W:Val, Vs:ValList)) => V ==K W orBool memVal(V, Vs)

  //   rulegroup Eval_exp/len
  rule <k> lenE(textV(S:String)) => natN(lengthString(S)) ... </k>
  rule <k> lenE(listV(Vs:ValList)) => natN(lengthVals(Vs)) ... </k>
  rule <k> lenE(_:Val) => error ... </k> [owise]

  //   rule Eval_exp/dot, /idx, /slice, and Eval_path/dot, /idx, /slice
  rule <k> dotE(strV(Fs:ValFieldList), A:Atom) => unopt(assocVal(A, Fs)) ... </k>
  rule <k> dotE(_:Val, _:Atom) => error ... </k> [owise]

  // The field found, back in flight.
  syntax KItem ::= unopt(ValOpt)
  rule <k> unopt(someVal(V:Val)) => V ... </k>
  rule <k> unopt(noVal()) => error ... </k>

  rule <k> idxE(textV(S:String), natN(N:Int)) => textV(substrString(S, N, N +Int 1)) ... </k>
    requires N >=Int 0 andBool N <Int lengthString(S)
  rule <k> idxE(listV(Vs:ValList), natN(N:Int)) => nthVal(Vs, N) ... </k>
    requires N >=Int 0 andBool N <Int lengthVals(Vs)
  rule <k> idxE(_:Val, _:Val) => error ... </k> [owise]

  syntax Val ::= nthVal(ValList, Int)       [function, symbol(nthVal)]
  rule nthVal((V:Val, _:ValList), 0) => V
  rule nthVal((_:Val, Vs:ValList), N:Int) => nthVal(Vs, N -Int 1) requires N >Int 0

  rule <k> sliceE(textV(S:String), natN(I:Int), natN(N:Int))
        => textV(substrString(S, I, I +Int N))
       ...
       </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthString(S)
  rule <k> sliceE(listV(Vs:ValList), natN(I:Int), natN(N:Int)) => listV(sliceVal(Vs, I, N)) ... </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthVals(Vs)
  rule <k> sliceE(_:Val, _:Val, _:Val) => error ... </k> [owise]

  syntax ValList ::= sliceVal(ValList, Int, Int)
                                            [function, symbol(sliceVal)]
  rule sliceVal(_:ValList, _:Int, 0) => .ValList
  rule sliceVal((V:Val, Vs:ValList), 0, N:Int) => V, sliceVal(Vs, 0, N -Int 1)
    requires N >Int 0
  rule sliceVal((_:Val, Vs:ValList), I:Int, N:Int) => sliceVal(Vs, I -Int 1, N)
    requires I >Int 0

  //   rule Eval_exp/upd
  rule <k> updE(Vb:Val, P:Path, Vn:Val) => upd(Vb, P, Vn) ... </k>

  //   rule Eval_exp/call
  //
  //   rule Eval_arg/exp, /fun
  rule <k> expA(V:Val) => V ... </k>
  rule <k> funA(X:Id) => funcV(resolveFuncParam(X, LF)) ... </k>
       <lfenv> LF:Map </lfenv>

  //   rule Eval_targs: the type arguments under the local tdenv's aliases
  rule <k> callE(X:Id, Ts:TArgList, Vs:ValList) => callTargs(X, substTargs(thetaOfTdenv(L), Ts), Vs) ... </k>
       <ltdenv> L:Map </ltdenv>

  syntax KItem ::= callTargs(Id, Res, ValList)
  rule <k> callTargs(X:Id, ok(Ts:TArgList), Vs:ValList) => callFunc(X, Ts, Vs) ... </k>
  rule <k> callTargs(_:Id, ERROR, _:ValList) => error ... </k>

  //   rulegroup Eval_exp/iter
  syntax KItem ::= evalIter(VarrOpt, Exp, IterExp)
  rule <k> iterE(E:Exp, IE:IterExp) => evalIter(isIterOnVar(iterE(E, IE)), E, IE) ... </k>

  //   rule Eval_exp/simple
  rule <k> evalIter(someVarr(R:Varr), _:Exp, _:IterExp) => V ... </k>
       <lvenv> ... R |-> V:Val ... </lvenv>
  rule <k> evalIter(someVarr(_:Varr), _:Exp, _:IterExp) => error ... </k> [owise]

  //   rule Eval_exp/opt-succ: the body under the sub-context, if there is one
  rule <k> evalIter(noVarr(), E:Exp, iterExp(quest(), Xs:VariList))
        => optSub(Xs, E, subOpt(Xs, M))
       ...
       </k>
       <lvenv> M:Map </lvenv>

  syntax KItem ::= optSub(VariList, Exp, Res)
  rule <k> optSub(_:VariList, _:Exp, ok(noSub())) => optV(noVal()) ... </k>
  rule <k> optSub(Xs:VariList, E:Exp, ok(someSub(Vs:ValList)))
        => optE(someExp(inScope(bindRow(Xs, Vs) ~> E)))
       ...
       </k>
  rule <k> optSub(_:VariList, _:Exp, ERROR) => error ... </k>

  //   rule Eval_exp/list-succ: the body under each sub-context in turn
  rule <k> evalIter(noVarr(), E:Exp, iterExp(star(), Xs:VariList))
        => listSub(Xs, E, subList(Xs, M))
       ...
       </k>
       <lvenv> M:Map </lvenv>

  syntax KItem ::= listSub(VariList, Exp, Res)
  rule <k> listSub(Xs:VariList, E:Exp, ok(Rows:ValListList))
        => listE(rowExps(Xs, E, Rows))
       ...
       </k>
  rule <k> listSub(_:VariList, _:Exp, ERROR) => error ... </k>

  syntax ExpList ::= rowExps(VariList, Exp, ValListList)
                                            [function, total, symbol(rowExps)]
  rule rowExps(_:VariList, _:Exp, .ValListList) => .ExpList
  rule rowExps(Xs:VariList, E:Exp, (Row:ValList ; Rows:ValListList))
    => inScope(bindRow(Xs, Row) ~> E), rowExps(Xs, E, Rows)

  // ==========================================================================
  // Eval_path: ctx |- val path : res<val>
  //
  // A path is the chain of the `dotE`/`idxE`/`sliceE` steps from its root.
  // ==========================================================================

  rule <k> path(V:Val, rootPath()) => V ... </k>
  rule <k> path(V:Val, dotPath(P:Path, A:Atom)) => dotE(path(V, P), A) ... </k>
  rule <k> path(V:Val, idxPath(P:Path, Ei:Exp)) => idxE(path(V, P), Ei) ... </k>
  rule <k> path(V:Val, slicePath(P:Path, Ei:Exp, En:Exp)) => sliceE(path(V, P), Ei, En) ... </k>

  // ==========================================================================
  // Eval_path_upd: ctx |- val path := val : res<val>
  //
  // The innermost step rebuilds the value at its prefix, then the prefix is
  // updated with the result.
  // ==========================================================================

  syntax Exp ::= updIdx(Exp, Exp, Val)      [seqstrict(1, 2)]
               | updSlice(Exp, Exp, Exp, Val)
                                            [seqstrict(1, 2, 3)]
               | updDot(Exp, Atom, Val)     [strict(1)]

  //   rule Eval_path_upd/root
  rule <k> upd(_:Val, rootPath(), Vn:Val) => Vn ... </k>

  //   rulegroup Eval_path_upd/idx
  rule <k> upd(Vb:Val, idxPath(P:Path, Ei:Exp), Vn:Val)
        => upd(Vb, P, updIdx(path(Vb, P), Ei, Vn))
       ...
       </k>

  // Text index update replaces one character (`-- if |t_n| = 1`).
  rule <k> updIdx(textV(S:String), natN(N:Int), textV(Sn:String))
        => textV(replaceAtString(S, N, Sn))
       ...
       </k>
    requires lengthString(Sn) ==Int 1 andBool N >=Int 0 andBool N <Int lengthString(S)
  rule <k> updIdx(listV(Vs:ValList), natN(N:Int), Vn:Val) => listV(updateNthVal(Vs, N, Vn)) ... </k>
    requires N >=Int 0 andBool N <Int lengthVals(Vs)
  rule <k> updIdx(_:Val, _:Val, _:Val) => error ... </k> [owise]

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
  rule <k> upd(Vb:Val, slicePath(P:Path, Ei:Exp, En:Exp), Vn:Val)
        => upd(Vb, P, updSlice(path(Vb, P), Ei, En, Vn))
       ...
       </k>

  rule <k> updSlice(textV(S:String), natN(I:Int), natN(N:Int), textV(Sn:String))
        => textV(substrString(S, 0, I) +String Sn +String substrString(S, I +Int N, lengthString(S)))
       ...
       </k>
    requires lengthString(Sn) ==Int N andBool I >=Int 0 andBool N >=Int 0
       andBool I +Int N <=Int lengthString(S)
  rule <k> updSlice(listV(Vs:ValList), natN(I:Int), natN(N:Int), listV(Ws:ValList))
        => listV(replaceSliceVal(Vs, I, N, Ws))
       ...
       </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthVals(Vs)
  rule <k> updSlice(_:Val, _:Val, _:Val, _:Val) => error ... </k> [owise]

  syntax ValList ::= replaceSliceVal(ValList, Int, Int, ValList)
                                            [function, symbol(replaceSliceVal)]
  rule replaceSliceVal(Vs:ValList, I:Int, N:Int, Ws:ValList)
    => appendVal(sliceVal(Vs, 0, I),
                 appendVal(Ws, sliceVal(Vs, I +Int N, lengthVals(Vs) -Int (I +Int N))))

  //   rule Eval_path_upd/dot
  rule <k> upd(Vb:Val, dotPath(P:Path, A:Atom), Vn:Val)
        => upd(Vb, P, updDot(path(Vb, P), A, Vn))
       ...
       </k>

  rule <k> updDot(strV(Fs:ValFieldList), A:Atom, Vn:Val) => strV(updateField(Fs, A, Vn)) ... </k>
  rule <k> updDot(_:Val, _:Atom, _:Val) => error ... </k> [owise]

  // Update every field selected by its atom.
  syntax ValFieldList ::= updateField(ValFieldList, Atom, Val)
                                            [function, total, symbol(updateField)]
  rule updateField(.ValFieldList, _:Atom, _:Val) => .ValFieldList
  rule updateField((valField(A, _:Val), Fs:ValFieldList), A:Atom, Vn:Val)
    => valField(A, Vn), updateField(Fs, A, Vn)
  rule updateField((F:ValField, Fs:ValFieldList), A:Atom, Vn:Val)
    => F, updateField(Fs, A, Vn) [owise]

endmodule
