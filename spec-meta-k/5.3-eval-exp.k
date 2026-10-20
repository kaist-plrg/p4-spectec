// Expression, path, and argument evaluation.
//
// Mirrors spec-meta/5.3-eval-exp.watsup and spec-meta/5.4-eval-arg.watsup.
// An expression at the head of `<k>` evaluates its operands by the
// strictness attributes of 1-syntax.k; the rules here compute on values.

requires "5.1-eval-typ.k"
requires "5.2-eval-assign.k"

module EVAL-EXP
  imports ASSIGN
  imports TYP

  // ==========================================================================
  // Eval_exp: ctx |- exp : res<val>
  // ==========================================================================

  //   rulegroup Eval_exp/literal
  rule boolE(B:Bool) => boolV(B)
  rule textE(S:String) => textV(S)

  //   rule Eval_exp/variable
  rule <k> varE(X:Id) => V ... </k>
       <lvenv> ... varr(X, .IterList) |-> V:Val ... </lvenv>
  rule <k> varE(_:Id) => backtrack ... </k> [owise]

  //   rulegroup Eval_exp/unary
  rule unE(notOp(), boolV(B:Bool)) => boolV(notBool B)
  rule unE(O:NumUnOp, N:Num) => unopNumber(O, N)
  rule unE(_:UnOp, _:Val) => backtrack [owise]

  //   rulegroup Eval_exp/binary and /compare
  rule binE(O:BoolBinOp, boolV(Bl:Bool), boolV(Br:Bool)) => boolV(binopBool(O, Bl, Br))
  rule binE(O:NumBinOp, Nl:Num, Nr:Num) => binopNumber(O, Nl, Nr)
  rule binE(_:BinOp, _:Val, _:Val) => backtrack [owise]

  rule cmpE(O:PolyCmpOp, Vl:Val, Vr:Val) => boolV(cmpopPoly(O, Vl, Vr))
  rule cmpE(O:NumCmpOp, Nl:Num, Nr:Num) => boolV(cmpopNumber(O, Nl, Nr))

  //   rule Eval_exp/upcast, /downcast, /subtype
  rule <k> upCastE(T:Typ, V:Val) => unres(upcast(updateMap(G, L), T, V)) ... </k>
       <ltdenv> L:Map </ltdenv>
       <gtdenv> G:Map </gtdenv>
  rule <k> downCastE(T:Typ, V:Val) => unres(downcast(updateMap(G, L), T, V)) ... </k>
       <ltdenv> L:Map </ltdenv>
       <gtdenv> G:Map </gtdenv>
  rule <k> subE(V:Val, T:Typ) => boolV(subtyp(updateMap(G, L), T, V)) ... </k>
       <ltdenv> L:Map </ltdenv>
       <gtdenv> G:Map </gtdenv>

  //   rulegroup Eval_exp/match
  rule matchE(injV(valCase(M':MixOp, _:ValList)), injPat(M:MixOp)) => boolV(M ==K M')
  rule matchE(listV(Vs:ValList), consPat()) => boolV(lengthVals(Vs) >Int 0)
  rule matchE(listV(Vs:ValList), fixedPat(N:Int)) => boolV(lengthVals(Vs) ==Int N)
  rule matchE(listV(Vs:ValList), nilPat()) => boolV(lengthVals(Vs) ==Int 0)
  rule matchE(optV(someVal(_:Val)), somePat()) => boolV(true)
  rule matchE(optV(noVal()), somePat()) => boolV(false)
  rule matchE(optV(someVal(_:Val)), nonePat()) => boolV(false)
  rule matchE(optV(noVal()), nonePat()) => boolV(true)
  rule matchE(_:Val, _:Pattern) => backtrack [owise]

  syntax Int ::= lengthVals(ValList)        [function, total, symbol(lengthVals)]
  rule lengthVals(.ValList) => 0
  rule lengthVals(_:Val, Vs:ValList) => 1 +Int lengthVals(Vs)

  //   rule Eval_exp/tuple, /case, /struct, /list
  rule tupE(Vs:ValList) => tupV(Vs)
  rule injE(C:ValCase) => injV(C)
  rule strE(Fs:ValFieldList) => strV(Fs)
  rule listE(Vs:ValList) => listV(Vs)

  //   rulegroup Eval_exp/opt
  context optE(someExp(HOLE))
  rule optE(someExp(V:Val)) => optV(someVal(V))
  rule optE(noExp()) => optV(noVal())

  //   rule Eval_exp/cons
  rule consE(V:Val, listV(Vs:ValList)) => listV(V, Vs)
  rule consE(_:Val, _:Val) => backtrack [owise]

  //   rulegroup Eval_exp/concat
  rule catE(textV(Sl:String), textV(Sr:String)) => textV(Sl +String Sr)
  rule catE(listV(Vl:ValList), listV(Vr:ValList)) => listV(appendVal(Vl, Vr))
  rule catE(_:Val, _:Val) => backtrack [owise]

  //   rule Eval_exp/mem
  rule memE(V:Val, listV(Vs:ValList)) => boolV(memVal(V, Vs))
  rule memE(_:Val, _:Val) => backtrack [owise]

  syntax Bool ::= memVal(Val, ValList)      [function, total, symbol(memVal)]
  rule memVal(_:Val, .ValList) => false
  rule memVal(V:Val, (W:Val, Vs:ValList)) => V ==K W orBool memVal(V, Vs)

  //   rulegroup Eval_exp/len
  rule lenE(textV(S:String)) => natN(lengthString(S))
  rule lenE(listV(Vs:ValList)) => natN(lengthVals(Vs))
  rule lenE(_:Val) => backtrack [owise]

  //   rule Eval_exp/dot, /idx, /slice, and Eval_path/dot, /idx, /slice
  rule dotE(strV(Fs:ValFieldList), A:Atom) => unopt(assocVal(A, Fs))
  rule dotE(_:Val, _:Atom) => backtrack [owise]

  // The field found, back in flight.
  syntax KItem ::= unopt(ValOpt)
  rule unopt(someVal(V:Val)) => V
  rule unopt(noVal()) => backtrack

  rule idxE(textV(S:String), natN(N:Int)) => textV(substrString(S, N, N +Int 1))
    requires N >=Int 0 andBool N <Int lengthString(S)
  rule idxE(listV(Vs:ValList), natN(N:Int)) => nthVal(Vs, N)
    requires N >=Int 0 andBool N <Int lengthVals(Vs)
  rule idxE(_:Val, _:Val) => backtrack [owise]

  syntax Val ::= nthVal(ValList, Int)       [function, symbol(nthVal)]
  rule nthVal((V:Val, _:ValList), 0) => V
  rule nthVal((_:Val, Vs:ValList), N:Int) => nthVal(Vs, N -Int 1) requires N >Int 0

  rule sliceE(textV(S:String), natN(I:Int), natN(N:Int)) => textV(substrString(S, I, I +Int N))
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthString(S)
  rule sliceE(listV(Vs:ValList), natN(I:Int), natN(N:Int)) => listV(sliceVal(Vs, I, N))
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthVals(Vs)
  rule sliceE(_:Val, _:Val, _:Val) => backtrack [owise]

  syntax ValList ::= sliceVal(ValList, Int, Int)
                                            [function, symbol(sliceVal)]
  rule sliceVal(_:ValList, _:Int, 0) => .ValList
  rule sliceVal((V:Val, Vs:ValList), 0, N:Int) => V, sliceVal(Vs, 0, N -Int 1)
    requires N >Int 0
  rule sliceVal((_:Val, Vs:ValList), I:Int, N:Int) => sliceVal(Vs, I -Int 1, N)
    requires I >Int 0

  //   rule Eval_exp/upd
  rule updE(Vb:Val, P:Path, Vn:Val) => upd(Vb, P, Vn)

  //   rule Eval_exp/call
  //
  //   rule Eval_arg/exp, /fun
  //   rule Eval_targs: the type arguments under the local tdenv's aliases
  rule expA(V:Val) => V
  rule funA(X:Id) => funcV(X)

  rule <k> callE(X:Id, Ts:TArgList, Vs:ValList) => callFunc(X, substTargs(L, Ts), Vs) ... </k>
       <ltdenv> L:Map </ltdenv>

  //   rulegroup Eval_exp/iter
  syntax KItem ::= evalIter(VarrOpt, Exp, IterExp)
  rule iterE(E:Exp, IE:IterExp) => evalIter(isIterOnVar(iterE(E, IE)), E, IE)

  //   rule Eval_exp/simple
  rule <k> evalIter(someVarr(R:Varr), _:Exp, _:IterExp) => V ... </k>
       <lvenv> ... R |-> V:Val ... </lvenv>
  rule <k> evalIter(someVarr(_:Varr), _:Exp, _:IterExp) => backtrack ... </k> [owise]

  //   rule Eval_exp/opt: the body under the sub-context, if there is one
  rule <k> evalIter(noVarr(), E:Exp, iterExp(quest(), Xs:VariList))
        => optSub(Xs, E, finds(iterVaris(Xs, quest()), M))
       ...
       </k>
       <lvenv> M:Map </lvenv>

  syntax KItem ::= optSub(VariList, Exp, ValList)
  rule optSub(_:VariList, _:Exp, Vs:ValList) => optV(noVal())
    requires allNoneOpts(Vs)
  rule optSub(Xs:VariList, E:Exp, Vs:ValList)
    => optE(someExp(inScope(bindRow(Xs, unwrapSomeOpts(Vs)) ~> E)))
    requires allSomeOpts(Vs)

  //   rule Eval_exp/list: the body under each sub-context in turn
  rule <k> evalIter(noVarr(), E:Exp, iterExp(star(), Xs:VariList))
        => listE(rowExps(Xs, E, rows(finds(iterVaris(Xs, star()), M))))
       ...
       </k>
       <lvenv> M:Map </lvenv>

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

  syntax Exp ::= path(Val, Path)

  rule path(V:Val, rootPath()) => V
  rule path(V:Val, dotPath(P:Path, A:Atom)) => dotE(path(V, P), A)
  rule path(V:Val, idxPath(P:Path, Ei:Exp)) => idxE(path(V, P), Ei)
  rule path(V:Val, slicePath(P:Path, Ei:Exp, En:Exp)) => sliceE(path(V, P), Ei, En)

  // ==========================================================================
  // Eval_path_upd: ctx |- val path := val : res<val>
  //
  // The innermost step rebuilds the value at its prefix, then the prefix is
  // updated with the result.
  // ==========================================================================

  syntax Exp ::= upd(Val, Path, Exp)        [strict(3)]
               | updIdx(Exp, Exp, Val)      [seqstrict(1, 2)]
               | updSlice(Exp, Exp, Exp, Val)
                                            [seqstrict(1, 2, 3)]
               | updDot(Exp, Atom, Val)     [strict(1)]

  //   rule Eval_path_upd/root
  rule upd(_:Val, rootPath(), Vn:Val) => Vn

  //   rulegroup Eval_path_upd/idx
  rule upd(Vb:Val, idxPath(P:Path, Ei:Exp), Vn:Val) => upd(Vb, P, updIdx(path(Vb, P), Ei, Vn))

  // Text index update replaces one character (`-- if |t_n| = 1`).
  rule updIdx(textV(S:String), natN(N:Int), textV(Sn:String)) => textV(replaceAtString(S, N, Sn))
    requires lengthString(Sn) ==Int 1 andBool N >=Int 0 andBool N <Int lengthString(S)
  rule updIdx(listV(Vs:ValList), natN(N:Int), Vn:Val) => listV(updateNthVal(Vs, N, Vn))
    requires N >=Int 0 andBool N <Int lengthVals(Vs)
  rule updIdx(_:Val, _:Val, _:Val) => backtrack [owise]

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
  rule upd(Vb:Val, slicePath(P:Path, Ei:Exp, En:Exp), Vn:Val)
    => upd(Vb, P, updSlice(path(Vb, P), Ei, En, Vn))

  rule updSlice(textV(S:String), natN(I:Int), natN(N:Int), textV(Sn:String))
    => textV(substrString(S, 0, I) +String Sn +String substrString(S, I +Int N, lengthString(S)))
    requires lengthString(Sn) ==Int N andBool I >=Int 0 andBool N >=Int 0
       andBool I +Int N <=Int lengthString(S)
  rule updSlice(listV(Vs:ValList), natN(I:Int), natN(N:Int), listV(Ws:ValList))
    => listV(replaceSliceVal(Vs, I, N, Ws))
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthVals(Vs)
  rule updSlice(_:Val, _:Val, _:Val, _:Val) => backtrack [owise]

  syntax ValList ::= replaceSliceVal(ValList, Int, Int, ValList)
                                            [function, symbol(replaceSliceVal)]
  rule replaceSliceVal(Vs:ValList, I:Int, N:Int, Ws:ValList)
    => appendVal(sliceVal(Vs, 0, I),
                 appendVal(Ws, sliceVal(Vs, I +Int N, lengthVals(Vs) -Int (I +Int N))))

  //   rule Eval_path_upd/dot
  rule upd(Vb:Val, dotPath(P:Path, A:Atom), Vn:Val) => upd(Vb, P, updDot(path(Vb, P), A, Vn))

  rule updDot(strV(Fs:ValFieldList), A:Atom, Vn:Val) => strV(updateField(Fs, A, Vn))
  rule updDot(_:Val, _:Atom, _:Val) => backtrack [owise]

  // Update every field selected by its atom.
  syntax ValFieldList ::= updateField(ValFieldList, Atom, Val)
                                            [function, total, symbol(updateField)]
  rule updateField(.ValFieldList, _:Atom, _:Val) => .ValFieldList
  rule updateField((valField(A, _:Val), Fs:ValFieldList), A:Atom, Vn:Val)
    => valField(A, Vn), updateField(Fs, A, Vn)
  rule updateField((F:ValField, Fs:ValFieldList), A:Atom, Vn:Val)
    => F, updateField(Fs, A, Vn) [owise]

  // ==========================================================================
  // Call_func forward declaration.
  // ==========================================================================

  syntax KItem ::= callFunc(Id, TArgList, ValList)

endmodule
