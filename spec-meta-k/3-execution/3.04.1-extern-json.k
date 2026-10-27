// JSON codec for the external interface to the host interpreter.
//
//   val   ::= ["boolV", <bool>]
//           | ["natN", "<decimal>"] | ["intN", "<decimal>"]
//           | ["textV", <string>]
//           | ["strV", [[<atom>, val], ...]]
//           | ["injV", mixop, [val, ...]]
//           | ["tupV", [val, ...]]
//           | ["optV", null] | ["optV", val]
//           | ["listV", [val, ...]]
//           | ["funcV", <id>]
//           | ["extV", <json>]
//
//   mixop ::= [[<atom>, ...], ...]     // an atoms matrix, exactly K's MixOp
//
//   typ   ::= ["natT"] | ["intT"] | ["boolT"] | ["textT"]
//           | ["varT", <id>, [typ, ...]] | ["tupT", [typ, ...]]
//           | ["iterT", typ, "?"|"*"] | ["funcT", [<id>, ...], [typ, ...], typ]
//
//   request  ::= {"builtin":     <id>, "targs": [typ, ...], "args": [val, ...]}
//              | {"extern-func": <id>, "targs": [typ, ...], "args": [val, ...]}
//              | {"extern-rel":  <id>, "args": [val, ...]}
//
//   response ::= {"ok": val}         // builtin and extern-func
//              | {"ok": [val, ...]}  // extern-rel: a relation yields val*
//              | {"fail": null}      // recoverable mismatch
//              | {"error": text}     // a fatal error of the host
//
// Calls from the host and their answers (3.14-entry.k). A spec is the JSON
// of 3.04.3-spec-json.k; call fields come sorted.
//
//   call     ::= {"load": spec}      // $load_ctx($empty_ctx, spec)
//              | {"eval-al": spec}   // Eval_AL
//              | {"args": [val, ...], "call-rel": <id>}
//              | {"args": [val, ...], "call-func": <id>, "targs": [typ, ...]}
//              | {"exit": null}
//
//   answer   ::= {"ready": null}     // before the first call
//              | {"ok": [val, ...]}  // load and call-rel
//              | {"ok": val}         // eval-al and call-func
//              | {"fail": null}      // UNMATCH
//              | {"error": text}     // ERROR

requires "json.md"
requires "../0-aux/0.1-config.k"
requires "3.02-context.k"

module EXTERN-JSON
  imports CONFIG
  imports CONTEXT
  imports JSON
  imports STRING
  imports INT
  imports K-IO

  // ==========================================================================
  // Outbound: K terms -> JSON
  // ==========================================================================

  syntax JSON ::= jsonOfVal(Val)              [function, symbol(jsonOfVal)]
  syntax JSONs ::= jsonOfVals(ValList)        [function, symbol(jsonOfVals)]
  syntax JSONs ::= jsonOfValFields(ValFieldList)
                                              [function, symbol(jsonOfValFields)]
  syntax JSON ::= jsonOfMixOp(MixOp)          [function, symbol(jsonOfMixOp)]
  syntax JSONs ::= jsonOfAtomRows(MixOp)      [function, symbol(jsonOfAtomRows)]
  syntax JSONs ::= jsonOfAtoms(AtomList)      [function, symbol(jsonOfAtoms)]
  syntax JSON ::= jsonOfTyp(Typ)              [function, symbol(jsonOfTyp)]
  syntax JSONs ::= jsonOfTyps(TypList)        [function, symbol(jsonOfTyps)]
  syntax JSONs ::= jsonOfTargs(TArgList)      [function, symbol(jsonOfTargs)]

  syntax JSON ::= jsonList(JSONs)             [function, total, symbol(jsonList)]
  rule jsonList(Js:JSONs) => [ Js ]

  rule jsonOfVal(boolV(B:Bool))   => [ "boolV", B, .JSONs ]
  rule jsonOfVal(natN(N:Int))     => [ "natN", Int2String(N), .JSONs ]
  rule jsonOfVal(intN(N:Int))     => [ "intN", Int2String(N), .JSONs ]
  rule jsonOfVal(textV(S:String)) => [ "textV", S, .JSONs ]
  rule jsonOfVal(strV(Fs:ValFieldList))
    => [ "strV", jsonList(jsonOfValFields(Fs)), .JSONs ]
  rule jsonOfVal(injV(valCase(M:MixOp, Vs:ValList)))
    => [ "injV", jsonOfMixOp(M), jsonList(jsonOfVals(Vs)), .JSONs ]
  rule jsonOfVal(tupV(Vs:ValList))  => [ "tupV", jsonList(jsonOfVals(Vs)), .JSONs ]
  rule jsonOfVal(optV(noVal()))     => [ "optV", null, .JSONs ]
  rule jsonOfVal(optV(someVal(V:Val))) => [ "optV", jsonOfVal(V), .JSONs ]
  rule jsonOfVal(listV(Vs:ValList)) => [ "listV", jsonList(jsonOfVals(Vs)), .JSONs ]
  rule jsonOfVal(funcV(X:Id))       => [ "funcV", X, .JSONs ]
  rule jsonOfVal(extV(J:JSON))       => [ "extV", J, .JSONs ]

  rule jsonOfVals(.ValList) => .JSONs
  rule jsonOfVals(V:Val, Vs:ValList) => jsonOfVal(V), jsonOfVals(Vs)

  rule jsonOfValFields(.ValFieldList) => .JSONs
  rule jsonOfValFields(valField(A:Atom, V:Val), Fs:ValFieldList)
    => [ A, jsonOfVal(V), .JSONs ], jsonOfValFields(Fs)

  rule jsonOfMixOp(M:MixOp) => jsonList(jsonOfAtomRows(M))

  rule jsonOfAtomRows(.MixOp) => .JSONs
  rule jsonOfAtomRows(As:AtomList ; M:MixOp)
    => jsonList(jsonOfAtoms(As)), jsonOfAtomRows(M)

  rule jsonOfAtoms(.AtomList) => .JSONs
  rule jsonOfAtoms(A:Atom, As:AtomList) => A, jsonOfAtoms(As)

  rule jsonOfTyp(boolT()) => [ "boolT", .JSONs ]
  rule jsonOfTyp(natT())  => [ "natT", .JSONs ]
  rule jsonOfTyp(intT())  => [ "intT", .JSONs ]
  rule jsonOfTyp(textT()) => [ "textT", .JSONs ]
  rule jsonOfTyp(varT(X:Id, Ts:TArgList))
    => [ "varT", X, jsonList(jsonOfTargs(Ts)), .JSONs ]
  rule jsonOfTyp(tupT(Ts:TypList)) => [ "tupT", jsonList(jsonOfTyps(Ts)), .JSONs ]
  rule jsonOfTyp(iterT(T:Typ, quest())) => [ "iterT", jsonOfTyp(T), "?", .JSONs ]
  rule jsonOfTyp(iterT(T:Typ, star()))  => [ "iterT", jsonOfTyp(T), "*", .JSONs ]
  rule jsonOfTyp(funcT(Ps:TParamList, Ts:TypList, T:Typ))
    => [ "funcT", jsonList(jsonOfTParams(Ps)), jsonList(jsonOfTyps(Ts)), jsonOfTyp(T), .JSONs ]

  rule jsonOfTyps(.TypList) => .JSONs
  rule jsonOfTyps(T:Typ, Ts:TypList) => jsonOfTyp(T), jsonOfTyps(Ts)

  rule jsonOfTargs(.TArgList) => .JSONs
  rule jsonOfTargs(T:Typ, Ts:TArgList) => jsonOfTyp(T), jsonOfTargs(Ts)

  syntax JSONs ::= jsonOfTParams(TParamList)  [function, symbol(jsonOfTParams)]
  rule jsonOfTParams(.TParamList) => .JSONs
  rule jsonOfTParams(P:Id, Ps:TParamList) => P, jsonOfTParams(Ps)

  // ==========================================================================
  // Requests
  // ==========================================================================

  syntax JSON ::= jsonOfRequest(Id, TypList, ValList)
                                              [function, symbol(jsonOfRequest)]
  rule jsonOfRequest(X:Id, Ts:TypList, Vs:ValList)
    => { "builtin" : X,
         "targs" : jsonList(jsonOfTyps(Ts)),
         "args" : jsonList(jsonOfVals(Vs)),
         .JSONs }

  syntax JSON ::= jsonOfExternFuncRequest(Id, TypList, ValList)
                                    [function, symbol(jsonOfExternFuncRequest)]
  rule jsonOfExternFuncRequest(X:Id, Ts:TypList, Vs:ValList)
    => { "extern-func" : X,
         "targs" : jsonList(jsonOfTyps(Ts)),
         "args" : jsonList(jsonOfVals(Vs)),
         .JSONs }

  syntax JSON ::= jsonOfExternRelRequest(Id, ValList)
                                     [function, symbol(jsonOfExternRelRequest)]
  rule jsonOfExternRelRequest(X:Id, Vs:ValList)
    => { "extern-rel" : X,
         "args" : jsonList(jsonOfVals(Vs)),
         .JSONs }

  // ==========================================================================
  // Inbound: JSON -> K terms
  // ==========================================================================

  syntax Val ::= valOfJson(JSON)              [function, symbol(valOfJson)]
  syntax ValList ::= valsOfJsons(JSONs)       [function, symbol(valsOfJsons)]
  syntax ValFieldList ::= valFieldsOfJsons(JSONs)
                                              [function, symbol(valFieldsOfJsons)]
  syntax MixOp ::= mixOpOfJson(JSON)          [function, symbol(mixOpOfJson)]
  syntax MixOp ::= atomRowsOfJsons(JSONs)     [function, symbol(atomRowsOfJsons)]
  syntax AtomList ::= atomsOfJsons(JSONs)     [function, symbol(atomsOfJsons)]

  rule valOfJson([ "boolV", B:Bool, .JSONs ]) => boolV(B)
  rule valOfJson([ "natN", S:String, .JSONs ]) => natN(String2Int(S))
  rule valOfJson([ "intN", S:String, .JSONs ]) => intN(String2Int(S))
  rule valOfJson([ "textV", S:String, .JSONs ]) => textV(S)
  rule valOfJson([ "strV", [ Js:JSONs ], .JSONs ]) => strV(valFieldsOfJsons(Js))
  rule valOfJson([ "injV", Jm:JSON, [ Js:JSONs ], .JSONs ])
    => injV(valCase(mixOpOfJson(Jm), valsOfJsons(Js)))
  rule valOfJson([ "tupV", [ Js:JSONs ], .JSONs ]) => tupV(valsOfJsons(Js))
  rule valOfJson([ "optV", null, .JSONs ]) => optV(noVal())
  rule valOfJson([ "optV", J:JSON, .JSONs ]) => optV(someVal(valOfJson(J)))
    requires J =/=K null
  rule valOfJson([ "listV", [ Js:JSONs ], .JSONs ]) => listV(valsOfJsons(Js))
  rule valOfJson([ "funcV", X:String, .JSONs ]) => funcV(X)
  rule valOfJson([ "extV", J:JSON, .JSONs ]) => extV(J)

  rule valsOfJsons(.JSONs) => .ValList
  rule valsOfJsons(J:JSON, Js:JSONs) => valOfJson(J), valsOfJsons(Js)

  rule valFieldsOfJsons(.JSONs) => .ValFieldList
  rule valFieldsOfJsons([ A:String, J:JSON, .JSONs ], Js:JSONs)
    => valField(A, valOfJson(J)), valFieldsOfJsons(Js)

  rule mixOpOfJson([ Js:JSONs ]) => atomRowsOfJsons(Js)

  rule atomRowsOfJsons(.JSONs) => .MixOp
  rule atomRowsOfJsons([ Jas:JSONs ], Js:JSONs)
    => atomsOfJsons(Jas) ; atomRowsOfJsons(Js)

  rule atomsOfJsons(.JSONs) => .AtomList
  rule atomsOfJsons(A:String, Js:JSONs) => A, atomsOfJsons(Js)

  syntax Typ ::= typOfJson(JSON)              [function, symbol(typOfJson)]
  syntax TypList ::= typsOfJsons(JSONs)       [function, symbol(typsOfJsons)]
  syntax TArgList ::= targsOfJsons(JSONs)     [function, symbol(targsOfJsons)]
  syntax TParamList ::= tparamsOfJsons(JSONs) [function, symbol(tparamsOfJsons)]

  rule typOfJson([ "boolT", .JSONs ]) => boolT()
  rule typOfJson([ "natT", .JSONs ])  => natT()
  rule typOfJson([ "intT", .JSONs ])  => intT()
  rule typOfJson([ "textT", .JSONs ]) => textT()
  rule typOfJson([ "varT", X:String, [ Js:JSONs ], .JSONs ])
    => varT(X, targsOfJsons(Js))
  rule typOfJson([ "tupT", [ Js:JSONs ], .JSONs ]) => tupT(typsOfJsons(Js))
  rule typOfJson([ "iterT", J:JSON, "?", .JSONs ]) => iterT(typOfJson(J), quest())
  rule typOfJson([ "iterT", J:JSON, "*", .JSONs ]) => iterT(typOfJson(J), star())
  rule typOfJson([ "funcT", [ Ps:JSONs ], [ Ts:JSONs ], T:JSON, .JSONs ])
    => funcT(tparamsOfJsons(Ps), typsOfJsons(Ts), typOfJson(T))

  rule typsOfJsons(.JSONs) => .TypList
  rule typsOfJsons(J:JSON, Js:JSONs) => typOfJson(J), typsOfJsons(Js)

  rule targsOfJsons(.JSONs) => .TArgList
  rule targsOfJsons(J:JSON, Js:JSONs) => typOfJson(J), targsOfJsons(Js)

  rule tparamsOfJsons(.JSONs) => .TParamList
  rule tparamsOfJsons(X:String, Js:JSONs) => X, tparamsOfJsons(Js)

  // ==========================================================================
  // Responses
  // ==========================================================================

  syntax KItem ::= builtinResult(JSON)        [symbol(builtinResult)]
                 | externFuncResult(JSON)     [symbol(externFuncResult)]
                 | externRelResult(JSON)      [symbol(externRelResult)]

  rule <k> builtinResult({ "ok" : J:JSON, .JSONs }) => valOfJson(J) ... </k>
  rule <k> builtinResult({ "fail" : null, .JSONs }) => unmatch ... </k>
  rule <k> builtinResult({ "error" : S:String, .JSONs }) => hostError(S) ... </k>

  rule <k> externFuncResult({ "ok" : J:JSON, .JSONs }) => valOfJson(J) ... </k>
  rule <k> externFuncResult({ "fail" : null, .JSONs }) => unmatch ... </k>
  rule <k> externFuncResult({ "error" : S:String, .JSONs }) => hostError(S) ... </k>

  rule <k> externRelResult({ "ok" : [ Js:JSONs ], .JSONs }) => valsOfJsons(Js) ... </k>
  rule <k> externRelResult({ "fail" : null, .JSONs }) => unmatch ... </k>
  rule <k> externRelResult({ "error" : S:String, .JSONs }) => hostError(S) ... </k>

  // A host error is an execution error; the host keeps its report.
  syntax KItem ::= hostError(String)
  rule <k> hostError(_:String) => error ... </k>

  // ==========================================================================
  // The debug log
  // ==========================================================================
  rule <k> logDebug(V:Val) => #write(1, JSON2String(jsonOfVal(V)) +String "\n") ... </k>

endmodule
