// JSON codec for the external interface to the host interpreter.
//
// `Call_builtin_func` is an `extern relation` in watsup: its meaning lives in
// `p4spec/lib/interface/builtin/`, not in the meta-language.  Rather than
// mirror those implementations in K — which duplicates authority, and covers
// only the handful of builtins someone got round to porting — a builtin call
// is serialized here, handed to `spectec-boot builtin`, and its result read
// back.  The OCaml stays the single authority for what a builtin computes.
//
// The OCaml half of this codec is `p4spec/lib/interface/spectec/ali/extern.ml`,
// which documents the wire format in full.  In brief:
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
//           | ["iterT", typ, "?"|"*"] | ["funcT"]
//
//   request  ::= {"builtin": <id>, "targs": [typ, ...], "args": [val, ...]}
//   response ::= {"ok": val}
//
// Numbers cross as decimal *strings*, not JSON numbers: `natN`/`intN` hold
// arbitrary-precision integers on both sides (K's `Int`, OCaml's `Bigint.t`)
// and P4 routinely exceeds 64 bits.
//
// A `JSON` *term* is built and stringified once with `JSON2String`, rather
// than concatenating strings by hand: atoms are rendered with quotes (`':'`)
// and identifiers may contain characters that need escaping, and `JSON2String`
// already escapes correctly.
//
// None of these functions is `total`, deliberately.  A malformed response
// leaves a visible non-`Val` stuck in the `<k>` cell — which is what you want
// to read — rather than quietly rewriting to a wrong answer.  This matches the
// convention used for the partial helpers in 4.5-eval-call-func.k.

requires "json.md"
requires "4.4-eval-prem.k"

module AL-EXTERN-JSON
  imports AL-PREM
  imports JSON
  imports K-IO
  imports STRING
  imports INT

  // ==========================================================================
  // Outbound: K terms -> JSON
  //
  // `Id` and `Atom` are subsorts of `String` (common/1-syntax.k:34-35), and
  // `String` is a `JSON`, so they drop straight into a JSON string position
  // with no coercion.
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

  //   `[ Js ]` cannot be written with a `JSONs`-valued call directly inside
  //   it: the parser cannot tell that from a one-element list whose element is
  //   that call.  This wrapper pins the `JSONs` position.
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

  // `extV(Json)` gets no rule on purpose: `Json` is declared abstract
  // (common/1-syntax.k:159) and has no K-reachable inhabitants, so a value
  // that somehow reaches here should stick visibly rather than be invented.

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
  rule jsonOfTyp(varT(X:Id, Ts:TypList))
    => [ "varT", X, jsonList(jsonOfTyps(Ts)), .JSONs ]
  rule jsonOfTyp(tupT(Ts:TypList)) => [ "tupT", jsonList(jsonOfTyps(Ts)), .JSONs ]
  rule jsonOfTyp(iterT(T:Typ, quest())) => [ "iterT", jsonOfTyp(T), "?", .JSONs ]
  rule jsonOfTyp(iterT(T:Typ, star()))  => [ "iterT", jsonOfTyp(T), "*", .JSONs ]
  rule jsonOfTyp(funcT()) => [ "funcT", .JSONs ]

  rule jsonOfTyps(.TypList) => .JSONs
  rule jsonOfTyps(T:Typ, Ts:TypList) => jsonOfTyp(T), jsonOfTyps(Ts)

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

  // ==========================================================================
  // Inbound: JSON -> K terms
  //
  // Only the `val` half is needed: a response never carries a type.
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
  //   `null` is itself a `JSON`, so the two `optV` shapes overlap; an explicit
  //   side condition says which is which, rather than leaving it to `[owise]`.
  rule valOfJson([ "optV", J:JSON, .JSONs ]) => optV(someVal(valOfJson(J)))
    requires J =/=K null
  rule valOfJson([ "listV", [ Js:JSONs ], .JSONs ]) => listV(valsOfJsons(Js))
  rule valOfJson([ "funcV", X:String, .JSONs ]) => funcV(X)

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

  // ==========================================================================
  // Responses
  // ==========================================================================

  syntax JSON ::= respOk(JSON)                [function, symbol(respOk)]
  rule respOk({ "ok" : J:JSON, .JSONs }) => J

endmodule
