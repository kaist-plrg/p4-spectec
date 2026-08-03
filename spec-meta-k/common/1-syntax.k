// Common syntax of the P4-SpecTec meta-language.
//
// Mirrors spec-meta/common/1-syntax.watsup.
//
// This is an *abstract* syntax definition: every production is a constructor
// application (`ctor(Arg, ...)`), never a concrete-syntax pattern.  AL terms
// are produced by the elaborator, not parsed from text, so no grammar,
// precedence or disambiguation concerns arise here.  Each production carries an
// explicit `symbol(_)` so the KORE label of a constructor is stable and
// predictable for tools that emit AL terms directly.
//
// Watsup base types map to K builtins as follows:
//   bool -> Bool, nat -> Int, int -> Int, text -> String
// (`nat` is not given a separate sort; non-negativity is a side condition.)
//
// K has no sort aliases, only subsorts.  Watsup aliases that would otherwise
// force injections into every term (`targ = typ`, `tparam = id`) are therefore
// inlined, and noted where they occur.  `id` and `atom` are kept as separate
// subsorts of String, since collapsing them would erase a real distinction.

module COMMON-SYNTAX
  imports BOOL-SYNTAX
  imports INT-SYNTAX
  imports STRING-SYNTAX

  // ==========================================================================
  // Identifiers
  //
  //   syntax id = text
  //   syntax atom = text
  //   syntax mixop = atom**
  // ==========================================================================

  syntax Id ::= String
  syntax Atom ::= String

  syntax AtomList ::= List{Atom, ","}       [symbol(atomList), terminator-symbol(.atomList)]
  syntax MixOp ::= List{AtomList, ";"}      [symbol(mixop), terminator-symbol(.mixop)]

  // ==========================================================================
  // Types
  //
  //   syntax numtyp = | NAT | INT
  //   syntax optyp = | BOOL | numtyp | TEXT
  //   syntax typ = | optyp | VAR id targ* | TUP typ* | ITER typ iter | FUNC
  // ==========================================================================

  syntax NumTyp ::= natT()                  [symbol(natT)]
                  | intT()                  [symbol(intT)]

  syntax OpTyp ::= boolT()                  [symbol(boolT)]
                 | NumTyp
                 | textT()                  [symbol(textT)]

  syntax Typ ::= OpTyp
               | varT(Id, TypList)          [symbol(varT)]   // targ* = typ*
               | tupT(TypList)              [symbol(tupT)]
               | iterT(Typ, Iter)           [symbol(iterT)]
               | funcT()                    [symbol(funcT)]

  syntax TypList ::= List{Typ, ","}         [symbol(typList), terminator-symbol(.typList)]

  //   syntax deftyp = | ALIAS typ | STRUCT typfield* | VARIANT typcase*
  //   syntax typfield = atom typ
  //   syntax typcase = mixop typ*

  syntax DefTyp ::= aliasDT(Typ)            [symbol(aliasDT)]
                  | structDT(TypFieldList)  [symbol(structDT)]
                  | variantDT(TypCaseList)  [symbol(variantDT)]

  syntax TypField ::= typField(Atom, Typ)   [symbol(typField)]
  syntax TypCase ::= typCase(MixOp, TypList)
                                            [symbol(typCase)]

  syntax TypFieldList ::= List{TypField, ","}
                                            [symbol(typFieldList), terminator-symbol(.typFieldList)]
  syntax TypCaseList ::= List{TypCase, ","} [symbol(typCaseList), terminator-symbol(.typCaseList)]

  // ==========================================================================
  // Iterators
  //
  //   syntax iter = QUEST | STAR
  // ==========================================================================

  syntax Iter ::= quest()                   [symbol(quest)]
                | star()                    [symbol(star)]

  syntax IterList ::= List{Iter, ","}       [symbol(iterList), terminator-symbol(.iterList)]

  // ==========================================================================
  // Variables
  //
  //   syntax vari = id typ iter*
  // ==========================================================================

  syntax Vari ::= vari(Id, Typ, IterList)   [symbol(vari)]

  syntax VariList ::= List{Vari, ","}       [symbol(variList), terminator-symbol(.variList)]

  // ==========================================================================
  // Numbers
  //
  //   syntax num = | NAT nat | INT int
  // ==========================================================================

  syntax Num ::= natN(Int)                  [symbol(natN)]
               | intN(Int)                  [symbol(intN)]

  // ==========================================================================
  // Operators
  //
  //   syntax unop = boolunop | numunop
  //   syntax binop = boolbinop | numbinop
  //   syntax cmpop = polycmpop | numcmpop
  // ==========================================================================

  syntax BoolUnOp ::= notOp()               [symbol(notOp)]

  syntax BoolBinOp ::= andOp()              [symbol(andOp)]
                     | orOp()               [symbol(orOp)]
                     | implOp()             [symbol(implOp)]
                     | equivOp()            [symbol(equivOp)]

  syntax NumUnOp ::= plusOp()               [symbol(plusOp)]
                   | minusOp()              [symbol(minusOp)]

  syntax NumBinOp ::= addOp()               [symbol(addOp)]
                    | subOp()               [symbol(subOp)]
                    | mulOp()               [symbol(mulOp)]
                    | divOp()               [symbol(divOp)]
                    | modOp()               [symbol(modOp)]
                    | powOp()               [symbol(powOp)]

  syntax NumCmpOp ::= ltOp()                [symbol(ltOp)]
                    | gtOp()                [symbol(gtOp)]
                    | leOp()                [symbol(leOp)]
                    | geOp()                [symbol(geOp)]

  syntax PolyCmpOp ::= eqOp()               [symbol(eqOp)]
                     | neOp()               [symbol(neOp)]

  syntax UnOp ::= BoolUnOp | NumUnOp
  syntax BinOp ::= BoolBinOp | NumBinOp
  syntax CmpOp ::= PolyCmpOp | NumCmpOp

  // ==========================================================================
  // Values
  //
  //   extern syntax json
  //   syntax val =
  //     | BOOL bool | num | TEXT text | STR valfield* | INJ valcase
  //     | TUP val* | OPT val? | LIST val* | FUNC id | EXT json
  //   syntax valfield = atom val
  //   syntax valcase = mixop val*
  // ==========================================================================

  // `extern syntax json`: an opaque sort, left undefined on purpose.  Its
  // inhabitants come from outside the meta-language.
  syntax Json

  syntax Val ::= boolV(Bool)                [symbol(boolV)]
               | Num
               | textV(String)              [symbol(textV)]
               | strV(ValFieldList)         [symbol(strV)]
               | injV(ValCase)              [symbol(injV)]
               | tupV(ValList)              [symbol(tupV)]
               | optV(ValOpt)               [symbol(optV)]
               | listV(ValList)             [symbol(listV)]
               | funcV(Id)                  [symbol(funcV)]
               | extV(Json)                 [symbol(extV)]

  syntax ValField ::= valField(Atom, Val)   [symbol(valField)]
  syntax ValCase ::= valCase(MixOp, ValList)
                                            [symbol(valCase)]

  syntax ValList ::= List{Val, ","}         [symbol(valList), terminator-symbol(.valList)]
  syntax ValFieldList ::= List{ValField, ","}
                                            [symbol(valFieldList), terminator-symbol(.valFieldList)]

  // `val?`
  syntax ValOpt ::= noVal()                 [symbol(noVal)]
                  | someVal(Val)            [symbol(someVal)]

  // ==========================================================================
  // Expressions
  //
  //   syntax exp = | BOOL bool | num | TEXT text | VAR id | ...
  //   syntax expcase = mixop exp*
  //   syntax expfield = atom exp
  //   syntax iterexp = iter vari*
  // ==========================================================================

  syntax Exp ::= boolE(Bool)                [symbol(boolE)]
               | Num
               | textE(String)              [symbol(textE)]
               | varE(Id)                   [symbol(varE)]
               | unE(UnOp, Exp)             [symbol(unE)]
               | binE(BinOp, Exp, Exp)      [symbol(binE)]
               | cmpE(CmpOp, Exp, Exp)      [symbol(cmpE)]
               | upCastE(Typ, Exp)          [symbol(upCastE)]
               | downCastE(Typ, Exp)        [symbol(downCastE)]
               | subE(Exp, Typ)             [symbol(subE)]
               | matchE(Exp, Pattern)       [symbol(matchE)]
               | tupE(ExpList)              [symbol(tupE)]
               | injE(ExpCase)              [symbol(injE)]
               | strE(ExpFieldList)         [symbol(strE)]
               | optE(ExpOpt)               [symbol(optE)]
               | listE(ExpList)             [symbol(listE)]
               | consE(Exp, Exp)            [symbol(consE)]
               | catE(Exp, Exp)             [symbol(catE)]
               | memE(Exp, Exp)             [symbol(memE)]
               | lenE(Exp)                  [symbol(lenE)]
               | dotE(Exp, Atom)            [symbol(dotE)]
               | idxE(Exp, Exp)             [symbol(idxE)]
               | sliceE(Exp, Exp, Exp)      [symbol(sliceE)]
               | updE(Exp, Path, Exp)       [symbol(updE)]
               | callE(Id, TypList, ArgList)  // targ* = typ*
                                            [symbol(callE)]
               | iterE(Exp, IterExp)        [symbol(iterE)]

  syntax ExpCase ::= expCase(MixOp, ExpList)
                                            [symbol(expCase)]
  syntax ExpField ::= expField(Atom, Exp)   [symbol(expField)]
  syntax IterExp ::= iterExp(Iter, VariList)
                                            [symbol(iterExp)]

  syntax ExpList ::= List{Exp, ","}         [symbol(expList), terminator-symbol(.expList)]
  syntax ExpFieldList ::= List{ExpField, ","}
                                            [symbol(expFieldList), terminator-symbol(.expFieldList)]

  // `exp?`
  syntax ExpOpt ::= noExp()                 [symbol(noExp)]
                  | someExp(Exp)            [symbol(someExp)]

  // ==========================================================================
  // Patterns
  //
  //   syntax listpattern = | CONS | FIXED nat | NIL
  //   syntax optpattern = | SOME | NONE
  //   syntax pattern = | INJ mixop | listpattern | optpattern
  // ==========================================================================

  syntax ListPattern ::= consPat()          [symbol(consPat)]
                       | fixedPat(Int)      [symbol(fixedPat)]
                       | nilPat()           [symbol(nilPat)]

  syntax OptPattern ::= somePat()           [symbol(somePat)]
                      | nonePat()           [symbol(nonePat)]

  syntax Pattern ::= injPat(MixOp)          [symbol(injPat)]
                   | ListPattern
                   | OptPattern

  // ==========================================================================
  // Paths
  //
  //   syntax path = | ROOT | IDX path exp | SLICE path exp exp | DOT path atom
  // ==========================================================================

  syntax Path ::= rootPath()                [symbol(rootPath)]
                | idxPath(Path, Exp)        [symbol(idxPath)]
                | slicePath(Path, Exp, Exp) [symbol(slicePath)]
                | dotPath(Path, Atom)       [symbol(dotPath)]

  // ==========================================================================
  // Arguments
  //
  //   syntax targ = typ
  //   syntax arg = | EXP exp | FUN id
  //
  // `targ` is an alias of `typ`, so `targ*` is written `TypList` throughout.
  // ==========================================================================

  syntax Arg ::= expA(Exp)                  [symbol(expA)]
               | funA(Id)                   [symbol(funA)]

  syntax ArgList ::= List{Arg, ","}         [symbol(argList), terminator-symbol(.argList)]

  // ==========================================================================
  // Type parameters
  //
  //   syntax tparam = id
  //
  // `tparam` is an alias of `id`, so `tparam*` is a list of `Id`.
  // ==========================================================================

  syntax TParamList ::= List{Id, ","}       [symbol(tparamList), terminator-symbol(.tparamList)]

endmodule
