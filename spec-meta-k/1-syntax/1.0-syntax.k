// Syntax of AL, the algorithmic layer of the P4-SpecTec meta-language.
//
// Mirrors spec-meta/1-syntax/1.0-common.watsup and 1.2-al.watsup, with the values of
// spec-meta/2-runtime/2.0-value.watsup: a value is an expression here
// (`Exp ::= Val`), so values are declared beside the expressions.

requires "json.md"

module SYNTAX
  imports BOOL-SYNTAX
  imports INT-SYNTAX
  imports JSON-SYNTAX
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
  // Numbers
  //
  //   syntax num = | NAT nat | INT int
  // ==========================================================================

  syntax Num ::= natN(Int)                  [symbol(natN)]
               | intN(Int)                  [symbol(intN)]

  // ==========================================================================
  // Types
  //
  //   syntax numtyp = | NAT | INT
  //   syntax optyp = | BOOL | numtyp | TEXT
  //   syntax typ =
  //     | optyp | VAR id targ* | TUP typ* | ITER typ iter | FUNC tparam* typ* typ
  //   syntax deftyp = | ALIAS typ | STRUCT typField* | VARIANT typCase*
  //   syntax typField = atom typ
  //   syntax typCase = mixop typ*
  // ==========================================================================

  syntax NumTyp ::= natT()                  [symbol(natT)]
                  | intT()                  [symbol(intT)]

  syntax OpTyp ::= boolT()                  [symbol(boolT)]
                 | NumTyp
                 | textT()                  [symbol(textT)]

  syntax Typ ::= OpTyp
               | varT(Id, TArgList)         [symbol(varT)]
               | tupT(TypList)              [symbol(tupT)]
               | iterT(Typ, Iter)           [symbol(iterT)]
               | funcT(TParamList, TypList, Typ)
                                            [symbol(funcT)]

  syntax TypList ::= List{Typ, ","}         [symbol(typList), terminator-symbol(.typList)]

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
  // Operators
  //
  //   syntax unop = boolUnop | numUnop
  //   syntax binop = boolBinop | numBinop
  //   syntax cmpop = polyCmpop | numCmpop
  // ==========================================================================

  syntax BoolUnOp ::= notOp()               [symbol(notOp)]

  syntax NumUnOp ::= plusOp()               [symbol(plusOp)]
                   | minusOp()              [symbol(minusOp)]

  syntax UnOp ::= BoolUnOp | NumUnOp

  syntax BoolBinOp ::= andOp()              [symbol(andOp)]
                     | orOp()               [symbol(orOp)]
                     | implOp()             [symbol(implOp)]
                     | equivOp()            [symbol(equivOp)]

  syntax NumBinOp ::= addOp()               [symbol(addOp)]
                    | subOp()               [symbol(subOp)]
                    | mulOp()               [symbol(mulOp)]
                    | divOp()               [symbol(divOp)]
                    | modOp()               [symbol(modOp)]
                    | powOp()               [symbol(powOp)]

  syntax BinOp ::= BoolBinOp | NumBinOp

  syntax NumCmpOp ::= ltOp()                [symbol(ltOp)]
                    | gtOp()                [symbol(gtOp)]
                    | leOp()                [symbol(leOp)]
                    | geOp()                [symbol(geOp)]

  syntax PolyCmpOp ::= eqOp()               [symbol(eqOp)]
                     | neOp()               [symbol(neOp)]

  syntax CmpOp ::= PolyCmpOp | NumCmpOp

  // ==========================================================================
  // Patterns
  //
  //   syntax listPat = | CONS | FIXED nat | NIL
  //   syntax optPat = | SOME | NONE
  //   syntax pat = | INJ mixop | listPat | optPat
  // ==========================================================================

  syntax ListPat ::= consPat()              [symbol(consPat)]
                   | fixedPat(Int)          [symbol(fixedPat)]
                   | nilPat()               [symbol(nilPat)]

  syntax OptPat ::= somePat()               [symbol(somePat)]
                  | nonePat()               [symbol(nonePat)]

  syntax Pat ::= injPat(MixOp)              [symbol(injPat)]
               | ListPat
               | OptPat

  // ==========================================================================
  // Values (spec-meta/2-runtime/2.0-value.watsup)
  //
  //   extern syntax json
  //   syntax val =
  //     | BOOL bool | num | TEXT text | STR valField* | INJ valCase
  //     | TUP val* | OPT val? | LIST val* | FUNC id | EXT json
  //   syntax valField = atom val
  //   syntax valCase = mixop val*
  // ==========================================================================

  syntax Val ::= boolV(Bool)                [symbol(boolV)]
               | Num
               | textV(String)              [symbol(textV)]
               | strV(ValFieldList)         [symbol(strV), overload(str)]
               | injV(ValCase)              [symbol(injV), overload(inj)]
               | tupV(ValList)              [symbol(tupV), overload(tup)]
               | optV(ValOpt)               [symbol(optV), overload(opt)]
               | listV(ValList)             [symbol(listV), overload(list)]
               | funcV(Id)                  [symbol(funcV)]
               | extV(JSON)                 [symbol(extV)]

  syntax ValField ::= valField(Atom, Val)   [symbol(valField), overload(field)]
  syntax ValCase ::= valCase(MixOp, ValList)
                                            [symbol(valCase), overload(case)]

  syntax ValList ::= List{Val, ","}         [symbol(valList), terminator-symbol(.valList), overload(vals)]
  syntax ValFieldList ::= List{ValField, ","}
                                            [symbol(valFieldList), terminator-symbol(.valFieldList), overload(fields)]

  // `val?`
  syntax ValOpt ::= noVal()                 [symbol(noVal), overload(none)]
                  | someVal(Val)            [symbol(someVal), overload(some)]

  // ==========================================================================
  // Expressions
  //
  //   syntax exp = | BOOL bool | num | TEXT text | VAR id | ...
  //   syntax expCase = mixop exp*
  //   syntax expField = atom exp
  //   syntax iterExp = iter vari*
  //
  // A value is an expression, so that an evaluated subexpression can take
  // its place; the strictness attributes give the evaluation order. An
  // expression constructor, list, field or case whose parts are all values
  // is the value constructor, list, field or case (`overload`).
  // ==========================================================================

  syntax Exp ::= Val
               | boolE(Bool)                [symbol(boolE)]
               | textE(String)              [symbol(textE)]
               | varE(Id)                   [symbol(varE)]
               | unE(UnOp, Exp)             [symbol(unE), strict(2)]
               | binE(BinOp, Exp, Exp)      [symbol(binE), seqstrict(2, 3)]
               | cmpE(CmpOp, Exp, Exp)      [symbol(cmpE), seqstrict(2, 3)]
               | upCastE(Typ, Exp)          [symbol(upCastE), strict(2)]
               | downCastE(Typ, Exp)        [symbol(downCastE), strict(2)]
               | subE(Exp, Typ)             [symbol(subE), strict(1)]
               | matchE(Exp, Pat)           [symbol(matchE), strict(1)]
               | tupE(ExpList)              [symbol(tupE), strict, overload(tup)]
               | injE(ExpCase)              [symbol(injE), strict, overload(inj)]
               | strE(ExpFieldList)         [symbol(strE), strict, overload(str)]
               | optE(ExpOpt)               [symbol(optE), strict, result(ValOpt), overload(opt)]
               | listE(ExpList)             [symbol(listE), strict, overload(list)]
               | consE(Exp, Exp)            [symbol(consE), seqstrict]
               | catE(Exp, Exp)             [symbol(catE), seqstrict]
               | memE(Exp, Exp)             [symbol(memE), seqstrict]
               | lenE(Exp)                  [symbol(lenE), strict]
               | dotE(Exp, Atom)            [symbol(dotE), strict(1)]
               | idxE(Exp, Exp)             [symbol(idxE), seqstrict]
               | sliceE(Exp, Exp, Exp)      [symbol(sliceE), seqstrict]
               | updE(Exp, Path, Exp)       [symbol(updE), seqstrict(1, 3)]
               | callE(Id, TArgList, ArgList)
                                            [symbol(callE), strict(3)]
               | iterE(Exp, IterExp)        [symbol(iterE)]

  syntax ExpCase ::= ValCase
                   | expCase(MixOp, ExpList)
                                            [symbol(expCase), strict(2), overload(case)]
  syntax ExpField ::= ValField
                    | expField(Atom, Exp)   [symbol(expField), strict(2), overload(field)]
  syntax IterExp ::= iterExp(Iter, VariList)
                                            [symbol(iterExp)]

  syntax ExpList ::= ValList
                   | List{Exp, ","}         [symbol(expList), terminator-symbol(.expList), seqstrict, overload(vals)]
  syntax ExpFieldList ::= ValFieldList
                        | List{ExpField, ","}
                                            [symbol(expFieldList), terminator-symbol(.expFieldList), seqstrict, overload(fields)]

  // `exp?`
  syntax ExpOpt ::= ValOpt
                  | noExp()                 [symbol(noExp), overload(none)]
                  | someExp(Exp)            [symbol(someExp), strict, overload(some)]

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
  // ==========================================================================

  syntax TArg ::= Typ
  syntax TArgList ::= List{TArg, ","}       [symbol(targList), terminator-symbol(.targList)]

  syntax Arg ::= Val
               | expA(Exp)                  [symbol(expA), strict]
               | funA(Id)                   [symbol(funA)]

  syntax ArgList ::= ValList
                   | List{Arg, ","}         [symbol(argList), terminator-symbol(.argList), seqstrict, overload(vals)]

  // ==========================================================================
  // Type parameters
  //
  //   syntax tparam = id
  // ==========================================================================

  syntax TParam ::= Id
  syntax TParamList ::= List{TParam, ","}   [symbol(tparamList), terminator-symbol(.tparamList)]

  // ==========================================================================
  // Parameters
  //
  //   syntax param =
  //     | EXP typ
  //     | FUN id ':' tparam* param* '->' typ
  // ==========================================================================

  syntax Param ::= expParam(Typ)            [symbol(expParam)]
                 | funParam(Id, TParamList, ParamList, Typ)
                                            [symbol(funParam)]

  syntax ParamList ::= List{Param, ","}     [symbol(paramList), terminator-symbol(.paramList)]

  // ==========================================================================
  // Premises
  //
  //   syntax prem =
  //     | REL id ':' exp* '->' exp*
  //     | IF exp
  //     | IFHOLD id ':' exp*
  //     | IFNOTHOLD id ':' exp*
  //     | LET exp '=' exp
  //     | ITER prem iterPrem
  //     | DEBUG exp
  //   syntax iterPrem = iter vari* vari*
  // ==========================================================================

  syntax Prem ::= relPr(Id, ExpList, ExpList)
                                            [symbol(relPr), strict(2)]
                | ifPr(Exp)                 [symbol(ifPr), strict]
                | ifHoldPr(Id, ExpList)     [symbol(ifHoldPr), strict(2)]
                | ifNotHoldPr(Id, ExpList)  [symbol(ifNotHoldPr), strict(2)]
                | letPr(Exp, Exp)           [symbol(letPr), strict(2)]
                | iterPr(Prem, IterPrem)    [symbol(iterPr)]
                | debugPr(Exp)              [symbol(debugPr), strict]

  syntax IterPrem ::= iterPrem(Iter, VariList, VariList)
                                            [symbol(iterPrem)]

  syntax PremList ::= List{Prem, ","}       [symbol(premList), terminator-symbol(.premList)]

  // ==========================================================================
  // Type definitions
  //
  //   syntax externTypDefn = EXTTYP id
  //   syntax definedTypDefn = TYP id tparam* '=' deftyp
  //   syntax typDefn = | externTypDefn | definedTypDefn
  // ==========================================================================

  syntax ExternTypDefn ::= extTypD(Id)      [symbol(extTypD)]
  syntax DefinedTypDefn ::= typD(Id, TParamList, DefTyp)
                                            [symbol(typD)]
  syntax TypDefn ::= ExternTypDefn | DefinedTypDefn

  // ==========================================================================
  // Relation definitions
  //
  //   syntax externRelDefn = EXTREL id ':' typ* '->' typ*
  //   syntax rulMatch = exp* '-' prem*
  //   syntax rulPath = id '=' exp* '-' prem*
  //   syntax rulGroup = id ':' rulMatch '=' rulPath*
  //   syntax elsGroup = id ':' rulMatch '=' rulPath
  //   syntax definedRelDefn = REL id ':' typ* '->' typ* '=' rulGroup* elsGroup?
  //   syntax relDefn = | externRelDefn | definedRelDefn
  // ==========================================================================

  syntax ExternRelDefn ::= extRelD(Id, TypList, TypList)
                                            [symbol(extRelD)]

  syntax RulMatch ::= rulMatch(ExpList, PremList)
                                            [symbol(rulMatch)]
  syntax RulPath ::= rulPath(Id, ExpList, PremList)
                                            [symbol(rulPath)]
  syntax RulGroup ::= rulGroup(Id, RulMatch, RulPathList)
                                            [symbol(rulGroup)]
  syntax ElsGroup ::= elsGroup(Id, RulMatch, RulPath)
                                            [symbol(elsGroup)]

  syntax RulPathList ::= List{RulPath, ","} [symbol(rulPathList), terminator-symbol(.rulPathList)]
  syntax RulGroupList ::= List{RulGroup, ","}
                                            [symbol(rulGroupList), terminator-symbol(.rulGroupList)]

  // `elsGroup?`
  syntax ElsGroupOpt ::= noElsGroup()       [symbol(noElsGroup)]
                       | someElsGroup(ElsGroup)
                                            [symbol(someElsGroup)]

  syntax DefinedRelDefn ::= relD(Id, TypList, TypList, RulGroupList, ElsGroupOpt)
                                            [symbol(relD)]
  syntax RelDefn ::= ExternRelDefn | DefinedRelDefn

  // ==========================================================================
  // Meta-functions
  //
  //   syntax externFuncDefn = EXTFUNC id tparam* param* ':' typ
  //   syntax builtinFuncDefn = BUILTINFUNC id tparam* param* ':' typ
  //   syntax tblRow = arg* '=' exp '-' prem*
  //   syntax tableFuncDefn = TABLEFUNC id param* ':' typ '=' tblRow*
  //   syntax clause = arg* '=' exp '-' prem*
  //   syntax elsClause = clause
  //   syntax definedFuncDefn = FUNC id tparam* param* ':' typ '=' clause* elsClause?
  //   syntax funcDefn =
  //     | externFuncDefn | builtinFuncDefn | tableFuncDefn | definedFuncDefn
  // ==========================================================================

  syntax ExternFuncDefn ::= extFuncD(Id, TParamList, ParamList, Typ)
                                            [symbol(extFuncD)]
  syntax BuiltinFuncDefn ::= builtinFuncD(Id, TParamList, ParamList, Typ)
                                            [symbol(builtinFuncD)]

  syntax TblRow ::= tblRow(ArgList, Exp, PremList)
                                            [symbol(tblRow)]
  syntax TblRowList ::= List{TblRow, ","}   [symbol(tblRowList), terminator-symbol(.tblRowList)]

  syntax TableFuncDefn ::= tableFuncD(Id, ParamList, Typ, TblRowList)
                                            [symbol(tableFuncD)]

  syntax Clause ::= clause(ArgList, Exp, PremList)
                                            [symbol(clause)]
  syntax ClauseList ::= List{Clause, ","}   [symbol(clauseList), terminator-symbol(.clauseList)]

  // `elsClause?`; `elsClause` is an alias of `clause`, so it is inlined.
  syntax ElsClauseOpt ::= noElsClause()     [symbol(noElsClause)]
                        | someElsClause(Clause)
                                            [symbol(someElsClause)]

  syntax DefinedFuncDefn ::= funcD(Id, TParamList, ParamList, Typ, ClauseList, ElsClauseOpt)
                                            [symbol(funcD)]
  syntax FuncDefn ::= ExternFuncDefn
                    | BuiltinFuncDefn
                    | TableFuncDefn
                    | DefinedFuncDefn

  // ==========================================================================
  // Definitions
  //
  //   syntax defn = | typDefn | relDefn | funcDefn
  // ==========================================================================

  syntax Defn ::= TypDefn | RelDefn | FuncDefn

  // ==========================================================================
  // Spec
  //
  //   syntax spec = defn*
  // ==========================================================================

  syntax Spec ::= List{Defn, ","}           [symbol(spec), terminator-symbol(.spec)]

endmodule
