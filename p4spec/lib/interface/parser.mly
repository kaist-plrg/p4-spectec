%{
  open Il.Ast
  open Context
  open Wrap
  open Extract

  let declare_var_of_il (v: value) (b: bool) : unit =
    let id = id_of_name v in
    declare_var id b

  let declare_vars_of_il (v: value) : unit =
    match v.it with
    | ListV vl -> List.iter (fun v -> declare_var_of_il v false) vl
    | _ ->
        failwith
          (Printf.sprintf "@declare_vars_of_il: expected list of names, got %s"
             (Il.Print.string_of_value v))

  let declare_type_of_il (v: value) (b: bool) : unit =
    let id = id_of_name v in
    declare_type id b

  let declare_types_of_il (v: value) : unit =
    match v.it with
    | ListV vl -> List.iter (fun v -> declare_type_of_il v false) vl
    | _ ->
        failwith
          (Printf.sprintf "@declare_types_of_il: expected list of names, got %s"
             (Il.Print.string_of_value v))
%}

(**************************** TOKENS ******************************)
%token<Source.info> END
%token TYPENAME IDENTIFIER
%token<Il.Ast.value> NAME STRING_LITERAL
%token<Il.Ast.value * string> NUMBER_INT NUMBER
%token<Source.info> LE GE SHL AND OR NE EQ
%token<Source.info> PLUS MINUS PLUS_SAT MINUS_SAT MUL INVALID DIV MOD
%token<Source.info> BIT_OR BIT_AND BIT_XOR COMPLEMENT
%token<Source.info> L_BRACKET R_BRACKET L_BRACE R_BRACE L_ANGLE L_ANGLE_ARGS R_ANGLE R_ANGLE_SHIFT L_PAREN R_PAREN
%token<Source.info> ASSIGN COLON COMMA QUESTION DOT NOT SEMICOLON
%token<Source.info> AT PLUSPLUS
%token<Il.Ast.value> DONTCARE
%token<Source.info> MASK DOTS RANGE
%token<Il.Ast.value> TRUE FALSE
%token<Il.Ast.value> ABSTRACT ACTION ACTIONS APPLY BOOL BIT CONST CONTROL DEFAULT
%token<Il.Ast.value> ELSE ENTRIES ENUM ERROR EXIT EXTERN HEADER HEADER_UNION IF IN INOUT FOR
%token<Il.Ast.value> INT KEY LIST SELECT MATCH_KIND OUT PACKAGE PARSER PRIORITY RETURN STATE STRING STRUCT
%token<Il.Ast.value> SWITCH TABLE THIS TRANSITION TUPLE TYPEDEF TYPE VALUE_SET VARBIT VOID
%token<Il.Ast.value> PRAGMA
%token<Source.info> PRAGMA_END
%token<Il.Ast.value> UNEXPECTED_TOKEN

(**************************** PRIORITY AND ASSOCIATIVITY ******************************)
%right THEN ELSE
%nonassoc QUESTION
%nonassoc COLON
%left OR
%left AND
%left EQ NE
%left L_ANGLE R_ANGLE LE GE
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left SHL R_ANGLE_SHIFT
%left PLUSPLUS PLUS MINUS PLUS_SAT MINUS_SAT
%left MUL DIV MOD
%right PREFIX
%nonassoc L_PAREN L_BRACKET L_ANGLE_ARGS
%left DOT

%start <Il.Ast.value> p4program
%%

(**************************** CONTEXTS ******************************)
push_scope:
  | (* empty *)
    { push_scope() }
;
push_name:
  | n = name
   { push_scope();
     declare_type_of_il n false;
     n }
;
push_externName:
  | n = externName
    { push_scope();
      declare_type_of_il n false;
      n }
;
pop_scope:
  | (* empty *)
    { pop_scope() }
;
go_toplevel:
  | (* empty *)
    { go_toplevel () }
;
go_local:
  | (* empty *)
    { go_local () }
;
toplevel(X):
  | go_toplevel x = X go_local
    { x }
;

(**************************** P4-16 GRAMMAR ******************************)
(* Aux *)
externName:
	| n = nonTypeName
		{ declare_type_of_il n false;
      n }
;
int:
	| int = NUMBER_INT
    { fst int }
;

%inline r_angle:
	| info_r = R_ANGLE
    { info_r }
	| info_r = R_ANGLE_SHIFT
    { info_r }
;
%inline l_angle:
	| info_r = L_ANGLE
    { info_r }
	| info_r = L_ANGLE_ARGS
    { info_r }
;

(* Misc *)
trailingCommaOpt:
	| (* empty *)
	| COMMA
    { [ Term "" ] #@ "trailingCommaOpt" }
;

(* Numbers *)
number:
	| int = int
    { [ Term "INT"; NT int ] #@ "number" }
(* Processed by lexer *)
	| number = NUMBER
    { fst number }
;

(* Strings *)
stringLiteral:
  | text = STRING_LITERAL { text }
;

(* Names *)
identifier:
  | text = NAME IDENTIFIER
    { text |> Util.Source.it |> with_typ (wrap_var_t "name") }
;

typeIdentifier:
  | text = NAME TYPENAME
    { text |> Util.Source.it |> with_typ (wrap_var_t "name") }
;

(* >> Non-type names *)
nonTypeName:
	| n = identifier
    { n }
	| t = APPLY
	| t = KEY
	| t = ACTIONS
	| t = STATE
	| t = ENTRIES
	| t = TYPE
	| t = PRIORITY
    { t |> Util.Source.it |> with_typ (wrap_var_t "name") }
;

prefixedNonTypeName:
  | n = nonTypeName
    { [ Term "CURRENT"; NT n ] #@ "prefixedName" }
	| DOT go_toplevel n = nonTypeName go_local
    { [ Term "TOP"; NT n ] #@ "prefixedName" }
;

(* >> Type names *)
typeName:
	| n = typeIdentifier
    { n }
;

prefixedTypeName:
	| n = typeName
    { [ Term "CURRENT"; NT n ] #@ "prefixedName" }
	| DOT go_toplevel n = typeName go_local
		{ [ Term "TOP"; NT n ] #@ "prefixedName" }
;

(* >> Table custom property names *)
tableCustomName:
	| n = identifier
	| n = typeIdentifier
    { n }
	| t = APPLY
	| t = STATE
	| t = TYPE
	| t = PRIORITY
    { t |> Util.Source.it |> with_typ (wrap_var_t "name") }
;

(* >> Names *)
name:
	| n = nonTypeName
	| n = typeName
    { n }
	| t = LIST
    { t |> Util.Source.it |> with_typ (wrap_var_t "name") }
;

nameList_:
  | n = name
    { [ n ] }
	| nl = nameList_ COMMA n = name
    { nl @ [ n ] }
;

%inline nameList:
  | nl = nameList_
    { nl |> wrap_list_v "name" }
;

member:
	| n = name
    { n }
;

(* Directions *)
direction:
	| (* empty *) { [ Term "NO" ] #@ "direction" }
	| IN { [ Term "IN" ] #@ "direction" }
	| OUT { [ Term "OUT" ] #@ "direction" }
	| INOUT { [ Term "INOUT" ] #@ "direction" }
;

(* Types *)
(* >> Base types *)
baseType:
	| BOOL
    { [ Term "BoolT" ] #@ "baseType" }
	| MATCH_KIND
    { [ Term "MatchKindT" ] #@ "baseType" }
	| ERROR
    { [ Term "ErrT" ] #@ "baseType" }
  | BIT
    { let w = NumV (`Int (Bigint.of_int 1)) |> with_typ (NumT `IntT) in
      let n = [ Term "INT"; NT w ] #@ "number" in
      let e = [ Term "NumE"; NT n ] #@ "literalExpression" in
      [ Term "FBitT"; NT e ] #@ "baseType" }
	| STRING
    { [ Term "StrT" ] #@ "baseType"}
	| INT
    { [ Term "IntT" ] #@ "baseType" }
	| BIT l_angle v = int r_angle
    { let n = [ Term "INT"; NT v ] #@ "number" in
      let e = [ Term "NumE"; NT n ] #@ "literalExpression" in
      [ Term "FBitT"; NT e ] #@ "baseType" }
	| INT l_angle v = int r_angle
    { let n = [ Term "INT"; NT v ] #@ "number" in
      let e = [ Term "NumE"; NT n ] #@ "literalExpression" in
      [ Term "FIntT"; NT e ] #@ "baseType" }
	| VARBIT l_angle v = int r_angle
    { let n = [ Term "INT"; NT v ] #@ "number" in
      let e = [ Term "NumE"; NT n ] #@ "literalExpression" in
      [ Term "VBitT"; NT e ] #@ "baseType" }
	| BIT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ Term "FBitT"; NT e ] #@ "baseType" }
	| INT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ Term "FIntT"; NT e ] #@ "baseType" }
	| VARBIT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ Term "VBitT"; NT e ] #@ "baseType" }
;

(* >> Named types *)
nameType:
  | n = prefixedTypeName
    { [ Term "NameT"; NT n ] #@ "nameType" }

specializedType:
  | n = prefixedTypeName l_angle tas = typeArgumentList r_angle
    { [ Term "SpecT"; NT n; NT tas ] #@ "specializedType" }
;

namedType:
  | t = nameType
  | t = specializedType
    { t #@@ "namedType" }
;

(* >> Header stack types *)
headerStackType:
  | t = namedType L_BRACKET e = expression R_BRACKET
    { [ Term "HeaderStackT"; NT t; NT e ] #@ "headerStackType" }
;

(* >> List types *)
listType:
  | LIST l_angle targ = typeArgument r_angle
    { [ Term "ListT"; NT targ ] #@ "listType" }
;

(* >> Tuple types *)
tupleType:
	| TUPLE l_angle targs = typeArgumentList r_angle
    { [ Term "TupleT"; NT targs ] #@ "tupleType" }
;

(* >> Types *)
typeRef:
	| t = baseType
	| t = namedType
	| t = headerStackType
	| t = listType
	| t = tupleType
    { t #@@ "type" }
;

typeOrVoid:
	| t = typeRef { t #@@ "typeOrVoid" }
	| VOID { [ Term "VoidT" ] #@ "typeOrVoid" }
  (* From Petr4: HACK for generic return type *)
	| id = identifier
    { let n = [ Term "CURRENT"; NT id ] #@ "prefixedName" in
      [ Term "NameT"; NT n ] #@ "nameType" #@@ "typeOrVoid" }
;

(* Type parameters *)
typeParameter:
	| n = name { n }

typeParameterList_:
  | tp = typeParameter
    { [ tp ] }
	| tpl = typeParameterList_ COMMA tp = typeParameter
    { tpl @ [ tp ] }
;

%inline typeParameterList:
  | tpl = typeParameterList_
    { tpl |> wrap_list_v "typeParameter" }
;

typeParameterListOpt:
	| (* empty *)
    { [] |> wrap_list_v "typeParameter" }
	| l_angle tpl = typeParameterList r_angle
    { declare_types_of_il tpl;
      tpl }
;

(* Parameters *)
parameter:
	| _al = annotationList dir = direction t = typeRef n = name iopt = initializerOpt
		{ declare_var_of_il n false;
      [ NT dir; NT t; NT n; NT iopt ] #@ "parameter" }
;

nonEmptyParameterList_:
  | p = parameter
    { [ p ] }
	| pl = nonEmptyParameterList_ COMMA p = parameter
    { pl @ [ p ] }
;

nonEmptyParameterList:
  | pl = nonEmptyParameterList_
    { pl |> wrap_list_v "parameter" }
;

%inline parameterList:
	| (* empty *)
    { [] |> wrap_list_v "parameter" }
	| pl = nonEmptyParameterList
    { pl }
;

(* Constructor parameters *)
constructorParameterListOpt:
	| (* empty *)
    { [] |> wrap_list_v "constructorParameter" }
	| L_PAREN pl = parameterList R_PAREN
    { pl }
;

(* Expression key-value pairs *)
namedExpression:
	| n = name ASSIGN e = expression
    { [ NT n; NT e ] #@ "namedExpression" }
;

namedExpressionList_:
	| e = namedExpression
    { [ e ] }
	| es = namedExpressionList_ COMMA e = namedExpression
    { es @ [ e ] }
;

%inline namedExpressionList:
  | nel = namedExpressionList_
    { nel |> wrap_list_v "namedExpression" }
;

(* Expressions *)
(* >> Literal expressions *)
%inline literalExpression:
  | TRUE
    { let b = BoolV true |> with_typ BoolT in
      [ Term "BoolE"; NT b  ] #@ "literalExpression" }
	| FALSE
    { let b = BoolV false |> with_typ BoolT in
      [ Term "BoolE"; NT b  ] #@ "literalExpression" }
	| num = number
    { [ Term "NumE"; NT num ] #@ "literalExpression" }
	| str = stringLiteral
    { [ Term "StrE"; NT str ] #@ "literalExpression" }
;

(* >> Reference expressions *)
%inline referenceExpression:
	| n = prefixedNonTypeName
    { [ Term "NameE"; NT n ] #@ "referenceExpression" }
	| n = THIS
    { let n = [ Term "CURRENT"; NT n ] #@ "prefixedName" in
      [ Term "NameE"; NT n ] #@ "referenceExpression" }
;

(* >> Default expressions *)
%inline defaultExpression:
	| DOTS
    { [ Term "DefaultE" ] #@ "defaultExpression" }
;

(* >> Unary, binary, and ternary expressions *)
%inline unop: 
	| NOT { [ Term "LNOT" ] #@ "unop" }
	| COMPLEMENT { [ Term "BNOT" ] #@ "unop" }
	| MINUS { [ Term "UMINUS" ] #@ "unop" }
	| PLUS { [ Term "UPLUS" ] #@ "unop" }
;

%inline unaryExpression:
	| o = unop e = expression %prec PREFIX
    { [ Term "UnE"; NT o; NT e ] #@ "unaryExpression" }
;

%inline binop:
  | MUL { [ Term "MUL" ] #@ "binop" }
  | DIV { [ Term "DIV" ] #@ "binop" }
  | MOD { [ Term "MOD" ] #@ "binop" }
  | PLUS { [ Term "PLUS" ] #@ "binop" }
  | PLUS_SAT { [ Term "SPLUS" ] #@ "binop" }
  | MINUS { [ Term "MINUS" ] #@ "binop" }
  | MINUS_SAT { [ Term "SMINUS" ] #@ "binop" }
  | SHL { [ Term "SHL" ] #@ "binop" }
  | r_angle R_ANGLE_SHIFT { [ Term "SHR" ] #@ "binop" }
  | LE { [ Term "LE" ] #@ "binop" }
  | GE { [ Term "GE" ] #@ "binop" }
  | l_angle { [ Term "LT" ] #@ "binop" }
  | r_angle { [ Term "GT" ] #@ "binop" }
  | NE { [ Term "NE" ] #@ "binop" }
  | EQ { [ Term "EQ" ] #@ "binop" }
  | BIT_AND { [ Term "BAND" ] #@ "binop" }
  | BIT_XOR { [ Term "BXOR" ] #@ "binop" }
  | BIT_OR { [ Term "BOR" ] #@ "binop" }
  | PLUSPLUS { [ Term "CONCAT" ] #@ "binop" }
  | AND { [ Term "LAND" ] #@ "binop" }
  | OR { [ Term "LOR" ] #@ "binop" }
;

%inline binaryExpression:
	| l = expression o = binop r = expression
    { [ Term "BinE"; NT l; NT o; NT r ] #@ "binaryExpression" }
;

%inline binaryExpressionNonBrace:
	| l = expressionNonBrace o = binop r = expression
    { [ Term "BinE"; NT l; NT o; NT r ] #@ "binaryExpression" }
;

%inline ternaryExpression:
	| c = expression QUESTION t = expression COLON f = expression
    { [ Term "TernE"; NT c; NT t; NT f ] #@ "ternaryExpression" }
;

%inline ternaryExpressionNonBrace:
	| c = expressionNonBrace QUESTION t = expression COLON f = expression
    { [ Term "TernE"; NT c; NT t; NT f ] #@ "ternaryExpression" }
;

(* >> Cast expressions *)
%inline castExpression:
	| L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
    { [ Term "CastE"; NT t; NT e ] #@ "castExpression" }
;

(* >> Data (aggregate) expressions *)
%inline dataExpression:
	| INVALID { [ Term "InvalidE" ] #@ "dataExpression" }
	| L_BRACE e = dataElementExpression _tcopt = trailingCommaOpt R_BRACE
    { e }
;

(* >> Member and index access expressions *)
%inline errorAccessExpression:
	| ERROR DOT m = member
		{ [ Term "ErrAccE"; NT m ] #@ "errorAccessExpression" }
;

%inline memberAccessExpression:
  | n = prefixedTypeName DOT m = member
    { [ Term "TypeAccE"; NT n; NT m ] #@ "memberAccessExpression" }
	| e = expression DOT m = member
    { [ Term "ExprAccE"; NT e; NT m ] #@ "memberAccessExpression" }
;

%inline indexAccessExpression:
	| a = expression L_BRACKET i = expression R_BRACKET
    { [ Term "ArrAccE"; NT a; NT i ] #@ "indexAccessExpression" }
	| a = expression L_BRACKET h = expression COLON l = expression R_BRACKET
    { [ Term "BitAccE"; NT a; NT h; NT l ] #@ "indexAccessExpression" }
;

%inline accessExpression:
	| e = errorAccessExpression
	| e = memberAccessExpression
	| e = indexAccessExpression
		{ e #@@ "accessExpression" }
;

%inline memberAccessExpressionNonBrace:
  | n = prefixedTypeName DOT m = member
    { [ Term "TypeAccE"; NT n; NT m ] #@ "memberAccessExpression" }
	| e = expressionNonBrace DOT m = member
    { [ Term "ExprAccE"; NT e; NT m ] #@ "memberAccessExpression" }
;

%inline indexAccessExpressionNonBrace:
	| a = expressionNonBrace L_BRACKET i = expression R_BRACKET
    { [ Term "ArrAccE"; NT a; NT i ] #@ "indexAccessExpression" }
	| a = expressionNonBrace L_BRACKET h = expression COLON l = expression R_BRACKET
    { [ Term "BitAccE"; NT a; NT h; NT l ] #@ "indexAccessExpression" }
;

%inline accessExpressionNonBrace:
	| e = errorAccessExpression
	| e = memberAccessExpressionNonBrace
	| e = indexAccessExpressionNonBrace
		{ e #@@ "accessExpression" }
;

(* >> Call expressions *)
%inline routineTarget:
  | e = expression { e }
;

%inline callExpression:
  | n = namedType L_PAREN args = argumentList R_PAREN
    { [ Term "InstE"; NT n; NT args ] #@ "callExpression" }
	| t = routineTarget L_PAREN args = argumentList R_PAREN
    { let targs = [] |> wrap_list_v "typeArgument" in
      [ Term "CallE"; NT t; NT targs; NT args ] #@ "callExpression" }
	| t = routineTarget l_angle targs = realTypeArgumentList r_angle
    L_PAREN args = argumentList R_PAREN
    { [ Term "CallE"; NT t; NT targs; NT args ] #@ "callExpression" }
;

%inline routineTargetNonBrace:
  | e = expressionNonBrace { e }
;

%inline callExpressionNonBrace:
  | n = namedType L_PAREN args = argumentList R_PAREN
    { [ Term "InstE"; NT n; NT args ] #@ "callExpression" }
  | t = routineTargetNonBrace L_PAREN args = argumentList R_PAREN
    { let targs = [] |> wrap_list_v "typeArgument" in
      [ Term "CallE"; NT t; NT targs; NT args ] #@ "callExpression" }
  | t = routineTargetNonBrace l_angle targs = realTypeArgumentList r_angle
    L_PAREN args = argumentList R_PAREN
    { [ Term "CallE"; NT t; NT targs; NT args ] #@ "callExpression" }

(* >> Parenthesized Expressions *)

%inline parenthesizedExpression:
	| L_PAREN e = expression R_PAREN
		{ e }
;

(* >> Expressions *)
expression:
	| e = literalExpression
	| e = referenceExpression
	| e = defaultExpression
	| e = unaryExpression
	| e = binaryExpression
	| e = ternaryExpression
	| e = castExpression
	| e = dataExpression
	| e = accessExpression
	| e = callExpression
	| e = parenthesizedExpression
		{ e #@@ "expression" }
;

expressionList_:
	| (* empty *)
    { [] }
	| e = expression
    { [ e ] }
	| es = expressionList_ COMMA e = expression
		{ es @ [ e ] }
;

%inline expressionList:
  | es = expressionList_ { es |> wrap_list_v "expression" }
;

%inline sequenceElementExpression:
	| el = expressionList
    { [ Term "SeqE"; NT el ] #@ "dataExpression" }
;

%inline recordElementExpression:
  | n = name ASSIGN e = expression
    { let ne = [ NT n; NT e ] #@ "namedExpression" in
      let nel = [ ne ] |> wrap_list_v "namedExpression" in
      [ Term "RecordE"; NT nel ] #@ "dataExpression" }
  | n = name ASSIGN e = expression COMMA DOTS
    { let ne = [ NT n; NT e ] #@ "namedExpression" in
      let nel = [ ne ] |> wrap_list_v "namedExpression" in
      [ Term "RecordDefaultE"; NT nel ] #@ "dataExpression" }
	| n = name ASSIGN e = expression COMMA nel = namedExpressionList_
    { let ne = [ NT n; NT e ] #@ "namedExpression" in 
      let nel = ne :: nel |> wrap_list_v "namedExpression" in
      [ Term "RecordE"; NT nel ] #@ "dataExpression" }
  | n = name ASSIGN e = expression COMMA nel = namedExpressionList_
    COMMA DOTS
    { let ne = [ NT n; NT e ] #@ "namedExpression" in 
      let nel = ne :: nel |> wrap_list_v "namedExpression" in
      [ Term "RecordDefaultE"; NT nel ] #@ "dataExpression" }
;

%inline dataElementExpression:
	| e = sequenceElementExpression
	| e = recordElementExpression 
    { e #@@ "dataElementExpression" }
;

(* >> Non-brace Expressions *)
expressionNonBrace:
	| e = literalExpression
	| e = referenceExpression
	| e = unaryExpression
	| e = binaryExpressionNonBrace
	| e = ternaryExpressionNonBrace
	| e = castExpression
	| e = accessExpressionNonBrace
	| e = callExpressionNonBrace
	| e = parenthesizedExpression
		{ e #@@ "expression" }
;

(* Keyset Expressions *)
simpleKeysetExpression:
	| e = expression
    { [ Term "ExprK"; NT e ] #@ "keysetExpression" }
	| b = expression MASK m = expression
    { [ Term "MaskK"; NT b; NT m ] #@ "keysetExpression" }
	| l = expression RANGE h = expression
    { [ Term "RangeK"; NT l; NT h ] #@ "keysetExpression" }
	| DEFAULT
    { [ Term "DefaultK" ] #@ "keysetExpression" }
	| DONTCARE
    { [ Term "AnyK" ] #@ "keysetExpression" }
;

simpleKeysetExpressionList_:
	| ke = simpleKeysetExpression
    { [ ke ] }
	| kel = simpleKeysetExpressionList_ COMMA ke = simpleKeysetExpression
    { kel @ [ ke ] }
;

tupleKeysetExpression:
	| L_PAREN b = expression MASK m = expression R_PAREN
    { let ke = [ Term "MaskK"; NT b; NT m ] #@ "keysetExpression" in
      [ ke ] }
	| L_PAREN l = expression RANGE h = expression R_PAREN
    { let ke = [ Term "RangeK"; NT l; NT h ] #@ "keysetExpression" in
      [ ke ] }
	| L_PAREN DEFAULT R_PAREN
    { let ke = [ Term "DefaultK" ] #@ "keysetExpression" in
      [ ke ] }
	| L_PAREN DONTCARE R_PAREN
    { let ke = [ Term "AnyK" ] #@ "keysetExpression" in
      [ ke ] }
	| L_PAREN ke = simpleKeysetExpression COMMA kel = simpleKeysetExpressionList_ R_PAREN
    { ke :: kel }
;

keysetExpressionList:
	| ke = simpleKeysetExpression
    { [ ke ] |> wrap_list_v "keysetExpression" }
	| kel = tupleKeysetExpression
    { kel |> wrap_list_v "keysetExpression" }
;

(* Type arguments *)
realTypeArgument:
	| t = typeRef { t #@@ "typeArgument" }
	| VOID
    { [ Term "VoidT" ] #@ "typeArgument" }
	| DONTCARE
    { [ Term "AnyT" ] #@ "typeArgument" }
;

realTypeArgumentList_:
	| targ = realTypeArgument
    { [ targ ] }
	| targs = realTypeArgumentList_ COMMA targ = realTypeArgument
    { targs @ [ targ ] }
;

%inline realTypeArgumentList:
  | targs = realTypeArgumentList_
    { targs |> wrap_list_v "typeArgument" }
;

typeArgument:
	| t = typeRef
    { t #@@ "typeArgument" }
	| t = nonTypeName
    { let n = [ Term "CURRENT"; NT t ] #@ "prefixedName" in
      [ Term "NameT"; NT n ] #@ "typeArgument" }
	| VOID
    { [ Term "VoidT" ] #@ "typeArgument" }
	| DONTCARE
    { [ Term "AnyT" ] #@ "typeArgument" }
;

typeArgumentList_:
	| (* empty *)
    { [] }
	| targ = typeArgument
    { [ targ ] }
	| targs = typeArgumentList_ COMMA targ = typeArgument
    { targs @ [ targ ] }
;

%inline typeArgumentList:
  | targs = typeArgumentList_
    { targs |> wrap_list_v "typeArgument" }
;

(* Arguments *)
argument:
	| e = expression
    { [ Term "ExprA"; NT e ] #@ "argument" }
	| n = name ASSIGN e = expression 
    { [ Term "NameA"; NT n; NT e ] #@ "argument" }
	| n = name ASSIGN DONTCARE
    { [ Term "NameAnyA"; NT n ] #@ "argument" }
	| DONTCARE
		{ [ Term "AnyA" ] #@ "argument" }
;

argumentListNonEmpty_:
	| arg = argument
    { [ arg ] }
	| args = argumentListNonEmpty_ COMMA arg = argument
    { args @ [ arg ] }
;

argumentList_:
	| (* empty *)
    { [] }
	| args = argumentListNonEmpty_
    { args }
;

%inline argumentList:
  | args = argumentList_
    { args |> wrap_list_v "argument" }
;

(* L-values *)
lvalue:
  | n = prefixedNonTypeName
    { [ Term "NameL"; NT n ] #@ "lvalue" }
	| n = THIS
    { let n = [ Term "CURRENT"; NT n ] #@ "prefixedName" in
      [ Term "NameL"; NT n ] #@ "lvalue" }
	| lv = lvalue DOT m = member
    { [ Term "LvalueAccL"; NT lv; NT m ] #@ "lvalue" }
	| lv = lvalue L_BRACKET i = expression R_BRACKET
    { [ Term "ArrAccL"; NT lv; NT i ] #@ "lvalue" }
	| lv = lvalue L_BRACKET h = expression COLON l = expression R_BRACKET
    { [ Term "BitAccL"; NT lv; NT h; NT l ] #@ "lvalue" }
	| L_PAREN lv = lvalue R_PAREN
    { lv }
;

(* Statements *)
(* >> Empty statements *)
emptyStatement:
	| SEMICOLON { [ Term "EmptyS" ] #@ "emptyStatement" }
;

(* >> Assignment statements *)
assignmentStatement:
	| lv = lvalue ASSIGN e = expression SEMICOLON
    { [ Term "AssignS"; NT lv; NT e ] #@ "assignmentStatement" }
;

(* >> Call statements *)
callStatement:
	| lv = lvalue L_PAREN args = argumentList R_PAREN SEMICOLON
    { let targs = [] |> wrap_list_v "typeArgument" in
      [ Term "CallS"; NT lv; NT targs; NT args ] #@ "callStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle
    L_PAREN args = argumentList R_PAREN SEMICOLON
    { [ Term "CallS"; NT lv; NT targs; NT args ] #@ "callStatement" }
;

(* >> Direct application statements *)
directApplicationStatement:
	| t = namedType DOT APPLY L_PAREN args = argumentList R_PAREN SEMICOLON
    { [ Term "InstS"; NT t; NT args ] #@ "directApplicationStatement" }
;

(* >> Return statements *)
returnStatement:
	| RETURN SEMICOLON
    { let eopt = None |> wrap_opt_v "expression" in
      [ Term "ReturnS"; NT eopt ] #@ "returnStatement" }
	| RETURN e = expression SEMICOLON
    { let eopt = Some e |> wrap_opt_v "expression" in
      [ Term "ReturnS"; NT eopt ] #@ "returnStatement" }
;

(* >> Exit statements *)
exitStatement:
	| EXIT SEMICOLON
    { [ Term "ExitS" ] #@ "exitStatement" }
;

(* >> Block statements *)
blockStatement:
	| _al = annotationList L_BRACE
    push_scope
    sl = blockElementStatementList R_BRACE
    pop_scope
		{ [ Term "BlockS"; NT sl ] #@ "blockStatement" }
;

(* >> Conditional statements *)
conditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { let fopt = None |> wrap_opt_v "statement" in
      [ Term "IfS"; NT c; NT t; NT fopt ] #@ "conditionalStatement" }
	| IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { let fopt = Some f |> wrap_opt_v "statement" in
      [ Term "IfS"; NT c; NT t; NT fopt ] #@ "conditionalStatement" }
;

(* >> Switch statements *)
switchLabel:
  | DEFAULT
    { [ Term "DefaultL" ] #@ "switchLabel" }
  | e = expressionNonBrace
    { [ Term "ExprL"; NT e ] #@ "switchLabel" }
;

switchCase:
  | l = switchLabel COLON s = blockStatement
    { [ Term "MatchC"; NT l; NT s ] #@ "switchCase" }
  | l = switchLabel COLON
    { [ Term "FallC"; NT l ] #@ "switchCase" }
;

switchCaseList_:
  | (* empty *)
    { [] }
  | cs = switchCaseList_ c = switchCase
    { cs @ [ c ] }
;

%inline switchCaseList:
  | cs = switchCaseList_
    { cs |> wrap_list_v "switchCase" }
;

switchStatement:
  | SWITCH L_PAREN e = expression R_PAREN L_BRACE cs = switchCaseList R_BRACE
    { [ Term "SwitchS"; NT e; NT cs ] #@ "switchStatement" }

(* >> Statements *)
statement:
  | s = emptyStatement
  | s = assignmentStatement
  | s = callStatement
  | s = directApplicationStatement
  | s = returnStatement
  | s = exitStatement
  | s = blockStatement
  | s = conditionalStatement
  | s = switchStatement
    { s #@@ "statement" }
;

(* Declarations *)
(* >> Constant and variable declarations *)

(* initializer -> initialValue due to reserved word in OCaml *)
initialValue:
  | ASSIGN e = expression { e }
;

constantDeclaration:
  | _al = annotationList CONST t = typeRef n = name i = initialValue SEMICOLON
    { [ Term "ConstD"; NT t; NT n; NT i ] #@ "constantDeclaration" }
;

initializerOpt_:
	| (* empty *)
		{ None }
	| i = initialValue
    { Some i }
;

initializerOpt:
  | i = initializerOpt_
    { i |> wrap_opt_v "initializer" }
;

variableDeclaration:
  | _al = annotationList t = typeRef n = name iopt = initializerOpt SEMICOLON
    { declare_var_of_il n false;
      [ Term "VarD"; NT t; NT n; NT iopt ] #@ "variableDeclaration" }
;

blockElementStatement:
  | d = constantDeclaration
  | d = variableDeclaration
  | d = statement
    { d #@@ "blockElementStatement" }
;

blockElementStatementList_:
  | (* empty *)
    { [] }
  | sl = blockElementStatementList_ s = blockElementStatement
    { sl @ [ s ] }
;

%inline blockElementStatementList:
  | sl = blockElementStatementList_
    { sl |> wrap_list_v "blockElementStatement" }
;

(* >> Function declarations *)
functionPrototype:
	| t = typeOrVoid n = name push_scope
    tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN
    { (t, n, tpl, pl) }
;

functionDeclaration:
	| _al = annotationList fp = functionPrototype b = blockStatement pop_scope
    { let (t, n, tpl, pl) = fp in
      [ Term "FuncD"; NT t; NT n; NT tpl; NT pl; NT b ] #@ "functionDeclaration" }
;

(* >> Action declarations *)
actionDeclaration: 
  | _al = annotationList ACTION n = name L_PAREN pl = parameterList R_PAREN s = blockStatement
    { [ Term "ActionD"; NT n; NT pl; NT s ] #@ "actionDeclaration" }
;

(* >> Instantiations *)
objectInitializer:
	| ASSIGN L_BRACE odl = objectDeclarationList R_BRACE
    { odl }
;

instantiation:
	| _al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { let iopt = None |> wrap_opt_v "objectInitializer" in
      [ Term "InstD"; NT t; NT args; NT n; NT iopt ] #@ "instantiation" }
	| _al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name
      i = objectInitializer SEMICOLON
    { let iopt = Some i |> wrap_opt_v "objectInitializer" in
      [ Term "InstD"; NT t; NT args; NT n; NT iopt ] #@ "instantiation" }
;

objectDeclaration:
	| d = functionDeclaration
	| d = instantiation
    { d #@@ "objectDeclaration" }
;

objectDeclarationList_:
	| (* empty *)
    { [] }
	| odl = objectDeclarationList_ od = objectDeclaration
    { odl @ [ od ] }
;

%inline objectDeclarationList:
  | odl = objectDeclarationList_
    { odl |> wrap_list_v "objectDeclaration" }
;

(* >> Error declarations *)
errorDeclaration:
	| ERROR L_BRACE nl = nameList R_BRACE
    { declare_vars_of_il nl;
      [ Term "ErrD"; NT nl ] #@ "errorDeclaration" }
;

(* >> Match kind declarations *)
matchKindDeclaration:
	| MATCH_KIND L_BRACE nl = nameList trailingCommaOpt R_BRACE
    { declare_vars_of_il nl;
      [ Term "MatchKindD"; NT nl ] #@ "matchKindDeclaration" }
;

(* >> Derived type declarations *)
(* >>>> Enum type declarations *)
enumTypeDeclaration:
  | _al = annotationList ENUM n = name L_BRACE
    nl = nameList trailingCommaOpt R_BRACE
    { [ Term "EnumD"; NT n; NT nl ] #@ "enumTypeDeclaration" }
  | _al = annotationList ENUM t = typeRef n = name L_BRACE
    nel = namedExpressionList trailingCommaOpt R_BRACE
    { [ Term "SEnumD"; NT t; NT n; NT nel ] #@ "enumTypeDeclaration" }
;

(* >>>>>> Struct, header, and union type declarations *)
typeField:
  | _al = annotationList t = typeRef n = name SEMICOLON
    { [ NT t; NT n ] #@ "typeField" }
;

typeFieldList_:
  | (* empty *)
    { [] }
  | tfl = typeFieldList_ tf = typeField
    { tfl @ [ tf ] }
;

%inline typeFieldList:
  | tfl = typeFieldList_
    { tfl |> wrap_list_v "typeField" }
;

structTypeDeclaration:
  | _al = annotationList STRUCT n = name tpl = typeParameterListOpt
    L_BRACE tfl = typeFieldList R_BRACE
    { [ Term "StructD"; NT n; NT tpl; NT tfl ] #@ "structTypeDeclaration" }
;

headerTypeDeclaration:
  | _al = annotationList HEADER n = name tpl = typeParameterListOpt
    L_BRACE tfl = typeFieldList R_BRACE
    { [ Term "HeaderD"; NT n; NT tpl; NT tfl ] #@ "headerTypeDeclaration" }
;

headerUnionTypeDeclaration:
  | _al = annotationList HEADER_UNION n = name tpl = typeParameterListOpt
    L_BRACE tfl = typeFieldList R_BRACE
    { [ Term "HeaderUnionD"; NT n; NT tpl; NT tfl ] #@ "headerUnionTypeDeclaration" }
;

derivedTypeDeclaration:
  | d = enumTypeDeclaration
  | d = structTypeDeclaration
  | d = headerTypeDeclaration
  | d = headerUnionTypeDeclaration
    { d #@@ "derivedTypeDeclaration" }
;

(* >> Typedef and newtype declarations *)
typedefType:
	| t = typeRef
    { [ Term "PlainT"; NT t ] #@ "typedefType" }
	| dtd = derivedTypeDeclaration
    { [ Term "DerivedT"; NT dtd ] #@ "typedefType" }
;

typedefDeclaration:
	| _al = annotationList TYPEDEF t = typedefType n = name SEMICOLON
    { [ Term "TypeDefD"; NT t; NT n ] #@ "typedefDeclaration" }
	| _al = annotationList TYPE t = typeRef n = name SEMICOLON
    { [ Term "NewTypeD"; NT t; NT n ] #@ "typedefDeclaration" }
;

(* >> Extern declarations *)
externFunctionDeclaration:
	| _al = annotationList EXTERN fp = functionPrototype pop_scope SEMICOLON
		{ let (t, n, tpl, pl) = fp in
      let decl =
        [ Term "ExternFuncD"; NT t; NT n; NT tpl; NT pl ] #@ "externFunctionDeclaration"
      in
      declare_var (id_of_name n) (has_type_params tpl);
      decl }
;

methodPrototype:
	| _al = annotationList n = typeIdentifier L_PAREN pl = parameterList R_PAREN SEMICOLON
    { [ Term "ConsM"; NT n; NT pl ] #@ "methodPrototype" }
	| _al = annotationList fp = functionPrototype pop_scope SEMICOLON
    { let (t, n, tpl, pl) = fp in
      [ Term "MethodM"; NT t; NT n; NT tpl; NT pl ] #@ "methodPrototype" }
	| _al = annotationList ABSTRACT fp = functionPrototype pop_scope SEMICOLON
    { let (t, n, tpl, pl) = fp in
      [ Term "AbstractMethodM"; NT t; NT n; NT tpl; NT pl ] #@ "methodPrototype" }
;

methodPrototypeList_:
  | (* empty *)
    { [] }
  | ms = methodPrototypeList_ m = methodPrototype
    { ms @ [ m ] }
;

%inline methodPrototypeList:
  | ms = methodPrototypeList_
    { ms |> wrap_list_v "methodPrototype" }
;

externObjectDeclaration:
  | _al = annotationList EXTERN n = push_externName tpl = typeParameterListOpt
    L_BRACE ms = methodPrototypeList R_BRACE pop_scope
    { let decl =
        [ Term "ExternObjectD"; NT n; NT tpl; NT ms ] #@ "externObjectDeclaration"
      in
      declare_type_of_il n (has_type_params_declaration decl);
      decl }
;

externDeclaration:
  | d = externFunctionDeclaration
  | d = externObjectDeclaration
    { d #@@ "externDeclaration" }
;

(* >> Parser statements and declarations *)
(* >>>> Select expressions *)
selectCase:
  | kel = keysetExpressionList COLON n = name SEMICOLON
    { [ NT kel; NT n ] #@ "selectCase" }
;

selectCaseList_:
  | (* empty *)
    { [] }
  | cs = selectCaseList_ c = selectCase
    { cs @ [ c ] }
;

%inline selectCaseList:
  | cs = selectCaseList_
    { cs |> wrap_list_v "selectCase" }
;

selectExpression:
  | SELECT L_PAREN es = expressionList R_PAREN L_BRACE cs = selectCaseList R_BRACE
    { [ Term "SelectE"; NT es; NT cs ] #@ "stateExpression" }
;

(* >>>> Transition statements *)
stateExpression:
  | n = name SEMICOLON
    { [ Term "NameE"; NT n ] #@ "stateExpression" }
  | e = selectExpression
    { e }
;

transitionStatementOpt:
  | (* empty *)
    { None |> wrap_opt_v "transitionStatement" }
  | TRANSITION e = stateExpression
    { Some ([ Term "TransS"; NT e ]) #@ "transitionStatement"
      |> wrap_opt_v "transitionStatement" }
;

(* >>>> Value set declarations *)
valueSetType:
	| t = baseType
	| t = tupleType
	| t = nameType
    { t #@@ "valueSetType" }
;

valueSetDeclaration:
	| _al = annotationList VALUE_SET l_angle t = valueSetType r_angle
    L_PAREN s = expression R_PAREN n = name SEMICOLON
    { [ Term "ValueSetD"; NT t; NT s; NT n ] #@ "valueSetDeclaration" }
;

(* >>>> Parser type declarations *)
parserTypeDeclaration:
  | _al = annotationList PARSER n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ Term "ParserTypeD"; NT n; NT tpl; NT pl ] #@ "parserTypeDeclaration" }
;

(* >>>> Parser declarations *)
parserBlockStatement:
  | _al = annotationList L_BRACE sl = parserStatementList R_BRACE
    { [ Term "ParserBlockS"; NT sl ] #@ "parserBlockStatement" }
;

parserStatement:
  | s = constantDeclaration
  | s = variableDeclaration
  | s = emptyStatement
  | s = assignmentStatement
  | s = callStatement
  | s = directApplicationStatement
  | s = parserBlockStatement
  | s = conditionalStatement
    { s #@@ "parserStatement" }
;

parserStatementList_:
  | (* empty *)
    { [] }
  | sl = parserStatementList_ s = parserStatement
    { sl @ [ s ] }
;

%inline parserStatementList:
  | sl = parserStatementList_
    { sl |> wrap_list_v "parserStatement" }
;

parserState:
  | _al = annotationList STATE n = push_name L_BRACE sl = parserStatementList
    topt = transitionStatementOpt R_BRACE
    { [ NT n; NT sl; NT topt ] #@ "parserState" }
;

parserStateList_:
  | s = parserState
    { [ s ] }
  | sl = parserStateList_ s = parserState
    { sl @ [ s ] }
;

%inline parserStateList:
  | sl = parserStateList_
    { sl |> wrap_list_v "parserState" }
;

parserLocalDeclaration:
  | d = constantDeclaration
  | d = instantiation
  | d = variableDeclaration
  | d = valueSetDeclaration
    { d #@@ "parserLocalDeclaration" }
;

parserLocalDeclarationList_:
  | (* empty *)
    { [] }
  | dl = parserLocalDeclarationList_ d = parserLocalDeclaration
    { dl @ [ d ] }
;

%inline parserLocalDeclarationList:
  | dl = parserLocalDeclarationList_
    { dl |> wrap_list_v "parserLocalDeclaration" }
;

parserDeclaration:
  | _al = annotationList PARSER n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
    { [ Term "ParserD"; NT n; NT tpl; NT pl; NT cpl; NT dl; NT sl ]
      #@ "parserDeclaration" }
;

(* >> Control statements and declarations *)
(* >>>> Table declarations *)
constOpt:
  | (* empty *)
    { None |> wrap_opt_v "const" }
  | CONST
    { Some ([ Term "CONST" ] #@ "const") |> wrap_opt_v "const" }
;

(* >>>>>> Table key property *)
tableKey:
  | e = expression COLON n = name _al = annotationList SEMICOLON
    { [ NT e; NT n ] #@ "tableKey" }
;

tableKeyList_:
  | (* empty *)
    { [] }
  | kl = tableKeyList_ k = tableKey
    { kl @ [ k ] }
;

%inline tableKeyList:
  | kl = tableKeyList_
    { kl |> wrap_list_v "tableKey" }
;

(* >>>>>> Table actions property *)
tableActionReference:
  | n = prefixedNonTypeName
    { let al = [] |> wrap_list_v "argument" in
      [ NT n; NT al ] #@ "tableActionReference" }
  | n = prefixedNonTypeName L_PAREN al = argumentList R_PAREN
    { [ NT n; NT al ] #@ "tableActionReference" }
;

tableAction:
  | _al = annotationList ac = tableActionReference SEMICOLON
    { ac #@@ "tableAction" }
;

tableActionList_:
  | (* empty *)
    { [] }
  | acl = tableActionList_ ac = tableAction
    { acl @ [ ac ] }
;

%inline tableActionList:
  | acl = tableActionList_
    { acl |> wrap_list_v "tableAction" }
;

(* >>>>>> Table entry property *)
tableEntryPriority:
  | PRIORITY ASSIGN num = number COLON
    { [ Term "NumE"; NT num ] #@ "literalExpression" }
  | PRIORITY ASSIGN L_PAREN e = expression R_PAREN COLON
    { e }
;

tableEntry:
  | copt = constOpt p = tableEntryPriority kel = keysetExpressionList
    COLON ac = tableActionReference _al = annotationList SEMICOLON
    { let popt = Some p |> wrap_opt_v "tableEntryPriority" in
      [ NT copt; NT popt; NT kel; NT ac ] #@ "tableEntry" }
  | copt = constOpt kel = keysetExpressionList COLON ac = tableActionReference
    _al = annotationList SEMICOLON
    { let popt = None |> wrap_opt_v "tableEntryPriority" in
      [ NT copt; NT popt; NT kel; NT ac ] #@ "tableEntry" }
;

tableEntryList_:
  | (* empty *)
    { [] }
  | el = tableEntryList_ e = tableEntry
    { el @ [ e ] }
;

tableEntryList:
  | el = tableEntryList_
    { el |> wrap_list_v "tableEntry" }
;

(* >>>>>> Table properties *)
tableProperty:
  | KEY ASSIGN L_BRACE kl = tableKeyList R_BRACE
    { [ Term "KeyP"; NT kl ] #@ "tableProperty" }
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { [ Term "ActionP"; NT acl ] #@ "tableProperty" }
  | _al = annotationList copt = constOpt ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { [ Term "EntryP"; NT copt; NT el ] #@ "tableProperty" }
  | _al = annotationList copt = constOpt n = tableCustomName i = initialValue SEMICOLON
    { [ Term "CustomP"; NT copt; NT n; NT i ] #@ "tableProperty" }
;

tablePropertyList_:
  | (* empty *)
    { [] }
  | pl = tablePropertyList_ p = tableProperty
    { pl @ [ p ] }
;

%inline tablePropertyList:
  | pl = tablePropertyList_
    { pl |> wrap_list_v "tableProperty" }
;

tableDeclaration:
  | _al = annotationList TABLE n = name L_BRACE pl = tablePropertyList R_BRACE
    { [ Term "TableD"; NT n; NT pl ] #@ "tableDeclaration" }

(* >>>> Control type declarations *)
controlTypeDeclaration:
  | _al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ Term "ControlTypeD"; NT n; NT tpl; NT pl ] #@ "controlTypeDeclaration" }
;

(* >>>> Control declarations *)
controlBody:
  | b = blockStatement { b #@@ "controlBody" }
;

controlLocalDeclaration:
  | d = constantDeclaration 
  | d = instantiation 
  | d = variableDeclaration
    { d #@@ "controlLocalDeclaration" }
  | d = actionDeclaration
  | d = tableDeclaration
    { declare_var (id_of_declaration d) false;
      d #@@ "controlLocalDeclaration" }
;

controlLocalDeclarationList_:
  | (* empty *)
    { [] }
  | dl = controlLocalDeclarationList_ d = controlLocalDeclaration
    { dl @ [ d ] }
;

%inline controlLocalDeclarationList:
  | dl = controlLocalDeclarationList_
    { dl |> wrap_list_v "controlLocalDeclaration" }
;

controlDeclaration:
  | _al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { [ Term "ControlD"; NT n; NT tpl; NT pl; NT cpl; NT dl; NT b ]
      #@ "controlDeclaration" }
;

(* >> Package type declarations *)
packageTypeDeclaration:
  | _al = annotationList PACKAGE n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ Term "PackageTypeD"; NT n; NT tpl; NT pl ] #@ "packageTypeDeclaration" }
;

(* >> Type declarations *)
typeDeclaration:
  | d = derivedTypeDeclaration
  | d = typedefDeclaration
  | d = parserTypeDeclaration
  | d = controlTypeDeclaration
  | d = packageTypeDeclaration
    { d #@@ "typeDeclaration" }
;

(* >> Declarations *)
declaration:
  | const = constantDeclaration
    { declare_var (id_of_declaration const) (has_type_params_declaration const);
      const #@@ "declaration" }
  | inst = instantiation
    { declare_var (id_of_declaration inst) false;
      inst #@@ "declaration" }
  | func = functionDeclaration
    { declare_var (id_of_declaration func) (has_type_params_declaration func);
      func #@@ "declaration" }
  | action = actionDeclaration
    { declare_var (id_of_declaration action) false;
      action #@@ "declaration" }
  | d = errorDeclaration
  | d = matchKindDeclaration
  | d = externDeclaration
    { d #@@ "declaration" }
  | d = parserDeclaration
  | d = controlDeclaration
  | d = typeDeclaration
    { declare_type (id_of_declaration d) (has_type_params_declaration d);
      d #@@ "declaration" }
;

(* Annotations *)
annotationToken:
	| UNEXPECTED_TOKEN | ABSTRACT | ACTION | ACTIONS | APPLY
	| BOOL | BIT | CONST | CONTROL | DEFAULT
	| ELSE | ENTRIES | ENUM | ERROR | EXIT
  | EXTERN | FALSE | FOR | HEADER | HEADER_UNION
	| IF | IN | INOUT | INT | KEY
	| MATCH_KIND | TYPE | OUT | PARSER | PACKAGE
  | PRAGMA | RETURN | SELECT | STATE | STRING
  | STRUCT | SWITCH | TABLE | THIS | TRANSITION
  | TRUE | TUPLE | TYPEDEF | VARBIT | VALUE_SET
	| LIST | VOID | DONTCARE
	| identifier | typeIdentifier | stringLiteral | number
	| MASK | RANGE | SHL | AND | OR
	| EQ | NE | GE | LE | PLUSPLUS
	| PLUS | PLUS_SAT | MINUS | MINUS_SAT | MUL
	| DIV | MOD | BIT_OR | BIT_AND | BIT_XOR
	| COMPLEMENT | L_BRACKET | R_BRACKET | L_BRACE | R_BRACE
	| L_ANGLE | R_ANGLE | NOT | COLON | COMMA
  | QUESTION | DOT | ASSIGN | SEMICOLON | AT
    { [ Term "" ] #@ "annotationToken" }
;

annotationBody:
	| (* empty *)
	| annotationBody L_PAREN annotationBody R_PAREN
	| annotationBody annotationToken
    { [ Term "" ] #@ "annotationBody" }
;

structuredAnnotationBody:
	| _e = dataElementExpression _tcopt = trailingCommaOpt
    { [ Term "" ] #@ "structuredAnnotationBody" }
;

annotation:
	| AT name
	| AT name L_PAREN annotationBody R_PAREN
	| AT name L_BRACKET structuredAnnotationBody R_BRACKET
	| PRAGMA name annotationBody PRAGMA_END
		{ [ Term "" ] #@ "annotation" }
;

annotationListNonEmpty_:
  | a = annotation
    { [ a ] }
	| al = annotationListNonEmpty_ a = annotation
		{ al @ [ a ] }
;

annotationListNonEmpty:
  | al = annotationListNonEmpty_
    { al |> wrap_list_v "annotation" }
;

%inline annotationList:
	| (* empty *)
    { [] |> wrap_list_v "annotation" }
	| al = annotationListNonEmpty
    { al }
;

(******** P4 program ********)
declarationList_:
  | (* empty *) { [] }
  | dl = declarationList_ d = declaration
    { dl @ [ d ] }
  | dl = declarationList_ SEMICOLON
    { dl }
;

%inline declarationList:
  | dl = declarationList_ { dl |> wrap_list_v "declaration" }

p4program:
	| dl = declarationList END { dl #@@ "program" }
