%{
  open Lang
  open Il
  open Context
  open Extract
  open Value.Make
  open Flatten

  let declare_var_of_il (value : value) (b : bool) : unit =
    let id = id_of_name value in
    declare_var id b

  let rec declare_vars_of_il (value : value) : unit =
    match flatten_case_v_opt value with
    | Some ("nameList", [ "," ], [ v_nameList; v_name ]) ->
        declare_vars_of_il v_nameList;
        declare_var_of_il v_name false
    | Some ("identifier", _, _)
    | Some ("nonTypeName", _, _)
    | Some ("name", _, _)
    | Some ("typeIdentifier", _, _) -> declare_var_of_il value false
    | _ ->
        failwith
          (Printf.sprintf "@declare_vars_of_il: expected name, got %s"
           (Il.Print.string_of_value value))

  let declare_type_of_il (value : value) (b : bool) : unit =
    let id = id_of_name value in
    declare_type id b

  let rec declare_types_of_il (value : value) : unit =
    match flatten_case_v_opt value with
    | Some ("typeParameterList", [ "," ], [ v_tpList; v_name ]) ->
        declare_types_of_il v_tpList;
        declare_type_of_il v_name false
    | Some ("identifier", _, _)
    | Some ("nonTypeName", _, _)
    | Some ("name", _, _)
    | Some ("typeIdentifier", _, _) -> declare_type_of_il value false
    | _ ->
        failwith
          (Printf.sprintf "@declare_types_of_il: expected name, got %s"
           (Il.Print.string_of_value value))

  (* Position handling *)

  let region_of_positions (sp : Lexing.position) (ep : Lexing.position) :
      Util.Source.region =
    let left =
      {
        Util.Source.file = sp.pos_fname;
        line = sp.pos_lnum;
        column = sp.pos_cnum - sp.pos_bol;
      }
    in
    let right =
      {
        Util.Source.file = ep.pos_fname;
        line = ep.pos_lnum;
        column = ep.pos_cnum - ep.pos_bol;
      }
    in
    { Util.Source.left; right }

  let mk_empty (sp : Lexing.position) (ep : Lexing.position)
      (s_mixop : string) (s : string) : value =
    if s_mixop = "`EMPTY" then s_mixop <| [] <<| s
    else
      Value.Make.with_at (region_of_positions sp ep) (s_mixop <| [] <<| s)
%}

(**************************** TOKENS ******************************)
%token<Source.info> END
%token TYPENAME IDENTIFIER
%token<Lang.Il.value> NAME STRING_LITERAL
%token<Lang.Il.value * string> NUMBER_INT NUMBER
%token<Source.info> LE GE SHL AND OR NE EQ
%token<Source.info> PLUS MINUS PLUS_SAT MINUS_SAT MUL INVALID DIV MOD
%token<Source.info> BIT_OR BIT_AND BIT_XOR COMPLEMENT
%token<Source.info> L_BRACKET R_BRACKET L_BRACE R_BRACE L_ANGLE L_ANGLE_ARGS R_ANGLE R_ANGLE_SHIFT L_PAREN R_PAREN
%token<Source.info> ASSIGN COLON COMMA QUESTION DOT NOT SEMICOLON
%token<Source.info> AT PLUSPLUS PLUSCOLON
%token<Source.info> DONTCARE
%token<Source.info> MASK DOTS RANGE
%token<Source.info> TRUE FALSE
%token<Source.info> ABSTRACT ACTION ACTIONS APPLY BOOL BIT BREAK CONST CONTINUE CONTROL DEFAULT
%token<Source.info> ELSE ENTRIES ENUM ERROR EXIT EXTERN HEADER HEADER_UNION IF IN INOUT FOR
%token<Source.info> INT KEY LIST SELECT MATCH_KIND OUT PACKAGE PARSER PRIORITY RETURN STATE STRING STRUCT
%token<Source.info> SWITCH TABLE THIS TRANSITION TUPLE TYPEDEF TYPE VALUE_SET VARBIT VOID
%token<Source.info> PRAGMA PRAGMA_END
%token<Source.info> PLUS_ASSIGN PLUS_SAT_ASSIGN MINUS_ASSIGN MINUS_SAT_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN  SHL_ASSIGN SHR_ASSIGN BIT_AND_ASSIGN BIT_XOR_ASSIGN BIT_OR_ASSIGN
%token<Lang.Il.value> UNEXPECTED_TOKEN

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

%start p4program

(**************************** TYPES ******************************)
%type <Lang.Il.value>
  (* Aux *) int externName declarationList
  (* Misc *) trailingCommaOpt (* Booleans *) booleanLiteral
  (* Integers *) integerLiteral (* Strings *) stringLiteral
  (* Names *)
  identifier typeIdentifier nonTypeName prefixedNonTypeName typeName prefixedTypeName tableCustomName name nameList member
  (* Directions *) direction
  (* Types *)
  baseType specializedType namedType headerStackType listType tupleType typeRef typeOrVoid
  (* Type parameters *) typeParameter typeParameterList typeParameterListOpt
  (* Parameters *) parameter nonEmptyParameterList parameterList 
  (* Constructor parameters *) constructorParameterListOpt
  (* Expression key-value pairs *) namedExpression namedExpressionList
  (* Expressions *)
  literalExpression referenceExpression defaultExpression 
  (* >> Unary, binary, and ternary expressions *) 
  unop unaryExpression binop binaryExpression binaryExpressionNonBrace ternaryExpression ternaryExpressionNonBrace 
  (* >> Cast expressions *) castExpression 
  (* >> Data (aggregate) expressions *) dataExpression
  (* >> Member and index access expressions *)
  errorAccessExpression memberAccessExpression indexAccessExpression accessExpression
  memberAccessExpressionNonBrace indexAccessExpressionNonBrace accessExpressionNonBrace
  (* >> Call expressions *)
  callableTarget constructorTarget callTarget callExpression
  callableTargetNonBrace callTargetNonBrace callExpressionNonBrace
  (* >> Parenthesized Expressions *) parenthesizedExpression
  (* >> Expressions *)
  expression expressionList memberAccessBase sequenceElementExpression recordElementExpression sequenceOrRecordElementExpression
  (* >> Non-brace Expressions *) expressionNonBrace memberAccessBaseNonBrace
  (* Keyset Expressions *) simpleKeysetExpression simpleKeysetExpressionList tupleKeysetExpression keysetExpression
  (* Type arguments *)
  realTypeArgument realTypeArgumentList typeArgument typeArgumentList argument argumentListNonEmpty argumentList
  (* L-values *) lvalue
  (* Statements *)
  emptyStatement assignop assignmentStatement callStatement directApplicationStatement returnStatement exitStatement blockStatement conditionalStatement 
  (* >> For statements *)
  forInitStatement forInitStatementListNonEmpty forInitStatementList forUpdateStatement forUpdateStatementListNonEmpty
  forUpdateStatementList forCollectionExpression forStatement
  (* >> Switch statements *) switchLabel switchCase switchCaseList switchStatement
  breakStatement continueStatement statement
  (* Declarations *)
  (* >> Constant and variable declarations *)
  initialValue constantDeclaration initializerOpt variableDeclaration blockElementStatement blockElementStatementList
  (* >> Function declarations *) functionPrototype functionDeclaration 
  (* >> Action declarations *) actionDeclaration
  (* >> Instantiations *) objectInitializer instantiation objectDeclaration objectDeclarationList
  (* >> Error declarations *) errorDeclaration
  (* >> Match kind declarations *) matchKindDeclaration
  (* >> Derived type declarations *)
  enumTypeDeclaration typeField typeFieldList structTypeDeclaration headerTypeDeclaration headerUnionTypeDeclaration derivedTypeDeclaration
  (* >> Typedef and newtype declarations *) typedef typedefDeclaration
  (* >> Extern declarations *)
  externFunctionDeclaration externConstructorPrototype externMethodPrototype
  externConstructorOrMethodPrototypeList externObjectDeclaration externDeclaration
  (* >> Parser statements and declarations *)
  (* >>>> Select expressions *) selectCase selectCaseList selectExpression
  (* >>>> Transition statements *) stateExpression transitionStatement
  (* >>>> Value set declarations *) valueSetType valueSetDeclaration
  (* >>>> Parser type declarations *) parserTypeDeclaration
  (* >>>> Parser Declarations *)
  parserBlockStatement parserStatement parserStatementList parserState
  parserStateList parserLocalDeclaration parserLocalDeclarationList parserDeclaration
  (* >> Control statements and declarations *)
  (* >>>> Table declarations *) constOpt
  (* >>>>>> Table key property *) tableKey tableKeyList
  (* >>>>>> Table actions property *) tableActionReference tableAction tableActionList
  (* >>>>>> Table entry property *) tableEntryPriority tableEntry tableEntryList
  (* >>>>>> Table properties *) tableProperty tablePropertyList tableDeclaration
  (* >>>> Control type declarations *) controlTypeDeclaration
  (* >>>> Control declarations *) controlBody controlLocalDeclaration controlLocalDeclarationList controlDeclaration
  (* >> Package type declarations *) packageTypeDeclaration
  (* >> Type declarations *) typeDeclaration
  (* >> Declaration *) declaration
  (* Annotations *) annotationToken annotationBody structuredAnnotationBody annotation annotationListNonEmpty annotationList p4program
%type <Lang.Il.value> push_name push_externName
%type <unit> push_scope pop_scope go_toplevel go_local
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
    { mk_empty $startpos $endpos "`EMPTY" "trailingCommaOpt" }
	| COMMA
    { mk_empty $startpos $endpos "`," "trailingCommaOpt" }
;

(* Booleans *)
%inline booleanLiteral:
  | TRUE
    { mk_empty $startpos $endpos "TRUE" "booleanLiteral" }
  | FALSE
    { mk_empty $startpos $endpos "FALSE" "booleanLiteral" }

(* Integers *)
integerLiteral:
	| int = int
    { "D int" <| [ int ] <<| "integerLiteral" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
(* Processed by lexer *)
	| number = NUMBER
    { fst number }
;

(* Strings *)
stringLiteral:
	| text = STRING_LITERAL
    { "`\" text `\"" <| [ text ] <<| "stringLiteral" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* Names *)
identifier:
	| text = NAME IDENTIFIER
    { "`ID text" <| [ text ] <<| "identifier" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

typeIdentifier:
	| text = NAME TYPENAME
    { "`TID text" <| [ text ] <<| "typeIdentifier" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Non-type names *)
nonTypeName:
	| id = identifier
    { id }
	| APPLY
    { mk_empty $startpos $endpos "APPLY" "nonTypeName" }
	| KEY
    { mk_empty $startpos $endpos "KEY" "nonTypeName" }
	| ACTIONS
    { mk_empty $startpos $endpos "ACTIONS" "nonTypeName" }
	| STATE
    { mk_empty $startpos $endpos "STATE" "nonTypeName" }
	| ENTRIES
    { mk_empty $startpos $endpos "ENTRIES" "nonTypeName" }
	| TYPE
    { mk_empty $startpos $endpos "TYPE" "nonTypeName" }
	| PRIORITY
    { mk_empty $startpos $endpos "PRIORITY" "nonTypeName" }
;

prefixedNonTypeName:
	| n = nonTypeName
    { n }
	| DOT go_toplevel n = nonTypeName go_local
    { "`ID `. nonTypeName" <| [ n ] <<| "prefixedNonTypeName" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Type names *)
typeName:
	| n = typeIdentifier { n }
;

prefixedTypeName:
	| n = typeName
    { n }
	| DOT go_toplevel tid = typeName go_local
    { "`TID `. typeName" <| [ tid ] <<| "prefixedTypeName" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Table custom property names *)
tableCustomName:
	| id = identifier
    { id }
	| tid = typeIdentifier
    { tid }
	| APPLY
    { mk_empty $startpos $endpos "APPLY" "tableCustomName" }
	| STATE
    { mk_empty $startpos $endpos "STATE" "tableCustomName" }
	| TYPE
    { mk_empty $startpos $endpos "TYPE" "tableCustomName" }
	| PRIORITY
    { mk_empty $startpos $endpos "PRIORITY" "tableCustomName" }
;

(* >> Names *)
name:
	| n = nonTypeName
	| n = typeName
    { n }
	| LIST
    { mk_empty $startpos $endpos "LIST" "name" }
;

nameList:
	| n = name
    { n }
	| ns = nameList COMMA n = name
    { "nameList `, name" <| [ ns; n ] <<| "nameList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

member:
	| name = name
    { name }
;

(* Directions *)
direction:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "direction" }
	| IN
    { mk_empty $startpos $endpos "IN" "direction" }
	| OUT
    { mk_empty $startpos $endpos "OUT" "direction" }
	| INOUT
    { mk_empty $startpos $endpos "INOUT" "direction" }
;

(* Types *)
(* >> Base types *)
baseType:
	| BOOL
    { mk_empty $startpos $endpos "BOOL" "baseType" }
	| MATCH_KIND
    { mk_empty $startpos $endpos "MATCH_KIND" "baseType" }
	| ERROR
    { mk_empty $startpos $endpos "ERROR" "baseType" }
	| BIT
    { mk_empty $startpos $endpos "BIT" "baseType" }
	| STRING
    { mk_empty $startpos $endpos "STRING" "baseType" }
	| INT
    { mk_empty $startpos $endpos "INT" "baseType" }
	| BIT l_angle v = int r_angle
    { "BIT `< int >" <| [ v ] <<| "baseType" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| INT l_angle v = int r_angle
    { "INT `< int >" <| [ v ] <<| "baseType" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| VARBIT l_angle v = int r_angle
    { "VARBIT `< int >" <| [ v ] <<| "baseType" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| BIT l_angle L_PAREN e = expression R_PAREN r_angle
    { "BIT `< `( expression ) >" <| [ e ] <<| "baseType" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| INT l_angle L_PAREN e = expression R_PAREN r_angle
    { "INT `< `( expression ) >" <| [ e ] <<| "baseType" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| VARBIT l_angle L_PAREN e = expression R_PAREN r_angle
    { "VARBIT `< `( expression ) >" <| [ e ] <<| "baseType" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Named types *)
specializedType:
  | n = prefixedTypeName l_angle tal = typeArgumentList r_angle
    { "prefixedTypeName `< typeArgumentList >" <| [ n; tal ] <<| "specializedType" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

namedType:
  | t = prefixedTypeName
  | t = specializedType
    { t }
;

(* >> Header stack types *)
headerStackType:
  | t = namedType L_BRACKET e = expression R_BRACKET
    { "namedType `[ expression ]" <| [ t; e ] <<| "headerStackType" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> List types *)
listType:
  | LIST l_angle targ = typeArgument r_angle
    { "LIST `< typeArgument >" <| [ targ ] <<| "listType" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Tuple types *)
tupleType:
	| TUPLE l_angle targs = typeArgumentList r_angle
    { "TUPLE `< typeArgumentList >" <| [ targs ] <<| "tupleType" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Types *)
typeRef:
	| t = baseType
	| t = namedType
	| t = headerStackType
	| t = listType
	| t = tupleType
    { t }
;

typeOrVoid:
	| t = typeRef
    { t }
	| VOID
    { mk_empty $startpos $endpos "VOID" "typeOrVoid" }
  | id = identifier
    { id }
;

(* Type parameters *)
typeParameter:
	| n = name { n }

typeParameterList:
	| tp = typeParameter
    { tp }
	| tps = typeParameterList COMMA tp = typeParameter
    { "typeParameterList `, typeParameter" <| [ tps; tp ] <<| "typeParameterList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

typeParameterListOpt:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "typeParameterListOpt" }
	| l_angle tps = typeParameterList r_angle
    { declare_types_of_il tps;
      "`< typeParameterList >" <| [ tps ] <<| "typeParameterListOpt" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* Parameters *)
parameter:
	| al = annotationList dir = direction t = typeRef n = name i = initializerOpt
		{ declare_var_of_il n false;
      "annotationList direction type name initializerOpt" <| [ al; dir; t; n; i ] <<| "parameter" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

nonEmptyParameterList:
	| p = parameter
    { p }
	| ps = nonEmptyParameterList COMMA p = parameter
    { "nonEmptyParameterList `, parameter" <| [ ps; p ] <<| "nonEmptyParameterList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

parameterList:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "parameterList" }
	| ps = nonEmptyParameterList
    { ps }
;

(* Constructor parameters *)
constructorParameterListOpt:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "constructorParameterListOpt" }
	| L_PAREN ps = parameterList R_PAREN
    { "`( parameterList )" <| [ ps ] <<| "constructorParameterListOpt" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* Expression key-value pairs *)
namedExpression:
	| n = name ASSIGN e = expression
    { "name `= expression" <| [ n; e ] <<| "namedExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

namedExpressionList:
	| e = namedExpression
    { e }
	| es = namedExpressionList COMMA e = namedExpression
    { "namedExpressionList `, namedExpression" <| [ es; e ] <<| "namedExpressionList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* Expressions *)
(* >> Literal expressions *)
%inline literalExpression:
  | bool = booleanLiteral { bool }
	| int = integerLiteral { int }
	| str = stringLiteral { str }
;

(* >> Reference expressions *)
%inline referenceExpression:
	| n = prefixedNonTypeName
    { n }
	| THIS
    { mk_empty $startpos $endpos "THIS" "referenceExpression" }
;

(* >> Default expressions *)
%inline defaultExpression:
	| DOTS
    { mk_empty $startpos $endpos "`..." "defaultExpression" }
;

(* >> Unary, binary, and ternary expressions *)
%inline unop: 
	| NOT
    { mk_empty $startpos $endpos "`!" "unop" }
	| COMPLEMENT
    { mk_empty $startpos $endpos "`~" "unop" }
	| MINUS
    { mk_empty $startpos $endpos "`-" "unop" }
	| PLUS
    { mk_empty $startpos $endpos "`+" "unop" }
;

%inline unaryExpression:
	| o = unop e = expression %prec PREFIX
		{ "unop expression" <| [ o; e ] <<| "unaryExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline binop:
  | MUL
    { mk_empty $startpos $endpos "`*" "binop" }
  | DIV
    { mk_empty $startpos $endpos "`/" "binop" }
  | MOD
    { mk_empty $startpos $endpos "`%" "binop" }
  | PLUS
    { mk_empty $startpos $endpos "`+" "binop" }
  | PLUS_SAT
    { mk_empty $startpos $endpos "`|+|" "binop" }
  | MINUS
    { mk_empty $startpos $endpos "`-" "binop" }
  | MINUS_SAT
    { mk_empty $startpos $endpos "`|-|" "binop" }
  | SHL
    { mk_empty $startpos $endpos "`<<" "binop" }
  | r_angle R_ANGLE_SHIFT
    { mk_empty $startpos $endpos "`>>" "binop" }
  | LE
    { mk_empty $startpos $endpos "`<=" "binop" }
  | GE
    { mk_empty $startpos $endpos "`>=" "binop" }
  | l_angle
    { mk_empty $startpos $endpos "``<" "binop" }
  | r_angle
    { mk_empty $startpos $endpos "``>" "binop" }
  | NE
    { mk_empty $startpos $endpos "`!=" "binop" }
  | EQ
    { mk_empty $startpos $endpos "`==" "binop" }
  | BIT_AND
    { mk_empty $startpos $endpos "`&" "binop" }
  | BIT_XOR
    { mk_empty $startpos $endpos "`^" "binop" }
  | BIT_OR
    { mk_empty $startpos $endpos "`|" "binop" }
  | PLUSPLUS
    { mk_empty $startpos $endpos "`++" "binop" }
  | AND
    { mk_empty $startpos $endpos "`&&" "binop" }
  | OR
    { mk_empty $startpos $endpos "`||" "binop" }
;

%inline binaryExpression:
	| l = expression o = binop r = expression
		{ "expression binop expression" <| [ l; o; r ] <<| "binaryExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline binaryExpressionNonBrace:
	| l = expressionNonBrace o = binop r = expression
		{ "expressionNonBrace binop expression" <| [ l; o; r ] <<| "binaryExpressionNonBrace" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline ternaryExpression:
	| c = expression QUESTION t = expression COLON f = expression
		{ "expression `? expression `: expression" <| [ c; t; f ] <<| "ternaryExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline ternaryExpressionNonBrace:
	| c = expressionNonBrace QUESTION t = expression COLON f = expression
		{ "expressionNonBrace `? expression `: expression" <| [ c; t; f ] <<| "ternaryExpressionNonBrace" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Cast expressions *)
%inline castExpression:
	| L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
    { "`( typeRef ) expression" <| [ t; e ] <<| "castExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Data (aggregate) expressions *)
%inline dataExpression:
	| INVALID
    { mk_empty $startpos $endpos "`{#}" "invalidHeaderExpression" }
	| L_BRACE e = sequenceOrRecordElementExpression c = trailingCommaOpt R_BRACE
    { "`{ sequenceOrRecordElementExpression trailingCommaOpt }" <| [ e; c ] <<| "sequenceOrRecordExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Member and index access expressions *)
%inline errorAccessExpression:
	| ERROR DOT m = member
		{ "ERROR `. member" <| [ m ] <<| "errorAccessExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline memberAccessExpression:
	| e = memberAccessBase DOT m = member %prec DOT
		{ "memberAccessBase `. member" <| [ e; m ] <<| "memberAccessExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline indexAccessExpression:
	| a = expression L_BRACKET i = expression R_BRACKET
		{ "expression `[ expression ]" <| [ a; i ] <<| "indexAccessExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline sliceAccessExpression:
  | a = expression L_BRACKET h = expression COLON l = expression R_BRACKET
    { "expression `[ expression `: expression ]" <| [ a; h; l ] <<| "sliceAccessExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | a = expression L_BRACKET l = expression PLUSCOLON w = expression R_BRACKET
    { "expression `[ expression `+: expression ]" <| [ a; l; w ] <<| "sliceAccessExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline accessExpression:
  | e = errorAccessExpression
  | e = memberAccessExpression
  | e = indexAccessExpression
  | e = sliceAccessExpression
    { e }
;

%inline memberAccessExpressionNonBrace:
	| e = memberAccessBaseNonBrace DOT m = member %prec DOT
		{ "memberAccessBaseNonBrace `. member" <| [ e; m ] <<| "memberAccessExpressionNonBrace" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline indexAccessExpressionNonBrace:
	| a = expressionNonBrace L_BRACKET i = expression R_BRACKET
		{ "expressionNonBrace `[ expression ]" <| [ a; i ] <<| "indexAccessExpressionNonBrace" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline sliceAccessExpressionNonBrace:
  | a = expressionNonBrace L_BRACKET h = expression COLON l = expression R_BRACKET
    { "expressionNonBrace `[ expression `: expression ]" <| [ a; h; l ] <<| "sliceAccessExpressionNonBrace" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | a = expressionNonBrace L_BRACKET h = expression PLUSCOLON l = expression R_BRACKET
    { "expressionNonBrace `[ expression `+: expression ]" <| [ a; h; l ] <<| "sliceAccessExpressionNonBrace" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline accessExpressionNonBrace:
  | e = errorAccessExpression
  | e = memberAccessExpressionNonBrace
  | e = indexAccessExpressionNonBrace
  | e = sliceAccessExpressionNonBrace
    { e }
;

(* >> Call expressions *)
%inline callableTarget:
  | e = expression { e }
;

%inline constructorTarget:
	| n = namedType { n }
;

%inline callTarget:
	| t = callableTarget
	| t = constructorTarget
		{ t }
;

%inline callExpression:
	| t = callTarget L_PAREN args = argumentList R_PAREN
		{ "callTarget `( argumentList )" <| [ t; args ] <<| "callExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| t = callableTarget l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ "callTarget `< realTypeArgumentList > `( argumentList )" <| [ t; targs; args ] <<| "callExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) } 
;

%inline callableTargetNonBrace:
  | e = expressionNonBrace { e }
;

%inline callTargetNonBrace:
	| t = callableTargetNonBrace
	| t = constructorTarget
		{ t }
;

%inline callExpressionNonBrace:
	| t = callTargetNonBrace L_PAREN args = argumentList R_PAREN
		{ "callTargetNonBrace `( argumentList )" <| [ t; args ] <<| "callExpressionNonBrace" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| t = callableTargetNonBrace l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ "callTargetNonBrace `< realTypeArgumentList > `( argumentList )" <| [ t; targs; args ] <<| "callExpressionNonBrace" |> Value.Make.with_at (region_of_positions $startpos $endpos) }

(* >> Parenthesized Expressions *)

%inline parenthesizedExpression:
	| L_PAREN e = expression R_PAREN
		{ "`( expression )" <| [ e ] <<| "parenthesizedExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
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
		{ e }
;

expressionList:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "expressionList" }
	| e = expression
    { e }
	| el = expressionList COMMA e = expression
		{ "expressionList `, expression" <| [ el; e ] <<| "expressionList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline memberAccessBase:
	| e = prefixedTypeName
	| e = expression
		{ e }
;

%inline sequenceElementExpression:
	| el = expressionList { el }
;

%inline recordElementExpression:
  | n = name ASSIGN e = expression
    { "name `= expression" <| [ n; e ] <<| "recordElementExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | n = name ASSIGN e = expression COMMA DOTS
    { "name `= expression `, `..." <| [ n; e ] <<| "recordElementExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| n = name ASSIGN e = expression COMMA el = namedExpressionList
    { "name `= expression `, namedExpressionList" <| [ n; e; el ] <<| "recordElementExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | n = name ASSIGN e = expression COMMA el = namedExpressionList COMMA DOTS
    { "name `= expression `, namedExpressionList `, `..." <| [ n; e; el ] <<| "recordElementExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline sequenceOrRecordElementExpression:
	| e = sequenceElementExpression
	| e = recordElementExpression 
    { e }
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
		{ e }
;

%inline memberAccessBaseNonBrace:
	| e = prefixedTypeName
	| e = expressionNonBrace
		{ e }
;

(* Keyset Expressions *)
simpleKeysetExpression:
	| e = expression
    { e }
	| b = expression MASK m = expression
    { "expression `&&& expression" <| [ b; m ] <<| "simpleKeysetExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| l = expression RANGE h = expression
    { "expression `.. expression" <| [ l; h ] <<| "simpleKeysetExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| DEFAULT
    { mk_empty $startpos $endpos "DEFAULT" "simpleKeysetExpression" }
	| DONTCARE
    { mk_empty $startpos $endpos "`_" "simpleKeysetExpression" }
;

simpleKeysetExpressionList:
	| e = simpleKeysetExpression
    { e }
	| el = simpleKeysetExpressionList COMMA e = simpleKeysetExpression
    { "simpleKeysetExpressionList `, simpleKeysetExpression" <| [ el; e ] <<| "simpleKeysetExpressionList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

tupleKeysetExpression:
	| L_PAREN b = expression MASK m = expression R_PAREN
		{ "`( expression `&&& expression )" <| [ b; m ] <<| "tupleKeysetExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| L_PAREN l = expression RANGE h = expression R_PAREN
		{ "`( expression `.. expression )" <| [ l; h ] <<| "tupleKeysetExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| L_PAREN DEFAULT R_PAREN
		{ mk_empty $startpos $endpos "`( DEFAULT )" "tupleKeysetExpression" }
	| L_PAREN DONTCARE R_PAREN
		{ mk_empty $startpos $endpos "`( `_ )" "tupleKeysetExpression" }
	| L_PAREN e = simpleKeysetExpression COMMA es = simpleKeysetExpressionList R_PAREN
		{ "`( simpleKeysetExpression `, simpleKeysetExpressionList )" <| [ e; es ] <<| "tupleKeysetExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

keysetExpression:
	| e = simpleKeysetExpression
	| e = tupleKeysetExpression
    { e }
;

(* Type arguments *)
realTypeArgument:
	| t = typeRef
    { t }
	| VOID
    { mk_empty $startpos $endpos "VOID" "realTypeArgument" }
	| DONTCARE
    { mk_empty $startpos $endpos "`_" "realTypeArgument" }
;

realTypeArgumentList:
	| targ = realTypeArgument
    { targ }
	| targs = realTypeArgumentList COMMA targ = realTypeArgument
    { "realTypeArgumentList `, realTypeArgument" <| [ targs; targ ] <<| "realTypeArgumentList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

typeArgument:
	| t = typeRef
	| t = nonTypeName 
		{ t }
	| VOID
    { mk_empty $startpos $endpos "VOID" "typeArgument" }
	| DONTCARE
    { mk_empty $startpos $endpos "`_" "typeArgument" }
;

typeArgumentList:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "typeArgumentList" }
	| targ = typeArgument
    { targ }
	| targs = typeArgumentList COMMA targ = typeArgument
    { "typeArgumentList `, typeArgument" <| [ targs; targ ] <<| "typeArgumentList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* Arguments *)
argument:
	| e = expression
    { e }
	| n = name ASSIGN e = expression 
		{ "name `= expression" <| [ n; e ] <<| "argument" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| name = name ASSIGN DONTCARE
		{ "name `= `_ " <| [ name ] <<| "argument" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| DONTCARE
		{ mk_empty $startpos $endpos "`_" "argument" }
;

argumentListNonEmpty:
	| arg = argument
    { arg }
	| args = argumentListNonEmpty COMMA arg = argument
    { "argumentListNonEmpty `, argument" <| [ args; arg ] <<| "argumentListNonEmpty" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

argumentList:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "argumentList" }
	| args = argumentListNonEmpty
    { args }
;

(* L-values *)
lvalue:
	| e = referenceExpression
    { e }
	| lv = lvalue DOT m = member %prec DOT
		{ "lvalue `. member" <| [ lv; m ] <<| "lvalue" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| lv = lvalue L_BRACKET i = expression R_BRACKET
		{ "lvalue `[ expression ]" <| [ lv; i ] <<| "lvalue" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| lv = lvalue L_BRACKET h = expression COLON l = expression R_BRACKET
		{ "lvalue `[ expression `: expression ]" <| [ lv; h; l ] <<| "lvalue" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| lv = lvalue L_BRACKET l = expression PLUSCOLON w = expression R_BRACKET
    { "lvalue `[ expression `+: expression ]" <| [ lv; l; w ] <<| "lvalue" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| L_PAREN lv = lvalue R_PAREN
		{ "`( lvalue )" <| [ lv ] <<| "lvalue" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* Statements *)
(* >> Empty statements *)
emptyStatement:
	| SEMICOLON
    { mk_empty $startpos $endpos "`;" "emptyStatement" }
;

(* >> Assignment statements *)
assignop:
	| ASSIGN
    { mk_empty $startpos $endpos "`=" "assignop" }
	| PLUS_ASSIGN
    { mk_empty $startpos $endpos "`+=" "assignop" }
	| PLUS_SAT_ASSIGN
    { mk_empty $startpos $endpos "`|+|=" "assignop" }
	| MINUS_ASSIGN
    { mk_empty $startpos $endpos "`-=" "assignop" }
	| MINUS_SAT_ASSIGN
    { mk_empty $startpos $endpos "`|-|=" "assignop" }
	| MUL_ASSIGN
    { mk_empty $startpos $endpos "`*=" "assignop" }
	| DIV_ASSIGN
    { mk_empty $startpos $endpos "`/=" "assignop" }
	| MOD_ASSIGN
    { mk_empty $startpos $endpos "`%=" "assignop" }
	| SHL_ASSIGN
    { mk_empty $startpos $endpos "`<<=" "assignop" }
	| SHR_ASSIGN
    { mk_empty $startpos $endpos "`>>=" "assignop" }
	| BIT_AND_ASSIGN
    { mk_empty $startpos $endpos "`&=" "assignop" }
	| BIT_XOR_ASSIGN
    { mk_empty $startpos $endpos "`^=" "assignop" }
	| BIT_OR_ASSIGN
    { mk_empty $startpos $endpos "`|=" "assignop" }
;

assignmentStatement:
	| lv = lvalue o = assignop e = expression SEMICOLON
		{ "lvalue assignop expression `;" <| [ lv; o; e ] <<| "assignmentStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Call statements *)
callStatement:
	| lv = lvalue L_PAREN args = argumentList R_PAREN SEMICOLON
		{ "lvalue `( argumentList ) `;" <| [ lv; args ] <<| "callStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN SEMICOLON
		{ "lvalue `< typeArgumentList > `( argumentList ) `;" <| [ lv; targs; args ] <<| "callStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Direct application statements *)
directApplicationStatement:
	| t = namedType DOT APPLY L_PAREN args = argumentList R_PAREN SEMICOLON
    { "namedType `. APPLY `( argumentList ) `;" <| [ t; args ] <<| "directApplicationStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Return statements *)
returnStatement:
	| RETURN SEMICOLON
    { mk_empty $startpos $endpos "RETURN `;" "returnStatement" }
	| RETURN e = expression SEMICOLON
    { "RETURN expression `;" <| [ e ] <<| "returnStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Exit statements *)
exitStatement:
	| EXIT SEMICOLON
    { mk_empty $startpos $endpos "EXIT `;" "exitStatement" }
;

(* >> Block statements *)
blockStatement:
	| al = annotationList L_BRACE
    push_scope
    sl = blockElementStatementList R_BRACE
    pop_scope
		{ "annotationList `{ blockElementStatementList }" <| [ al; sl ] <<| "blockStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Conditional statements *)
conditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { "IF `( expression ) statement" <| [ c; t ] <<| "conditionalStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { "IF `( expression ) statement ELSE statement" <| [ c; t; f ] <<| "conditionalStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> For statements *)
forInitStatement:
	| al = annotationList t = typeRef n = name i = initializerOpt
		{ "annotationList type name initializerOpt" <| [ al; t; n; i ] <<| "forInitStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| lv = lvalue L_PAREN args = argumentList R_PAREN
		{ "lvalue `( argumentList )" <| [ lv; args ] <<| "forInitStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ "lvalue `< typeArgumentList > `( argumentList )" <| [ lv; targs; args ] <<| "forInitStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| lv = lvalue o = assignop e = expression
		{ "lvalue assignop expression" <| [ lv; o; e ] <<| "forInitStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

forInitStatementListNonEmpty:
	| s = forInitStatement { s }
	| sl = forInitStatementListNonEmpty COMMA s = forInitStatement
    { "forInitStatementListNonEmpty `, forInitStatement" <| [ sl; s ] <<| "forInitStatementListNonEmpty" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

forInitStatementList:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "forInitStatementList" }
	| sl = forInitStatementListNonEmpty { sl }
;

forUpdateStatement:
	| s = forInitStatement { s }
;

forUpdateStatementListNonEmpty:
	| s = forUpdateStatement { s }
	| sl = forUpdateStatementListNonEmpty COMMA s = forUpdateStatement
    { "forUpdateStatementListNonEmpty `, forUpdateStatement" <| [ sl; s ] <<| "forUpdateStatementListNonEmpty" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

forUpdateStatementList:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "forUpdateStatementList" }
	| sl = forUpdateStatementListNonEmpty { sl }
;

forCollectionExpression:
	| e = expression { e }
	| l = expression RANGE h = expression
    { "expression `.. expression" <| [ l; h ] <<| "forCollectionExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

forStatement:
  | al = annotationList FOR L_PAREN
    il = forInitStatementList SEMICOLON c = expression SEMICOLON
    ul = forUpdateStatementList R_PAREN b = statement
		{ "annotationList FOR `( forInitStatementList `; expression `; forUpdateStatementList ) statement" <| [ al; il; c; ul; b ] <<| "forStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | al = annotationList FOR L_PAREN
    t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { "annotationList FOR `( typeRef name IN forCollectionExpression ) statement" <| [ al; t; n; e; b ] <<| "forStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | al = annotationList FOR L_PAREN
    al_in = annotationList t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { "annotationList FOR `( annotationList typeRef name IN forCollectionExpression ) statement" <| [ al; al_in; t; n; e; b ] <<| "forStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Switch statements *)
switchLabel:
  | DEFAULT
    { mk_empty $startpos $endpos "DEFAULT" "switchLabel" }
  | e = expressionNonBrace
    { e }
;

switchCase:
  | l = switchLabel COLON s = blockStatement
    { "switchLabel `: blockStatement" <| [ l; s ] <<| "switchCase" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | l = switchLabel COLON
    { "switchLabel `:" <| [ l ] <<| "switchCase" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

switchCaseList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "switchCaseList" }
  | cs = switchCaseList c = switchCase
    { "switchCaseList switchCase" <| [ cs; c ] <<| "switchCaseList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

switchStatement:
  | SWITCH L_PAREN e = expression R_PAREN L_BRACE cs = switchCaseList R_BRACE
    { "SWITCH `( expression ) `{ switchCaseList }" <| [ e; cs ] <<| "switchStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }

(* >> Break and continue statements *)
breakStatement:
  | BREAK SEMICOLON
    { mk_empty $startpos $endpos "BREAK `;" "breakStatement" }
;

continueStatement:
  | CONTINUE SEMICOLON
    { mk_empty $startpos $endpos "CONTINUE `;" "continueStatement" }
;

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
  | s = forStatement
  | s = breakStatement
  | s = continueStatement
  | s = switchStatement
    { s }
;

(* Declarations *)
(* >> Constant and variable declarations *)

(* initializer -> initialValue due to reserved word in OCaml *)
initialValue:
	| ASSIGN e = expression
		{ "`= expression" <| [ e ] <<| "initializer" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

constantDeclaration:
  | al = annotationList CONST t = typeRef n = name i = initialValue SEMICOLON
    { "annotationList CONST typeRef name initializer `;" <| [ al; t; n; i ] <<| "constantDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

initializerOpt:
	| (* empty *)
		{ mk_empty $startpos $endpos "`EMPTY" "initializerOpt" }
	| i = initialValue { i }
;

variableDeclaration:
  | al = annotationList t = typeRef n = name i = initializerOpt SEMICOLON
    { declare_var_of_il n false;
      "annotationList typeRef name initializerOpt `;" <| [ al; t; n; i ] <<| "variableDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

blockElementStatement:
  | d = constantDeclaration
  | d = variableDeclaration
  | d = statement
    { d }
;

blockElementStatementList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "blockElementStatementList" }
  | sl = blockElementStatementList s = blockElementStatement
    { "blockElementStatementList blockElementStatement" <| [ sl; s ] <<| "blockElementStatementList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Function declarations *)
functionPrototype:
	| t = typeOrVoid n = name push_scope
    tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN
    { "typeOrVoid name typeParameterListOpt `( parameterList )" <| [ t; n; tpl; pl ] <<| "functionPrototype" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

functionDeclaration:
	| al = annotationList p = functionPrototype b = blockStatement pop_scope
    { "annotationList functionPrototype blockStatement" <| [ al; p; b ] <<| "functionDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Action declarations *)
actionDeclaration: 
  | al = annotationList ACTION n = name L_PAREN pl = parameterList R_PAREN s = blockStatement
    { "annotationList ACTION name `( parameterList ) blockStatement" <| [ al; n; pl; s ] <<| "actionDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Instantiations *)
objectInitializer:
	| ASSIGN L_BRACE ds = objectDeclarationList R_BRACE
    { "`= `{ objectDeclarationList }" <| [ ds ] <<| "objectInitializer" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

instantiation:
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { "annotationList typeRef `( argumentList ) name `;" <| [ al; t; args; n ] <<| "instantiation" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name
    i = objectInitializer SEMICOLON
    { "annotationList typeRef `( argumentList ) name objectInitializer `;" <| [ al; t; args; n; i ] <<| "instantiation" |> Value.Make.with_at (region_of_positions $startpos $endpos) } 
;

objectDeclaration:
	| d = functionDeclaration
	| d = instantiation
    { d }
;

objectDeclarationList:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "objectDeclarationList" }
	| ds = objectDeclarationList d = objectDeclaration
    { "objectDeclarationList objectDeclaration" <| [ ds; d ] <<| "objectDeclarationList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Error declarations *)
errorDeclaration:
	| ERROR L_BRACE nl = nameList R_BRACE
    { declare_vars_of_il nl;
      "ERROR `{ nameList }" <| [ nl ] <<| "errorDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Match kind declarations *)
matchKindDeclaration:
	| MATCH_KIND L_BRACE nl = nameList c = trailingCommaOpt R_BRACE
    { declare_vars_of_il nl;
      "MATCH_KIND `{ nameList trailingCommaOpt }" <| [ nl; c ] <<| "matchKindDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) } 
;

(* >> Derived type declarations *)
(* >>>> Enum type declarations *)
enumTypeDeclaration:
  | al = annotationList ENUM n = name L_BRACE
    nl = nameList c = trailingCommaOpt R_BRACE
    { "annotationList ENUM name `{ nameList trailingCommaOpt }" <| [ al; n; nl; c ] <<| "enumTypeDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | al = annotationList ENUM t = typeRef n = name L_BRACE
    el = namedExpressionList c = trailingCommaOpt R_BRACE
    { "annotationList ENUM typeRef name `{ namedExpressionList trailingCommaOpt }" <| [ al; t; n; el; c ] <<| "enumTypeDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >>>>>> Struct, header, and union type declarations *)
typeField:
  | al = annotationList t = typeRef n = name SEMICOLON
    { "annotationList typeRef name `;" <| [ al; t; n ] <<| "typeField" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

typeFieldList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "typeFieldList" }
  | fl = typeFieldList f = typeField
    { "typeFieldList typeField" <| [ fl; f ] <<| "typeFieldList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

structTypeDeclaration:
  | al = annotationList STRUCT n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { "annotationList STRUCT name typeParameterListOpt `{ typeFieldList }" <| [ al; n; tpl; fl ] <<| "structTypeDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

headerTypeDeclaration:
  | al = annotationList HEADER n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { "annotationList HEADER name typeParameterListOpt `{ typeFieldList }" <| [ al; n; tpl; fl ] <<| "headerTypeDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

headerUnionTypeDeclaration:
  | al = annotationList HEADER_UNION n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { "annotationList HEADER_UNION name typeParameterListOpt `{ typeFieldList }" <| [ al; n; tpl; fl ] <<| "headerUnionTypeDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

derivedTypeDeclaration:
  | d = enumTypeDeclaration
  | d = structTypeDeclaration
  | d = headerTypeDeclaration
  | d = headerUnionTypeDeclaration
    { d }
;

(* >> Typedef and newtype declarations *)
typedef:
	| t = typeRef
	| t = derivedTypeDeclaration
		{ t }
;

typedefDeclaration:
	| al = annotationList TYPEDEF t = typedef n = name SEMICOLON
    { "annotationList TYPEDEF typedef name `;" <| [ al; t; n ] <<| "typedefDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| al = annotationList TYPE t = typeRef n = name SEMICOLON
    { "annotationList TYPE typeRef name `;" <| [ al; t; n ] <<| "typedefDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Extern declarations *)
externFunctionDeclaration:
	| al = annotationList EXTERN p = functionPrototype pop_scope SEMICOLON
		{ declare_var (id_of_function_prototype p) (has_type_params_function_prototype p);
      "annotationList EXTERN functionPrototype `;" <| [ al; p ] <<| "externFunctionDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline externConstructorPrototype:
	| al = annotationList tid = typeIdentifier L_PAREN pl = parameterList R_PAREN SEMICOLON
    { "annotationList typeIdentifier `( parameterList ) `;" <| [ al; tid; pl ] <<| "externConstructorPrototype" |> Value.Make.with_at (region_of_positions $startpos $endpos) }

%inline externMethodPrototype:
	| al = annotationList p = functionPrototype pop_scope SEMICOLON
    { "annotationList functionPrototype `;" <| [ al; p ] <<| "externMethodPrototype" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| al = annotationList ABSTRACT p = functionPrototype
    pop_scope SEMICOLON
    { "annotationList ABSTRACT functionPrototype `;" <| [ al; p ] <<| "externMethodPrototype" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

externConstructorOrMethodPrototype:
  | p = externConstructorPrototype
  | p = externMethodPrototype
    { p }
;

externConstructorOrMethodPrototypeList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "externConstructorOrMethodPrototypeList" }
  | pl = externConstructorOrMethodPrototypeList p = externConstructorOrMethodPrototype
    { "externConstructorOrMethodPrototypeList externConstructorOrMethodPrototype" <| [ pl; p ] <<| "externConstructorOrMethodPrototypeList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

externObjectDeclaration:
  | al = annotationList EXTERN n = push_externName tpl = typeParameterListOpt
    L_BRACE pl = externConstructorOrMethodPrototypeList R_BRACE pop_scope
    { let decl = "annotationList EXTERN name typeParameterListOpt `{ externConstructorOrMethodPrototypeList }" <| [ al; n; tpl; pl ] <<| "externObjectDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) in
      declare_type_of_il n (has_type_params_declaration decl);
      decl }
;

externDeclaration:
  | d = externFunctionDeclaration
  | d = externObjectDeclaration
    { d }
;

(* >> Parser statements and declarations *)
(* >>>> Select expressions *)
selectCase:
  | k = keysetExpression COLON n = name SEMICOLON
    { "keysetExpression `: name `;" <| [ k; n ] <<| "selectCase" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

selectCaseList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "selectCaseList" }
  | cl = selectCaseList c = selectCase
    { "selectCaseList selectCase" <| [ cl; c ] <<| "selectCaseList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

selectExpression:
  | SELECT L_PAREN el = expressionList R_PAREN L_BRACE cl = selectCaseList R_BRACE
    { "SELECT `( expressionList ) `{ selectCaseList }" <| [ el; cl ] <<| "selectExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >>>> Transition statements *)
stateExpression:
  | n = name SEMICOLON
    { "name `;" <| [ n ] <<| "stateExpression" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | e = selectExpression
    { e }
;

transitionStatement:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "transitionStatement" }
  | TRANSITION e = stateExpression
    { "TRANSITION stateExpression" <| [ e ] <<| "transitionStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >>>> Value set declarations *)
valueSetType:
	| t = baseType
	| t = tupleType
	| t = prefixedTypeName
    { t }
;

valueSetDeclaration:
	| al = annotationList VALUE_SET l_angle t = valueSetType r_angle
    L_PAREN s = expression R_PAREN n = name SEMICOLON
    { "annotationList VALUE_SET `< valueSetType > `( expression ) name `;" <| [ al; t; s; n ] <<| "valueSetDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >>>> Parser type declarations *)
parserTypeDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
      L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { "annotationList PARSER name typeParameterListOpt `( parameterList ) `;" <| [ al; n; tpl; pl ] <<| "parserTypeDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >>>> Parser declarations *)
parserBlockStatement:
  | al = annotationList L_BRACE sl = parserStatementList R_BRACE
    { "annotationList `{ parserStatementList }" <| [ al; sl ] <<| "parserBlockStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

parserConditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = parserStatement %prec THEN
    { "IF `( expression ) parserStatement" <| [ c; t ] <<| "parserConditionalStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| IF L_PAREN c = expression R_PAREN t = parserStatement ELSE f = parserStatement
    { "IF `( expression ) parserStatement ELSE parserStatement" <| [ c; t; f ] <<| "parserConditionalStatement" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

parserStatement:
  | s = constantDeclaration
  | s = variableDeclaration
  | s = emptyStatement
  | s = assignmentStatement
  | s = callStatement
  | s = directApplicationStatement
  | s = parserBlockStatement
  | s = parserConditionalStatement
    { s }
;

parserStatementList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "parserStatementList" }
  | sl = parserStatementList s = parserStatement
    { "parserStatementList parserStatement" <| [ sl; s ] <<| "parserStatementList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

parserState:
  | al = annotationList STATE n = push_name L_BRACE sl = parserStatementList t = transitionStatement R_BRACE
    { "annotationList STATE name `{ parserStatementList transitionStatement }" <| [ al; n; sl; t ] <<| "parserState" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

parserStateList:
  | s = parserState
    { s }
  | sl = parserStateList s = parserState
    { "parserStateList parserState" <| [ sl; s ] <<| "parserStateList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

parserLocalDeclaration:
  | d = constantDeclaration
  | d = instantiation
  | d = variableDeclaration
  | d = valueSetDeclaration
    { d }
;

parserLocalDeclarationList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "parserLocalDeclarationList" }
  | dl = parserLocalDeclarationList d = parserLocalDeclaration
    { "parserLocalDeclarationList parserLocalDeclaration" <| [ dl; d ] <<| "parserLocalDeclarationList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

parserDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
		{ "annotationList PARSER name typeParameterListOpt `( parameterList ) constructorParameterListOpt `{ parserLocalDeclarationList parserStateList }" <| [ al; n; tpl; pl; cpl; dl; sl ] <<| "parserDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Control statements and declarations *)
(* >>>> Table declarations *)
constOpt:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "constOpt" }
  | CONST
    { mk_empty $startpos $endpos "CONST" "constOpt" }
;

(* >>>>>> Table key property *)
tableKey:
  | e = expression COLON n = name al = annotationList SEMICOLON
    { "expression `: name annotationList `;" <| [ e; n; al ] <<| "tableKey" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

tableKeyList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "tableKeyList" }
  | kl = tableKeyList k = tableKey
    { "tableKeyList tableKey" <| [ kl; k ] <<| "tableKeyList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >>>>>> Table actions property *)
tableActionReference:
  | n = prefixedNonTypeName
    { n }
  | n = prefixedNonTypeName L_PAREN al = argumentList R_PAREN
    { "prefixedNonTypeName `( argumentList )" <| [ n; al ] <<| "tableActionReference" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

tableAction:
  | al = annotationList ac = tableActionReference SEMICOLON
    { "annotationList tableActionReference `;" <| [ al; ac ] <<| "tableAction" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

tableActionList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "tableActionList" }
  | acl = tableActionList ac = tableAction
    { "tableActionList tableAction" <| [ acl; ac ] <<| "tableActionList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >>>>>> Table entry property *)
tableEntryPriority:
  | PRIORITY ASSIGN int = integerLiteral COLON
    { "PRIORITY `= integerLiteral `:" <| [ int ] <<| "tableEntryPriority" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | PRIORITY ASSIGN L_PAREN e = expression R_PAREN COLON
    { "PRIORITY `= `( expression ) `:" <| [ e ] <<| "tableEntryPriority" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

tableEntry:
  | c = constOpt p = tableEntryPriority k = keysetExpression COLON ac = tableActionReference
    al = annotationList SEMICOLON
    { "constOpt tableEntryPriority keysetExpression `: tableActionReference annotationList `;" <| [ c; p; k; ac; al ] <<| "tableEntry" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | c = constOpt k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { "constOpt keysetExpression `: tableActionReference annotationList `;" <| [ c; k; ac; al ] <<| "tableEntry" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

tableEntryList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "tableEntryList" }
  | el = tableEntryList e = tableEntry
    { "tableEntryList tableEntry" <| [ el; e ] <<| "tableEntryList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >>>>>> Table properties *)
tableProperty:
  | KEY ASSIGN L_BRACE kl = tableKeyList R_BRACE
    { "KEY `= `{ tableKeyList }" <| [ kl ] <<| "tableProperty" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { "ACTIONS `= `{ tableActionList }" <| [ acl ] <<| "tableProperty" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | al = annotationList c = constOpt ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { "annotationList constOpt ENTRIES `= `{ tableEntryList }" <| [ al; c; el ] <<| "tableProperty" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | al = annotationList c = constOpt n = tableCustomName i = initialValue SEMICOLON
    { "annotationList constOpt tableCustomName initializer `;" <| [ al; c; n; i ] <<| "tableProperty" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

tablePropertyList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "tablePropertyList" }
  | pl = tablePropertyList p = tableProperty
    { "tablePropertyList tableProperty" <| [ pl; p ] <<| "tablePropertyList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

tableDeclaration:
  | al = annotationList TABLE n = name L_BRACE pl = tablePropertyList R_BRACE
    { "annotationList TABLE name `{ tablePropertyList }" <| [ al; n; pl ] <<| "tableDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }

(* >>>> Control type declarations *)
controlTypeDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { "annotationList CONTROL name typeParameterListOpt `( parameterList ) `;" <| [ al; n; tpl; pl ] <<| "controlTypeDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >>>> Control declarations *)
controlBody:
  | b = blockStatement { b }
;

controlLocalDeclaration:
  | d = constantDeclaration 
  | d = instantiation 
  | d = variableDeclaration
    { d }
  | d = actionDeclaration
  | d = tableDeclaration
    { declare_var (id_of_declaration d) false;
      d }
;

controlLocalDeclarationList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "controlLocalDeclarationList" }
  | dl = controlLocalDeclarationList d = controlLocalDeclaration
    { "controlLocalDeclarationList controlLocalDeclaration" <| [ dl; d ] <<| "controlLocalDeclarationList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

controlDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { "annotationList CONTROL name typeParameterListOpt `( parameterList ) constructorParameterListOpt `{ controlLocalDeclarationList APPLY controlBody }" <| [ al; n; tpl; pl; cpl; dl; b ] <<| "controlDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Package type declarations *)
packageTypeDeclaration:
  | al = annotationList PACKAGE n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { "annotationList PACKAGE name typeParameterListOpt `( parameterList ) `;" <| [ al; n; tpl; pl ] <<| "packageTypeDeclaration" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

(* >> Type declarations *)
typeDeclaration:
  | d = derivedTypeDeclaration
  | d = typedefDeclaration
  | d = parserTypeDeclaration
  | d = controlTypeDeclaration
  | d = packageTypeDeclaration
    { d }
;

(* >> Declarations *)
declaration:
  | const = constantDeclaration
    { declare_var (id_of_declaration const) (has_type_params_declaration const);
      const }
  | inst = instantiation
    { declare_var (id_of_declaration inst) false;
      inst }
  | func = functionDeclaration
    { declare_var (id_of_declaration func) (has_type_params_declaration func);
      func }
  | action = actionDeclaration
    { declare_var (id_of_declaration action) false;
      action }
  | d = errorDeclaration
  | d = matchKindDeclaration
  | d = externDeclaration
    { d }
  | d = parserDeclaration
  | d = controlDeclaration
  | d = typeDeclaration
    { declare_type (id_of_declaration d) (has_type_params_declaration d);
      d }
;

(* Annotations *)
annotationToken:
	| UNEXPECTED_TOKEN
    { mk_empty $startpos $endpos "UNEXPECTED_TOKEN" "annotationToken" }
	| ABSTRACT
    { mk_empty $startpos $endpos "ABSTRACT" "annotationToken" }
	| ACTION
    { mk_empty $startpos $endpos "ACTION" "annotationToken" }
	| ACTIONS
    { mk_empty $startpos $endpos "ACTIONS" "annotationToken" }
	| APPLY
    { mk_empty $startpos $endpos "APPLY" "annotationToken" }
	| BOOL
    { mk_empty $startpos $endpos "BOOL" "annotationToken" }
	| BIT
    { mk_empty $startpos $endpos "BIT" "annotationToken" }
	| BREAK
    { mk_empty $startpos $endpos "BREAK" "annotationToken" }
	| CONST
    { mk_empty $startpos $endpos "CONST" "annotationToken" }
	| CONTINUE
    { mk_empty $startpos $endpos "CONTINUE" "annotationToken" }
	| CONTROL
    { mk_empty $startpos $endpos "CONTROL" "annotationToken" }
	| DEFAULT
    { mk_empty $startpos $endpos "DEFAULT" "annotationToken" }
	| ELSE
    { mk_empty $startpos $endpos "ELSE" "annotationToken" }
	| ENTRIES
    { mk_empty $startpos $endpos "ENTRIES" "annotationToken" }
	| ENUM
    { mk_empty $startpos $endpos "ENUM" "annotationToken" }
	| ERROR
    { mk_empty $startpos $endpos "ERROR" "annotationToken" }
	| EXIT
    { mk_empty $startpos $endpos "EXIT" "annotationToken" }
	| EXTERN
    { mk_empty $startpos $endpos "EXTERN" "annotationToken" }
	| FALSE
    { mk_empty $startpos $endpos "FALSE" "annotationToken" }
	| FOR
    { mk_empty $startpos $endpos "FOR" "annotationToken" }
	| HEADER
    { mk_empty $startpos $endpos "HEADER" "annotationToken" }
	| HEADER_UNION
    { mk_empty $startpos $endpos "HEADER_UNION" "annotationToken" }
	| IF
    { mk_empty $startpos $endpos "IF" "annotationToken" }
	| IN
    { mk_empty $startpos $endpos "IN" "annotationToken" }
	| INOUT
    { mk_empty $startpos $endpos "INOUT" "annotationToken" }
	| INT
    { mk_empty $startpos $endpos "INT" "annotationToken" }
	| KEY
    { mk_empty $startpos $endpos "KEY" "annotationToken" }
	| MATCH_KIND
    { mk_empty $startpos $endpos "MATCH_KIND" "annotationToken" }
	| TYPE
    { mk_empty $startpos $endpos "TYPE" "annotationToken" }
	| OUT
    { mk_empty $startpos $endpos "OUT" "annotationToken" }
	| PARSER
    { mk_empty $startpos $endpos "PARSER" "annotationToken" }
	| PACKAGE
    { mk_empty $startpos $endpos "PACKAGE" "annotationToken" }
	| PRAGMA
    { mk_empty $startpos $endpos "PRAGMA" "annotationToken" }
	| RETURN
    { mk_empty $startpos $endpos "RETURN" "annotationToken" }
	| SELECT
    { mk_empty $startpos $endpos "SELECT" "annotationToken" }
	| STATE
    { mk_empty $startpos $endpos "STATE" "annotationToken" }
	| STRING
    { mk_empty $startpos $endpos "STRING" "annotationToken" }
	| STRUCT
    { mk_empty $startpos $endpos "STRUCT" "annotationToken" }
	| SWITCH
    { mk_empty $startpos $endpos "SWITCH" "annotationToken" }
	| TABLE
    { mk_empty $startpos $endpos "TABLE" "annotationToken" }
	| THIS
    { mk_empty $startpos $endpos "THIS" "annotationToken" }
	| TRANSITION
    { mk_empty $startpos $endpos "TRANSITION" "annotationToken" }
	| TRUE
    { mk_empty $startpos $endpos "TRUE" "annotationToken" }
	| TUPLE
    { mk_empty $startpos $endpos "TUPLE" "annotationToken" }
	| TYPEDEF
    { mk_empty $startpos $endpos "TYPEDEF" "annotationToken" }
	| VARBIT
    { mk_empty $startpos $endpos "VARBIT" "annotationToken" }
	| VALUE_SET
    { mk_empty $startpos $endpos "VALUE_SET" "annotationToken" }
	| LIST
    { mk_empty $startpos $endpos "LIST" "annotationToken" }
	| VOID
    { mk_empty $startpos $endpos "VOID" "annotationToken" }
	| DONTCARE
    { mk_empty $startpos $endpos "`_" "annotationToken" }
	| id = identifier
    { id }
	| tid = typeIdentifier
    { tid }
	| str = stringLiteral
    { str }
	| int = integerLiteral
    { int }
	| MASK
    { mk_empty $startpos $endpos "`&&&" "annotationToken" }
  (* TODO: missing DOTS "..." in spec *)
	| RANGE
    { mk_empty $startpos $endpos "`.." "annotationToken" }
	| SHL
    { mk_empty $startpos $endpos "`<<" "annotationToken" }
	| AND
    { mk_empty $startpos $endpos "`&&" "annotationToken" }
	| OR
    { mk_empty $startpos $endpos "`||" "annotationToken" }
	| EQ
    { mk_empty $startpos $endpos "`==" "annotationToken" }
	| NE
    { mk_empty $startpos $endpos "`!=" "annotationToken" }
	| GE
    { mk_empty $startpos $endpos "`>=" "annotationToken" }
	| LE
    { mk_empty $startpos $endpos "`<=" "annotationToken" }
	| PLUSPLUS
    { mk_empty $startpos $endpos "`++" "annotationToken" }
	| PLUS
    { mk_empty $startpos $endpos "`+" "annotationToken" }
	| PLUS_SAT
    { mk_empty $startpos $endpos "`|+|" "annotationToken" }
	| MINUS
    { mk_empty $startpos $endpos "`-" "annotationToken" }
	| MINUS_SAT
    { mk_empty $startpos $endpos "`|-|" "annotationToken" }
	| MUL
    { mk_empty $startpos $endpos "`*" "annotationToken" }
	| DIV
    { mk_empty $startpos $endpos "`/" "annotationToken" }
	| MOD
    { mk_empty $startpos $endpos "`%" "annotationToken" }
	| BIT_OR
    { mk_empty $startpos $endpos "`|" "annotationToken" }
	| BIT_AND
    { mk_empty $startpos $endpos "`&" "annotationToken" }
	| BIT_XOR
    { mk_empty $startpos $endpos "`^" "annotationToken" }
	| COMPLEMENT
    { mk_empty $startpos $endpos "`~" "annotationToken" }
	| L_BRACKET
    { mk_empty $startpos $endpos "``[" "annotationToken" }
	| R_BRACKET
    { mk_empty $startpos $endpos "``]" "annotationToken" }
	| L_BRACE
    { mk_empty $startpos $endpos "``{" "annotationToken" }
	| R_BRACE
    { mk_empty $startpos $endpos "``}" "annotationToken" }
	| L_ANGLE
    { mk_empty $startpos $endpos "``<" "annotationToken" }
	| R_ANGLE
    { mk_empty $startpos $endpos "``>" "annotationToken" }
	| NOT
    { mk_empty $startpos $endpos "`!" "annotationToken" }
	| COLON
    { mk_empty $startpos $endpos "`:" "annotationToken" }
	| COMMA
    { mk_empty $startpos $endpos "`," "annotationToken" }
	| QUESTION
    { mk_empty $startpos $endpos "`?" "annotationToken" }
	| DOT
    { mk_empty $startpos $endpos "`." "annotationToken" }
	| ASSIGN
    { mk_empty $startpos $endpos "`=" "annotationToken" }
	| SEMICOLON
    { mk_empty $startpos $endpos "`;" "annotationToken" }
	| AT
    { mk_empty $startpos $endpos "`@" "annotationToken" }
;

annotationBody:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "annotationBody" }
	| ab = annotationBody L_PAREN ab_in = annotationBody R_PAREN
    { "annotationBody `( annotationBody )" <| [ ab; ab_in ] <<| "annotationBody" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| ab = annotationBody at = annotationToken
    { "annotationBody annotationToken" <| [ ab; at ] <<| "annotationBody" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

structuredAnnotationBody:
	| e = sequenceOrRecordElementExpression c = trailingCommaOpt
    { "sequenceOrRecordElementExpression trailingCommaOpt" <| [ e; c ] <<| "structuredAnnotationBody" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

annotation:
	| AT name = name
    { "`@ name" <| [ name ] <<| "annotation" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| AT name = name L_PAREN body = annotationBody R_PAREN
    { "`@ name `( annotationBody )" <| [ name; body ] <<| "annotation" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
	| AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { "`@ name `[ structuredAnnotationBody ]" <| [ name; body ] <<| "annotation" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
(* From Petr4: PRAGMA not in Spec, but in Petr4/p4c *)
	| PRAGMA name = name body = annotationBody PRAGMA_END
    { "`@ PRAGMA name annotationBody" <| [ name; body ] <<| "annotation" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

annotationListNonEmpty:
	| a = annotation
    { a }
	| al = annotationListNonEmpty a = annotation
		{ "annotationListNonEmpty annotation" <| [ al; a ] <<| "annotationListNonEmpty" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

%inline annotationList:
	| (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "annotationList" }
	| al = annotationListNonEmpty
    { al }
;

(******** P4 program ********)
declarationList:
  | (* empty *)
    { mk_empty $startpos $endpos "`EMPTY" "declarationList" }
  | ds = declarationList d = declaration
    { "declarationList declaration" <| [ ds; d ] <<| "declarationList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
  | ds = declarationList SEMICOLON
    { "declarationList `;" <| [ ds ] <<| "declarationList" |> Value.Make.with_at (region_of_positions $startpos $endpos) }
;

p4program:
	| dl = declarationList END { dl #@@ "p4program" }
