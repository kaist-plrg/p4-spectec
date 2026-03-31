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
    { "`EMPTY" <| [] <<| "trailingCommaOpt" }
	| COMMA
    { "`," <| [] <<| "trailingCommaOpt" }
;

(* Booleans *)
%inline booleanLiteral:
  | TRUE
    { "TRUE" <| [] <<| "booleanLiteral" }
  | FALSE
    { "FALSE" <| [] <<| "booleanLiteral" }

(* Integers *)
integerLiteral:
	| int = int
    { "D int" <| [ int ] <<| "integerLiteral" }
(* Processed by lexer *)
	| number = NUMBER
    { fst number }
;

(* Strings *)
stringLiteral:
	| text = STRING_LITERAL
    { "`\" text `\"" <| [ text ] <<| "stringLiteral" }
;

(* Names *)
identifier:
	| text = NAME IDENTIFIER
    { "`ID text" <| [ text ] <<| "identifier" }
;

typeIdentifier:
	| text = NAME TYPENAME
    { "`TID text" <| [ text ] <<| "typeIdentifier" }
;

(* >> Non-type names *)
nonTypeName:
	| id = identifier
    { id }
	| APPLY
    { "APPLY" <| [] <<| "nonTypeName" }
	| KEY
    { "KEY" <| [] <<| "nonTypeName" }
	| ACTIONS
    { "ACTIONS" <| [] <<| "nonTypeName" }
	| STATE
    { "STATE" <| [] <<| "nonTypeName" }
	| ENTRIES
    { "ENTRIES" <| [] <<| "nonTypeName" }
	| TYPE
    { "TYPE" <| [] <<| "nonTypeName" }
	| PRIORITY
    { "PRIORITY" <| [] <<| "nonTypeName" }
;

prefixedNonTypeName:
	| n = nonTypeName
    { n }
	| DOT go_toplevel n = nonTypeName go_local
    { "`ID `. nonTypeName" <| [ n ] <<| "prefixedNonTypeName" }
;

(* >> Type names *)
typeName:
	| n = typeIdentifier { n }
;

prefixedTypeName:
	| n = typeName
    { n }
	| DOT go_toplevel tid = typeName go_local
    { "`TID `. typeName" <| [ tid ] <<| "prefixedTypeName" }
;

(* >> Table custom property names *)
tableCustomName:
	| id = identifier
    { id }
	| tid = typeIdentifier
    { tid }
	| APPLY
    { "APPLY" <| [] <<| "tableCustomName" }
	| STATE
    { "STATE" <| [] <<| "tableCustomName" }
	| TYPE
    { "TYPE" <| [] <<| "tableCustomName" }
	| PRIORITY
    { "PRIORITY" <| [] <<| "tableCustomName" }
;

(* >> Names *)
name:
	| n = nonTypeName
	| n = typeName
    { n }
	| LIST
    { "LIST" <| [] <<| "name" }
;

nameList:
	| n = name
    { n }
	| ns = nameList COMMA n = name
    { "nameList `, name" <| [ ns; n ] <<| "nameList" }
;

member:
	| name = name
    { name }
;

(* Directions *)
direction:
	| (* empty *)
    { "`EMPTY" <| [] <<| "direction" }
	| IN
    { "IN" <| [] <<| "direction" }
	| OUT
    { "OUT" <| [] <<| "direction" }
	| INOUT
    { "INOUT" <| [] <<| "direction" }
;

(* Types *)
(* >> Base types *)
baseType:
	| BOOL
    { "BOOL" <| [] <<| "baseType" }
	| MATCH_KIND
    { "MATCH_KIND" <| [] <<| "baseType" }
	| ERROR
    { "ERROR" <| [] <<| "baseType" }
	| BIT
    { "BIT" <| [] <<| "baseType" }
	| STRING
    { "STRING" <| [] <<| "baseType" }
	| INT
    { "INT" <| [] <<| "baseType" }
	| BIT l_angle v = int r_angle
    { "BIT `< int >" <| [ v ] <<| "baseType" }
	| INT l_angle v = int r_angle
    { "INT `< int >" <| [ v ] <<| "baseType" }
	| VARBIT l_angle v = int r_angle
    { "VARBIT `< int >" <| [ v ] <<| "baseType" }
	| BIT l_angle L_PAREN e = expression R_PAREN r_angle
    { "BIT `< `( expression ) >" <| [ e ] <<| "baseType" }
	| INT l_angle L_PAREN e = expression R_PAREN r_angle
    { "INT `< `( expression ) >" <| [ e ] <<| "baseType" }
	| VARBIT l_angle L_PAREN e = expression R_PAREN r_angle
    { "VARBIT `< `( expression ) >" <| [ e ] <<| "baseType" }
;

(* >> Named types *)
specializedType:
  | n = prefixedTypeName l_angle tal = typeArgumentList r_angle
    { "prefixedTypeName `< typeArgumentList >" <| [ n; tal ] <<| "specializedType" }
;

namedType:
  | t = prefixedTypeName
  | t = specializedType
    { t }
;

(* >> Header stack types *)
headerStackType:
  | t = namedType L_BRACKET e = expression R_BRACKET
    { "namedType `[ expression ]" <| [ t; e ] <<| "headerStackType" }
;

(* >> List types *)
listType:
  | LIST l_angle targ = typeArgument r_angle
    { "LIST `< typeArgument >" <| [ targ ] <<| "listType" }
;

(* >> Tuple types *)
tupleType:
	| TUPLE l_angle targs = typeArgumentList r_angle
    { "TUPLE `< typeArgumentList >" <| [ targs ] <<| "tupleType" }
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
    { "VOID" <| [] <<| "typeOrVoid" }
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
    { "typeParameterList `, typeParameter" <| [ tps; tp ] <<| "typeParameterList" }
;

typeParameterListOpt:
	| (* empty *)
    { "`EMPTY" <| [] <<| "typeParameterListOpt" }
	| l_angle tps = typeParameterList r_angle
    { declare_types_of_il tps;
      "`< typeParameterList >" <| [ tps ] <<| "typeParameterListOpt" }
;

(* Parameters *)
parameter:
	| al = annotationList dir = direction t = typeRef n = name i = initializerOpt
		{ declare_var_of_il n false;
      "annotationList direction type name initializerOpt" <| [ al; dir; t; n; i ] <<| "parameter" }
;

nonEmptyParameterList:
	| p = parameter
    { p }
	| ps = nonEmptyParameterList COMMA p = parameter
    { "nonEmptyParameterList `, parameter" <| [ ps; p ] <<| "nonEmptyParameterList" }
;

parameterList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "parameterList" }
	| ps = nonEmptyParameterList
    { ps }
;

(* Constructor parameters *)
constructorParameterListOpt:
	| (* empty *)
    { "`EMPTY" <| [] <<| "constructorParameterListOpt" }
	| L_PAREN ps = parameterList R_PAREN
    { "`( parameterList )" <| [ ps ] <<| "constructorParameterListOpt" }
;

(* Expression key-value pairs *)
namedExpression:
	| n = name ASSIGN e = expression
    { "name `= expression" <| [ n; e ] <<| "namedExpression" }
;

namedExpressionList:
	| e = namedExpression
    { e }
	| es = namedExpressionList COMMA e = namedExpression
    { "namedExpressionList `, namedExpression" <| [ es; e ] <<| "namedExpressionList" }
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
    { "THIS" <| [] <<| "referenceExpression" }
;

(* >> Default expressions *)
%inline defaultExpression:
	| DOTS
    { "`..." <| [] <<| "defaultExpression" }
;

(* >> Unary, binary, and ternary expressions *)
%inline unop: 
	| NOT
    { "`!" <| [] <<| "unop" }
	| COMPLEMENT
    { "`~" <| [] <<| "unop" }
	| MINUS
    { "`-" <| [] <<| "unop" }
	| PLUS
    { "`+" <| [] <<| "unop" }
;

%inline unaryExpression:
	| o = unop e = expression %prec PREFIX
		{ "unop expression" <| [ o; e ] <<| "unaryExpression" }
;

%inline binop:
  | MUL
    { "`*" <| [] <<| "binop" }
  | DIV
    { "`/" <| [] <<| "binop" }
  | MOD
    { "`%" <| [] <<| "binop" }
  | PLUS
    { "`+" <| [] <<| "binop" }
  | PLUS_SAT
    { "`|+|" <| [] <<| "binop" }
  | MINUS
    { "`-" <| [] <<| "binop" }
  | MINUS_SAT
    { "`|-|" <| [] <<| "binop" }
  | SHL
    { "`<<" <| [] <<| "binop" }
  | r_angle R_ANGLE_SHIFT
    { "`>>" <| [] <<| "binop" }
  | LE
    { "`<=" <| [] <<| "binop" }
  | GE
    { "`>=" <| [] <<| "binop" }
  | l_angle
    { "``<" <| [] <<| "binop" }
  | r_angle
    { "``>" <| [] <<| "binop" }
  | NE
    { "`!=" <| [] <<| "binop" }
  | EQ
    { "`==" <| [] <<| "binop" }
  | BIT_AND
    { "`&" <| [] <<| "binop" }
  | BIT_XOR
    { "`^" <| [] <<| "binop" }
  | BIT_OR
    { "`|" <| [] <<| "binop" }
  | PLUSPLUS
    { "`++" <| [] <<| "binop" }
  | AND
    { "`&&" <| [] <<| "binop" }
  | OR
    { "`||" <| [] <<| "binop" }
;

%inline binaryExpression:
	| l = expression o = binop r = expression
		{ "expression binop expression" <| [ l; o; r ] <<| "binaryExpression" }
;

%inline binaryExpressionNonBrace:
	| l = expressionNonBrace o = binop r = expression
		{ "expressionNonBrace binop expression" <| [ l; o; r ] <<| "binaryExpressionNonBrace" }
;

%inline ternaryExpression:
	| c = expression QUESTION t = expression COLON f = expression
		{ "expression `? expression `: expression" <| [ c; t; f ] <<| "ternaryExpression" }
;

%inline ternaryExpressionNonBrace:
	| c = expressionNonBrace QUESTION t = expression COLON f = expression
		{ "expressionNonBrace `? expression `: expression" <| [ c; t; f ] <<| "ternaryExpressionNonBrace" }
;

(* >> Cast expressions *)
%inline castExpression:
	| L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
    { "`( typeRef ) expression" <| [ t; e ] <<| "castExpression" }
;

(* >> Data (aggregate) expressions *)
%inline dataExpression:
	| INVALID
    { "`{#}" <| [] <<| "invalidHeaderExpression" }
	| L_BRACE e = sequenceOrRecordElementExpression c = trailingCommaOpt R_BRACE
    { "`{ sequenceOrRecordElementExpression trailingCommaOpt }" <| [ e; c ] <<| "sequenceOrRecordExpression" }
;

(* >> Member and index access expressions *)
%inline errorAccessExpression:
	| ERROR DOT m = member
		{ "ERROR `. member" <| [ m ] <<| "errorAccessExpression" }
;

%inline memberAccessExpression:
	| e = memberAccessBase DOT m = member %prec DOT
		{ "memberAccessBase `. member" <| [ e; m ] <<| "memberAccessExpression" }
;

%inline indexAccessExpression:
	| a = expression L_BRACKET i = expression R_BRACKET
		{ "expression `[ expression ]" <| [ a; i ] <<| "indexAccessExpression" }
;

%inline sliceop:
  | COLON
    { "`:" <| [] <<| "sliceop" }
  | PLUSCOLON
    { "`+:" <| [] <<| "sliceop" }

%inline sliceAccessExpression:
  | a = expression L_BRACKET l = expression s = sliceop r = expression R_BRACKET
    { "expression `[ expression sliceop expression ]" <| [ a; l; s; r ] <<| "sliceAccessExpression" }
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
		{ "memberAccessBaseNonBrace `. member" <| [ e; m ] <<| "memberAccessExpressionNonBrace" }
;

%inline indexAccessExpressionNonBrace:
	| a = expressionNonBrace L_BRACKET i = expression R_BRACKET
		{ "expressionNonBrace `[ expression ]" <| [ a; i ] <<| "indexAccessExpressionNonBrace" }
;

%inline sliceAccessExpressionNonBrace:
  | a = expressionNonBrace L_BRACKET l = expression s = sliceop r = expression R_BRACKET
    { "expressionNonBrace `[ expression sliceop expression ]" <| [ a; l; s; r ] <<| "sliceAccessExpressionNonBrace" }
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
		{ "callTarget `( argumentList )" <| [ t; args ] <<| "callExpression" }
	| t = callableTarget l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ "callTarget `< realTypeArgumentList > `( argumentList )" <| [ t; targs; args ] <<| "callExpression" } 
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
		{ "callTargetNonBrace `( argumentList )" <| [ t; args ] <<| "callExpressionNonBrace" }
	| t = callableTargetNonBrace l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ "callTargetNonBrace `< realTypeArgumentList > `( argumentList )" <| [ t; targs; args ] <<| "callExpressionNonBrace" }

(* >> Parenthesized Expressions *)

%inline parenthesizedExpression:
	| L_PAREN e = expression R_PAREN
		{ "`( expression )" <| [ e ] <<| "parenthesizedExpression" }
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
    { "`EMPTY" <| [] <<| "expressionList" }
	| e = expression
    { e }
	| el = expressionList COMMA e = expression
		{ "expressionList `, expression" <| [ el; e ] <<| "expressionList" }
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
    { "name `= expression" <| [ n; e ] <<| "recordElementExpression" }
  | n = name ASSIGN e = expression COMMA DOTS
    { "name `= expression `, `..." <| [ n; e ] <<| "recordElementExpression" }
	| n = name ASSIGN e = expression COMMA el = namedExpressionList
    { "name `= expression `, namedExpressionList" <| [ n; e; el ] <<| "recordElementExpression" }
  | n = name ASSIGN e = expression COMMA el = namedExpressionList COMMA DOTS
    { "name `= expression `, namedExpressionList `, `..." <| [ n; e; el ] <<| "recordElementExpression" }
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
    { "expression `&&& expression" <| [ b; m ] <<| "simpleKeysetExpression" }
	| l = expression RANGE h = expression
    { "expression `.. expression" <| [ l; h ] <<| "simpleKeysetExpression" }
	| DEFAULT
    { "DEFAULT" <| [] <<| "simpleKeysetExpression" }
	| DONTCARE
    { "`_" <| [] <<| "simpleKeysetExpression" }
;

simpleKeysetExpressionList:
	| e = simpleKeysetExpression
    { e }
	| el = simpleKeysetExpressionList COMMA e = simpleKeysetExpression
    { "simpleKeysetExpressionList `, simpleKeysetExpression" <| [ el; e ] <<| "simpleKeysetExpressionList" }
;

tupleKeysetExpression:
	| L_PAREN b = expression MASK m = expression R_PAREN
		{ "`( expression `&&& expression )" <| [ b; m ] <<| "tupleKeysetExpression" }
	| L_PAREN l = expression RANGE h = expression R_PAREN
		{ "`( expression `.. expression )" <| [ l; h ] <<| "tupleKeysetExpression" }
	| L_PAREN DEFAULT R_PAREN
		{ "`( DEFAULT )" <| [] <<| "tupleKeysetExpression" }
	| L_PAREN DONTCARE R_PAREN
		{ "`( `_ )" <| [] <<| "tupleKeysetExpression" }
	| L_PAREN e = simpleKeysetExpression COMMA es = simpleKeysetExpressionList R_PAREN
		{ "`( simpleKeysetExpression `, simpleKeysetExpressionList )" <| [ e; es ] <<| "tupleKeysetExpression" }
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
    { "VOID" <| [] <<| "realTypeArgument" }
	| DONTCARE
    { "`_" <| [] <<| "realTypeArgument" }
;

realTypeArgumentList:
	| targ = realTypeArgument
    { targ }
	| targs = realTypeArgumentList COMMA targ = realTypeArgument
    { "realTypeArgumentList `, realTypeArgument" <| [ targs; targ ] <<| "realTypeArgumentList" }
;

typeArgument:
	| t = typeRef
	| t = nonTypeName 
		{ t }
	| VOID
    { "VOID" <| [] <<| "typeArgument" }
	| DONTCARE
    { "`_" <| [] <<| "typeArgument" }
;

typeArgumentList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "typeArgumentList" }
	| targ = typeArgument
    { targ }
	| targs = typeArgumentList COMMA targ = typeArgument
    { "typeArgumentList `, typeArgument" <| [ targs; targ ] <<| "typeArgumentList" }
;

(* Arguments *)
argument:
	| e = expression
    { e }
	| n = name ASSIGN e = expression 
		{ "name `= expression" <| [ n; e ] <<| "argument" }
	| name = name ASSIGN DONTCARE
		{ "name `= `_ " <| [ name ] <<| "argument" }
	| DONTCARE
		{ "`_" <| [] <<| "argument" }
;

argumentListNonEmpty:
	| arg = argument
    { arg }
	| args = argumentListNonEmpty COMMA arg = argument
    { "argumentListNonEmpty `, argument" <| [ args; arg ] <<| "argumentListNonEmpty" }
;

argumentList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "argumentList" }
	| args = argumentListNonEmpty
    { args }
;

(* L-values *)
lvalue:
	| e = referenceExpression
    { e }
	| lv = lvalue DOT m = member %prec DOT
		{ "lvalue `. member" <| [ lv; m ] <<| "lvalue" }
	| lv = lvalue L_BRACKET i = expression R_BRACKET
		{ "lvalue `[ expression ]" <| [ lv; i ] <<| "lvalue" }
	| lv = lvalue L_BRACKET l = expression s = sliceop r = expression R_BRACKET
    { "lvalue `[ expression sliceop expression ]" <| [ lv; l; s; r ] <<| "lvalue" }
	| L_PAREN lv = lvalue R_PAREN
		{ "`( lvalue )" <| [ lv ] <<| "lvalue" }
;

(* Statements *)
(* >> Empty statements *)
emptyStatement:
	| SEMICOLON
    { "`;" <| [] <<| "emptyStatement" }
;

(* >> Assignment statements *)
assignop:
	| ASSIGN
    { "`=" <| [] <<| "assignop" }
	| PLUS_ASSIGN
    { "`+=" <| [] <<| "assignop" }
	| PLUS_SAT_ASSIGN
    { "`|+|=" <| [] <<| "assignop" }
	| MINUS_ASSIGN
    { "`-=" <| [] <<| "assignop" }
	| MINUS_SAT_ASSIGN
    { "`|-|=" <| [] <<| "assignop" }
	| MUL_ASSIGN
    { "`*=" <| [] <<| "assignop" }
	| DIV_ASSIGN
    { "`/=" <| [] <<| "assignop" }
	| MOD_ASSIGN
    { "`%=" <| [] <<| "assignop" }
	| SHL_ASSIGN
    { "`<<=" <| [] <<| "assignop" }
	| SHR_ASSIGN
    { "`>>=" <| [] <<| "assignop" }
	| BIT_AND_ASSIGN
    { "`&=" <| [] <<| "assignop" }
	| BIT_XOR_ASSIGN
    { "`^=" <| [] <<| "assignop" }
	| BIT_OR_ASSIGN
    { "`|=" <| [] <<| "assignop" }
;

assignmentStatement:
	| lv = lvalue o = assignop e = expression SEMICOLON
		{ "lvalue assignop expression `;" <| [ lv; o; e ] <<| "assignmentStatement" }
;

(* >> Call statements *)
callStatement:
	| lv = lvalue L_PAREN args = argumentList R_PAREN SEMICOLON
		{ "lvalue `( argumentList ) `;" <| [ lv; args ] <<| "callStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN SEMICOLON
		{ "lvalue `< typeArgumentList > `( argumentList ) `;" <| [ lv; targs; args ] <<| "callStatement" }
;

(* >> Direct application statements *)
directApplicationStatement:
	| t = namedType DOT APPLY L_PAREN args = argumentList R_PAREN SEMICOLON
    { "namedType `. APPLY `( argumentList ) `;" <| [ t; args ] <<| "directApplicationStatement" }
;

(* >> Return statements *)
returnStatement:
	| RETURN SEMICOLON
    { "RETURN `;" <| [] <<| "returnStatement" }
	| RETURN e = expression SEMICOLON
    { "RETURN expression `;" <| [ e ] <<| "returnStatement" }
;

(* >> Exit statements *)
exitStatement:
	| EXIT SEMICOLON
    { "EXIT `;" <| [] <<| "exitStatement" }
;

(* >> Block statements *)
blockStatement:
	| al = annotationList L_BRACE
    push_scope
    sl = blockElementStatementList R_BRACE
    pop_scope
		{ "annotationList `{ blockElementStatementList }" <| [ al; sl ] <<| "blockStatement" }
;

(* >> Conditional statements *)
conditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { "IF `( expression ) statement" <| [ c; t ] <<| "conditionalStatement" }
	| IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { "IF `( expression ) statement ELSE statement" <| [ c; t; f ] <<| "conditionalStatement" }
;

(* >> For statements *)
forInitStatement:
	| al = annotationList t = typeRef n = name i = initializerOpt
		{ "annotationList type name initializerOpt" <| [ al; t; n; i ] <<| "forInitStatement" }
	| lv = lvalue L_PAREN args = argumentList R_PAREN
		{ "lvalue `( argumentList )" <| [ lv; args ] <<| "forInitStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ "lvalue `< typeArgumentList > `( argumentList )" <| [ lv; targs; args ] <<| "forInitStatement" }
	| lv = lvalue o = assignop e = expression
		{ "lvalue assignop expression" <| [ lv; o; e ] <<| "forInitStatement" }
;

forInitStatementListNonEmpty:
	| s = forInitStatement { s }
	| sl = forInitStatementListNonEmpty COMMA s = forInitStatement
    { "forInitStatementListNonEmpty `, forInitStatement" <| [ sl; s ] <<| "forInitStatementListNonEmpty" }
;

forInitStatementList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "forInitStatementList" }
	| sl = forInitStatementListNonEmpty { sl }
;

forUpdateStatement:
	| s = forInitStatement { s }
;

forUpdateStatementListNonEmpty:
	| s = forUpdateStatement { s }
	| sl = forUpdateStatementListNonEmpty COMMA s = forUpdateStatement
    { "forUpdateStatementListNonEmpty `, forUpdateStatement" <| [ sl; s ] <<| "forUpdateStatementListNonEmpty" }
;

forUpdateStatementList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "forUpdateStatementList" }
	| sl = forUpdateStatementListNonEmpty { sl }
;

forCollectionExpression:
	| e = expression { e }
	| l = expression RANGE h = expression
    { "expression `.. expression" <| [ l; h ] <<| "forCollectionExpression" }
;

forStatement:
  | al = annotationList FOR L_PAREN
    il = forInitStatementList SEMICOLON c = expression SEMICOLON
    ul = forUpdateStatementList R_PAREN b = statement
		{ "annotationList FOR `( forInitStatementList `; expression `; forUpdateStatementList ) statement" <| [ al; il; c; ul; b ] <<| "forStatement" }
  | al = annotationList FOR L_PAREN
    t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { "annotationList FOR `( typeRef name IN forCollectionExpression ) statement" <| [ al; t; n; e; b ] <<| "forStatement" }
  | al = annotationList FOR L_PAREN
    al_in = annotationList t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { "annotationList FOR `( annotationList typeRef name IN forCollectionExpression ) statement" <| [ al; al_in; t; n; e; b ] <<| "forStatement" }
;

(* >> Switch statements *)
switchLabel:
  | DEFAULT
    { "DEFAULT" <| [] <<| "switchLabel" }
  | e = expressionNonBrace
    { e }
;

switchCase:
  | l = switchLabel COLON s = blockStatement
    { "switchLabel `: blockStatement" <| [ l; s ] <<| "switchCase" }
  | l = switchLabel COLON
    { "switchLabel `:" <| [ l ] <<| "switchCase" }
;

switchCaseList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "switchCaseList" }
  | cs = switchCaseList c = switchCase
    { "switchCaseList switchCase" <| [ cs; c ] <<| "switchCaseList" }
;

switchStatement:
  | SWITCH L_PAREN e = expression R_PAREN L_BRACE cs = switchCaseList R_BRACE
    { "SWITCH `( expression ) `{ switchCaseList }" <| [ e; cs ] <<| "switchStatement" }

(* >> Break and continue statements *)
breakStatement:
  | BREAK SEMICOLON
    { "BREAK `;" <| [] <<| "breakStatement" }
;

continueStatement:
  | CONTINUE SEMICOLON
    { "CONTINUE `;" <| [] <<| "continueStatement" }
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
		{ "`= expression" <| [ e ] <<| "initializer" }
;

constantDeclaration:
  | al = annotationList CONST t = typeRef n = name i = initialValue SEMICOLON
    { "annotationList CONST typeRef name initializer `;" <| [ al; t; n; i ] <<| "constantDeclaration" }
;

initializerOpt:
	| (* empty *)
		{ "`EMPTY" <| [] <<| "initializerOpt" }
	| i = initialValue { i }
;

variableDeclaration:
  | al = annotationList t = typeRef n = name i = initializerOpt SEMICOLON
    { declare_var_of_il n false;
      "annotationList typeRef name initializerOpt `;" <| [ al; t; n; i ] <<| "variableDeclaration" }
;

blockElementStatement:
  | d = constantDeclaration
  | d = variableDeclaration
  | d = statement
    { d }
;

blockElementStatementList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "blockElementStatementList" }
  | sl = blockElementStatementList s = blockElementStatement
    { "blockElementStatementList blockElementStatement" <| [ sl; s ] <<| "blockElementStatementList" }
;

(* >> Function declarations *)
functionPrototype:
	| t = typeOrVoid n = name push_scope
    tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN
    { "typeOrVoid name typeParameterListOpt `( parameterList )" <| [ t; n; tpl; pl ] <<| "functionPrototype" }
;

functionDeclaration:
	| al = annotationList p = functionPrototype b = blockStatement pop_scope
    { "annotationList functionPrototype blockStatement" <| [ al; p; b ] <<| "functionDeclaration" }
;

(* >> Action declarations *)
actionDeclaration: 
  | al = annotationList ACTION n = name L_PAREN pl = parameterList R_PAREN s = blockStatement
    { "annotationList ACTION name `( parameterList ) blockStatement" <| [ al; n; pl; s ] <<| "actionDeclaration" }
;

(* >> Instantiations *)
objectInitializer:
	| ASSIGN L_BRACE ds = objectDeclarationList R_BRACE
    { "`= `{ objectDeclarationList }" <| [ ds ] <<| "objectInitializer" }
;

instantiation:
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { "annotationList typeRef `( argumentList ) name `;" <| [ al; t; args; n ] <<| "instantiation" }
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name
    i = objectInitializer SEMICOLON
    { "annotationList typeRef `( argumentList ) name objectInitializer `;" <| [ al; t; args; n; i ] <<| "instantiation" } 
;

objectDeclaration:
	| d = functionDeclaration
	| d = instantiation
    { d }
;

objectDeclarationList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "objectDeclarationList" }
	| ds = objectDeclarationList d = objectDeclaration
    { "objectDeclarationList objectDeclaration" <| [ ds; d ] <<| "objectDeclarationList" }
;

(* >> Error declarations *)
errorDeclaration:
	| ERROR L_BRACE nl = nameList R_BRACE
    { declare_vars_of_il nl;
      "ERROR `{ nameList }" <| [ nl ] <<| "errorDeclaration" }
;

(* >> Match kind declarations *)
matchKindDeclaration:
	| MATCH_KIND L_BRACE nl = nameList c = trailingCommaOpt R_BRACE
    { declare_vars_of_il nl;
      "MATCH_KIND `{ nameList trailingCommaOpt }" <| [ nl; c ] <<| "matchKindDeclaration" } 
;

(* >> Derived type declarations *)
(* >>>> Enum type declarations *)
enumTypeDeclaration:
  | al = annotationList ENUM n = name L_BRACE
    nl = nameList c = trailingCommaOpt R_BRACE
    { "annotationList ENUM name `{ nameList trailingCommaOpt }" <| [ al; n; nl; c ] <<| "enumTypeDeclaration" }
  | al = annotationList ENUM t = typeRef n = name L_BRACE
    el = namedExpressionList c = trailingCommaOpt R_BRACE
    { "annotationList ENUM typeRef name `{ namedExpressionList trailingCommaOpt }" <| [ al; t; n; el; c ] <<| "enumTypeDeclaration" }
;

(* >>>>>> Struct, header, and union type declarations *)
typeField:
  | al = annotationList t = typeRef n = name SEMICOLON
    { "annotationList typeRef name `;" <| [ al; t; n ] <<| "typeField" }
;

typeFieldList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "typeFieldList" }
  | fl = typeFieldList f = typeField
    { "typeFieldList typeField" <| [ fl; f ] <<| "typeFieldList" }
;

structTypeDeclaration:
  | al = annotationList STRUCT n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { "annotationList STRUCT name typeParameterListOpt `{ typeFieldList }" <| [ al; n; tpl; fl ] <<| "structTypeDeclaration" }
;

headerTypeDeclaration:
  | al = annotationList HEADER n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { "annotationList HEADER name typeParameterListOpt `{ typeFieldList }" <| [ al; n; tpl; fl ] <<| "headerTypeDeclaration" }
;

headerUnionTypeDeclaration:
  | al = annotationList HEADER_UNION n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { "annotationList HEADER_UNION name typeParameterListOpt `{ typeFieldList }" <| [ al; n; tpl; fl ] <<| "headerUnionTypeDeclaration" }
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
    { "annotationList TYPEDEF typedef name `;" <| [ al; t; n ] <<| "typedefDeclaration" }
	| al = annotationList TYPE t = typeRef n = name SEMICOLON
    { "annotationList TYPE typeRef name `;" <| [ al; t; n ] <<| "typedefDeclaration" }
;

(* >> Extern declarations *)
externFunctionDeclaration:
	| al = annotationList EXTERN p = functionPrototype pop_scope SEMICOLON
		{ declare_var (id_of_function_prototype p) (has_type_params_function_prototype p);
      "annotationList EXTERN functionPrototype `;" <| [ al; p ] <<| "externFunctionDeclaration" }
;

%inline externConstructorPrototype:
	| al = annotationList tid = typeIdentifier L_PAREN pl = parameterList R_PAREN SEMICOLON
    { "annotationList typeIdentifier `( parameterList ) `;" <| [ al; tid; pl ] <<| "externConstructorPrototype" }

%inline externMethodPrototype:
	| al = annotationList p = functionPrototype pop_scope SEMICOLON
    { "annotationList functionPrototype `;" <| [ al; p ] <<| "externMethodPrototype" }
	| al = annotationList ABSTRACT p = functionPrototype
    pop_scope SEMICOLON
    { "annotationList ABSTRACT functionPrototype `;" <| [ al; p ] <<| "externMethodPrototype" }
;

externConstructorOrMethodPrototype:
  | p = externConstructorPrototype
  | p = externMethodPrototype
    { p }
;

externConstructorOrMethodPrototypeList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "externConstructorOrMethodPrototypeList" }
  | pl = externConstructorOrMethodPrototypeList p = externConstructorOrMethodPrototype
    { "externConstructorOrMethodPrototypeList externConstructorOrMethodPrototype" <| [ pl; p ] <<| "externConstructorOrMethodPrototypeList" }
;

externObjectDeclaration:
  | al = annotationList EXTERN n = push_externName tpl = typeParameterListOpt
    L_BRACE pl = externConstructorOrMethodPrototypeList R_BRACE pop_scope
    { let decl = "annotationList EXTERN name typeParameterListOpt `{ externConstructorOrMethodPrototypeList }" <| [ al; n; tpl; pl ] <<| "externObjectDeclaration" in
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
    { "keysetExpression `: name `;" <| [ k; n ] <<| "selectCase" }
;

selectCaseList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "selectCaseList" }
  | cl = selectCaseList c = selectCase
    { "selectCaseList selectCase" <| [ cl; c ] <<| "selectCaseList" }
;

selectExpression:
  | SELECT L_PAREN el = expressionList R_PAREN L_BRACE cl = selectCaseList R_BRACE
    { "SELECT `( expressionList ) `{ selectCaseList }" <| [ el; cl ] <<| "selectExpression" }
;

(* >>>> Transition statements *)
stateExpression:
  | n = name SEMICOLON
    { "name `;" <| [ n ] <<| "stateExpression" }
  | e = selectExpression
    { e }
;

transitionStatement:
  | (* empty *)
    { "`EMPTY" <| [] <<| "transitionStatement" }
  | TRANSITION e = stateExpression
    { "TRANSITION stateExpression" <| [ e ] <<| "transitionStatement" }
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
    { "annotationList VALUE_SET `< valueSetType > `( expression ) name `;" <| [ al; t; s; n ] <<| "valueSetDeclaration" }
;

(* >>>> Parser type declarations *)
parserTypeDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
      L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { "annotationList PARSER name typeParameterListOpt `( parameterList ) `;" <| [ al; n; tpl; pl ] <<| "parserTypeDeclaration" }
;

(* >>>> Parser declarations *)
parserBlockStatement:
  | al = annotationList L_BRACE sl = parserStatementList R_BRACE
    { "annotationList `{ parserStatementList }" <| [ al; sl ] <<| "parserBlockStatement" }
;

parserConditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = parserStatement %prec THEN
    { "IF `( expression ) parserStatement" <| [ c; t ] <<| "parserConditionalStatement" }
	| IF L_PAREN c = expression R_PAREN t = parserStatement ELSE f = parserStatement
    { "IF `( expression ) parserStatement ELSE parserStatement" <| [ c; t; f ] <<| "parserConditionalStatement" }
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
    { "`EMPTY" <| [] <<| "parserStatementList" }
  | sl = parserStatementList s = parserStatement
    { "parserStatementList parserStatement" <| [ sl; s ] <<| "parserStatementList" }
;

parserState:
  | al = annotationList STATE n = push_name L_BRACE sl = parserStatementList t = transitionStatement R_BRACE
    { "annotationList STATE name `{ parserStatementList transitionStatement }" <| [ al; n; sl; t ] <<| "parserState" }
;

parserStateList:
  | s = parserState
    { s }
  | sl = parserStateList s = parserState
    { "parserStateList parserState" <| [ sl; s ] <<| "parserStateList" }
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
    { "`EMPTY" <| [] <<| "parserLocalDeclarationList" }
  | dl = parserLocalDeclarationList d = parserLocalDeclaration
    { "parserLocalDeclarationList parserLocalDeclaration" <| [ dl; d ] <<| "parserLocalDeclarationList" }
;

parserDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
		{ "annotationList PARSER name typeParameterListOpt `( parameterList ) constructorParameterListOpt `{ parserLocalDeclarationList parserStateList }" <| [ al; n; tpl; pl; cpl; dl; sl ] <<| "parserDeclaration" }
;

(* >> Control statements and declarations *)
(* >>>> Table declarations *)
constOpt:
  | (* empty *)
    { "`EMPTY" <| [] <<| "constOpt" }
  | CONST
    { "CONST" <| [] <<| "constOpt" }
;

(* >>>>>> Table key property *)
tableKey:
  | e = expression COLON n = name al = annotationList SEMICOLON
    { "expression `: name annotationList `;" <| [ e; n; al ] <<| "tableKey" }
;

tableKeyList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tableKeyList" }
  | kl = tableKeyList k = tableKey
    { "tableKeyList tableKey" <| [ kl; k ] <<| "tableKeyList" }
;

(* >>>>>> Table actions property *)
tableActionReference:
  | n = prefixedNonTypeName
    { n }
  | n = prefixedNonTypeName L_PAREN al = argumentList R_PAREN
    { "prefixedNonTypeName `( argumentList )" <| [ n; al ] <<| "tableActionReference" }
;

tableAction:
  | al = annotationList ac = tableActionReference SEMICOLON
    { "annotationList tableActionReference `;" <| [ al; ac ] <<| "tableAction" }
;

tableActionList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tableActionList" }
  | acl = tableActionList ac = tableAction
    { "tableActionList tableAction" <| [ acl; ac ] <<| "tableActionList" }
;

(* >>>>>> Table entry property *)
tableEntryPriority:
  | PRIORITY ASSIGN int = integerLiteral COLON
    { "PRIORITY `= integerLiteral `:" <| [ int ] <<| "tableEntryPriority" }
  | PRIORITY ASSIGN L_PAREN e = expression R_PAREN COLON
    { "PRIORITY `= `( expression ) `:" <| [ e ] <<| "tableEntryPriority" }
;

tableEntry:
  | c = constOpt p = tableEntryPriority k = keysetExpression COLON ac = tableActionReference
    al = annotationList SEMICOLON
    { "constOpt tableEntryPriority keysetExpression `: tableActionReference annotationList `;" <| [ c; p; k; ac; al ] <<| "tableEntry" }
  | c = constOpt k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { "constOpt keysetExpression `: tableActionReference annotationList `;" <| [ c; k; ac; al ] <<| "tableEntry" }
;

tableEntryList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tableEntryList" }
  | el = tableEntryList e = tableEntry
    { "tableEntryList tableEntry" <| [ el; e ] <<| "tableEntryList" }
;

(* >>>>>> Table properties *)
tableProperty:
  | KEY ASSIGN L_BRACE kl = tableKeyList R_BRACE
    { "KEY `= `{ tableKeyList }" <| [ kl ] <<| "tableProperty" }
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { "ACTIONS `= `{ tableActionList }" <| [ acl ] <<| "tableProperty" }
  | al = annotationList c = constOpt ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { "annotationList constOpt ENTRIES `= `{ tableEntryList }" <| [ al; c; el ] <<| "tableProperty" }
  | al = annotationList c = constOpt n = tableCustomName i = initialValue SEMICOLON
    { "annotationList constOpt tableCustomName initializer `;" <| [ al; c; n; i ] <<| "tableProperty" }
;

tablePropertyList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tablePropertyList" }
  | pl = tablePropertyList p = tableProperty
    { "tablePropertyList tableProperty" <| [ pl; p ] <<| "tablePropertyList" }
;

tableDeclaration:
  | al = annotationList TABLE n = name L_BRACE pl = tablePropertyList R_BRACE
    { "annotationList TABLE name `{ tablePropertyList }" <| [ al; n; pl ] <<| "tableDeclaration" }

(* >>>> Control type declarations *)
controlTypeDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { "annotationList CONTROL name typeParameterListOpt `( parameterList ) `;" <| [ al; n; tpl; pl ] <<| "controlTypeDeclaration" }
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
    { "`EMPTY" <| [] <<| "controlLocalDeclarationList" }
  | dl = controlLocalDeclarationList d = controlLocalDeclaration
    { "controlLocalDeclarationList controlLocalDeclaration" <| [ dl; d ] <<| "controlLocalDeclarationList" }
;

controlDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { "annotationList CONTROL name typeParameterListOpt `( parameterList ) constructorParameterListOpt `{ controlLocalDeclarationList APPLY controlBody }" <| [ al; n; tpl; pl; cpl; dl; b ] <<| "controlDeclaration" }
;

(* >> Package type declarations *)
packageTypeDeclaration:
  | al = annotationList PACKAGE n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { "annotationList PACKAGE name typeParameterListOpt `( parameterList ) `;" <| [ al; n; tpl; pl ] <<| "packageTypeDeclaration" }
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
    { "UNEXPECTED_TOKEN" <| [] <<| "annotationToken" }
	| ABSTRACT
    { "ABSTRACT" <| [] <<| "annotationToken" }
	| ACTION
    { "ACTION" <| [] <<| "annotationToken" }
	| ACTIONS
    { "ACTIONS" <| [] <<| "annotationToken" }
	| APPLY
    { "APPLY" <| [] <<| "annotationToken" }
	| BOOL
    { "BOOL" <| [] <<| "annotationToken" }
	| BIT
    { "BIT" <| [] <<| "annotationToken" }
	| BREAK
    { "BREAK" <| [] <<| "annotationToken" }
	| CONST
    { "CONST" <| [] <<| "annotationToken" }
	| CONTINUE
    { "CONTINUE" <| [] <<| "annotationToken" }
	| CONTROL
    { "CONTROL" <| [] <<| "annotationToken" }
	| DEFAULT
    { "DEFAULT" <| [] <<| "annotationToken" }
	| ELSE
    { "ELSE" <| [] <<| "annotationToken" }
	| ENTRIES
    { "ENTRIES" <| [] <<| "annotationToken" }
	| ENUM
    { "ENUM" <| [] <<| "annotationToken" }
	| ERROR
    { "ERROR" <| [] <<| "annotationToken" }
	| EXIT
    { "EXIT" <| [] <<| "annotationToken" }
	| EXTERN
    { "EXTERN" <| [] <<| "annotationToken" }
	| FALSE
    { "FALSE" <| [] <<| "annotationToken" }
	| FOR
    { "FOR" <| [] <<| "annotationToken" }
	| HEADER
    { "HEADER" <| [] <<| "annotationToken" }
	| HEADER_UNION
    { "HEADER_UNION" <| [] <<| "annotationToken" }
	| IF
    { "IF" <| [] <<| "annotationToken" }
	| IN
    { "IN" <| [] <<| "annotationToken" }
	| INOUT
    { "INOUT" <| [] <<| "annotationToken" }
	| INT
    { "INT" <| [] <<| "annotationToken" }
	| KEY
    { "KEY" <| [] <<| "annotationToken" }
	| MATCH_KIND
    { "MATCH_KIND" <| [] <<| "annotationToken" }
	| TYPE
    { "TYPE" <| [] <<| "annotationToken" }
	| OUT
    { "OUT" <| [] <<| "annotationToken" }
	| PARSER
    { "PARSER" <| [] <<| "annotationToken" }
	| PACKAGE
    { "PACKAGE" <| [] <<| "annotationToken" }
	| PRAGMA
    { "PRAGMA" <| [] <<| "annotationToken" }
	| RETURN
    { "RETURN" <| [] <<| "annotationToken" }
	| SELECT
    { "SELECT" <| [] <<| "annotationToken" }
	| STATE
    { "STATE" <| [] <<| "annotationToken" }
	| STRING
    { "STRING" <| [] <<| "annotationToken" }
	| STRUCT
    { "STRUCT" <| [] <<| "annotationToken" }
	| SWITCH
    { "SWITCH" <| [] <<| "annotationToken" }
	| TABLE
    { "TABLE" <| [] <<| "annotationToken" }
	| THIS
    { "THIS" <| [] <<| "annotationToken" }
	| TRANSITION
    { "TRANSITION" <| [] <<| "annotationToken" }
	| TRUE
    { "TRUE" <| [] <<| "annotationToken" }
	| TUPLE
    { "TUPLE" <| [] <<| "annotationToken" }
	| TYPEDEF
    { "TYPEDEF" <| [] <<| "annotationToken" }
	| VARBIT
    { "VARBIT" <| [] <<| "annotationToken" }
	| VALUE_SET
    { "VALUE_SET" <| [] <<| "annotationToken" }
	| LIST
    { "LIST" <| [] <<| "annotationToken" }
	| VOID
    { "VOID" <| [] <<| "annotationToken" }
	| DONTCARE
    { "`_" <| [] <<| "annotationToken" }
	| id = identifier
    { id }
	| tid = typeIdentifier
    { tid }
	| str = stringLiteral
    { str }
	| int = integerLiteral
    { int }
	| MASK
    { "`&&&" <| [] <<| "annotationToken" }
  (* TODO: missing DOTS "..." in spec *)
	| RANGE
    { "`.." <| [] <<| "annotationToken" }
	| SHL
    { "`<<" <| [] <<| "annotationToken" }
	| AND
    { "`&&" <| [] <<| "annotationToken" }
	| OR
    { "`||" <| [] <<| "annotationToken" }
	| EQ
    { "`==" <| [] <<| "annotationToken" }
	| NE
    { "`!=" <| [] <<| "annotationToken" }
	| GE
    { "`>=" <| [] <<| "annotationToken" }
	| LE
    { "`<=" <| [] <<| "annotationToken" }
	| PLUSPLUS
    { "`++" <| [] <<| "annotationToken" }
	| PLUS
    { "`+" <| [] <<| "annotationToken" }
	| PLUS_SAT
    { "`|+|" <| [] <<| "annotationToken" }
	| MINUS
    { "`-" <| [] <<| "annotationToken" }
	| MINUS_SAT
    { "`|-|" <| [] <<| "annotationToken" }
	| MUL
    { "`*" <| [] <<| "annotationToken" }
	| DIV
    { "`/" <| [] <<| "annotationToken" }
	| MOD
    { "`%" <| [] <<| "annotationToken" }
	| BIT_OR
    { "`|" <| [] <<| "annotationToken" }
	| BIT_AND
    { "`&" <| [] <<| "annotationToken" }
	| BIT_XOR
    { "`^" <| [] <<| "annotationToken" }
	| COMPLEMENT
    { "`~" <| [] <<| "annotationToken" }
	| L_BRACKET
    { "``[" <| [] <<| "annotationToken" }
	| R_BRACKET
    { "``]" <| [] <<| "annotationToken" }
	| L_BRACE
    { "``{" <| [] <<| "annotationToken" }
	| R_BRACE
    { "``}" <| [] <<| "annotationToken" }
	| L_ANGLE
    { "``<" <| [] <<| "annotationToken" }
	| R_ANGLE
    { "``>" <| [] <<| "annotationToken" }
	| NOT
    { "`!" <| [] <<| "annotationToken" }
	| COLON
    { "`:" <| [] <<| "annotationToken" }
	| COMMA
    { "`," <| [] <<| "annotationToken" }
	| QUESTION
    { "`?" <| [] <<| "annotationToken" }
	| DOT
    { "`." <| [] <<| "annotationToken" }
	| ASSIGN
    { "`=" <| [] <<| "annotationToken" }
	| SEMICOLON
    { "`;" <| [] <<| "annotationToken" }
	| AT
    { "`@" <| [] <<| "annotationToken" }
;

annotationBody:
	| (* empty *)
    { "`EMPTY" <| [] <<| "annotationBody" }
	| ab = annotationBody L_PAREN ab_in = annotationBody R_PAREN
    { "annotationBody `( annotationBody )" <| [ ab; ab_in ] <<| "annotationBody" }
	| ab = annotationBody at = annotationToken
    { "annotationBody annotationToken" <| [ ab; at ] <<| "annotationBody" }
;

structuredAnnotationBody:
	| e = sequenceOrRecordElementExpression c = trailingCommaOpt
    { "sequenceOrRecordElementExpression trailingCommaOpt" <| [ e; c ] <<| "structuredAnnotationBody" }
;

annotation:
	| AT name = name
    { "`@ name" <| [ name ] <<| "annotation" }
	| AT name = name L_PAREN body = annotationBody R_PAREN
    { "`@ name `( annotationBody )" <| [ name; body ] <<| "annotation" }
	| AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { "`@ name `[ structuredAnnotationBody ]" <| [ name; body ] <<| "annotation" }
(* From Petr4: PRAGMA not in Spec, but in Petr4/p4c *)
	| PRAGMA name = name body = annotationBody PRAGMA_END
    { "`@ PRAGMA name annotationBody" <| [ name; body ] <<| "annotation" }
;

annotationListNonEmpty:
	| a = annotation
    { a }
	| al = annotationListNonEmpty a = annotation
		{ "annotationListNonEmpty annotation" <| [ al; a ] <<| "annotationListNonEmpty" }
;

%inline annotationList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "annotationList" }
	| al = annotationListNonEmpty
    { al }
;

(******** P4 program ********)
declarationList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "declarationList" }
  | ds = declarationList d = declaration
    { "declarationList declaration" <| [ ds; d ] <<| "declarationList" }
  | ds = declarationList SEMICOLON
    { "declarationList `;" <| [ ds ] <<| "declarationList" }
;

p4program:
	| dl = declarationList END { dl #@@ "p4program" }
