%{
  open Lang
  open Il
  open Context
  open Extract
  open Value.Make
  open Flatten
  open Util.Source

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

  let position_to_pos position =
    {
      file = position.Lexing.pos_fname;
      line = position.Lexing.pos_lnum;
      column = position.Lexing.pos_cnum - position.Lexing.pos_bol
    }

  let positions_to_region position_left position_right =
    {
      left = position_to_pos position_left;
      right = position_to_pos position_right
    }

  let at (position_left, position_right) = positions_to_region position_left position_right
%}

(**************************** TOKENS ******************************)
%token END
%token TYPENAME IDENTIFIER
%token<Lang.Il.value> NAME STRING_LITERAL
%token<Lang.Il.value * string> NUMBER_INT NUMBER
%token LE GE SHL AND OR NE EQ
%token PLUS MINUS PLUS_SAT MINUS_SAT MUL INVALID DIV MOD
%token BIT_OR BIT_AND BIT_XOR COMPLEMENT
%token L_BRACKET R_BRACKET L_BRACE R_BRACE L_ANGLE L_ANGLE_ARGS R_ANGLE R_ANGLE_SHIFT L_PAREN R_PAREN
%token ASSIGN COLON COMMA QUESTION DOT NOT SEMICOLON
%token AT PLUSPLUS PLUSCOLON
%token DONTCARE
%token MASK DOTS RANGE
%token TRUE FALSE
%token ABSTRACT ACTION ACTIONS APPLY BOOL BIT BREAK CONST CONTINUE CONTROL DEFAULT
%token ELSE ENTRIES ENUM ERROR EXIT EXTERN HEADER HEADER_UNION IF IN INOUT FOR
%token INT KEY LIST SELECT MATCH_KIND OUT PACKAGE PARSER PRIORITY RETURN STATE STRING STRUCT
%token SWITCH TABLE THIS TRANSITION TUPLE TYPEDEF TYPE VALUE_SET VARBIT VOID
%token PRAGMA PRAGMA_END
%token PLUS_ASSIGN PLUS_SAT_ASSIGN MINUS_ASSIGN MINUS_SAT_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN SHL_ASSIGN SHR_ASSIGN BIT_AND_ASSIGN BIT_XOR_ASSIGN BIT_OR_ASSIGN
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
	| r_angle = R_ANGLE
    { r_angle }
	| r_angle = R_ANGLE_SHIFT
    { r_angle }
;
%inline l_angle:
	| l_angle = L_ANGLE
    { l_angle }
	| l_angle = L_ANGLE_ARGS
    { l_angle }
;

(* Misc *)
trailingCommaOpt:
	| (* empty *)
    { "`EMPTY" <| [] <<| "trailingCommaOpt" <<<| (at $sloc) }
	| COMMA
    { "`," <| [] <<| "trailingCommaOpt" <<<| (at $sloc) }
;

(* Booleans *)
%inline booleanLiteral:
  | TRUE
    { "TRUE" <| [] <<| "booleanLiteral" <<<| (at $sloc) }
  | FALSE
    { "FALSE" <| [] <<| "booleanLiteral" <<<| (at $sloc) }

(* Integers *)
integerLiteral:
	| int = int
    { "D int" <| [ int ] <<| "integerLiteral" <<<| (at $sloc) }
(* Processed by lexer *)
	| number = NUMBER
    { fst number }
;

(* Strings *)
stringLiteral:
	| text = STRING_LITERAL
    { "`\" text `\"" <| [ text ] <<| "stringLiteral" <<<| (at $sloc) }
;

(* Names *)
identifier:
	| text = NAME IDENTIFIER
    { "`ID text" <| [ text ] <<| "identifier" <<<| (at $sloc) }
;

typeIdentifier:
	| text = NAME TYPENAME
    { "`TID text" <| [ text ] <<| "typeIdentifier" <<<| (at $sloc) }
;

(* >> Non-type names *)
nonTypeName:
	| id = identifier
    { id }
	| APPLY
    { "APPLY" <| [] <<| "nonTypeName" <<<| (at $sloc) }
	| KEY
    { "KEY" <| [] <<| "nonTypeName" <<<| (at $sloc) }
	| ACTIONS
    { "ACTIONS" <| [] <<| "nonTypeName" <<<| (at $sloc) }
	| STATE
    { "STATE" <| [] <<| "nonTypeName" <<<| (at $sloc) }
	| ENTRIES
    { "ENTRIES" <| [] <<| "nonTypeName" <<<| (at $sloc) }
	| TYPE
    { "TYPE" <| [] <<| "nonTypeName" <<<| (at $sloc) }
	| PRIORITY
    { "PRIORITY" <| [] <<| "nonTypeName" <<<| (at $sloc) }
;

prefixedNonTypeName:
	| n = nonTypeName
    { n }
	| DOT go_toplevel n = nonTypeName go_local
    { "`ID `. nonTypeName" <| [ n ] <<| "prefixedNonTypeName" <<<| (at $sloc) }
;

(* >> Type names *)
typeName:
	| n = typeIdentifier { n }
;

prefixedTypeName:
	| n = typeName
    { n }
	| DOT go_toplevel tid = typeName go_local
    { "`TID `. typeName" <| [ tid ] <<| "prefixedTypeName" <<<| (at $sloc) }
;

(* >> Table custom property names *)
tableCustomName:
	| id = identifier
    { id }
	| tid = typeIdentifier
    { tid }
	| APPLY
    { "APPLY" <| [] <<| "tableCustomName" <<<| (at $sloc) }
	| STATE
    { "STATE" <| [] <<| "tableCustomName" <<<| (at $sloc) }
	| TYPE
    { "TYPE" <| [] <<| "tableCustomName" <<<| (at $sloc) }
	| PRIORITY
    { "PRIORITY" <| [] <<| "tableCustomName" <<<| (at $sloc) }
;

(* >> Names *)
name:
	| n = nonTypeName
	| n = typeName
    { n }
	| LIST
    { "LIST" <| [] <<| "name" <<<| (at $sloc) }
;

nameList:
	| n = name
    { n }
	| ns = nameList COMMA n = name
    { "nameList `, name" <| [ ns; n ] <<| "nameList" <<<| (at $sloc) }
;

member:
	| name = name
    { name }
;

(* Directions *)
direction:
	| (* empty *)
    { "`EMPTY" <| [] <<| "direction" <<<| (at $sloc) }
	| IN
    { "IN" <| [] <<| "direction" <<<| (at $sloc) }
	| OUT
    { "OUT" <| [] <<| "direction" <<<| (at $sloc) }
	| INOUT
    { "INOUT" <| [] <<| "direction" <<<| (at $sloc) }
;

(* Types *)
(* >> Base types *)
baseType:
	| BOOL
    { "BOOL" <| [] <<| "baseType" <<<| (at $sloc) }
	| MATCH_KIND
    { "MATCH_KIND" <| [] <<| "baseType" <<<| (at $sloc) }
	| ERROR
    { "ERROR" <| [] <<| "baseType" <<<| (at $sloc) }
	| BIT
    { "BIT" <| [] <<| "baseType" <<<| (at $sloc) }
	| STRING
    { "STRING" <| [] <<| "baseType" <<<| (at $sloc) }
	| INT
    { "INT" <| [] <<| "baseType" <<<| (at $sloc) }
	| BIT l_angle v = int r_angle
    { "BIT `< int >" <| [ v ] <<| "baseType" <<<| (at $sloc) }
	| INT l_angle v = int r_angle
    { "INT `< int >" <| [ v ] <<| "baseType" <<<| (at $sloc) }
	| VARBIT l_angle v = int r_angle
    { "VARBIT `< int >" <| [ v ] <<| "baseType" <<<| (at $sloc) }
	| BIT l_angle L_PAREN e = expression R_PAREN r_angle
    { "BIT `< `( expression ) >" <| [ e ] <<| "baseType" <<<| (at $sloc) }
	| INT l_angle L_PAREN e = expression R_PAREN r_angle
    { "INT `< `( expression ) >" <| [ e ] <<| "baseType" <<<| (at $sloc) }
	| VARBIT l_angle L_PAREN e = expression R_PAREN r_angle
    { "VARBIT `< `( expression ) >" <| [ e ] <<| "baseType" <<<| (at $sloc) }
;

(* >> Named types *)
specializedType:
  | n = prefixedTypeName l_angle tal = typeArgumentList r_angle
    { "prefixedTypeName `< typeArgumentList >" <| [ n; tal ] <<| "specializedType" <<<| (at $sloc) }
;

namedType:
  | t = prefixedTypeName
  | t = specializedType
    { t }
;

(* >> Header stack types *)
headerStackType:
  | t = namedType L_BRACKET e = expression R_BRACKET
    { "namedType `[ expression ]" <| [ t; e ] <<| "headerStackType" <<<| (at $sloc) }
;

(* >> List types *)
listType:
  | LIST l_angle targ = typeArgument r_angle
    { "LIST `< typeArgument >" <| [ targ ] <<| "listType" <<<| (at $sloc) }
;

(* >> Tuple types *)
tupleType:
	| TUPLE l_angle targs = typeArgumentList r_angle
    { "TUPLE `< typeArgumentList >" <| [ targs ] <<| "tupleType" <<<| (at $sloc) }
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
    { "VOID" <| [] <<| "typeOrVoid" <<<| (at $sloc) }
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
    { "typeParameterList `, typeParameter" <| [ tps; tp ] <<| "typeParameterList" <<<| (at $sloc) }
;

typeParameterListOpt:
	| (* empty *)
    { "`EMPTY" <| [] <<| "typeParameterListOpt" <<<| (at $sloc) }
	| l_angle tps = typeParameterList r_angle
    { declare_types_of_il tps;
      "`< typeParameterList >" <| [ tps ] <<| "typeParameterListOpt" <<<| (at $sloc) }
;

(* Parameters *)
parameter:
	| al = annotationList dir = direction t = typeRef n = name i = initializerOpt
		{ declare_var_of_il n false;
      "annotationList direction type name initializerOpt" <| [ al; dir; t; n; i ] <<| "parameter" <<<| (at $sloc) }
;

nonEmptyParameterList:
	| p = parameter
    { p }
	| ps = nonEmptyParameterList COMMA p = parameter
    { "nonEmptyParameterList `, parameter" <| [ ps; p ] <<| "nonEmptyParameterList" <<<| (at $sloc) }
;

parameterList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "parameterList" <<<| (at $sloc) }
	| ps = nonEmptyParameterList
    { ps }
;

(* Constructor parameters *)
constructorParameterListOpt:
	| (* empty *)
    { "`EMPTY" <| [] <<| "constructorParameterListOpt" <<<| (at $sloc) }
	| L_PAREN ps = parameterList R_PAREN
    { "`( parameterList )" <| [ ps ] <<| "constructorParameterListOpt" <<<| (at $sloc) }
;

(* Expression key-value pairs *)
namedExpression:
	| n = name ASSIGN e = expression
    { "name `= expression" <| [ n; e ] <<| "namedExpression" <<<| (at $sloc) }
;

namedExpressionList:
	| e = namedExpression
    { e }
	| es = namedExpressionList COMMA e = namedExpression
    { "namedExpressionList `, namedExpression" <| [ es; e ] <<| "namedExpressionList" <<<| (at $sloc) }
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
    { "THIS" <| [] <<| "referenceExpression" <<<| (at $sloc) }
;

(* >> Default expressions *)
%inline defaultExpression:
	| DOTS
    { "`..." <| [] <<| "defaultExpression" <<<| (at $sloc) }
;

(* >> Unary, binary, and ternary expressions *)
%inline unop: 
	| NOT
    { "`!" <| [] <<| "unop" <<<| (at $sloc) }
	| COMPLEMENT
    { "`~" <| [] <<| "unop" <<<| (at $sloc) }
	| MINUS
    { "`-" <| [] <<| "unop" <<<| (at $sloc) }
	| PLUS
    { "`+" <| [] <<| "unop" <<<| (at $sloc) }
;

%inline unaryExpression:
	| o = unop e = expression %prec PREFIX
		{ "unop expression" <| [ o; e ] <<| "unaryExpression" <<<| (at $sloc) }
;

%inline binop:
  | MUL
    { "`*" <| [] <<| "binop" <<<| (at $sloc) }
  | DIV
    { "`/" <| [] <<| "binop" <<<| (at $sloc) }
  | MOD
    { "`%" <| [] <<| "binop" <<<| (at $sloc) }
  | PLUS
    { "`+" <| [] <<| "binop" <<<| (at $sloc) }
  | PLUS_SAT
    { "`|+|" <| [] <<| "binop" <<<| (at $sloc) }
  | MINUS
    { "`-" <| [] <<| "binop" <<<| (at $sloc) }
  | MINUS_SAT
    { "`|-|" <| [] <<| "binop" <<<| (at $sloc) }
  | SHL
    { "`<<" <| [] <<| "binop" <<<| (at $sloc) }
  | r_angle R_ANGLE_SHIFT
    { "`>>" <| [] <<| "binop" <<<| (at $sloc) }
  | LE
    { "`<=" <| [] <<| "binop" <<<| (at $sloc) }
  | GE
    { "`>=" <| [] <<| "binop" <<<| (at $sloc) }
  | l_angle
    { "``<" <| [] <<| "binop" <<<| (at $sloc) }
  | r_angle
    { "``>" <| [] <<| "binop" <<<| (at $sloc) }
  | NE
    { "`!=" <| [] <<| "binop" <<<| (at $sloc) }
  | EQ
    { "`==" <| [] <<| "binop" <<<| (at $sloc) }
  | BIT_AND
    { "`&" <| [] <<| "binop" <<<| (at $sloc) }
  | BIT_XOR
    { "`^" <| [] <<| "binop" <<<| (at $sloc) }
  | BIT_OR
    { "`|" <| [] <<| "binop" <<<| (at $sloc) }
  | PLUSPLUS
    { "`++" <| [] <<| "binop" <<<| (at $sloc) }
  | AND
    { "`&&" <| [] <<| "binop" <<<| (at $sloc) }
  | OR
    { "`||" <| [] <<| "binop" <<<| (at $sloc) }
;

%inline binaryExpression:
	| l = expression o = binop r = expression
		{ "expression binop expression" <| [ l; o; r ] <<| "binaryExpression" <<<| (at $sloc) }
;

%inline binaryExpressionNonBrace:
	| l = expressionNonBrace o = binop r = expression
		{ "expressionNonBrace binop expression" <| [ l; o; r ] <<| "binaryExpressionNonBrace" <<<| (at $sloc) }
;

%inline ternaryExpression:
	| c = expression QUESTION t = expression COLON f = expression
		{ "expression `? expression `: expression" <| [ c; t; f ] <<| "ternaryExpression" <<<| (at $sloc) }
;

%inline ternaryExpressionNonBrace:
	| c = expressionNonBrace QUESTION t = expression COLON f = expression
		{ "expressionNonBrace `? expression `: expression" <| [ c; t; f ] <<| "ternaryExpressionNonBrace" <<<| (at $sloc) }
;

(* >> Cast expressions *)
%inline castExpression:
	| L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
    { "`( typeRef ) expression" <| [ t; e ] <<| "castExpression" <<<| (at $sloc) }
;

(* >> Data (aggregate) expressions *)
%inline dataExpression:
	| INVALID
    { "`{#}" <| [] <<| "invalidHeaderExpression" <<<| (at $sloc) }
	| L_BRACE e = sequenceOrRecordElementExpression c = trailingCommaOpt R_BRACE
    { "`{ sequenceOrRecordElementExpression trailingCommaOpt }" <| [ e; c ] <<| "sequenceOrRecordExpression" <<<| (at $sloc) }
;

(* >> Member and index access expressions *)
%inline errorAccessExpression:
	| ERROR DOT m = member
		{ "ERROR `. member" <| [ m ] <<| "errorAccessExpression" <<<| (at $sloc) }
;

%inline memberAccessExpression:
	| e = memberAccessBase DOT m = member %prec DOT
		{ "memberAccessBase `. member" <| [ e; m ] <<| "memberAccessExpression" <<<| (at $sloc) }
;

%inline indexAccessExpression:
	| a = expression L_BRACKET i = expression R_BRACKET
		{ "expression `[ expression ]" <| [ a; i ] <<| "indexAccessExpression" <<<| (at $sloc) }
;

%inline sliceAccessExpression:
  | a = expression L_BRACKET h = expression COLON l = expression R_BRACKET
    { "expression `[ expression `: expression ]" <| [ a; h; l ] <<| "sliceAccessExpression" <<<| (at $sloc) }
  | a = expression L_BRACKET l = expression PLUSCOLON w = expression R_BRACKET
    { "expression `[ expression `+: expression ]" <| [ a; l; w ] <<| "sliceAccessExpression" <<<| (at $sloc) }
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
		{ "memberAccessBaseNonBrace `. member" <| [ e; m ] <<| "memberAccessExpressionNonBrace" <<<| (at $sloc) }
;

%inline indexAccessExpressionNonBrace:
	| a = expressionNonBrace L_BRACKET i = expression R_BRACKET
		{ "expressionNonBrace `[ expression ]" <| [ a; i ] <<| "indexAccessExpressionNonBrace" <<<| (at $sloc) }
;

%inline sliceAccessExpressionNonBrace:
  | a = expressionNonBrace L_BRACKET h = expression COLON l = expression R_BRACKET
    { "expressionNonBrace `[ expression `: expression ]" <| [ a; h; l ] <<| "sliceAccessExpressionNonBrace" <<<| (at $sloc) }
  | a = expressionNonBrace L_BRACKET h = expression PLUSCOLON l = expression R_BRACKET
    { "expressionNonBrace `[ expression `+: expression ]" <| [ a; h; l ] <<| "sliceAccessExpressionNonBrace" <<<| (at $sloc) }
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
		{ "callTarget `( argumentList )" <| [ t; args ] <<| "callExpression" <<<| (at $sloc) }
	| t = callableTarget l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ "callTarget `< realTypeArgumentList > `( argumentList )" <| [ t; targs; args ] <<| "callExpression" <<<| (at $sloc) } 
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
		{ "callTargetNonBrace `( argumentList )" <| [ t; args ] <<| "callExpressionNonBrace" <<<| (at $sloc) }
	| t = callableTargetNonBrace l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ "callTargetNonBrace `< realTypeArgumentList > `( argumentList )" <| [ t; targs; args ] <<| "callExpressionNonBrace" <<<| (at $sloc) }

(* >> Parenthesized Expressions *)

%inline parenthesizedExpression:
	| L_PAREN e = expression R_PAREN
		{ "`( expression )" <| [ e ] <<| "parenthesizedExpression" <<<| (at $sloc) }
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
    { "`EMPTY" <| [] <<| "expressionList" <<<| (at $sloc) }
	| e = expression
    { e }
	| el = expressionList COMMA e = expression
		{ "expressionList `, expression" <| [ el; e ] <<| "expressionList" <<<| (at $sloc) }
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
    { "name `= expression" <| [ n; e ] <<| "recordElementExpression" <<<| (at $sloc) }
  | n = name ASSIGN e = expression COMMA DOTS
    { "name `= expression `, `..." <| [ n; e ] <<| "recordElementExpression" <<<| (at $sloc) }
	| n = name ASSIGN e = expression COMMA el = namedExpressionList
    { "name `= expression `, namedExpressionList" <| [ n; e; el ] <<| "recordElementExpression" <<<| (at $sloc) }
  | n = name ASSIGN e = expression COMMA el = namedExpressionList COMMA DOTS
    { "name `= expression `, namedExpressionList `, `..." <| [ n; e; el ] <<| "recordElementExpression" <<<| (at $sloc) }
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
    { "expression `&&& expression" <| [ b; m ] <<| "simpleKeysetExpression" <<<| (at $sloc) }
	| l = expression RANGE h = expression
    { "expression `.. expression" <| [ l; h ] <<| "simpleKeysetExpression" <<<| (at $sloc) }
	| DEFAULT
    { "DEFAULT" <| [] <<| "simpleKeysetExpression" <<<| (at $sloc) }
	| DONTCARE
    { "`_" <| [] <<| "simpleKeysetExpression" <<<| (at $sloc) }
;

simpleKeysetExpressionList:
	| e = simpleKeysetExpression
    { e }
	| el = simpleKeysetExpressionList COMMA e = simpleKeysetExpression
    { "simpleKeysetExpressionList `, simpleKeysetExpression" <| [ el; e ] <<| "simpleKeysetExpressionList" <<<| (at $sloc) }
;

tupleKeysetExpression:
	| L_PAREN b = expression MASK m = expression R_PAREN
		{ "`( expression `&&& expression )" <| [ b; m ] <<| "tupleKeysetExpression" <<<| (at $sloc) }
	| L_PAREN l = expression RANGE h = expression R_PAREN
		{ "`( expression `.. expression )" <| [ l; h ] <<| "tupleKeysetExpression" <<<| (at $sloc) }
	| L_PAREN DEFAULT R_PAREN
		{ "`( DEFAULT )" <| [] <<| "tupleKeysetExpression" <<<| (at $sloc) }
	| L_PAREN DONTCARE R_PAREN
		{ "`( `_ )" <| [] <<| "tupleKeysetExpression" <<<| (at $sloc) }
	| L_PAREN e = simpleKeysetExpression COMMA es = simpleKeysetExpressionList R_PAREN
		{ "`( simpleKeysetExpression `, simpleKeysetExpressionList )" <| [ e; es ] <<| "tupleKeysetExpression" <<<| (at $sloc) }
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
    { "VOID" <| [] <<| "realTypeArgument" <<<| (at $sloc) }
	| DONTCARE
    { "`_" <| [] <<| "realTypeArgument" <<<| (at $sloc) }
;

realTypeArgumentList:
	| targ = realTypeArgument
    { targ }
	| targs = realTypeArgumentList COMMA targ = realTypeArgument
    { "realTypeArgumentList `, realTypeArgument" <| [ targs; targ ] <<| "realTypeArgumentList" <<<| (at $sloc) }
;

typeArgument:
	| t = typeRef
	| t = nonTypeName 
		{ t }
	| VOID
    { "VOID" <| [] <<| "typeArgument" <<<| (at $sloc) }
	| DONTCARE
    { "`_" <| [] <<| "typeArgument" <<<| (at $sloc) }
;

typeArgumentList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "typeArgumentList" <<<| (at $sloc) }
	| targ = typeArgument
    { targ }
	| targs = typeArgumentList COMMA targ = typeArgument
    { "typeArgumentList `, typeArgument" <| [ targs; targ ] <<| "typeArgumentList" <<<| (at $sloc) }
;

(* Arguments *)
argument:
	| e = expression
    { e }
	| n = name ASSIGN e = expression 
		{ "name `= expression" <| [ n; e ] <<| "argument" <<<| (at $sloc) }
	| name = name ASSIGN DONTCARE
		{ "name `= `_ " <| [ name ] <<| "argument" <<<| (at $sloc) }
	| DONTCARE
		{ "`_" <| [] <<| "argument" <<<| (at $sloc) }
;

argumentListNonEmpty:
	| arg = argument
    { arg }
	| args = argumentListNonEmpty COMMA arg = argument
    { "argumentListNonEmpty `, argument" <| [ args; arg ] <<| "argumentListNonEmpty" <<<| (at $sloc) }
;

argumentList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "argumentList" <<<| (at $sloc) }
	| args = argumentListNonEmpty
    { args }
;

(* L-values *)
lvalue:
	| e = referenceExpression
    { e }
	| lv = lvalue DOT m = member %prec DOT
		{ "lvalue `. member" <| [ lv; m ] <<| "lvalue" <<<| (at $sloc) }
	| lv = lvalue L_BRACKET i = expression R_BRACKET
		{ "lvalue `[ expression ]" <| [ lv; i ] <<| "lvalue" <<<| (at $sloc) }
	| lv = lvalue L_BRACKET h = expression COLON l = expression R_BRACKET
		{ "lvalue `[ expression `: expression ]" <| [ lv; h; l ] <<| "lvalue" <<<| (at $sloc) }
	| lv = lvalue L_BRACKET l = expression PLUSCOLON w = expression R_BRACKET
    { "lvalue `[ expression `+: expression ]" <| [ lv; l; w ] <<| "lvalue" <<<| (at $sloc) }
	| L_PAREN lv = lvalue R_PAREN
		{ "`( lvalue )" <| [ lv ] <<| "lvalue" <<<| (at $sloc) }
;

(* Statements *)
(* >> Empty statements *)
emptyStatement:
	| SEMICOLON
    { "`;" <| [] <<| "emptyStatement" <<<| (at $sloc) }
;

(* >> Assignment statements *)
assignop:
	| ASSIGN
    { "`=" <| [] <<| "assignop" <<<| (at $sloc) }
	| PLUS_ASSIGN
    { "`+=" <| [] <<| "assignop" <<<| (at $sloc) }
	| PLUS_SAT_ASSIGN
    { "`|+|=" <| [] <<| "assignop" <<<| (at $sloc) }
	| MINUS_ASSIGN
    { "`-=" <| [] <<| "assignop" <<<| (at $sloc) }
	| MINUS_SAT_ASSIGN
    { "`|-|=" <| [] <<| "assignop" <<<| (at $sloc) }
	| MUL_ASSIGN
    { "`*=" <| [] <<| "assignop" <<<| (at $sloc) }
	| DIV_ASSIGN
    { "`/=" <| [] <<| "assignop" <<<| (at $sloc) }
	| MOD_ASSIGN
    { "`%=" <| [] <<| "assignop" <<<| (at $sloc) }
	| SHL_ASSIGN
    { "`<<=" <| [] <<| "assignop" <<<| (at $sloc) }
	| SHR_ASSIGN
    { "`>>=" <| [] <<| "assignop" <<<| (at $sloc) }
	| BIT_AND_ASSIGN
    { "`&=" <| [] <<| "assignop" <<<| (at $sloc) }
	| BIT_XOR_ASSIGN
    { "`^=" <| [] <<| "assignop" <<<| (at $sloc) }
	| BIT_OR_ASSIGN
    { "`|=" <| [] <<| "assignop" <<<| (at $sloc) }
;

assignmentStatement:
	| lv = lvalue o = assignop e = expression SEMICOLON
		{ "lvalue assignop expression `;" <| [ lv; o; e ] <<| "assignmentStatement" <<<| (at $sloc) }
;

(* >> Call statements *)
callStatement:
	| lv = lvalue L_PAREN args = argumentList R_PAREN SEMICOLON
		{ "lvalue `( argumentList ) `;" <| [ lv; args ] <<| "callStatement" <<<| (at $sloc) }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN SEMICOLON
		{ "lvalue `< typeArgumentList > `( argumentList ) `;" <| [ lv; targs; args ] <<| "callStatement" <<<| (at $sloc) }
;

(* >> Direct application statements *)
directApplicationStatement:
	| t = namedType DOT APPLY L_PAREN args = argumentList R_PAREN SEMICOLON
    { "namedType `. APPLY `( argumentList ) `;" <| [ t; args ] <<| "directApplicationStatement" <<<| (at $sloc) }
;

(* >> Return statements *)
returnStatement:
	| RETURN SEMICOLON
    { "RETURN `;" <| [] <<| "returnStatement" <<<| (at $sloc) }
	| RETURN e = expression SEMICOLON
    { "RETURN expression `;" <| [ e ] <<| "returnStatement" <<<| (at $sloc) }
;

(* >> Exit statements *)
exitStatement:
	| EXIT SEMICOLON
    { "EXIT `;" <| [] <<| "exitStatement" <<<| (at $sloc) }
;

(* >> Block statements *)
blockStatement:
	| al = annotationList L_BRACE
    push_scope
    sl = blockElementStatementList R_BRACE
    pop_scope
		{ "annotationList `{ blockElementStatementList }" <| [ al; sl ] <<| "blockStatement" <<<| (at $sloc) }
;

(* >> Conditional statements *)
conditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { "IF `( expression ) statement" <| [ c; t ] <<| "conditionalStatement" <<<| (at $sloc) }
	| IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { "IF `( expression ) statement ELSE statement" <| [ c; t; f ] <<| "conditionalStatement" <<<| (at $sloc) }
;

(* >> For statements *)
forInitStatement:
	| al = annotationList t = typeRef n = name i = initializerOpt
		{ "annotationList type name initializerOpt" <| [ al; t; n; i ] <<| "forInitStatement" <<<| (at $sloc) }
	| lv = lvalue L_PAREN args = argumentList R_PAREN
		{ "lvalue `( argumentList )" <| [ lv; args ] <<| "forInitStatement" <<<| (at $sloc) }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ "lvalue `< typeArgumentList > `( argumentList )" <| [ lv; targs; args ] <<| "forInitStatement" <<<| (at $sloc) }
	| lv = lvalue o = assignop e = expression
		{ "lvalue assignop expression" <| [ lv; o; e ] <<| "forInitStatement" <<<| (at $sloc) }
;

forInitStatementListNonEmpty:
	| s = forInitStatement { s }
	| sl = forInitStatementListNonEmpty COMMA s = forInitStatement
    { "forInitStatementListNonEmpty `, forInitStatement" <| [ sl; s ] <<| "forInitStatementListNonEmpty" <<<| (at $sloc) }
;

forInitStatementList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "forInitStatementList" <<<| (at $sloc) }
	| sl = forInitStatementListNonEmpty { sl }
;

forUpdateStatement:
	| s = forInitStatement { s }
;

forUpdateStatementListNonEmpty:
	| s = forUpdateStatement { s }
	| sl = forUpdateStatementListNonEmpty COMMA s = forUpdateStatement
    { "forUpdateStatementListNonEmpty `, forUpdateStatement" <| [ sl; s ] <<| "forUpdateStatementListNonEmpty" <<<| (at $sloc) }
;

forUpdateStatementList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "forUpdateStatementList" <<<| (at $sloc) }
	| sl = forUpdateStatementListNonEmpty { sl }
;

forCollectionExpression:
	| e = expression { e }
	| l = expression RANGE h = expression
    { "expression `.. expression" <| [ l; h ] <<| "forCollectionExpression" <<<| (at $sloc) }
;

forStatement:
  | al = annotationList FOR L_PAREN
    il = forInitStatementList SEMICOLON c = expression SEMICOLON
    ul = forUpdateStatementList R_PAREN b = statement
		{ "annotationList FOR `( forInitStatementList `; expression `; forUpdateStatementList ) statement" <| [ al; il; c; ul; b ] <<| "forStatement" <<<| (at $sloc) }
  | al = annotationList FOR L_PAREN
    t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { "annotationList FOR `( typeRef name IN forCollectionExpression ) statement" <| [ al; t; n; e; b ] <<| "forStatement" <<<| (at $sloc) }
  | al = annotationList FOR L_PAREN
    al_in = annotationList t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { "annotationList FOR `( annotationList typeRef name IN forCollectionExpression ) statement" <| [ al; al_in; t; n; e; b ] <<| "forStatement" <<<| (at $sloc) }
;

(* >> Switch statements *)
switchLabel:
  | DEFAULT
    { "DEFAULT" <| [] <<| "switchLabel" <<<| (at $sloc) }
  | e = expressionNonBrace
    { e }
;

switchCase:
  | l = switchLabel COLON s = blockStatement
    { "switchLabel `: blockStatement" <| [ l; s ] <<| "switchCase" <<<| (at $sloc) }
  | l = switchLabel COLON
    { "switchLabel `:" <| [ l ] <<| "switchCase" <<<| (at $sloc) }
;

switchCaseList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "switchCaseList" <<<| (at $sloc) }
  | cs = switchCaseList c = switchCase
    { "switchCaseList switchCase" <| [ cs; c ] <<| "switchCaseList" <<<| (at $sloc) }
;

switchStatement:
  | SWITCH L_PAREN e = expression R_PAREN L_BRACE cs = switchCaseList R_BRACE
    { "SWITCH `( expression ) `{ switchCaseList }" <| [ e; cs ] <<| "switchStatement" <<<| (at $sloc) }

(* >> Break and continue statements *)
breakStatement:
  | BREAK SEMICOLON
    { "BREAK `;" <| [] <<| "breakStatement" <<<| (at $sloc) }
;

continueStatement:
  | CONTINUE SEMICOLON
    { "CONTINUE `;" <| [] <<| "continueStatement" <<<| (at $sloc) }
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
		{ "`= expression" <| [ e ] <<| "initializer" <<<| (at $sloc) }
;

constantDeclaration:
  | al = annotationList CONST t = typeRef n = name i = initialValue SEMICOLON
    { "annotationList CONST typeRef name initializer `;" <| [ al; t; n; i ] <<| "constantDeclaration" <<<| (at $sloc) }
;

initializerOpt:
	| (* empty *)
		{ "`EMPTY" <| [] <<| "initializerOpt" <<<| (at $sloc) }
	| i = initialValue { i }
;

variableDeclaration:
  | al = annotationList t = typeRef n = name i = initializerOpt SEMICOLON
    { declare_var_of_il n false;
      "annotationList typeRef name initializerOpt `;" <| [ al; t; n; i ] <<| "variableDeclaration" <<<| (at $sloc) }
;

blockElementStatement:
  | d = constantDeclaration
  | d = variableDeclaration
  | d = statement
    { d }
;

blockElementStatementList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "blockElementStatementList" <<<| (at $sloc) }
  | sl = blockElementStatementList s = blockElementStatement
    { "blockElementStatementList blockElementStatement" <| [ sl; s ] <<| "blockElementStatementList" <<<| (at $sloc) }
;

(* >> Function declarations *)
functionPrototype:
	| t = typeOrVoid n = name push_scope
    tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN
    { "typeOrVoid name typeParameterListOpt `( parameterList )" <| [ t; n; tpl; pl ] <<| "functionPrototype" <<<| (at $sloc) }
;

functionDeclaration:
	| al = annotationList p = functionPrototype b = blockStatement pop_scope
    { "annotationList functionPrototype blockStatement" <| [ al; p; b ] <<| "functionDeclaration" <<<| (at $sloc) }
;

(* >> Action declarations *)
actionDeclaration: 
  | al = annotationList ACTION n = name L_PAREN pl = parameterList R_PAREN s = blockStatement
    { "annotationList ACTION name `( parameterList ) blockStatement" <| [ al; n; pl; s ] <<| "actionDeclaration" <<<| (at $sloc) }
;

(* >> Instantiations *)
objectInitializer:
	| ASSIGN L_BRACE ds = objectDeclarationList R_BRACE
    { "`= `{ objectDeclarationList }" <| [ ds ] <<| "objectInitializer" <<<| (at $sloc) }
;

instantiation:
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { "annotationList typeRef `( argumentList ) name `;" <| [ al; t; args; n ] <<| "instantiation" <<<| (at $sloc) }
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name
    i = objectInitializer SEMICOLON
    { "annotationList typeRef `( argumentList ) name objectInitializer `;" <| [ al; t; args; n; i ] <<| "instantiation" <<<| (at $sloc) } 
;

objectDeclaration:
	| d = functionDeclaration
	| d = instantiation
    { d }
;

objectDeclarationList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "objectDeclarationList" <<<| (at $sloc) }
	| ds = objectDeclarationList d = objectDeclaration
    { "objectDeclarationList objectDeclaration" <| [ ds; d ] <<| "objectDeclarationList" <<<| (at $sloc) }
;

(* >> Error declarations *)
errorDeclaration:
	| ERROR L_BRACE nl = nameList R_BRACE
    { declare_vars_of_il nl;
      "ERROR `{ nameList }" <| [ nl ] <<| "errorDeclaration" <<<| (at $sloc) }
;

(* >> Match kind declarations *)
matchKindDeclaration:
	| MATCH_KIND L_BRACE nl = nameList c = trailingCommaOpt R_BRACE
    { declare_vars_of_il nl;
      "MATCH_KIND `{ nameList trailingCommaOpt }" <| [ nl; c ] <<| "matchKindDeclaration" <<<| (at $sloc) } 
;

(* >> Derived type declarations *)
(* >>>> Enum type declarations *)
enumTypeDeclaration:
  | al = annotationList ENUM n = name L_BRACE
    nl = nameList c = trailingCommaOpt R_BRACE
    { "annotationList ENUM name `{ nameList trailingCommaOpt }" <| [ al; n; nl; c ] <<| "enumTypeDeclaration" <<<| (at $sloc) }
  | al = annotationList ENUM t = typeRef n = name L_BRACE
    el = namedExpressionList c = trailingCommaOpt R_BRACE
    { "annotationList ENUM typeRef name `{ namedExpressionList trailingCommaOpt }" <| [ al; t; n; el; c ] <<| "enumTypeDeclaration" <<<| (at $sloc) }
;

(* >>>>>> Struct, header, and union type declarations *)
typeField:
  | al = annotationList t = typeRef n = name SEMICOLON
    { "annotationList typeRef name `;" <| [ al; t; n ] <<| "typeField" <<<| (at $sloc) }
;

typeFieldList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "typeFieldList" <<<| (at $sloc) }
  | fl = typeFieldList f = typeField
    { "typeFieldList typeField" <| [ fl; f ] <<| "typeFieldList" <<<| (at $sloc) }
;

structTypeDeclaration:
  | al = annotationList STRUCT n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { "annotationList STRUCT name typeParameterListOpt `{ typeFieldList }" <| [ al; n; tpl; fl ] <<| "structTypeDeclaration" <<<| (at $sloc) }
;

headerTypeDeclaration:
  | al = annotationList HEADER n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { "annotationList HEADER name typeParameterListOpt `{ typeFieldList }" <| [ al; n; tpl; fl ] <<| "headerTypeDeclaration" <<<| (at $sloc) }
;

headerUnionTypeDeclaration:
  | al = annotationList HEADER_UNION n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { "annotationList HEADER_UNION name typeParameterListOpt `{ typeFieldList }" <| [ al; n; tpl; fl ] <<| "headerUnionTypeDeclaration" <<<| (at $sloc) }
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
    { "annotationList TYPEDEF typedef name `;" <| [ al; t; n ] <<| "typedefDeclaration" <<<| (at $sloc) }
	| al = annotationList TYPE t = typeRef n = name SEMICOLON
    { "annotationList TYPE typeRef name `;" <| [ al; t; n ] <<| "typedefDeclaration" <<<| (at $sloc) }
;

(* >> Extern declarations *)
externFunctionDeclaration:
	| al = annotationList EXTERN p = functionPrototype pop_scope SEMICOLON
		{ declare_var (id_of_function_prototype p) (has_type_params_function_prototype p);
      "annotationList EXTERN functionPrototype `;" <| [ al; p ] <<| "externFunctionDeclaration" <<<| (at $sloc) }
;

%inline externConstructorPrototype:
	| al = annotationList tid = typeIdentifier L_PAREN pl = parameterList R_PAREN SEMICOLON
    { "annotationList typeIdentifier `( parameterList ) `;" <| [ al; tid; pl ] <<| "externConstructorPrototype" <<<| (at $sloc) }

%inline externMethodPrototype:
	| al = annotationList p = functionPrototype pop_scope SEMICOLON
    { "annotationList functionPrototype `;" <| [ al; p ] <<| "externMethodPrototype" <<<| (at $sloc) }
	| al = annotationList ABSTRACT p = functionPrototype
    pop_scope SEMICOLON
    { "annotationList ABSTRACT functionPrototype `;" <| [ al; p ] <<| "externMethodPrototype" <<<| (at $sloc) }
;

externConstructorOrMethodPrototype:
  | p = externConstructorPrototype
  | p = externMethodPrototype
    { p }
;

externConstructorOrMethodPrototypeList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "externConstructorOrMethodPrototypeList" <<<| (at $sloc) }
  | pl = externConstructorOrMethodPrototypeList p = externConstructorOrMethodPrototype
    { "externConstructorOrMethodPrototypeList externConstructorOrMethodPrototype" <| [ pl; p ] <<| "externConstructorOrMethodPrototypeList" <<<| (at $sloc) }
;

externObjectDeclaration:
  | al = annotationList EXTERN n = push_externName tpl = typeParameterListOpt
    L_BRACE pl = externConstructorOrMethodPrototypeList R_BRACE pop_scope
    { let decl = "annotationList EXTERN name typeParameterListOpt `{ externConstructorOrMethodPrototypeList }" <| [ al; n; tpl; pl ] <<| "externObjectDeclaration" <<<| (at $sloc) in
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
    { "keysetExpression `: name `;" <| [ k; n ] <<| "selectCase" <<<| (at $sloc) }
;

selectCaseList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "selectCaseList" <<<| (at $sloc) }
  | cl = selectCaseList c = selectCase
    { "selectCaseList selectCase" <| [ cl; c ] <<| "selectCaseList" <<<| (at $sloc) }
;

selectExpression:
  | SELECT L_PAREN el = expressionList R_PAREN L_BRACE cl = selectCaseList R_BRACE
    { "SELECT `( expressionList ) `{ selectCaseList }" <| [ el; cl ] <<| "selectExpression" <<<| (at $sloc) }
;

(* >>>> Transition statements *)
stateExpression:
  | n = name SEMICOLON
    { "name `;" <| [ n ] <<| "stateExpression" <<<| (at $sloc) }
  | e = selectExpression
    { e }
;

transitionStatement:
  | (* empty *)
    { "`EMPTY" <| [] <<| "transitionStatement" <<<| (at $sloc) }
  | TRANSITION e = stateExpression
    { "TRANSITION stateExpression" <| [ e ] <<| "transitionStatement" <<<| (at $sloc) }
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
    { "annotationList VALUE_SET `< valueSetType > `( expression ) name `;" <| [ al; t; s; n ] <<| "valueSetDeclaration" <<<| (at $sloc) }
;

(* >>>> Parser type declarations *)
parserTypeDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
      L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { "annotationList PARSER name typeParameterListOpt `( parameterList ) `;" <| [ al; n; tpl; pl ] <<| "parserTypeDeclaration" <<<| (at $sloc) }
;

(* >>>> Parser declarations *)
parserBlockStatement:
  | al = annotationList L_BRACE sl = parserStatementList R_BRACE
    { "annotationList `{ parserStatementList }" <| [ al; sl ] <<| "parserBlockStatement" <<<| (at $sloc) }
;

parserConditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = parserStatement %prec THEN
    { "IF `( expression ) parserStatement" <| [ c; t ] <<| "parserConditionalStatement" <<<| (at $sloc) }
	| IF L_PAREN c = expression R_PAREN t = parserStatement ELSE f = parserStatement
    { "IF `( expression ) parserStatement ELSE parserStatement" <| [ c; t; f ] <<| "parserConditionalStatement" <<<| (at $sloc) }
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
    { "`EMPTY" <| [] <<| "parserStatementList" <<<| (at $sloc) }
  | sl = parserStatementList s = parserStatement
    { "parserStatementList parserStatement" <| [ sl; s ] <<| "parserStatementList" <<<| (at $sloc) }
;

parserState:
  | al = annotationList STATE n = push_name L_BRACE sl = parserStatementList t = transitionStatement R_BRACE
    { "annotationList STATE name `{ parserStatementList transitionStatement }" <| [ al; n; sl; t ] <<| "parserState" <<<| (at $sloc) }
;

parserStateList:
  | s = parserState
    { s }
  | sl = parserStateList s = parserState
    { "parserStateList parserState" <| [ sl; s ] <<| "parserStateList" <<<| (at $sloc) }
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
    { "`EMPTY" <| [] <<| "parserLocalDeclarationList" <<<| (at $sloc) }
  | dl = parserLocalDeclarationList d = parserLocalDeclaration
    { "parserLocalDeclarationList parserLocalDeclaration" <| [ dl; d ] <<| "parserLocalDeclarationList" <<<| (at $sloc) }
;

parserDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
		{ "annotationList PARSER name typeParameterListOpt `( parameterList ) constructorParameterListOpt `{ parserLocalDeclarationList parserStateList }" <| [ al; n; tpl; pl; cpl; dl; sl ] <<| "parserDeclaration" <<<| (at $sloc) }
;

(* >> Control statements and declarations *)
(* >>>> Table declarations *)
constOpt:
  | (* empty *)
    { "`EMPTY" <| [] <<| "constOpt" <<<| (at $sloc) }
  | CONST
    { "CONST" <| [] <<| "constOpt" <<<| (at $sloc) }
;

(* >>>>>> Table key property *)
tableKey:
  | e = expression COLON n = name al = annotationList SEMICOLON
    { "expression `: name annotationList `;" <| [ e; n; al ] <<| "tableKey" <<<| (at $sloc) }
;

tableKeyList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tableKeyList" <<<| (at $sloc) }
  | kl = tableKeyList k = tableKey
    { "tableKeyList tableKey" <| [ kl; k ] <<| "tableKeyList" <<<| (at $sloc) }
;

(* >>>>>> Table actions property *)
tableActionReference:
  | n = prefixedNonTypeName
    { n }
  | n = prefixedNonTypeName L_PAREN al = argumentList R_PAREN
    { "prefixedNonTypeName `( argumentList )" <| [ n; al ] <<| "tableActionReference" <<<| (at $sloc) }
;

tableAction:
  | al = annotationList ac = tableActionReference SEMICOLON
    { "annotationList tableActionReference `;" <| [ al; ac ] <<| "tableAction" <<<| (at $sloc) }
;

tableActionList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tableActionList" <<<| (at $sloc) }
  | acl = tableActionList ac = tableAction
    { "tableActionList tableAction" <| [ acl; ac ] <<| "tableActionList" <<<| (at $sloc) }
;

(* >>>>>> Table entry property *)
tableEntryPriority:
  | PRIORITY ASSIGN int = integerLiteral COLON
    { "PRIORITY `= integerLiteral `:" <| [ int ] <<| "tableEntryPriority" <<<| (at $sloc) }
  | PRIORITY ASSIGN L_PAREN e = expression R_PAREN COLON
    { "PRIORITY `= `( expression ) `:" <| [ e ] <<| "tableEntryPriority" <<<| (at $sloc) }
;

tableEntry:
  | c = constOpt p = tableEntryPriority k = keysetExpression COLON ac = tableActionReference
    al = annotationList SEMICOLON
    { "constOpt tableEntryPriority keysetExpression `: tableActionReference annotationList `;" <| [ c; p; k; ac; al ] <<| "tableEntry" <<<| (at $sloc) }
  | c = constOpt k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { "constOpt keysetExpression `: tableActionReference annotationList `;" <| [ c; k; ac; al ] <<| "tableEntry" <<<| (at $sloc) }
;

tableEntryList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tableEntryList" <<<| (at $sloc) }
  | el = tableEntryList e = tableEntry
    { "tableEntryList tableEntry" <| [ el; e ] <<| "tableEntryList" <<<| (at $sloc) }
;

(* >>>>>> Table properties *)
tableProperty:
  | KEY ASSIGN L_BRACE kl = tableKeyList R_BRACE
    { "KEY `= `{ tableKeyList }" <| [ kl ] <<| "tableProperty" <<<| (at $sloc) }
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { "ACTIONS `= `{ tableActionList }" <| [ acl ] <<| "tableProperty" <<<| (at $sloc) }
  | al = annotationList c = constOpt ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { "annotationList constOpt ENTRIES `= `{ tableEntryList }" <| [ al; c; el ] <<| "tableProperty" <<<| (at $sloc) }
  | al = annotationList c = constOpt n = tableCustomName i = initialValue SEMICOLON
    { "annotationList constOpt tableCustomName initializer `;" <| [ al; c; n; i ] <<| "tableProperty" <<<| (at $sloc) }
;

tablePropertyList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tablePropertyList" <<<| (at $sloc) }
  | pl = tablePropertyList p = tableProperty
    { "tablePropertyList tableProperty" <| [ pl; p ] <<| "tablePropertyList" <<<| (at $sloc) }
;

tableDeclaration:
  | al = annotationList TABLE n = name L_BRACE pl = tablePropertyList R_BRACE
    { "annotationList TABLE name `{ tablePropertyList }" <| [ al; n; pl ] <<| "tableDeclaration" <<<| (at $sloc) }

(* >>>> Control type declarations *)
controlTypeDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { "annotationList CONTROL name typeParameterListOpt `( parameterList ) `;" <| [ al; n; tpl; pl ] <<| "controlTypeDeclaration" <<<| (at $sloc) }
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
    { "`EMPTY" <| [] <<| "controlLocalDeclarationList" <<<| (at $sloc) }
  | dl = controlLocalDeclarationList d = controlLocalDeclaration
    { "controlLocalDeclarationList controlLocalDeclaration" <| [ dl; d ] <<| "controlLocalDeclarationList" <<<| (at $sloc) }
;

controlDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { "annotationList CONTROL name typeParameterListOpt `( parameterList ) constructorParameterListOpt `{ controlLocalDeclarationList APPLY controlBody }" <| [ al; n; tpl; pl; cpl; dl; b ] <<| "controlDeclaration" <<<| (at $sloc) }
;

(* >> Package type declarations *)
packageTypeDeclaration:
  | al = annotationList PACKAGE n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { "annotationList PACKAGE name typeParameterListOpt `( parameterList ) `;" <| [ al; n; tpl; pl ] <<| "packageTypeDeclaration" <<<| (at $sloc) }
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
    { "UNEXPECTED_TOKEN" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| ABSTRACT
    { "ABSTRACT" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| ACTION
    { "ACTION" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| ACTIONS
    { "ACTIONS" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| APPLY
    { "APPLY" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| BOOL
    { "BOOL" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| BIT
    { "BIT" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| BREAK
    { "BREAK" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| CONST
    { "CONST" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| CONTINUE
    { "CONTINUE" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| CONTROL
    { "CONTROL" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| DEFAULT
    { "DEFAULT" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| ELSE
    { "ELSE" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| ENTRIES
    { "ENTRIES" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| ENUM
    { "ENUM" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| ERROR
    { "ERROR" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| EXIT
    { "EXIT" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| EXTERN
    { "EXTERN" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| FALSE
    { "FALSE" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| FOR
    { "FOR" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| HEADER
    { "HEADER" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| HEADER_UNION
    { "HEADER_UNION" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| IF
    { "IF" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| IN
    { "IN" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| INOUT
    { "INOUT" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| INT
    { "INT" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| KEY
    { "KEY" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| MATCH_KIND
    { "MATCH_KIND" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| TYPE
    { "TYPE" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| OUT
    { "OUT" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| PARSER
    { "PARSER" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| PACKAGE
    { "PACKAGE" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| PRAGMA
    { "PRAGMA" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| RETURN
    { "RETURN" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| SELECT
    { "SELECT" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| STATE
    { "STATE" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| STRING
    { "STRING" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| STRUCT
    { "STRUCT" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| SWITCH
    { "SWITCH" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| TABLE
    { "TABLE" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| THIS
    { "THIS" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| TRANSITION
    { "TRANSITION" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| TRUE
    { "TRUE" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| TUPLE
    { "TUPLE" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| TYPEDEF
    { "TYPEDEF" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| VARBIT
    { "VARBIT" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| VALUE_SET
    { "VALUE_SET" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| LIST
    { "LIST" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| VOID
    { "VOID" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| DONTCARE
    { "`_" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| id = identifier
    { id }
	| tid = typeIdentifier
    { tid }
	| str = stringLiteral
    { str }
	| int = integerLiteral
    { int }
	| MASK
    { "`&&&" <| [] <<| "annotationToken" <<<| (at $sloc) }
  (* TODO: missing DOTS "..." in spec *)
	| RANGE
    { "`.." <| [] <<| "annotationToken" <<<| (at $sloc) }
	| SHL
    { "`<<" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| AND
    { "`&&" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| OR
    { "`||" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| EQ
    { "`==" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| NE
    { "`!=" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| GE
    { "`>=" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| LE
    { "`<=" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| PLUSPLUS
    { "`++" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| PLUS
    { "`+" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| PLUS_SAT
    { "`|+|" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| MINUS
    { "`-" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| MINUS_SAT
    { "`|-|" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| MUL
    { "`*" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| DIV
    { "`/" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| MOD
    { "`%" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| BIT_OR
    { "`|" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| BIT_AND
    { "`&" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| BIT_XOR
    { "`^" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| COMPLEMENT
    { "`~" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| L_BRACKET
    { "``[" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| R_BRACKET
    { "``]" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| L_BRACE
    { "``{" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| R_BRACE
    { "``}" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| L_ANGLE
    { "``<" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| R_ANGLE
    { "``>" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| NOT
    { "`!" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| COLON
    { "`:" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| COMMA
    { "`," <| [] <<| "annotationToken" <<<| (at $sloc) }
	| QUESTION
    { "`?" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| DOT
    { "`." <| [] <<| "annotationToken" <<<| (at $sloc) }
	| ASSIGN
    { "`=" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| SEMICOLON
    { "`;" <| [] <<| "annotationToken" <<<| (at $sloc) }
	| AT
    { "`@" <| [] <<| "annotationToken" <<<| (at $sloc) }
;

annotationBody:
	| (* empty *)
    { "`EMPTY" <| [] <<| "annotationBody" <<<| (at $sloc) }
	| ab = annotationBody L_PAREN ab_in = annotationBody R_PAREN
    { "annotationBody `( annotationBody )" <| [ ab; ab_in ] <<| "annotationBody" <<<| (at $sloc) }
	| ab = annotationBody atoken = annotationToken
    { "annotationBody annotationToken" <| [ ab; atoken ] <<| "annotationBody" <<<| (at $sloc) }
;

structuredAnnotationBody:
	| e = sequenceOrRecordElementExpression c = trailingCommaOpt
    { "sequenceOrRecordElementExpression trailingCommaOpt" <| [ e; c ] <<| "structuredAnnotationBody" <<<| (at $sloc) }
;

annotation:
	| AT name = name
    { "`@ name" <| [ name ] <<| "annotation" <<<| (at $sloc) }
	| AT name = name L_PAREN body = annotationBody R_PAREN
    { "`@ name `( annotationBody )" <| [ name; body ] <<| "annotation" <<<| (at $sloc) }
	| AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { "`@ name `[ structuredAnnotationBody ]" <| [ name; body ] <<| "annotation" <<<| (at $sloc) }
(* From Petr4: PRAGMA not in Spec, but in Petr4/p4c *)
	| PRAGMA name = name body = annotationBody PRAGMA_END
    { "`@ PRAGMA name annotationBody" <| [ name; body ] <<| "annotation" <<<| (at $sloc) }
;

annotationListNonEmpty:
	| a = annotation
    { a }
	| al = annotationListNonEmpty a = annotation
		{ "annotationListNonEmpty annotation" <| [ al; a ] <<| "annotationListNonEmpty" <<<| (at $sloc) }
;

%inline annotationList:
	| (* empty *)
    { "`EMPTY" <| [] <<| "annotationList" <<<| (at $sloc) }
	| al = annotationListNonEmpty
    { al }
;

(******** P4 program ********)
declarationList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "declarationList" <<<| (at $sloc) }
  | ds = declarationList d = declaration
    { "declarationList declaration" <| [ ds; d ] <<| "declarationList" <<<| (at $sloc) }
  | ds = declarationList SEMICOLON
    { "declarationList `;" <| [ ds ] <<| "declarationList" <<<| (at $sloc) }
;

p4program:
	| dl = declarationList END { dl #@@ "p4program" }
