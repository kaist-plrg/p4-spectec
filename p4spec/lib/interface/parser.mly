%{
  open Lang
  open Il
  open Context
  open Extract
  open Wrap
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
%token<Source.info> AT PLUSPLUS
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
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "trailingCommaOpt" }
	| COMMA
    { let mixop = mixop_of "`," in
      (mixop, []) #@ "trailingCommaOpt" }
;

(* Booleans *)
%inline booleanLiteral:
  | TRUE
    { let mixop = mixop_of "TRUE" in
      (mixop, []) #@ "booleanLiteral" }
  | FALSE
    { let mixop = mixop_of "FALSE" in
      (mixop, []) #@ "booleanLiteral" }

(* Integers *)
integerLiteral:
	| int = int
    { let mixop = mixop_of "D int" in
       (mixop, [ int ]) #@ "integerLiteral" }
(* Processed by lexer *)
	| number = NUMBER
    { fst number }
;

(* Strings *)
stringLiteral:
	| text = STRING_LITERAL
    { let mixop = mixop_of "`\" text `\"" in
       (mixop, [ text ]) #@ "stringLiteral" }
;

(* Names *)
identifier:
	| text = NAME IDENTIFIER
    { let mixop = mixop_of "`ID text" in
      (mixop, [ text ]) #@ "identifier" }
;

typeIdentifier:
	| text = NAME TYPENAME
    { let mixop = mixop_of "`TID text" in
      (mixop, [ text ]) #@ "typeIdentifier" }
;

(* >> Non-type names *)
nonTypeName:
	| id = identifier
    { id }
	| APPLY
    { let mixop = mixop_of "APPLY" in
      (mixop, []) #@ "nonTypeName" }
	| KEY
    { let mixop = mixop_of "KEY" in
      (mixop, []) #@ "nonTypeName" }
	| ACTIONS
    { let mixop = mixop_of "ACTIONS" in
      (mixop, []) #@ "nonTypeName" }
	| STATE
    { let mixop = mixop_of "STATE" in
      (mixop, []) #@ "nonTypeName" }
	| ENTRIES
    { let mixop = mixop_of "ENTRIES" in
      (mixop, []) #@ "nonTypeName" }
	| TYPE
    { let mixop = mixop_of "TYPE" in
      (mixop, []) #@ "nonTypeName" }
	| PRIORITY
    { let mixop = mixop_of "PRIORITY" in
      (mixop, []) #@ "nonTypeName" }
;

prefixedNonTypeName:
	| n = nonTypeName
    { n }
	| DOT go_toplevel n = nonTypeName go_local
    { let mixop = mixop_of "`ID `. nonTypeName" in
      (mixop, [ n ]) #@ "prefixedNonTypeName" }
;

(* >> Type names *)
typeName:
	| n = typeIdentifier { n }
;

prefixedTypeName:
	| n = typeName
    { n }
	| DOT go_toplevel tid = typeName go_local
    { let mixop = mixop_of "`TID `. typeName" in
      (mixop, [ tid ]) #@ "prefixedTypeName" }
;

(* >> Table custom property names *)
tableCustomName:
	| id = identifier
    { id }
	| tid = typeIdentifier
    { tid }
	| APPLY
    { let mixop = mixop_of "APPLY" in
      (mixop, []) #@ "tableCustomName" }
	| STATE
    { let mixop = mixop_of "STATE" in
      (mixop, []) #@ "tableCustomName" }
	| TYPE
    { let mixop = mixop_of "TYPE" in
      (mixop, []) #@ "tableCustomName" }
	| PRIORITY
    { let mixop = mixop_of "PRIORITY" in
      (mixop, []) #@ "tableCustomName" }
;

(* >> Names *)
name:
	| n = nonTypeName
	| n = typeName
    { n }
	| LIST
    { let mixop = mixop_of "LIST" in
      (mixop, []) #@ "name" }
;

nameList:
	| n = name
    { n }
	| ns = nameList COMMA n = name
    { let mixop = mixop_of "nameList `, name" in
      (mixop, [ ns; n ]) #@ "nameList" }
;

member:
	| name = name
    { name }
;

(* Directions *)
direction:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "direction" }
	| IN
    { let mixop = mixop_of "IN" in
      (mixop, []) #@ "direction" }
	| OUT
    { let mixop = mixop_of "OUT" in
      (mixop, []) #@ "direction" }
	| INOUT
    { let mixop = mixop_of "INOUT" in
      (mixop, []) #@ "direction" }
;

(* Types *)
(* >> Base types *)
baseType:
	| BOOL
    { let mixop = mixop_of "BOOL" in
      (mixop, []) #@ "baseType" }
	| MATCH_KIND
    { let mixop = mixop_of "MATCH_KIND" in
      (mixop, []) #@ "baseType" }
	| ERROR
    { let mixop = mixop_of "ERROR" in
      (mixop, []) #@ "baseType" }
	| BIT
    { let mixop = mixop_of "BIT" in
      (mixop, []) #@ "baseType" }
	| STRING
    { let mixop = mixop_of "STRING" in
      (mixop, []) #@ "baseType" }
	| INT
    { let mixop = mixop_of "INT" in
      (mixop, []) #@ "baseType" }
	| BIT l_angle v = int r_angle
    { let mixop = mixop_of "BIT `< int >" in
      (mixop, [ v ]) #@ "baseType" }
	| INT l_angle v = int r_angle
    { let mixop = mixop_of "INT `< int >" in
      (mixop, [ v ]) #@ "baseType" }
	| VARBIT l_angle v = int r_angle
    { let mixop = mixop_of "VARBIT `< int >" in
      (mixop, [ v ]) #@ "baseType" }
	| BIT l_angle L_PAREN e = expression R_PAREN r_angle
    { let mixop = mixop_of "BIT `< `( expression ) >" in
      (mixop, [ e ]) #@ "baseType" }
	| INT l_angle L_PAREN e = expression R_PAREN r_angle
    { let mixop = mixop_of "INT `< `( expression ) >" in
      (mixop, [ e ]) #@ "baseType" }
	| VARBIT l_angle L_PAREN e = expression R_PAREN r_angle
    { let mixop = mixop_of "VARBIT `< `( expression ) >" in
      (mixop, [ e ]) #@ "baseType" }
;

(* >> Named types *)
specializedType:
  | n = prefixedTypeName l_angle tal = typeArgumentList r_angle
    { let mixop = mixop_of "prefixedTypeName `< typeArgumentList >" in
      (mixop, [ n; tal ]) #@ "specializedType" }
;

namedType:
  | t = prefixedTypeName
  | t = specializedType
    { t }
;

(* >> Header stack types *)
headerStackType:
  | t = namedType L_BRACKET e = expression R_BRACKET
    { let mixop = mixop_of "namedType `[ expression ]" in
      (mixop, [ t; e ]) #@ "headerStackType" }
;

(* >> List types *)
listType:
  | LIST l_angle targ = typeArgument r_angle
    { let mixop = mixop_of "LIST `< typeArgument >" in
      (mixop, [ targ ]) #@ "listType" }
;

(* >> Tuple types *)
tupleType:
	| TUPLE l_angle targs = typeArgumentList r_angle
    { let mixop = mixop_of "TUPLE `< typeArgumentList >" in
      (mixop, [ targs ]) #@ "tupleType" }
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
    { let mixop = mixop_of "VOID" in
      (mixop, []) #@ "typeOrVoid" }
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
    { let mixop = mixop_of "typeParameterList `, typeParameter" in
      (mixop, [ tps; tp ]) #@ "typeParameterList" }
;

typeParameterListOpt:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "typeParameterListOpt" }
	| l_angle tps = typeParameterList r_angle
    { declare_types_of_il tps;
      let mixop = mixop_of "`< typeParameterList >" in
      (mixop, [ tps ]) #@ "typeParameterListOpt" }
;

(* Parameters *)
parameter:
	| al = annotationList dir = direction t = typeRef n = name i = initializerOpt
		{ declare_var_of_il n false;
      let mixop = mixop_of "annotationList direction type name initializerOpt" in
      (mixop, [ al; dir; t; n; i ]) #@ "parameter" }
;

nonEmptyParameterList:
	| p = parameter
    { p }
	| ps = nonEmptyParameterList COMMA p = parameter
    { let mixop = mixop_of "nonEmptyParameterList `, parameter" in
      (mixop, [ ps; p ]) #@ "nonEmptyParameterList" }
;

parameterList:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "parameterList" }
	| ps = nonEmptyParameterList
    { ps }
;

(* Constructor parameters *)
constructorParameterListOpt:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "constructorParameterListOpt" }
	| L_PAREN ps = parameterList R_PAREN
    { let mixop = mixop_of "`( parameterList )" in
      (mixop, [ ps ]) #@ "constructorParameterListOpt" }
;

(* Expression key-value pairs *)
namedExpression:
	| n = name ASSIGN e = expression
    { let mixop = mixop_of "name `= expression" in
      (mixop, [ n; e ]) #@ "namedExpression" }
;

namedExpressionList:
	| e = namedExpression
    { e }
	| es = namedExpressionList COMMA e = namedExpression
    { let mixop = mixop_of "namedExpressionList `, namedExpression" in
      (mixop, [ es; e ]) #@ "namedExpressionList" }
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
    { let mixop = mixop_of "THIS" in
      (mixop, []) #@ "referenceExpression" }
;

(* >> Default expressions *)
%inline defaultExpression:
	| DOTS
    { let mixop = mixop_of "`..." in
      (mixop, []) #@ "defaultExpression" }
;

(* >> Unary, binary, and ternary expressions *)
%inline unop: 
	| NOT
    { let mixop = mixop_of "`!" in
      (mixop, []) #@ "unop" }
	| COMPLEMENT
    { let mixop = mixop_of "`~" in
      (mixop, []) #@ "unop" }
	| MINUS
    { let mixop = mixop_of "`-" in
      (mixop, []) #@ "unop" }
	| PLUS
    { let mixop = mixop_of "`+" in
      (mixop, []) #@ "unop" }
;

%inline unaryExpression:
	| o = unop e = expression %prec PREFIX
		{ let mixop = mixop_of "unop expression" in
      (mixop, [ o; e ]) #@ "unaryExpression" }
;

%inline binop:
  | MUL
    { let mixop = mixop_of "`*" in
      (mixop, []) #@ "binop" }
  | DIV
    { let mixop = mixop_of "`/" in
      (mixop, []) #@ "binop" }
  | MOD
    { let mixop = mixop_of "`%" in
      (mixop, []) #@ "binop" }
  | PLUS
    { let mixop = mixop_of "`+" in
      (mixop, []) #@ "binop" }
  | PLUS_SAT
    { let mixop = mixop_of "`|+|" in
      (mixop, []) #@ "binop" }
  | MINUS
    { let mixop = mixop_of "`-" in
      (mixop, []) #@ "binop" }
  | MINUS_SAT
    { let mixop = mixop_of "`|-|" in
      (mixop, []) #@ "binop" }
  | SHL
    { let mixop = mixop_of "`<<" in
      (mixop, []) #@ "binop" }
  | r_angle R_ANGLE_SHIFT
    { let mixop = mixop_of "`>>" in
      (mixop, []) #@ "binop" }
  | LE
    { let mixop = mixop_of "`<=" in
      (mixop, []) #@ "binop" }
  | GE
    { let mixop = mixop_of "`>=" in
      (mixop, []) #@ "binop" }
  | l_angle
    { let mixop = mixop_of "``<" in
      (mixop, []) #@ "binop" }
  | r_angle
    { let mixop = mixop_of "``>" in
      (mixop, []) #@ "binop" }
  | NE
    { let mixop = mixop_of "`!=" in
      (mixop, []) #@ "binop" }
  | EQ
    { let mixop = mixop_of "`==" in
      (mixop, []) #@ "binop" }
  | BIT_AND
    { let mixop = mixop_of "`&" in
      (mixop, []) #@ "binop" }
  | BIT_XOR
    { let mixop = mixop_of "`^" in
      (mixop, []) #@ "binop" }
  | BIT_OR
    { let mixop = mixop_of "`|" in
      (mixop, []) #@ "binop" }
  | PLUSPLUS
    { let mixop = mixop_of "`++" in
      (mixop, []) #@ "binop" }
  | AND
    { let mixop = mixop_of "`&&" in
      (mixop, []) #@ "binop" }
  | OR
    { let mixop = mixop_of "`||" in
      (mixop, []) #@ "binop" }
;

%inline binaryExpression:
	| l = expression o = binop r = expression
		{ let mixop = mixop_of "expression binop expression" in
      (mixop, [ l; o; r ]) #@ "binaryExpression" }
;

%inline binaryExpressionNonBrace:
	| l = expressionNonBrace o = binop r = expression
		{ let mixop = mixop_of "expressionNonBrace binop expression" in
      (mixop, [ l; o; r ]) #@ "binaryExpressionNonBrace" }
;

%inline ternaryExpression:
	| c = expression QUESTION t = expression COLON f = expression
		{ let mixop = mixop_of "expression `? expression `: expression" in
      (mixop, [ c; t; f ]) #@ "ternaryExpression" }
;

%inline ternaryExpressionNonBrace:
	| c = expressionNonBrace QUESTION t = expression COLON f = expression
		{ let mixop = mixop_of "expressionNonBrace `? expression `: expression" in
      (mixop, [ c; t; f ]) #@ "ternaryExpressionNonBrace" }
;

(* >> Cast expressions *)
%inline castExpression:
	| L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
    { let mixop = mixop_of "`( typeRef ) expression" in
      (mixop, [ t; e ]) #@ "castExpression" }
;

(* >> Data (aggregate) expressions *)
%inline dataExpression:
	| INVALID
    { let mixop = mixop_of "`{#}" in
      (mixop, []) #@ "invalidHeaderExpression" }
	| L_BRACE e = sequenceOrRecordElementExpression c = trailingCommaOpt R_BRACE
    { let mixop = mixop_of "`{ sequenceOrRecordElementExpression trailingCommaOpt }" in
      (mixop, [ e; c ]) #@ "sequenceOrRecordExpression" }
;

(* >> Member and index access expressions *)
%inline errorAccessExpression:
	| ERROR DOT m = member
		{ let mixop = mixop_of "ERROR `. member" in
      (mixop, [ m ]) #@ "errorAccessExpression" }
;

%inline memberAccessExpression:
	| e = memberAccessBase DOT m = member %prec DOT
		{ let mixop = mixop_of "memberAccessBase `. member" in
      (mixop, [ e; m ]) #@ "memberAccessExpression" }
;

%inline indexAccessExpression:
	| a = expression L_BRACKET i = expression R_BRACKET
		{ let mixop = mixop_of "expression `[ expression ]" in
      (mixop, [ a; i ]) #@ "indexAccessExpression" }
;

%inline sliceAccessExpression:
  | a = expression L_BRACKET h = expression COLON l = expression R_BRACKET
    { let mixop = mixop_of "expression `[ expression `: expression ]" in
      (mixop, [ a; h; l ]) #@ "sliceAccessExpression" }
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
		{ let mixop = mixop_of "memberAccessBaseNonBrace `. member" in
      (mixop, [ e; m ]) #@ "memberAccessExpressionNonBrace" }
;

%inline indexAccessExpressionNonBrace:
	| a = expressionNonBrace L_BRACKET i = expression R_BRACKET
		{ let mixop = mixop_of "expressionNonBrace `[ expression ]" in
      (mixop, [ a; i ]) #@ "indexAccessExpressionNonBrace" }
;

%inline sliceAccessExpressionNonBrace:
  | a = expressionNonBrace L_BRACKET h = expression COLON l = expression R_BRACKET
    { let mixop = mixop_of "expressionNonBrace `[ expression `: expression ]" in
      (mixop, [ a; h; l ]) #@ "sliceAccessExpressionNonBrace" }
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
		{ let mixop = mixop_of "callTarget `( argumentList )" in
      (mixop, [ t; args ]) #@ "callExpression" }
	| t = callableTarget l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ let mixop = mixop_of "callTarget `< realTypeArgumentList > `( argumentList )" in
      (mixop, [ t; targs; args ]) #@ "callExpression" } 
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
		{ let mixop = mixop_of "callTargetNonBrace `( argumentList )" in
      (mixop, [ t; args ]) #@ "callExpressionNonBrace" }
	| t = callableTargetNonBrace l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ let mixop = mixop_of "callTargetNonBrace `< realTypeArgumentList > `( argumentList )" in
      (mixop, [ t; targs; args ]) #@ "callExpressionNonBrace" }

(* >> Parenthesized Expressions *)

%inline parenthesizedExpression:
	| L_PAREN e = expression R_PAREN
		{ let mixop = mixop_of "`( expression )" in
      (mixop, [ e ]) #@ "parenthesizedExpression" }
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
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "expressionList" }
	| e = expression
    { e }
	| el = expressionList COMMA e = expression
		{ let mixop = mixop_of "expressionList `, expression" in
      (mixop, [ el; e ]) #@ "expressionList" }
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
    { let mixop = mixop_of "name `= expression" in
      (mixop, [ n; e ]) #@ "recordElementExpression" }
  | n = name ASSIGN e = expression COMMA DOTS
    { let mixop = mixop_of "name `= expression `, `..." in
      (mixop, [ n; e ]) #@ "recordElementExpression" }
	| n = name ASSIGN e = expression COMMA el = namedExpressionList
    { let mixop = mixop_of "name `= expression `, namedExpressionList" in
      (mixop, [ n; e; el ]) #@ "recordElementExpression" }
  | n = name ASSIGN e = expression COMMA el = namedExpressionList COMMA DOTS
    { let mixop = mixop_of "name `= expression `, namedExpressionList `, `..." in
      (mixop, [ n; e; el ]) #@ "recordElementExpression" }
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
    { let mixop = mixop_of "expression `&&& expression" in
      (mixop, [ b; m ]) #@ "simpleKeysetExpression" }
	| l = expression RANGE h = expression
    { let mixop = mixop_of "expression `.. expression" in
      (mixop, [ l; h ]) #@ "simpleKeysetExpression" }
	| DEFAULT
    { let mixop = mixop_of "DEFAULT" in
      (mixop, []) #@ "simpleKeysetExpression" }
	| DONTCARE
    { let mixop = mixop_of "`_" in
      (mixop, []) #@ "simpleKeysetExpression" }
;

simpleKeysetExpressionList:
	| e = simpleKeysetExpression
    { e }
	| el = simpleKeysetExpressionList COMMA e = simpleKeysetExpression
    { let mixop = mixop_of "simpleKeysetExpressionList `, simpleKeysetExpression" in
      (mixop, [ el; e ]) #@ "simpleKeysetExpressionList" }
;

tupleKeysetExpression:
	| L_PAREN b = expression MASK m = expression R_PAREN
		{ let mixop = mixop_of "`( expression `&&& expression )" in
      (mixop, [ b; m ]) #@ "tupleKeysetExpression" }
	| L_PAREN l = expression RANGE h = expression R_PAREN
		{ let mixop = mixop_of "`( expression `.. expression )" in
      (mixop, [ l; h ]) #@ "tupleKeysetExpression" }
	| L_PAREN DEFAULT R_PAREN
		{ let mixop = mixop_of "`( DEFAULT )" in
      (mixop, []) #@ "tupleKeysetExpression" }
	| L_PAREN DONTCARE R_PAREN
		{ let mixop = mixop_of "`( `_ )" in
      (mixop, []) #@ "tupleKeysetExpression" }
	| L_PAREN e = simpleKeysetExpression COMMA es = simpleKeysetExpressionList R_PAREN
		{ let mixop = mixop_of "`( simpleKeysetExpression `, simpleKeysetExpressionList )" in
      (mixop, [ e; es ]) #@ "tupleKeysetExpression" }
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
    { let mixop = mixop_of "VOID" in
      (mixop, []) #@ "realTypeArgument" }
	| DONTCARE
    { let mixop = mixop_of "`_" in
      (mixop, []) #@ "realTypeArgument" }
;

realTypeArgumentList:
	| targ = realTypeArgument
    { targ }
	| targs = realTypeArgumentList COMMA targ = realTypeArgument
    { let mixop = mixop_of "realTypeArgumentList `, realTypeArgument" in
      (mixop, [ targs; targ ]) #@ "realTypeArgumentList" }
;

typeArgument:
	| t = typeRef
	| t = nonTypeName 
		{ t }
	| VOID
    { let mixop = mixop_of "VOID" in
      (mixop, []) #@ "typeArgument" }
	| DONTCARE
    { let mixop = mixop_of "`_" in
      (mixop, []) #@ "typeArgument" }
;

typeArgumentList:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "typeArgumentList" }
	| targ = typeArgument
    { targ }
	| targs = typeArgumentList COMMA targ = typeArgument
    { let mixop = mixop_of "typeArgumentList `, typeArgument" in
      (mixop, [ targs; targ ]) #@ "typeArgumentList" }
;

(* Arguments *)
argument:
	| e = expression
    { e }
	| n = name ASSIGN e = expression 
		{ let mixop = mixop_of "name `= expression" in
      (mixop, [ n; e ]) #@ "argument" }
	| name = name ASSIGN DONTCARE
		{ let mixop = mixop_of "name `= `_ " in
      (mixop, [ name ]) #@ "argument" }
	| DONTCARE
		{ let mixop = mixop_of "`_" in
      (mixop, []) #@ "argument" }
;

argumentListNonEmpty:
	| arg = argument
    { arg }
	| args = argumentListNonEmpty COMMA arg = argument
    { let mixop = mixop_of "argumentListNonEmpty `, argument" in
      (mixop, [ args; arg ]) #@ "argumentListNonEmpty" }
;

argumentList:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "argumentList" }
	| args = argumentListNonEmpty
    { args }
;

(* L-values *)
lvalue:
	| e = referenceExpression
    { e }
	| lv = lvalue DOT m = member %prec DOT
		{ let mixop = mixop_of "lvalue `. member" in
      (mixop, [ lv; m ]) #@ "lvalue" }
	| lv = lvalue L_BRACKET i = expression R_BRACKET
		{ let mixop = mixop_of "lvalue `[ expression ]" in
      (mixop, [ lv; i ]) #@ "lvalue" }
	| lv = lvalue L_BRACKET h = expression COLON l = expression R_BRACKET
		{ let mixop = mixop_of "lvalue `[ expression `: expression ]" in
      (mixop, [ lv; h; l ]) #@ "lvalue" }
	| L_PAREN lv = lvalue R_PAREN
		{ let mixop = mixop_of "`( lvalue )" in
      (mixop, [ lv ]) #@ "lvalue" }
;

(* Statements *)
(* >> Empty statements *)
emptyStatement:
	| SEMICOLON
    { let mixop = mixop_of "`;" in
      (mixop, []) #@ "emptyStatement" }
;

(* >> Assignment statements *)
assignop:
	| ASSIGN
    { let mixop = mixop_of "`=" in
      (mixop, []) #@ "assignop" }
	| PLUS_ASSIGN
    { let mixop = mixop_of "`+=" in
      (mixop, []) #@ "assignop" }
	| PLUS_SAT_ASSIGN
    { let mixop = mixop_of "`|+|=" in
      (mixop, []) #@ "assignop" }
	| MINUS_ASSIGN
    { let mixop = mixop_of "`-=" in
      (mixop, []) #@ "assignop" }
	| MINUS_SAT_ASSIGN
    { let mixop = mixop_of "`|-|=" in
      (mixop, []) #@ "assignop" }
	| MUL_ASSIGN
    { let mixop = mixop_of "`*=" in
      (mixop, []) #@ "assignop" }
	| DIV_ASSIGN
    { let mixop = mixop_of "`/=" in
      (mixop, []) #@ "assignop" }
	| MOD_ASSIGN
    { let mixop = mixop_of "`%=" in
      (mixop, []) #@ "assignop" }
	| SHL_ASSIGN
    { let mixop = mixop_of "`<<=" in
      (mixop, []) #@ "assignop" }
	| SHR_ASSIGN
    { let mixop = mixop_of "`>>=" in
      (mixop, []) #@ "assignop" }
	| BIT_AND_ASSIGN
    { let mixop = mixop_of "`&=" in
      (mixop, []) #@ "assignop" }
	| BIT_XOR_ASSIGN
    { let mixop = mixop_of "`^=" in
      (mixop, []) #@ "assignop" }
	| BIT_OR_ASSIGN
    { let mixop = mixop_of "`|=" in
      (mixop, []) #@ "assignop" }
;

assignmentStatement:
	| lv = lvalue o = assignop e = expression SEMICOLON
		{ let mixop = mixop_of "lvalue assignop expression `;" in
      (mixop, [ lv; o; e ]) #@ "assignmentStatement" }
;

(* >> Call statements *)
callStatement:
	| lv = lvalue L_PAREN args = argumentList R_PAREN SEMICOLON
		{ let mixop = mixop_of "lvalue `( argumentList ) `;" in
      (mixop, [ lv; args ]) #@ "callStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN SEMICOLON
		{ let mixop = mixop_of "lvalue `< typeArgumentList > `( argumentList ) `;" in
      (mixop, [ lv; targs; args ]) #@ "callStatement" }
;

(* >> Direct application statements *)
directApplicationStatement:
	| t = namedType DOT APPLY L_PAREN args = argumentList R_PAREN SEMICOLON
    { let mixop = mixop_of "namedType `. APPLY `( argumentList ) `;" in
      (mixop, [ t; args ]) #@ "directApplicationStatement" }
;

(* >> Return statements *)
returnStatement:
	| RETURN SEMICOLON
    { let mixop = mixop_of "RETURN `;" in
      (mixop, []) #@ "returnStatement" }
	| RETURN e = expression SEMICOLON
    { let mixop = mixop_of "RETURN expression `;" in
      (mixop, [ e ]) #@ "returnStatement" }
;

(* >> Exit statements *)
exitStatement:
	| EXIT SEMICOLON
    { let mixop = mixop_of "EXIT `;" in
      (mixop, []) #@ "exitStatement" }
;

(* >> Block statements *)
blockStatement:
	| al = annotationList L_BRACE
    push_scope
    sl = blockElementStatementList R_BRACE
    pop_scope
		{ let mixop = mixop_of "annotationList `{ blockElementStatementList }" in
      (mixop, [ al; sl ]) #@ "blockStatement" }
;

(* >> Conditional statements *)
conditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { let mixop = mixop_of "IF `( expression ) statement" in
      (mixop, [ c; t ]) #@ "conditionalStatement" }
	| IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { let mixop = mixop_of "IF `( expression ) statement ELSE statement" in
      (mixop, [ c; t; f ]) #@ "conditionalStatement" }
;

(* >> For statements *)
forInitStatement:
	| al = annotationList t = typeRef n = name i = initializerOpt
		{ let mixop = mixop_of "annotationList type name initializerOpt" in
      (mixop, [ al; t; n; i ]) #@ "forInitStatement" }
	| lv = lvalue L_PAREN args = argumentList R_PAREN
		{ let mixop = mixop_of "lvalue `( argumentList )" in
      (mixop, [ lv; args ]) #@ "forInitStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ let mixop = mixop_of "lvalue `< typeArgumentList > `( argumentList )" in
      (mixop, [ lv; targs; args ]) #@ "forInitStatement" }
	| lv = lvalue o = assignop e = expression
		{ let mixop = mixop_of "lvalue assignop expression" in
      (mixop, [ lv; o; e ]) #@ "forInitStatement" }
;

forInitStatementListNonEmpty:
	| s = forInitStatement { s }
	| sl = forInitStatementListNonEmpty COMMA s = forInitStatement
    { let mixop = mixop_of "forInitStatementListNonEmpty `, forInitStatement" in
      (mixop, [ sl; s ]) #@ "forInitStatementListNonEmpty" }
;

forInitStatementList:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "forInitStatementList" }
	| sl = forInitStatementListNonEmpty { sl }
;

forUpdateStatement:
	| s = forInitStatement { s }
;

forUpdateStatementListNonEmpty:
	| s = forUpdateStatement { s }
	| sl = forUpdateStatementListNonEmpty COMMA s = forUpdateStatement
    { let mixop = mixop_of "forUpdateStatementListNonEmpty `, forUpdateStatement" in
      (mixop, [ sl; s ]) #@ "forUpdateStatementListNonEmpty" }
;

forUpdateStatementList:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "forUpdateStatementList" }
	| sl = forUpdateStatementListNonEmpty { sl }
;

forCollectionExpression:
	| e = expression { e }
	| l = expression RANGE h = expression
    { let mixop = mixop_of "expression `.. expression" in
      (mixop, [ l; h ]) #@ "forCollectionExpression" }
;

forStatement:
  | al = annotationList FOR L_PAREN
    il = forInitStatementList SEMICOLON c = expression SEMICOLON
    ul = forUpdateStatementList R_PAREN b = statement
		{ let mixop = mixop_of "annotationList FOR `( forInitStatementList `; expression `; forUpdateStatementList ) statement" in
      (mixop, [ al; il; c; ul; b ]) #@ "forStatement" }
  | al = annotationList FOR L_PAREN
    t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { let mixop = mixop_of "annotationList FOR `( typeRef name IN forCollectionExpression ) statement" in
      (mixop, [ al; t; n; e; b ]) #@ "forStatement" }
  | al = annotationList FOR L_PAREN
    al_in = annotationList t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { let mixop = mixop_of "annotationList FOR `( annotationList typeRef name IN forCollectionExpression ) statement" in
      (mixop, [ al; al_in; t; n; e; b ]) #@ "forStatement" }
;

(* >> Switch statements *)
switchLabel:
  | DEFAULT
    { let mixop = mixop_of "DEFAULT" in
      (mixop, []) #@ "switchLabel" }
  | e = expressionNonBrace
    { e }
;

switchCase:
  | l = switchLabel COLON s = blockStatement
    { let mixop = mixop_of "switchLabel `: blockStatement" in
      (mixop, [ l; s ]) #@ "switchCase" }
  | l = switchLabel COLON
    { let mixop = mixop_of "switchLabel `:" in
      (mixop, [ l ]) #@ "switchCase" }
;

switchCaseList:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "switchCaseList" }
  | cs = switchCaseList c = switchCase
    { let mixop = mixop_of "switchCaseList switchCase" in
      (mixop, [ cs; c ]) #@ "switchCaseList" }
;

switchStatement:
  | SWITCH L_PAREN e = expression R_PAREN L_BRACE cs = switchCaseList R_BRACE
    { let mixop = mixop_of "SWITCH `( expression ) `{ switchCaseList }" in
      (mixop, [ e; cs ]) #@ "switchStatement" }

(* >> Break and continue statements *)
breakStatement:
  | BREAK SEMICOLON
    { let mixop = mixop_of "BREAK `;" in
      (mixop, []) #@ "breakStatement" }
;

continueStatement:
  | CONTINUE SEMICOLON
    { let mixop = mixop_of "CONTINUE `;" in
      (mixop, []) #@ "continueStatement" }
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
		{ let mixop = mixop_of "`= expression" in
      (mixop, [ e ]) #@ "initializer" }
;

constantDeclaration:
  | al = annotationList CONST t = typeRef n = name i = initialValue SEMICOLON
    { let mixop = mixop_of "annotationList CONST typeRef name initializer `;" in
      (mixop, [ al; t; n; i ]) #@ "constantDeclaration" }
;

initializerOpt:
	| (* empty *)
		{ let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "initializerOpt" }
	| i = initialValue { i }
;

variableDeclaration:
  | al = annotationList t = typeRef n = name i = initializerOpt SEMICOLON
    { declare_var_of_il n false;
      let mixop = mixop_of "annotationList typeRef name initializerOpt `;" in
      (mixop, [ al; t; n; i ]) #@ "variableDeclaration" }
;

blockElementStatement:
  | d = constantDeclaration
  | d = variableDeclaration
  | d = statement
    { d }
;

blockElementStatementList:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "blockElementStatementList" }
  | sl = blockElementStatementList s = blockElementStatement
    { let mixop = mixop_of "blockElementStatementList blockElementStatement" in
      (mixop, [ sl; s ]) #@ "blockElementStatementList" }
;

(* >> Function declarations *)
functionPrototype:
	| t = typeOrVoid n = name push_scope
    tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN
    { let mixop = mixop_of "typeOrVoid name typeParameterListOpt `( parameterList )" in
      (mixop, [ t; n; tpl; pl ]) #@ "functionPrototype" }
;

functionDeclaration:
	| al = annotationList p = functionPrototype b = blockStatement pop_scope
    { let mixop = mixop_of "annotationList functionPrototype blockStatement" in
      (mixop, [ al; p; b ]) #@ "functionDeclaration" }
;

(* >> Action declarations *)
actionDeclaration: 
  | al = annotationList ACTION n = name L_PAREN pl = parameterList R_PAREN s = blockStatement
    { let mixop = mixop_of "annotationList ACTION name `( parameterList ) blockStatement" in
      (mixop, [ al; n; pl; s ]) #@ "actionDeclaration" }
;

(* >> Instantiations *)
objectInitializer:
	| ASSIGN L_BRACE ds = objectDeclarationList R_BRACE
    { let mixop = mixop_of "`= `{ objectDeclarationList }" in
      (mixop, [ ds ]) #@ "objectInitializer" }
;

instantiation:
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { let mixop = mixop_of "annotationList typeRef `( argumentList ) name `;" in
      (mixop, [ al; t; args; n ]) #@ "instantiation" }
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name
    i = objectInitializer SEMICOLON
    { let mixop = mixop_of "annotationList typeRef `( argumentList ) name objectInitializer `;" in
      (mixop, [ al; t; args; n; i ]) #@ "instantiation" } 
;

objectDeclaration:
	| d = functionDeclaration
	| d = instantiation
    { d }
;

objectDeclarationList:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "objectDeclarationList" }
	| ds = objectDeclarationList d = objectDeclaration
    { let mixop = mixop_of "objectDeclarationList objectDeclaration" in
      (mixop, [ ds; d ]) #@ "objectDeclarationList" }
;

(* >> Error declarations *)
errorDeclaration:
	| ERROR L_BRACE nl = nameList R_BRACE
    { declare_vars_of_il nl;
      let mixop = mixop_of "ERROR `{ nameList }" in
      (mixop, [ nl ]) #@ "errorDeclaration" }
;

(* >> Match kind declarations *)
matchKindDeclaration:
	| MATCH_KIND L_BRACE nl = nameList c = trailingCommaOpt R_BRACE
    { declare_vars_of_il nl;
      let mixop = mixop_of "MATCH_KIND `{ nameList trailingCommaOpt }" in
      (mixop, [ nl; c ]) #@ "matchKindDeclaration" } 
;

(* >> Derived type declarations *)
(* >>>> Enum type declarations *)
enumTypeDeclaration:
  | al = annotationList ENUM n = name L_BRACE
    nl = nameList c = trailingCommaOpt R_BRACE
    { let mixop = mixop_of "annotationList ENUM name `{ nameList trailingCommaOpt }" in
      (mixop, [ al; n; nl; c ]) #@ "enumTypeDeclaration" }
  | al = annotationList ENUM t = typeRef n = name L_BRACE
    el = namedExpressionList c = trailingCommaOpt R_BRACE
    { let mixop = mixop_of "annotationList ENUM typeRef name `{ namedExpressionList trailingCommaOpt }" in
      (mixop, [ al; t; n; el; c ]) #@ "enumTypeDeclaration" }
;

(* >>>>>> Struct, header, and union type declarations *)
typeField:
  | al = annotationList t = typeRef n = name SEMICOLON
    { let mixop = mixop_of "annotationList typeRef name `;" in
      (mixop, [ al; t; n ]) #@ "typeField" }
;

typeFieldList:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "typeFieldList" }
  | fl = typeFieldList f = typeField
    { let mixop = mixop_of "typeFieldList typeField" in
      (mixop, [ fl; f ]) #@ "typeFieldList" }
;

structTypeDeclaration:
  | al = annotationList STRUCT n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { let mixop = mixop_of "annotationList STRUCT name typeParameterListOpt `{ typeFieldList }" in
      (mixop, [ al; n; tpl; fl ]) #@ "structTypeDeclaration" }
;

headerTypeDeclaration:
  | al = annotationList HEADER n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { let mixop = mixop_of "annotationList HEADER name typeParameterListOpt `{ typeFieldList }" in
      (mixop, [ al; n; tpl; fl ]) #@ "headerTypeDeclaration" }
;

headerUnionTypeDeclaration:
  | al = annotationList HEADER_UNION n = name tpl = typeParameterListOpt
    L_BRACE fl = typeFieldList R_BRACE
    { let mixop = mixop_of "annotationList HEADER_UNION name typeParameterListOpt `{ typeFieldList }" in
      (mixop, [ al; n; tpl; fl ]) #@ "headerUnionTypeDeclaration" }
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
    { let mixop = mixop_of "annotationList TYPEDEF typedef name `;" in
      (mixop, [ al; t; n ]) #@ "typedefDeclaration" }
	| al = annotationList TYPE t = typeRef n = name SEMICOLON
    { let mixop = mixop_of "annotationList TYPE typeRef name `;" in
      (mixop, [ al; t; n ]) #@ "typedefDeclaration" }
;

(* >> Extern declarations *)
externFunctionDeclaration:
	| al = annotationList EXTERN p = functionPrototype pop_scope SEMICOLON
		{ declare_var (id_of_function_prototype p) (has_type_params_function_prototype p);
      let mixop = mixop_of "annotationList EXTERN functionPrototype `;" in
      (mixop, [ al; p ]) #@ "externFunctionDeclaration" }
;

%inline externConstructorPrototype:
	| al = annotationList tid = typeIdentifier L_PAREN pl = parameterList R_PAREN SEMICOLON
    { let mixop = mixop_of "annotationList typeIdentifier `( parameterList ) `;" in
      (mixop, [ al; tid; pl ]) #@ "externConstructorPrototype" }

%inline externMethodPrototype:
	| al = annotationList p = functionPrototype pop_scope SEMICOLON
    { let mixop = mixop_of "annotationList functionPrototype `;" in
      (mixop, [ al; p ]) #@ "externMethodPrototype" }
	| al = annotationList ABSTRACT p = functionPrototype
    pop_scope SEMICOLON
    { let mixop = mixop_of "annotationList ABSTRACT functionPrototype `;" in
      (mixop, [ al; p ]) #@ "externMethodPrototype" }
;

externConstructorOrMethodPrototype:
  | p = externConstructorPrototype
  | p = externMethodPrototype
    { p }
;

externConstructorOrMethodPrototypeList:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "externConstructorOrMethodPrototypeList" }
  | pl = externConstructorOrMethodPrototypeList p = externConstructorOrMethodPrototype
    { let mixop = mixop_of "externConstructorOrMethodPrototypeList externConstructorOrMethodPrototype" in
       (mixop, [ pl; p ]) #@ "externConstructorOrMethodPrototypeList" }
;

externObjectDeclaration:
  | al = annotationList EXTERN n = push_externName tpl = typeParameterListOpt
    L_BRACE pl = externConstructorOrMethodPrototypeList R_BRACE pop_scope
    { let mixop = mixop_of "annotationList EXTERN name typeParameterListOpt `{ externConstructorOrMethodPrototypeList }" in
      let decl = (mixop, [ al; n; tpl; pl ]) #@ "externObjectDeclaration" in
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
    { let mixop = mixop_of "keysetExpression `: name `;" in
      (mixop, [ k; n ]) #@ "selectCase" }
;

selectCaseList:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
       (mixop, []) #@ "selectCaseList" }
  | cl = selectCaseList c = selectCase
    { let mixop = mixop_of "selectCaseList selectCase" in
       (mixop, [ cl; c ]) #@ "selectCaseList" }
;

selectExpression:
  | SELECT L_PAREN el = expressionList R_PAREN L_BRACE cl = selectCaseList R_BRACE
    { let mixop = mixop_of "SELECT `( expressionList ) `{ selectCaseList }" in
       (mixop, [ el; cl ]) #@ "selectExpression" }
;

(* >>>> Transition statements *)
stateExpression:
  | n = name SEMICOLON
    { let mixop = mixop_of "name `;" in
       (mixop, [ n ]) #@ "stateExpression" }
  | e = selectExpression
    { e }
;

transitionStatement:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
       (mixop, []) #@ "transitionStatement" }
  | TRANSITION e = stateExpression
    { let mixop = mixop_of "TRANSITION stateExpression" in
       (mixop, [ e ]) #@ "transitionStatement" }
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
    { let mixop = mixop_of "annotationList VALUE_SET `< valueSetType > `( expression ) name `;" in
      (mixop, [ al; t; s; n ]) #@ "valueSetDeclaration" }
;

(* >>>> Parser type declarations *)
parserTypeDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
      L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { let mixop = mixop_of "annotationList PARSER name typeParameterListOpt `( parameterList ) `;" in
      (mixop, [ al; n; tpl; pl ]) #@ "parserTypeDeclaration" }
;

(* >>>> Parser declarations *)
parserBlockStatement:
  | al = annotationList L_BRACE sl = parserStatementList R_BRACE
    { let mixop = mixop_of "annotationList `{ parserStatementList }" in
       (mixop, [ al; sl ]) #@ "parserBlockStatement" }
;

parserConditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = parserStatement %prec THEN
    { let mixop = mixop_of "IF `( expression ) parserStatement" in
      (mixop, [ c; t ]) #@ "parserConditionalStatement" }
	| IF L_PAREN c = expression R_PAREN t = parserStatement ELSE f = parserStatement
    { let mixop = mixop_of "IF `( expression ) parserStatement ELSE parserStatement" in
      (mixop, [ c; t; f ]) #@ "parserConditionalStatement" }
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
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "parserStatementList" }
  | sl = parserStatementList s = parserStatement
    { let mixop = mixop_of "parserStatementList parserStatement" in
      (mixop, [ sl; s ]) #@ "parserStatementList" }
;

parserState:
  | al = annotationList STATE n = push_name L_BRACE sl = parserStatementList t = transitionStatement R_BRACE
    { let mixop = mixop_of "annotationList STATE name `{ parserStatementList transitionStatement }" in
      (mixop, [ al; n; sl; t ]) #@ "parserState" }
;

parserStateList:
  | s = parserState
    { s }
  | sl = parserStateList s = parserState
    { let mixop = mixop_of "parserStateList parserState" in
      (mixop, [ sl; s ]) #@ "parserStateList" }
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
    { let mixop = mixop_of "`EMPTY" in
       (mixop, []) #@ "parserLocalDeclarationList" }
  | dl = parserLocalDeclarationList d = parserLocalDeclaration
    { let mixop = mixop_of "parserLocalDeclarationList parserLocalDeclaration" in
      (mixop, [ dl; d ]) #@ "parserLocalDeclarationList" }
;

parserDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
		{ let mixop = mixop_of "annotationList PARSER name typeParameterListOpt `( parameterList ) constructorParameterListOpt `{ parserLocalDeclarationList parserStateList }" in
      (mixop, [ al; n; tpl; pl; cpl; dl; sl ]) #@ "parserDeclaration" }
;

(* >> Control statements and declarations *)
(* >>>> Table declarations *)
constOpt:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "constOpt" }
  | CONST
    { let mixop = mixop_of "CONST" in
      (mixop, []) #@ "constOpt" }
;

(* >>>>>> Table key property *)
tableKey:
  | e = expression COLON n = name al = annotationList SEMICOLON
    { let mixop = mixop_of "expression `: name annotationList `;" in
      (mixop, [ e; n; al ]) #@ "tableKey" }
;

tableKeyList:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "tableKeyList" }
  | kl = tableKeyList k = tableKey
    { let mixop = mixop_of "tableKeyList tableKey" in
      (mixop, [ kl; k ]) #@ "tableKeyList" }
;

(* >>>>>> Table actions property *)
tableActionReference:
  | n = prefixedNonTypeName
    { n }
  | n = prefixedNonTypeName L_PAREN al = argumentList R_PAREN
    { let mixop = mixop_of "prefixedNonTypeName `( argumentList )" in
      (mixop, [ n; al ]) #@ "tableActionReference" }
;

tableAction:
  | al = annotationList ac = tableActionReference SEMICOLON
    { let mixop = mixop_of "annotationList tableActionReference `;" in
      (mixop, [ al; ac ]) #@ "tableAction" }
;

tableActionList:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "tableActionList" }
  | acl = tableActionList ac = tableAction
    { let mixop = mixop_of "tableActionList tableAction" in
      (mixop, [ acl; ac ]) #@ "tableActionList" }
;

(* >>>>>> Table entry property *)
tableEntryPriority:
  | PRIORITY ASSIGN int = integerLiteral COLON
    { let mixop = mixop_of "PRIORITY `= integerLiteral `:" in
      (mixop, [ int ]) #@ "tableEntryPriority" }
  | PRIORITY ASSIGN L_PAREN e = expression R_PAREN COLON
    { let mixop = mixop_of "PRIORITY `= `( expression ) `:" in
      (mixop, [ e ]) #@ "tableEntryPriority" }
;

tableEntry:
  | c = constOpt p = tableEntryPriority k = keysetExpression COLON ac = tableActionReference
    al = annotationList SEMICOLON
    { let mixop = mixop_of "constOpt tableEntryPriority keysetExpression `: tableActionReference annotationList `;" in
      (mixop, [ c; p; k; ac; al ]) #@ "tableEntry" }
  | c = constOpt k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { let mixop = mixop_of "constOpt keysetExpression `: tableActionReference annotationList `;" in
      (mixop, [ c; k; ac; al ]) #@ "tableEntry" }
;

tableEntryList:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "tableEntryList" }
  | el = tableEntryList e = tableEntry
    { let mixop = mixop_of "tableEntryList tableEntry" in
      (mixop, [ el; e ]) #@ "tableEntryList" }
;

(* >>>>>> Table properties *)
tableProperty:
  | KEY ASSIGN L_BRACE kl = tableKeyList R_BRACE
    { let mixop = mixop_of "KEY `= `{ tableKeyList }" in
      (mixop, [ kl ]) #@ "tableProperty" }
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { let mixop = mixop_of "ACTIONS `= `{ tableActionList }" in
      (mixop, [ acl ]) #@ "tableProperty" }
  | al = annotationList c = constOpt ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { let mixop = mixop_of "annotationList constOpt ENTRIES `= `{ tableEntryList }" in
      (mixop, [ al; c; el ]) #@ "tableProperty" }
  | al = annotationList c = constOpt n = tableCustomName i = initialValue SEMICOLON
    { let mixop = mixop_of "annotationList constOpt tableCustomName initializer `;" in
      (mixop, [ al; c; n; i ]) #@ "tableProperty" }
;

tablePropertyList:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "tablePropertyList" }
  | pl = tablePropertyList p = tableProperty
    { let mixop = mixop_of "tablePropertyList tableProperty" in
      (mixop, [ pl; p ]) #@ "tablePropertyList" }
;

tableDeclaration:
  | al = annotationList TABLE n = name L_BRACE pl = tablePropertyList R_BRACE
    { let mixop = mixop_of "annotationList TABLE name `{ tablePropertyList }" in
      (mixop, [ al; n; pl ]) #@ "tableDeclaration" }

(* >>>> Control type declarations *)
controlTypeDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { let mixop = mixop_of "annotationList CONTROL name typeParameterListOpt `( parameterList ) `;" in
      (mixop, [ al; n; tpl; pl ]) #@ "controlTypeDeclaration" }
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
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "controlLocalDeclarationList" }
  | dl = controlLocalDeclarationList d = controlLocalDeclaration
    { let mixop = mixop_of "controlLocalDeclarationList controlLocalDeclaration" in
      (mixop, [ dl; d ]) #@ "controlLocalDeclarationList" }
;

controlDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { let mixop = mixop_of "annotationList CONTROL name typeParameterListOpt `( parameterList ) constructorParameterListOpt `{ controlLocalDeclarationList APPLY controlBody }" in
    (mixop, [ al; n; tpl; pl; cpl; dl; b ]) #@ "controlDeclaration" }
;

(* >> Package type declarations *)
packageTypeDeclaration:
  | al = annotationList PACKAGE n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { let mixop = mixop_of "annotationList PACKAGE name typeParameterListOpt `( parameterList ) `;" in
      (mixop, [ al; n; tpl; pl ]) #@ "packageTypeDeclaration" }
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
    { let mixop = mixop_of "UNEXPECTED_TOKEN" in
      (mixop, []) #@ "annotationToken" }
	| ABSTRACT
    { let mixop = mixop_of "ABSTRACT" in
      (mixop, []) #@ "annotationToken" }
	| ACTION
    { let mixop = mixop_of "ACTION" in
      (mixop, []) #@ "annotationToken" }
	| ACTIONS
    { let mixop = mixop_of "ACTIONS" in
      (mixop, []) #@ "annotationToken" }
	| APPLY
    { let mixop = mixop_of "APPLY" in
      (mixop, []) #@ "annotationToken" }
	| BOOL
    { let mixop = mixop_of "BOOL" in
      (mixop, []) #@ "annotationToken" }
	| BIT
    { let mixop = mixop_of "BIT" in
      (mixop, []) #@ "annotationToken" }
	| BREAK
    { let mixop = mixop_of "BREAK" in
      (mixop, []) #@ "annotationToken" }
	| CONST
    { let mixop = mixop_of "CONST" in
      (mixop, []) #@ "annotationToken" }
	| CONTINUE
    { let mixop = mixop_of "CONTINUE" in
      (mixop, []) #@ "annotationToken" }
	| CONTROL
    { let mixop = mixop_of "CONTROL" in
      (mixop, []) #@ "annotationToken" }
	| DEFAULT
    { let mixop = mixop_of "DEFAULT" in
      (mixop, []) #@ "annotationToken" }
	| ELSE
    { let mixop = mixop_of "ELSE" in
      (mixop, []) #@ "annotationToken" }
	| ENTRIES
    { let mixop = mixop_of "ENTRIES" in
      (mixop, []) #@ "annotationToken" }
	| ENUM
    { let mixop = mixop_of "ENUM" in
      (mixop, []) #@ "annotationToken" }
	| ERROR
    { let mixop = mixop_of "ERROR" in
      (mixop, []) #@ "annotationToken" }
	| EXIT
    { let mixop = mixop_of "EXIT" in
      (mixop, []) #@ "annotationToken" }
	| EXTERN
    { let mixop = mixop_of "EXTERN" in
      (mixop, []) #@ "annotationToken" }
	| FALSE
    { let mixop = mixop_of "FALSE" in
      (mixop, []) #@ "annotationToken" }
	| FOR
    { let mixop = mixop_of "FOR" in
      (mixop, []) #@ "annotationToken" }
	| HEADER
    { let mixop = mixop_of "HEADER" in
      (mixop, []) #@ "annotationToken" }
	| HEADER_UNION
    { let mixop = mixop_of "HEADER_UNION" in
      (mixop, []) #@ "annotationToken" }
	| IF
    { let mixop = mixop_of "IF" in
      (mixop, []) #@ "annotationToken" }
	| IN
    { let mixop = mixop_of "IN" in
      (mixop, []) #@ "annotationToken" }
	| INOUT
    { let mixop = mixop_of "INOUT" in
      (mixop, []) #@ "annotationToken" }
	| INT
    { let mixop = mixop_of "INT" in
      (mixop, []) #@ "annotationToken" }
	| KEY
    { let mixop = mixop_of "KEY" in
      (mixop, []) #@ "annotationToken" }
	| MATCH_KIND
    { let mixop = mixop_of "MATCH_KIND" in
      (mixop, []) #@ "annotationToken" }
	| TYPE
    { let mixop = mixop_of "TYPE" in
      (mixop, []) #@ "annotationToken" }
	| OUT
    { let mixop = mixop_of "OUT" in
      (mixop, []) #@ "annotationToken" }
	| PARSER
    { let mixop = mixop_of "PARSER" in
      (mixop, []) #@ "annotationToken" }
	| PACKAGE
    { let mixop = mixop_of "PACKAGE" in
      (mixop, []) #@ "annotationToken" }
	| PRAGMA
    { let mixop = mixop_of "PRAGMA" in
      (mixop, []) #@ "annotationToken" }
	| RETURN
    { let mixop = mixop_of "RETURN" in
      (mixop, []) #@ "annotationToken" }
	| SELECT
    { let mixop = mixop_of "SELECT" in
      (mixop, []) #@ "annotationToken" }
	| STATE
    { let mixop = mixop_of "STATE" in
      (mixop, []) #@ "annotationToken" }
	| STRING
    { let mixop = mixop_of "STRING" in
      (mixop, []) #@ "annotationToken" }
	| STRUCT
    { let mixop = mixop_of "STRUCT" in
      (mixop, []) #@ "annotationToken" }
	| SWITCH
    { let mixop = mixop_of "SWITCH" in
      (mixop, []) #@ "annotationToken" }
	| TABLE
    { let mixop = mixop_of "TABLE" in
      (mixop, []) #@ "annotationToken" }
	| THIS
    { let mixop = mixop_of "THIS" in
      (mixop, []) #@ "annotationToken" }
	| TRANSITION
    { let mixop = mixop_of "TRANSITION" in
      (mixop, []) #@ "annotationToken" }
	| TRUE
    { let mixop = mixop_of "TRUE" in
      (mixop, []) #@ "annotationToken" }
	| TUPLE
    { let mixop = mixop_of "TUPLE" in
      (mixop, []) #@ "annotationToken" }
	| TYPEDEF
    { let mixop = mixop_of "TYPEDEF" in
      (mixop, []) #@ "annotationToken" }
	| VARBIT
    { let mixop = mixop_of "VARBIT" in
      (mixop, []) #@ "annotationToken" }
	| VALUE_SET
    { let mixop = mixop_of "VALUE_SET" in
      (mixop, []) #@ "annotationToken" }
	| LIST
    { let mixop = mixop_of "LIST" in
      (mixop, []) #@ "annotationToken" }
	| VOID
    { let mixop = mixop_of "VOID" in
      (mixop, []) #@ "annotationToken" }
	| DONTCARE
    { let mixop = mixop_of "`_" in
      (mixop, []) #@ "annotationToken" }
	| id = identifier
    { id }
	| tid = typeIdentifier
    { tid }
	| str = stringLiteral
    { str }
	| int = integerLiteral
    { int }
	| MASK
    { let mixop = mixop_of "`&&&" in
      (mixop, []) #@ "annotationToken" }
  (* TODO: missing DOTS "..." in spec *)
	| RANGE
    { let mixop = mixop_of "`.." in
      (mixop, []) #@ "annotationToken" }
	| SHL
    { let mixop = mixop_of "`<<" in
      (mixop, []) #@ "annotationToken" }
	| AND
    { let mixop = mixop_of "`&&" in
      (mixop, []) #@ "annotationToken" }
	| OR
    { let mixop = mixop_of "`||" in
      (mixop, []) #@ "annotationToken" }
	| EQ
    { let mixop = mixop_of "`==" in
      (mixop, []) #@ "annotationToken" }
	| NE
    { let mixop = mixop_of "`!=" in
      (mixop, []) #@ "annotationToken" }
	| GE
    { let mixop = mixop_of "`>=" in
      (mixop, []) #@ "annotationToken" }
	| LE
    { let mixop = mixop_of "`<=" in
      (mixop, []) #@ "annotationToken" }
	| PLUSPLUS
    { let mixop = mixop_of "`++" in
      (mixop, []) #@ "annotationToken" }
	| PLUS
    { let mixop = mixop_of "`+" in
      (mixop, []) #@ "annotationToken" }
	| PLUS_SAT
    { let mixop = mixop_of "`|+|" in
      (mixop, []) #@ "annotationToken" }
	| MINUS
    { let mixop = mixop_of "`-" in
      (mixop, []) #@ "annotationToken" }
	| MINUS_SAT
    { let mixop = mixop_of "`|-|" in
      (mixop, []) #@ "annotationToken" }
	| MUL
    { let mixop = mixop_of "`*" in
      (mixop, []) #@ "annotationToken" }
	| DIV
    { let mixop = mixop_of "`/" in
      (mixop, []) #@ "annotationToken" }
	| MOD
    { let mixop = mixop_of "`%" in
      (mixop, []) #@ "annotationToken" }
	| BIT_OR
    { let mixop = mixop_of "`|" in
      (mixop, []) #@ "annotationToken" }
	| BIT_AND
    { let mixop = mixop_of "`&" in
      (mixop, []) #@ "annotationToken" }
	| BIT_XOR
    { let mixop = mixop_of "`^" in
      (mixop, []) #@ "annotationToken" }
	| COMPLEMENT
    { let mixop = mixop_of "`~" in
      (mixop, []) #@ "annotationToken" }
	| L_BRACKET
    { let mixop = mixop_of "``[" in
      (mixop, []) #@ "annotationToken" }
	| R_BRACKET
    { let mixop = mixop_of "``]" in
      (mixop, []) #@ "annotationToken" }
	| L_BRACE
    { let mixop = mixop_of "``{" in
      (mixop, []) #@ "annotationToken" }
	| R_BRACE
    { let mixop = mixop_of "``}" in
      (mixop, []) #@ "annotationToken" }
	| L_ANGLE
    { let mixop = mixop_of "``<" in
      (mixop, []) #@ "annotationToken" }
	| R_ANGLE
    { let mixop = mixop_of "``>" in
      (mixop, []) #@ "annotationToken" }
	| NOT
    { let mixop = mixop_of "`!" in
      (mixop, []) #@ "annotationToken" }
	| COLON
    { let mixop = mixop_of "`:" in
      (mixop, []) #@ "annotationToken" }
	| COMMA
    { let mixop = mixop_of "`," in
      (mixop, []) #@ "annotationToken" }
	| QUESTION
    { let mixop = mixop_of "`?" in
      (mixop, []) #@ "annotationToken" }
	| DOT
    { let mixop = mixop_of "`." in
      (mixop, []) #@ "annotationToken" }
	| ASSIGN
    { let mixop = mixop_of "`=" in
      (mixop, []) #@ "annotationToken" }
	| SEMICOLON
    { let mixop = mixop_of "`;" in
      (mixop, []) #@ "annotationToken" }
	| AT
    { let mixop = mixop_of "`@" in
      (mixop, []) #@ "annotationToken" }
;

annotationBody:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "annotationBody" }
	| ab = annotationBody L_PAREN ab_in = annotationBody R_PAREN
    { let mixop = mixop_of "annotationBody `( annotationBody )" in
      (mixop, [ ab; ab_in ]) #@ "annotationBody" }
	| ab = annotationBody at = annotationToken
    { let mixop = mixop_of "annotationBody annotationToken" in
      (mixop, [ ab; at ]) #@ "annotationBody" }
;

structuredAnnotationBody:
	| e = sequenceOrRecordElementExpression c = trailingCommaOpt
    { let mixop = mixop_of "sequenceOrRecordElementExpression trailingCommaOpt" in
      (mixop, [ e; c ]) #@ "structuredAnnotationBody" }
;

annotation:
	| AT name = name
    { let mixop = mixop_of "`@ name" in
      (mixop, [ name ]) #@ "annotation" }
	| AT name = name L_PAREN body = annotationBody R_PAREN
    { let mixop = mixop_of "`@ name `( annotationBody )" in
      (mixop, [ name; body ]) #@ "annotation" }
	| AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { let mixop = mixop_of "`@ name `[ structuredAnnotationBody ]" in
      (mixop, [ name; body ]) #@ "annotation" }
(* From Petr4: PRAGMA not in Spec, but in Petr4/p4c *)
	| PRAGMA name = name body = annotationBody PRAGMA_END
    { let mixop = mixop_of "`@ PRAGMA name annotationBody" in
      (mixop, [ name; body ]) #@ "annotation" }
;

annotationListNonEmpty:
	| a = annotation
    { a }
	| al = annotationListNonEmpty a = annotation
		{ let mixop = mixop_of "annotationListNonEmpty annotation" in
      (mixop, [ al; a ]) #@ "annotationListNonEmpty" }
;

%inline annotationList:
	| (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "annotationList" }
	| al = annotationListNonEmpty
    { al }
;

(******** P4 program ********)
declarationList:
  | (* empty *)
    { let mixop = mixop_of "`EMPTY" in
      (mixop, []) #@ "declarationList" }
  | ds = declarationList d = declaration
    { let mixop = mixop_of "declarationList declaration" in
      (mixop, [ ds; d ]) #@ "declarationList" }
  | ds = declarationList SEMICOLON
    { let mixop = mixop_of "declarationList `;" in
      (mixop, [ ds ]) #@ "declarationList" }
;

p4program:
	| dl = declarationList END { dl #@@ "p4program" }
