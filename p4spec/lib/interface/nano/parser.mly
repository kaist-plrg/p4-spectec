%{
  open Context
  open Extract
  open Value.Make
  open Util.Source

  let position_to_pos (position : Lexing.position) =
    {
      file = position.pos_fname;
      line = position.pos_lnum;
      column = position.pos_cnum - position.pos_bol
    }

  let positions_to_region (position_left : Lexing.position)
      (position_right : Lexing.position) =
    {
      left = position_to_pos position_left;
      right = position_to_pos position_right
    }

  let at ((position_left, position_right) : Lexing.position * Lexing.position)
    = positions_to_region position_left position_right

  let rec declare_types_of_il (value : Value.t) : unit =
    Value.Get.mtch value
      [
        ("typeParameterList `, typeParameter",
          fun values ->
            declare_types_of_il (List.nth values 0);
            declare_type (id_of_name (List.nth values 1)));
        ("ID text", fun _ -> declare_type (id_of_name value));
        ("TID text", fun _ -> declare_type (id_of_name value));
      ]
      (fun _ -> ())
%}

(**************************** TOKENS ******************************)
%token END
%token TYPENAME IDENTIFIER
%token<Lang.Il.value> NAME STRING_LITERAL
%token<Lang.Il.value * string> NUMBER_INT NUMBER
%token LE GE SHL SHR AND OR NE EQ
%token PLUS MINUS MUL DIV MOD
%token BIT_OR BIT_AND BIT_XOR COMPLEMENT
%token L_BRACE R_BRACE L_ANGLE R_ANGLE L_PAREN R_PAREN
%token ASSIGN COLON COMMA QUESTION DOT NOT SEMICOLON
%token PLUSPLUS
%token DONTCARE
%token TRUE FALSE
%token ACTION ACTIONS APPLY BOOL BIT CONST CONTROL
%token ELSE ENTRIES EXTERN HEADER IF IN INOUT
%token INT KEY SELECT MATCH_KIND OUT PACKAGE PARSER STATE STRING STRUCT
%token TABLE TRANSITION VOID

(**************************** PRIORITY AND ASSOCIATIVITY ******************************)
%right THEN ELSE
%nonassoc QUESTION
%nonassoc COLON
%left OR
%left AND
%left EQ NE
%left LE GE L_ANGLE R_ANGLE
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left SHL SHR
%left PLUSPLUS PLUS MINUS
%left MUL DIV MOD
%right PREFIX
%left DOT

%start program

(**************************** TYPES ******************************)
%type <Lang.Il.value>
  int
  trailingCommaOpt booleanLiteral
  integerLiteral stringLiteral
  identifier typeIdentifier nonTypeName typeName name nameList member
  direction
  baseType specializedType namedType type_ typeOrVoid
  typeParameter typeParameterList typeParameterListOpt
  parameter nonEmptyParameterList parameterList
  literalExpression referenceExpression
  unop unaryExpression binop binaryExpression ternaryExpression
  memberAccessExpression accessExpression
  callExpression parenthesizedExpression
  expression memberAccessBase
  typeArgument typeArgumentList argument argumentListNonEmpty argumentList
  lvalue
  emptyStatement assignmentStatement callStatement blockStatement conditionalStatement statement
  initializer_ initializerOpt constantDeclaration variableDeclaration
  blockElementStatement blockElementStatementList
  functionPrototype
  actionDeclaration
  instantiation
  matchKindDeclaration
  typeField typeFieldList structTypeDeclaration headerTypeDeclaration derivedTypeDeclaration
  externFunctionDeclaration externMethodPrototype externMethodPrototypeList
  externObjectDeclaration externDeclaration
  selectCase selectCaseList selectExpression
  stateExpression transitionStatement
  parserTypeDeclaration
  parserBlockStatement parserConditionalStatement parserStatement parserStatementList
  parserState parserStateList
  parserLocalDeclaration parserLocalDeclarationList parserDeclaration
  tableKey tableActionReference tableAction tableActionList
  tableEntry tableEntryList
  tableProperty tablePropertyList tableDeclaration
  controlTypeDeclaration controlBody
  controlLocalDeclaration controlLocalDeclarationList controlDeclaration
  packageTypeDeclaration
  typeDeclaration declaration declarationList program
%type <unit> push_scope pop_scope
%%

(**************************** CONTEXTS ******************************)
push_scope:
  | (* empty *)
    { push_scope () }
;
pop_scope:
  | (* empty *)
    { pop_scope () }
;

(**************************** NANO-P4 GRAMMAR ******************************)

(* Aux *)
int:
  | int = NUMBER_INT
    { fst int }
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
    { "ID text" <| [ text ] <<| "identifier" <<<| (at $sloc) }
;

typeIdentifier:
  | text = NAME TYPENAME
    { "TID text" <| [ text ] <<| "typeIdentifier" <<<| (at $sloc) }
;

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
;

typeName:
  | n = typeIdentifier { n }
;

name:
  | n = nonTypeName
  | n = typeName
    { n }
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
baseType:
  | BOOL
    { "BOOL" <| [] <<| "baseType" <<<| (at $sloc) }
  | MATCH_KIND
    { "MATCH_KIND" <| [] <<| "baseType" <<<| (at $sloc) }
  | STRING
    { "STRING" <| [] <<| "baseType" <<<| (at $sloc) }
  | INT
    { "INT" <| [] <<| "baseType" <<<| (at $sloc) }
  | INT L_ANGLE v = int R_ANGLE
    { "INT `< int >" <| [ v ] <<| "baseType" <<<| (at $sloc) }
  | BIT
    { "BIT" <| [] <<| "baseType" <<<| (at $sloc) }
  | BIT L_ANGLE v = int R_ANGLE
    { "BIT `< int >" <| [ v ] <<| "baseType" <<<| (at $sloc) }
;

specializedType:
  | n = typeName L_ANGLE tal = typeArgumentList R_ANGLE
    { "typeName `< typeArgumentList >" <| [ n; tal ] <<| "specializedType" <<<| (at $sloc) }
;

namedType:
  | t = typeName
  | t = specializedType
    { t }
;

type_:
  | t = baseType
  | t = namedType
    { t }
;

typeOrVoid:
  | t = type_
    { t }
  | VOID
    { "VOID" <| [] <<| "typeOrVoid" <<<| (at $sloc) }
  | id = identifier
    { id }
;

(* Type parameters *)
typeParameter:
  | n = name { n }
;

typeParameterList:
  | tp = typeParameter
    { tp }
  | tps = typeParameterList COMMA tp = typeParameter
    { "typeParameterList `, typeParameter" <| [ tps; tp ] <<| "typeParameterList" <<<| (at $sloc) }
;

typeParameterListOpt:
  | (* empty *)
    { "`EMPTY" <| [] <<| "typeParameterListOpt" <<<| (at $sloc) }
  | L_ANGLE tps = typeParameterList R_ANGLE
    { declare_types_of_il tps;
      "`< typeParameterList >" <| [ tps ] <<| "typeParameterListOpt" <<<| (at $sloc) }
;

(* Parameters *)
parameter:
  | dir = direction t = type_ n = name
    { declare_var (id_of_name n);
      "direction type name" <| [ dir; t; n ] <<| "parameter" <<<| (at $sloc) }
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

(* Expressions *)
%inline literalExpression:
  | bool = booleanLiteral { bool }
  | int = integerLiteral  { int }
  | str = stringLiteral   { str }
;

%inline referenceExpression:
  | n = nonTypeName
    { n }
;

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
  | MINUS
    { "`-" <| [] <<| "binop" <<<| (at $sloc) }
  | SHL
    { "`<<" <| [] <<| "binop" <<<| (at $sloc) }
  | SHR
    { "`>>" <| [] <<| "binop" <<<| (at $sloc) }
  | LE
    { "`<=" <| [] <<| "binop" <<<| (at $sloc) }
  | GE
    { "`>=" <| [] <<| "binop" <<<| (at $sloc) }
  | L_ANGLE
    { "``<" <| [] <<| "binop" <<<| (at $sloc) }
  | R_ANGLE
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

%inline ternaryExpression:
  | c = expression QUESTION t = expression COLON f = expression
    { "expression `? expression `: expression" <| [ c; t; f ] <<| "ternaryExpression" <<<| (at $sloc) }
;

%inline memberAccessExpression:
  | e = memberAccessBase DOT m = member
    { "memberAccessBase `. member" <| [ e; m ] <<| "memberAccessExpression" <<<| (at $sloc) }
;

%inline accessExpression:
  | e = memberAccessExpression
    { e }
;

%inline callExpression:
  | t = namedType L_PAREN args = argumentList R_PAREN
    { "callTarget `( argumentList )" <| [ t; args ] <<| "callExpression" <<<| (at $sloc) }
;

%inline parenthesizedExpression:
  | L_PAREN e = expression R_PAREN
    { "`( expression )" <| [ e ] <<| "parenthesizedExpression" <<<| (at $sloc) }
;

expression:
  | e = literalExpression
  | e = referenceExpression
  | e = unaryExpression
  | e = binaryExpression
  | e = ternaryExpression
  | e = accessExpression
  | e = callExpression
  | e = parenthesizedExpression
    { e }
;

%inline memberAccessBase:
  | e = typeName
  | e = expression
    { e }
;

(* Type arguments *)
typeArgument:
  | t = type_
  | t = nonTypeName
    { t }
  | VOID
    { "VOID" <| [] <<| "typeArgument" <<<| (at $sloc) }
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
  | n = name ASSIGN DONTCARE
    { "name `= `_" <| [ n ] <<| "argument" <<<| (at $sloc) }
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
  | lv = lvalue DOT m = member
    { "lvalue `. member" <| [ lv; m ] <<| "lvalue" <<<| (at $sloc) }
  | L_PAREN lv = lvalue R_PAREN
    { "`( lvalue )" <| [ lv ] <<| "lvalue" <<<| (at $sloc) }
;

(* Statements *)
emptyStatement:
  | SEMICOLON
    { "`;" <| [] <<| "emptyStatement" <<<| (at $sloc) }
;

assignmentStatement:
  | lv = lvalue ASSIGN e = expression SEMICOLON
    { "lvalue `= expression `;" <| [ lv; e ] <<| "assignmentStatement" <<<| (at $sloc) }
;

callStatement:
  | lv = lvalue L_PAREN args = argumentList R_PAREN SEMICOLON
    { "lvalue `( argumentList ) `;" <| [ lv; args ] <<| "callStatement" <<<| (at $sloc) }
  | lv = lvalue L_ANGLE targs = typeArgumentList R_ANGLE L_PAREN args = argumentList R_PAREN SEMICOLON
    { "lvalue `< typeArgumentList > `( argumentList ) `;" <| [ lv; targs; args ] <<| "callStatement" <<<| (at $sloc) }
;

blockStatement:
  | L_BRACE push_scope sl = blockElementStatementList R_BRACE pop_scope
    { "`{ blockElementStatementList }" <| [ sl ] <<| "blockStatement" <<<| (at $sloc) }
;

conditionalStatement:
  | IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { "IF `( expression ) statement" <| [ c; t ] <<| "conditionalStatement" <<<| (at $sloc) }
  | IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { "IF `( expression ) statement ELSE statement" <| [ c; t; f ] <<| "conditionalStatement" <<<| (at $sloc) }
;

statement:
  | s = emptyStatement
  | s = assignmentStatement
  | s = callStatement
  | s = blockStatement
  | s = conditionalStatement
    { s }
;

(* Declarations *)
initializer_:
  | ASSIGN e = expression
    { "`= expression" <| [ e ] <<| "initializer" <<<| (at $sloc) }
;

initializerOpt:
  | (* empty *)
    { "`EMPTY" <| [] <<| "initializerOpt" <<<| (at $sloc) }
  | i = initializer_ { i }
;

constantDeclaration:
  | CONST t = type_ n = name i = initializer_ SEMICOLON
    { "CONST type name initializer `;" <| [ t; n; i ] <<| "constantDeclaration" <<<| (at $sloc) }
;

variableDeclaration:
  | t = type_ n = name i = initializerOpt SEMICOLON
    { declare_var (id_of_name n);
      "type name initializerOpt `;" <| [ t; n; i ] <<| "variableDeclaration" <<<| (at $sloc) }
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

(* Function prototype *)
functionPrototype:
  | VOID n = name push_scope tpl = typeParameterListOpt L_PAREN pl = parameterList R_PAREN
    { "VOID name typeParameterListOpt `( parameterList )" <| [ n; tpl; pl ] <<| "functionPrototype" <<<| (at $sloc) }
;

(* Action declarations *)
actionDeclaration:
  | ACTION n = name L_PAREN push_scope pl = parameterList R_PAREN s = blockStatement pop_scope
    { "ACTION name `( parameterList ) blockStatement" <| [ n; pl; s ] <<| "actionDeclaration" <<<| (at $sloc) }
;

(* Instantiations *)
instantiation:
  | t = type_ L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { "type `( argumentList ) name `;" <| [ t; args; n ] <<| "instantiation" <<<| (at $sloc) }
;

(* Match kind declarations *)
matchKindDeclaration:
  | MATCH_KIND L_BRACE nl = nameList c = trailingCommaOpt R_BRACE
    { "MATCH_KIND `{ nameList trailingCommaOpt }" <| [ nl; c ] <<| "matchKindDeclaration" <<<| (at $sloc) }
;

(* Derived type declarations *)
typeField:
  | t = type_ n = name SEMICOLON
    { "type name `;" <| [ t; n ] <<| "typeField" <<<| (at $sloc) }
;

typeFieldList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "typeFieldList" <<<| (at $sloc) }
  | fl = typeFieldList f = typeField
    { "typeFieldList typeField" <| [ fl; f ] <<| "typeFieldList" <<<| (at $sloc) }
;

structTypeDeclaration:
  | STRUCT n = name L_BRACE fl = typeFieldList R_BRACE
    { declare_type (id_of_name n);
      "STRUCT name `{ typeFieldList }" <| [ n; fl ] <<| "structTypeDeclaration" <<<| (at $sloc) }
;

headerTypeDeclaration:
  | HEADER n = name L_BRACE fl = typeFieldList R_BRACE
    { declare_type (id_of_name n);
      "HEADER name `{ typeFieldList }" <| [ n; fl ] <<| "headerTypeDeclaration" <<<| (at $sloc) }
;

derivedTypeDeclaration:
  | d = structTypeDeclaration
  | d = headerTypeDeclaration
    { d }
;

(* Extern declarations *)
externFunctionDeclaration:
  | EXTERN p = functionPrototype pop_scope SEMICOLON
    { "EXTERN functionPrototype `;" <| [ p ] <<| "externFunctionDeclaration" <<<| (at $sloc) }
;

externMethodPrototype:
  | p = functionPrototype pop_scope SEMICOLON
    { "functionPrototype `;" <| [ p ] <<| "externMethodPrototype" <<<| (at $sloc) }
;

externMethodPrototypeList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "externMethodPrototypeList" <<<| (at $sloc) }
  | pl = externMethodPrototypeList p = externMethodPrototype
    { "externMethodPrototypeList externMethodPrototype" <| [ pl; p ] <<| "externMethodPrototypeList" <<<| (at $sloc) }
;

externObjectDeclaration:
  | EXTERN n = nonTypeName push_scope tpl = typeParameterListOpt
    L_BRACE pl = externMethodPrototypeList R_BRACE pop_scope
    { declare_type (id_of_name n);
      "EXTERN nonTypeName typeParameterListOpt `{ externMethodPrototypeList }" <| [ n; tpl; pl ] <<| "externObjectDeclaration" <<<| (at $sloc) }
;

externDeclaration:
  | d = externFunctionDeclaration
  | d = externObjectDeclaration
    { d }
;

(* Parser statements and declarations *)
selectCase:
  | e = expression COLON n = name SEMICOLON
    { "expression `: name `;" <| [ e; n ] <<| "selectCase" <<<| (at $sloc) }
;

selectCaseList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "selectCaseList" <<<| (at $sloc) }
  | cl = selectCaseList c = selectCase
    { "selectCaseList selectCase" <| [ cl; c ] <<| "selectCaseList" <<<| (at $sloc) }
;

selectExpression:
  | SELECT L_PAREN e = expression R_PAREN L_BRACE cl = selectCaseList R_BRACE
    { "SELECT `( expression ) `{ selectCaseList }" <| [ e; cl ] <<| "selectExpression" <<<| (at $sloc) }
;

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

parserTypeDeclaration:
  | PARSER n = name push_scope tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { declare_type (id_of_name n);
      "PARSER name typeParameterListOpt `( parameterList ) `;" <| [ n; tpl; pl ] <<| "parserTypeDeclaration" <<<| (at $sloc) }
;

parserBlockStatement:
  | L_BRACE sl = parserStatementList R_BRACE
    { "`{ parserStatementList }" <| [ sl ] <<| "parserBlockStatement" <<<| (at $sloc) }
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
  | STATE n = name push_scope L_BRACE sl = parserStatementList t = transitionStatement R_BRACE pop_scope
    { declare_var (id_of_name n);
      "STATE name `{ parserStatementList transitionStatement }" <| [ n; sl; t ] <<| "parserState" <<<| (at $sloc) }
;

parserStateList:
  | s = parserState
    { s }
  | sl = parserStateList s = parserState
    { "parserStateList parserState" <| [ sl; s ] <<| "parserStateList" <<<| (at $sloc) }
;

parserLocalDeclaration:
  | d = constantDeclaration
  | d = variableDeclaration
    { d }
;

parserLocalDeclarationList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "parserLocalDeclarationList" <<<| (at $sloc) }
  | dl = parserLocalDeclarationList d = parserLocalDeclaration
    { "parserLocalDeclarationList parserLocalDeclaration" <| [ dl; d ] <<| "parserLocalDeclarationList" <<<| (at $sloc) }
;

parserDeclaration:
  | PARSER n = name push_scope tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
    { declare_type (id_of_name n);
      "PARSER name typeParameterListOpt `( parameterList ) `{ parserLocalDeclarationList parserStateList }" <| [ n; tpl; pl; dl; sl ] <<| "parserDeclaration" <<<| (at $sloc) }
;

(* Control declarations *)
tableKey:
  | L_BRACE e = expression COLON n = name SEMICOLON R_BRACE
    { "`{ expression `: name `; }" <| [ e; n ] <<| "tableKey" <<<| (at $sloc) }
;

tableActionReference:
  | n = nonTypeName
    { n }
  | n = nonTypeName L_PAREN al = argumentList R_PAREN
    { "nonTypeName `( argumentList )" <| [ n; al ] <<| "tableActionReference" <<<| (at $sloc) }
;

tableAction:
  | ac = tableActionReference SEMICOLON
    { "tableActionReference `;" <| [ ac ] <<| "tableAction" <<<| (at $sloc) }
;

tableActionList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tableActionList" <<<| (at $sloc) }
  | acl = tableActionList ac = tableAction
    { "tableActionList tableAction" <| [ acl; ac ] <<| "tableActionList" <<<| (at $sloc) }
;

tableEntry:
  | L_PAREN e = expression R_PAREN COLON ac = tableActionReference SEMICOLON
    { "`( expression ) `: tableActionReference `;" <| [ e; ac ] <<| "tableEntry" <<<| (at $sloc) }
;

tableEntryList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tableEntryList" <<<| (at $sloc) }
  | el = tableEntryList e = tableEntry
    { "tableEntryList tableEntry" <| [ el; e ] <<| "tableEntryList" <<<| (at $sloc) }
;

tableProperty:
  | KEY ASSIGN tableKey
    { let k = $3 in
      "KEY `= tableKey" <| [ k ] <<| "tableProperty" <<<| (at $sloc) }
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { "ACTIONS `= `{ tableActionList }" <| [ acl ] <<| "tableProperty" <<<| (at $sloc) }
  | CONST ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { "CONST ENTRIES `= `{ tableEntryList }" <| [ el ] <<| "tableProperty" <<<| (at $sloc) }
;

tablePropertyList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "tablePropertyList" <<<| (at $sloc) }
  | pl = tablePropertyList p = tableProperty
    { "tablePropertyList tableProperty" <| [ pl; p ] <<| "tablePropertyList" <<<| (at $sloc) }
;

tableDeclaration:
  | TABLE n = name L_BRACE pl = tablePropertyList R_BRACE
    { "TABLE name `{ tablePropertyList }" <| [ n; pl ] <<| "tableDeclaration" <<<| (at $sloc) }
;

controlTypeDeclaration:
  | CONTROL n = name push_scope tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { declare_type (id_of_name n);
      "CONTROL name typeParameterListOpt `( parameterList ) `;" <| [ n; tpl; pl ] <<| "controlTypeDeclaration" <<<| (at $sloc) }
;

controlBody:
  | b = blockStatement { b }
;

controlLocalDeclaration:
  | d = constantDeclaration
  | d = variableDeclaration
  | d = tableDeclaration
    { d }
;

controlLocalDeclarationList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "controlLocalDeclarationList" <<<| (at $sloc) }
  | dl = controlLocalDeclarationList d = controlLocalDeclaration
    { "controlLocalDeclarationList controlLocalDeclaration" <| [ dl; d ] <<| "controlLocalDeclarationList" <<<| (at $sloc) }
;

controlDeclaration:
  | CONTROL n = name push_scope tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { declare_type (id_of_name n);
      "CONTROL name typeParameterListOpt `( parameterList ) `{ controlLocalDeclarationList APPLY controlBody }" <| [ n; tpl; pl; dl; b ] <<| "controlDeclaration" <<<| (at $sloc) }
;

(* Package type declarations *)
packageTypeDeclaration:
  | PACKAGE n = name push_scope tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { declare_type (id_of_name n);
      "PACKAGE name typeParameterListOpt `( parameterList ) `;" <| [ n; tpl; pl ] <<| "packageTypeDeclaration" <<<| (at $sloc) }
;

(* Type declarations *)
typeDeclaration:
  | d = derivedTypeDeclaration
  | d = parserTypeDeclaration
  | d = controlTypeDeclaration
  | d = packageTypeDeclaration
    { d }
;

(* Declarations *)
declaration:
  | d = constantDeclaration
  | d = instantiation
  | d = actionDeclaration
  | d = matchKindDeclaration
  | d = externDeclaration
  | d = parserDeclaration
  | d = controlDeclaration
  | d = typeDeclaration
    { d }
;

declarationList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "declarationList" <<<| (at $sloc) }
  | ds = declarationList d = declaration
    { "declarationList declaration" <| [ ds; d ] <<| "declarationList" <<<| (at $sloc) }
;

(* P4 program *)
program:
  | dl = declarationList END
    { dl #@@ "program" }
;
