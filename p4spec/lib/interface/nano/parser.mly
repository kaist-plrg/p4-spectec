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
  booleanLiteral
  integerLiteral
  identifier typeIdentifier nonTypeName typeName name nameList member
  direction
  baseType namedType type_ typeOrVoid
  parameter nonEmptyParameterList parameterList
  literalExpression referenceExpression
  unop unaryExpression binop binaryExpression
  memberAccessExpression accessExpression
  callExpression parenthesizedExpression
  expression
  argument argumentListNonEmpty argumentList
  lvalue
  emptyStatement assignmentStatement callStatement blockStatement conditionalStatement statement
  statementList
  initializer_ variableDeclaration
  functionPrototype
  actionDeclaration
  instantiation
  matchKindDeclaration
  typeField typeFieldList structTypeDeclaration headerTypeDeclaration derivedTypeDeclaration
  externMethodPrototype externMethodPrototypeList
  externObjectDeclaration externDeclaration
  selectCase selectCaseList selectExpression
  stateExpression transitionStatement
  parserTypeDeclaration
  parserState parserStateList
  parserLocalDeclaration parserLocalDeclarationList parserDeclaration
  tableKey tableActionReference tableAction tableActionList
  tableEntry tableEntryList
  tableKeyProperty tableActionsProperty tableEntriesProperty
  tableProperties tableDeclaration
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

(* Names *)
identifier:
  | text = NAME IDENTIFIER
    { "`ID text" <| [ text ] <<| "identifier" <<<| (at $sloc) }
;

typeIdentifier:
  | text = NAME TYPENAME
    { "`TID text" <| [ text ] <<| "typeIdentifier" <<<| (at $sloc) }
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
  | INT L_ANGLE v = int R_ANGLE
    { "INT `< int >" <| [ v ] <<| "baseType" <<<| (at $sloc) }
  | BIT L_ANGLE v = int R_ANGLE
    { "BIT `< int >" <| [ v ] <<| "baseType" <<<| (at $sloc) }
;

namedType:
  | t = typeName
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
  | e = accessExpression
  | e = callExpression
  | e = parenthesizedExpression
    { e }
;

%inline memberAccessBase:
  | e = expression
    { e }
;

(* Arguments *)
argument:
  | e = expression
    { e }
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
;

blockStatement:
  | L_BRACE push_scope sl = statementList R_BRACE pop_scope
    { "`{ statementList }" <| [ sl ] <<| "blockStatement" <<<| (at $sloc) }
;

conditionalStatement:
  | IF L_PAREN c = expression R_PAREN t = blockStatement ELSE f = blockStatement
    { "IF `( expression ) blockStatement ELSE blockStatement" <| [ c; t; f ] <<| "conditionalStatement" <<<| (at $sloc) }
;

statement:
  | s = variableDeclaration
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

variableDeclaration:
  | t = type_ n = name i = initializer_ SEMICOLON
    { declare_var (id_of_name n);
      "type name initializer `;" <| [ t; n; i ] <<| "variableDeclaration" <<<| (at $sloc) }
;

statementList:
  | (* empty *)
    { "`EMPTY" <| [] <<| "statementList" <<<| (at $sloc) }
  | sl = statementList s = statement
    { "statementList statement" <| [ sl; s ] <<| "statementList" <<<| (at $sloc) }
;

(* Function prototype *)
functionPrototype:
  | VOID n = name push_scope L_PAREN pl = parameterList R_PAREN
    { "VOID name `( parameterList )" <| [ n; pl ] <<| "functionPrototype" <<<| (at $sloc) }
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
  | MATCH_KIND L_BRACE nl = nameList R_BRACE
    { "MATCH_KIND `{ nameList }" <| [ nl ] <<| "matchKindDeclaration" <<<| (at $sloc) }
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
  | EXTERN n = nonTypeName push_scope
    L_BRACE pl = externMethodPrototypeList R_BRACE pop_scope
    { declare_type (id_of_name n);
      "EXTERN nonTypeName `{ externMethodPrototypeList }" <| [ n; pl ] <<| "externObjectDeclaration" <<<| (at $sloc) }
;

externDeclaration:
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
  | TRANSITION e = stateExpression
    { "TRANSITION stateExpression" <| [ e ] <<| "transitionStatement" <<<| (at $sloc) }
;

parserTypeDeclaration:
  | PARSER n = name push_scope
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { declare_type (id_of_name n);
      "PARSER name `( parameterList ) `;" <| [ n; pl ] <<| "parserTypeDeclaration" <<<| (at $sloc) }
;

parserState:
  | STATE n = name push_scope L_BRACE sl = statementList t = transitionStatement R_BRACE pop_scope
    { declare_var (id_of_name n);
      "STATE name `{ statementList transitionStatement }" <| [ n; sl; t ] <<| "parserState" <<<| (at $sloc) }
;

parserStateList:
  | s = parserState
    { s }
  | sl = parserStateList s = parserState
    { "parserStateList parserState" <| [ sl; s ] <<| "parserStateList" <<<| (at $sloc) }
;

parserLocalDeclaration:
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
  | PARSER n = name push_scope
    L_PAREN pl = parameterList R_PAREN
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
    { declare_type (id_of_name n);
      "PARSER name `( parameterList ) `{ parserLocalDeclarationList parserStateList }" <| [ n; pl; dl; sl ] <<| "parserDeclaration" <<<| (at $sloc) }
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
  | ac = tableAction
    { "tableAction" <| [ ac ] <<| "tableActionList" <<<| (at $sloc) }
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

tableKeyProperty:
  | KEY ASSIGN k = tableKey
    { "KEY `= tableKey" <| [ k ] <<| "tableKeyProperty" <<<| (at $sloc) }
;

tableActionsProperty:
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { "ACTIONS `= `{ tableActionList }" <| [ acl ] <<| "tableActionsProperty" <<<| (at $sloc) }
;

tableEntriesProperty:
  | CONST ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { "CONST ENTRIES `= `{ tableEntryList }" <| [ el ] <<| "tableEntriesProperty" <<<| (at $sloc) }
;

tableProperties:
  | kp = tableKeyProperty ap = tableActionsProperty
    { "tableKeyProperty tableActionsProperty"
      <| [ kp; ap ] <<| "tableProperties" <<<| (at $sloc) }
  | kp = tableKeyProperty ap = tableActionsProperty ep = tableEntriesProperty
    { "tableKeyProperty tableActionsProperty tableEntriesProperty"
      <| [ kp; ap; ep ] <<| "tableProperties" <<<| (at $sloc) }
;

tableDeclaration:
  | TABLE n = name L_BRACE tp = tableProperties R_BRACE
    { "TABLE name `{ tableProperties }" <| [ n; tp ] <<| "tableDeclaration" <<<| (at $sloc) }
;

controlTypeDeclaration:
  | CONTROL n = name push_scope
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { declare_type (id_of_name n);
      "CONTROL name `( parameterList ) `;" <| [ n; pl ] <<| "controlTypeDeclaration" <<<| (at $sloc) }
;

controlBody:
  | b = blockStatement { b }
;

controlLocalDeclaration:
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
  | CONTROL n = name push_scope
    L_PAREN pl = parameterList R_PAREN
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { declare_type (id_of_name n);
      "CONTROL name `( parameterList ) `{ controlLocalDeclarationList APPLY controlBody }" <| [ n; pl; dl; b ] <<| "controlDeclaration" <<<| (at $sloc) }
;

(* Package type declarations *)
packageTypeDeclaration:
  | PACKAGE n = name push_scope
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { declare_type (id_of_name n);
      "PACKAGE name `( parameterList ) `;" <| [ n; pl ] <<| "packageTypeDeclaration" <<<| (at $sloc) }
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
