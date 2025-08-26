open Il.Ast
open Flatten
open Util.Source
module F = Format

(* Separator *)

type sep = Nl | Comma

let is_nl = function Nl -> true | _ -> false

let pp_sep fmt = function
  | Nl -> F.fprintf fmt "\n"
  | Comma -> F.fprintf fmt ", "

(* Printers *)

let indent level = String.make (2 * level) ' '

let pp_list ?(level = 0) pp_elem ~(sep : sep) fmt l =
  List.iteri
    (fun i elem ->
      let startline = if is_nl sep then indent level else "" in
      F.fprintf fmt "%s%a" startline pp_elem elem;
      if i < List.length l - 1 then pp_sep fmt sep)
    l

(* Numbers *)

let pp_number fmt number =
  match flatten_case_v number with
  | [ [ "INT" ]; [] ], [ i ] -> (
      match i.it with
      | NumV (`Nat n) -> Bigint.pp fmt n
      | NumV (`Int i) -> Bigint.pp fmt i
      | _ -> failwith "@pp_number: expected NumV")
  | [ [ "FINT" ]; []; [] ], [ w; i ] -> (
      match (w.it, i.it) with
      | NumV (`Nat w), NumV (`Nat n) ->
          F.fprintf fmt "%as%a" Bigint.pp w Bigint.pp n
      | NumV (`Nat w), NumV (`Int i) ->
          F.fprintf fmt "%as%a" Bigint.pp w Bigint.pp i
      | _ -> failwith "@pp_number: expected NumV")
  | [ [ "FBIT" ]; []; [] ], [ w; i ] -> (
      match (w.it, i.it) with
      | NumV (`Nat w), NumV (`Nat n) ->
          F.fprintf fmt "%as%a" Bigint.pp w Bigint.pp n
      | NumV (`Nat w), NumV (`Int i) ->
          F.fprintf fmt "%as%a" Bigint.pp w Bigint.pp i
      | _ -> failwith "@pp_number: expected NumV")
  | _ -> failwith "@pp_number: unknown number"

(* Identifiers *)

let pp_name fmt name =
  match name.it with
  | TextV s -> F.fprintf fmt "%s" s
  | _ -> failwith "@pp_name: expected TextV"

let pp_nameList fmt nameList =
  match nameList.it with
  | ListV names -> F.fprintf fmt "%a" (pp_list pp_name ~sep:Comma) names
  | _ -> failwith "@pp_nameList: expected ListV"

(* Variables (scoped identifiers) *)

let pp_prefixedName fmt prefixedName =
  match flatten_case_v prefixedName with
  | [ [ "TOP" ]; [] ], [ name ] -> F.fprintf fmt ".%a" pp_name name
  | [ [ "CURRENT" ]; [] ], [ name ] -> F.fprintf fmt "%a" pp_name name
  | _ -> failwith "@pp_prefixedName: unknown prefixedName"

(* Directions *)

let pp_direction fmt direction =
  match flatten_case_v direction with
  | [ [ "NO" ] ], [] -> ()
  | [ [ "IN" ] ], [] -> F.fprintf fmt "in"
  | [ [ "OUT" ] ], [] -> F.fprintf fmt "out"
  | [ [ "INOUT" ] ], [] -> F.fprintf fmt "inout"
  | _ -> failwith "@pp_direction: unknown direction"

(* Base types *)

let rec pp_baseType fmt baseType =
  match flatten_case_v baseType with
  | [ [ "BoolT" ] ], [] -> F.fprintf fmt "bool"
  | [ [ "ErrT" ] ], [] -> F.fprintf fmt "error"
  | [ [ "MatchKindT" ] ], [] -> F.fprintf fmt "match_kind"
  | [ [ "StrT" ] ], [] -> F.fprintf fmt "string"
  | [ [ "IntT" ] ], [] -> F.fprintf fmt "int"
  | [ [ "FIntT" ]; [] ], [ expression ] ->
      F.fprintf fmt "int<%a>" pp_expression expression
  | [ [ "FBitT" ]; [] ], [ expression ] ->
      F.fprintf fmt "bit<%a>" pp_expression expression
  | [ [ "VBitT" ]; [] ], [ expression ] ->
      F.fprintf fmt "varbit<%a>" pp_expression expression
  | _ -> failwith "@pp_baseType: unknown baseType"

(* Named types *)

and pp_nameType fmt nameType =
  match flatten_case_v nameType with
  | [ [ "NameT" ]; [] ], [ prefixedName ] -> pp_prefixedName fmt prefixedName
  | _ -> failwith "@pp_nameType: unknown nameType"

and pp_specializedType fmt specializedType =
  match flatten_case_v specializedType with
  | [ [ "SpecT" ]; []; [] ], [ prefixedName; typeArgumentList ] ->
      F.fprintf fmt "%a%a" pp_prefixedName prefixedName pp_typeArgumentList
        typeArgumentList
  | _ -> failwith "@pp_specializedType: unknown specializedType"

and pp_namedType fmt namedType =
  match flatten_case_v namedType with
  | [ [ "NameT" ]; [] ], _ -> pp_nameType fmt namedType
  | [ [ "SpecT" ]; []; [] ], _ -> pp_specializedType fmt namedType
  | _ -> failwith "@pp_namedType: unknown namedType"

(* Header stack types *)

and pp_headerStackType fmt headerStackType =
  match flatten_case_v headerStackType with
  | [ [ "HeaderStackT" ]; []; [] ], [ namedType; expression ] ->
      F.fprintf fmt "%a[%a]" pp_namedType namedType pp_expression expression
  | _ -> failwith "@pp_headerStackType: unknown headerStackType"

(* List types *)

and pp_listType fmt listType =
  match flatten_case_v listType with
  | [ [ "ListT" ]; [] ], [ typeArgument ] ->
      F.fprintf fmt "list<%a>" pp_typeArgument typeArgument
  | _ -> failwith "@pp_listType: unknown listType"

(* Tuple types *)

and pp_tupleType fmt tupleType =
  match flatten_case_v tupleType with
  | [ [ "TupleT" ]; [] ], [ typeArgumentList ] ->
      F.fprintf fmt "tuple%a" pp_typeArgumentList typeArgumentList
  | _ -> failwith "@pp_tupleType: unknown tupleType"

(* Types *)

and pp_type fmt typ =
  match flatten_case_v typ with
  | [ [ "BoolT" ] ], _
  | [ [ "ErrT" ] ], _
  | [ [ "MatchKindT" ] ], _
  | [ [ "StrT" ] ], _
  | [ [ "IntT" ] ], _
  | [ [ "FIntT" ]; [] ], _
  | [ [ "FBitT" ]; [] ], _
  | [ [ "VBitT" ]; [] ], _ ->
      pp_baseType fmt typ
  | [ [ "NameT" ]; [] ], _ | [ [ "SpecT" ]; []; [] ], _ -> pp_namedType fmt typ
  | [ [ "HeaderStackT" ]; []; [] ], _ -> pp_headerStackType fmt typ
  | [ [ "ListT" ]; [] ], _ -> pp_listType fmt typ
  | [ [ "TupleT" ]; [] ], _ -> pp_tupleType fmt typ
  | _ -> failwith "@pp_type: unknown type"

and pp_typeOrVoid fmt typeOrVoid =
  match flatten_case_v typeOrVoid with
  | [ [ "BoolT" ] ], _
  | [ [ "ErrT" ] ], _
  | [ [ "MatchKindT" ] ], _
  | [ [ "StrT" ] ], _
  | [ [ "IntT" ] ], _
  | [ [ "FIntT" ]; [] ], _
  | [ [ "FBitT" ]; [] ], _
  | [ [ "VBitT" ]; [] ], _
  | [ [ "NameT" ]; [] ], _
  | [ [ "SpecT" ]; []; [] ], _
  | [ [ "HeaderStackT" ]; []; [] ], _
  | [ [ "ListT" ]; [] ], _
  | [ [ "TupleT" ]; [] ], _ ->
      pp_type fmt typeOrVoid
  | [ [ "VoidT" ] ], [] -> F.fprintf fmt "void"
  | _ -> failwith "@pp_typeOrVoid: unknown type"

(* Type parameters *)

and pp_typeParameter fmt typeParameter = pp_name fmt typeParameter

and pp_typeParameterList fmt typeParameterList =
  match typeParameterList.it with
  | ListV [] -> ()
  | ListV typeParameters ->
      F.fprintf fmt "<%a>" (pp_list pp_typeParameter ~sep:Comma) typeParameters
  | _ -> failwith "@pp_typeParameterList: expected ListV"

(* Parameters *)

and pp_parameter fmt parameter =
  match flatten_case_v parameter with
  | [ []; []; []; []; [] ], [ direction; typ; name; initialValueOpt ] ->
      F.fprintf fmt "%a %a %a%a" pp_direction direction pp_type typ pp_name name
        pp_initialValueOpt initialValueOpt
  | _ -> failwith "@pp_parameter: unknown parameter"

and pp_parameterList fmt parameterList =
  match parameterList.it with
  | ListV parameters ->
      F.fprintf fmt "(%a)" (pp_list pp_parameter ~sep:Comma) parameters
  | _ -> failwith "@pp_parameterList: expected ListV"

(* Constructor parameters *)

and pp_constructorParameter fmt constructorParameter =
  pp_parameter fmt constructorParameter

and pp_constructorParameterList fmt constructorParameterList =
  pp_parameterList fmt constructorParameterList

(* Expression key-value pairs *)

and pp_namedExpression fmt namedExpression =
  match flatten_case_v namedExpression with
  | [ []; []; [] ], [ name; expression ] ->
      F.fprintf fmt "%a = %a" pp_name name pp_expression expression
  | _ -> failwith "@pp_namedExpression: unknown namedExpression"

and pp_namedExpressionList fmt namedExpressionList =
  match namedExpressionList.it with
  | ListV namedExpressions ->
      F.fprintf fmt "%a"
        (pp_list pp_namedExpression ~sep:Comma)
        namedExpressions
  | _ -> failwith "@pp_namedExpressionList: expected ListV"

(* Literal expressions *)

and pp_literalExpression fmt literalExpression =
  match flatten_case_v literalExpression with
  | [ [ "BoolE" ]; [] ], [ b ] -> (
      match b.it with
      | BoolV true -> F.fprintf fmt "true"
      | BoolV false -> F.fprintf fmt "false"
      | _ -> failwith "@pp_literalExpression: expected BoolV")
  | [ [ "NumE" ]; [] ], [ number ] -> pp_number fmt number
  | [ [ "StrE" ]; [] ], [ s ] -> (
      match s.it with
      | TextV s -> F.fprintf fmt "\"%s\"" s
      | _ -> failwith "@pp_literalExpression: expected TextV")
  | _ -> failwith "@pp_literalExpression: unknown literalExpression"

(* Reference expressions *)

and pp_referenceExpression fmt referenceExpression =
  match flatten_case_v referenceExpression with
  | [ [ "NameE" ]; [] ], [ prefixedName ] -> pp_prefixedName fmt prefixedName
  | _ -> failwith "@pp_referenceExpression: unknown referenceExpression"

(* Default expressions *)

and pp_defaultExpression fmt defaultExpression =
  match flatten_case_v defaultExpression with
  | [ [ "DefaultE" ] ], [] -> F.fprintf fmt "..."
  | _ -> failwith "@pp_defaultExpression: unknown defaultExpression"

(* Unary, binary, and ternary expressions *)

and pp_unop fmt unop =
  match flatten_case_v unop with
  | [ [ "BNOT" ] ], [] -> F.fprintf fmt "~"
  | [ [ "LNOT" ] ], [] -> F.fprintf fmt "!"
  | [ [ "UPLUS" ] ], [] -> F.fprintf fmt "+"
  | [ [ "UMINUS" ] ], [] -> F.fprintf fmt "-"
  | _ -> failwith "@pp_unop: unknown unop"

and pp_unaryExpression fmt unaryExpression =
  match flatten_case_v unaryExpression with
  | [ [ "UnE" ]; []; [] ], [ unop; expression ] ->
      F.fprintf fmt "%a%a" pp_unop unop pp_expression expression
  | _ -> failwith "@pp_unaryExpression: unknown unaryExpression"

and pp_binop fmt binop =
  match flatten_case_v binop with
  | [ [ "PLUS" ] ], [] -> F.fprintf fmt "+"
  | [ [ "SPLUS" ] ], [] -> F.fprintf fmt "|+|"
  | [ [ "MINUS" ] ], [] -> F.fprintf fmt "-"
  | [ [ "SMINUS" ] ], [] -> F.fprintf fmt "|-|"
  | [ [ "MUL" ] ], [] -> F.fprintf fmt "*"
  | [ [ "DIV" ] ], [] -> F.fprintf fmt "/"
  | [ [ "MOD" ] ], [] -> F.fprintf fmt "%s" "%"
  | [ [ "SHL" ] ], [] -> F.fprintf fmt "<<"
  | [ [ "SHR" ] ], [] -> F.fprintf fmt ">>"
  | [ [ "LE" ] ], [] -> F.fprintf fmt "<="
  | [ [ "GE" ] ], [] -> F.fprintf fmt ">="
  | [ [ "LT" ] ], [] -> F.fprintf fmt "<"
  | [ [ "GT" ] ], [] -> F.fprintf fmt ">"
  | [ [ "EQ" ] ], [] -> F.fprintf fmt "=="
  | [ [ "NE" ] ], [] -> F.fprintf fmt "!="
  | [ [ "BAND" ] ], [] -> F.fprintf fmt "&"
  | [ [ "BXOR" ] ], [] -> F.fprintf fmt "^"
  | [ [ "BOR" ] ], [] -> F.fprintf fmt "|"
  | [ [ "CONCAT" ] ], [] -> F.fprintf fmt "++"
  | [ [ "LAND" ] ], [] -> F.fprintf fmt "&&"
  | [ [ "LOR" ] ], [] -> F.fprintf fmt "||"
  | _ -> failwith "@pp_binop: unknown binop"

and pp_binaryExpression fmt binaryExpression =
  match flatten_case_v binaryExpression with
  | [ [ "BinE" ]; []; []; [] ], [ expression_l; binop; expression_r ] ->
      F.fprintf fmt "%a %a %a" pp_expression expression_l pp_binop binop
        pp_expression expression_r
  | _ -> failwith "@pp_binaryExpression: unknown binaryExpression"

and pp_ternaryExpression fmt ternaryExpression =
  match flatten_case_v ternaryExpression with
  | [ [ "TernE" ]; []; []; [] ], [ expression_c; expression_t; expression_f ] ->
      F.fprintf fmt "%a ? %a : %a" pp_expression expression_c pp_expression
        expression_t pp_expression expression_f
  | _ -> failwith "@pp_ternaryExpression: unknown ternaryExpression"

(* Cast expressions *)

and pp_castExpression fmt castExpression =
  match flatten_case_v castExpression with
  | [ [ "CastE" ]; []; [] ], [ typ; expression ] ->
      F.fprintf fmt "(%a) %a" pp_type typ pp_expression expression
  | _ -> failwith "@pp_castExpression: unknown castExpression"

(* Data (aggregate) expressions *)

and pp_dataExpression fmt dataExpression =
  match flatten_case_v dataExpression with
  | [ [ "InvalidE" ] ], [] -> F.fprintf fmt "{#}"
  | [ [ "SeqE" ]; [] ], [ expressionList ] ->
      F.fprintf fmt "{ %a }" pp_expressionList expressionList
  | [ [ "RecordE" ]; [] ], [ namedExpressionList ] ->
      F.fprintf fmt "{ %a }" pp_namedExpressionList namedExpressionList
  | [ [ "RecordDefaultE" ]; [] ], [ namedExpressionList ] ->
      F.fprintf fmt "{ %a, ... }" pp_namedExpressionList namedExpressionList
  | _ -> failwith "@pp_dataExpression: unknown dataExpression"

(* Member and index access expressions *)

and pp_errorAccessExpression fmt errorAccessExpression =
  match flatten_case_v errorAccessExpression with
  | [ [ "ErrAccE" ]; [] ], [ name ] -> F.fprintf fmt "error.%a" pp_name name
  | _ -> failwith "@pp_errorAccessExpression: unknown errorAccessExpression"

and pp_memberAccessExpression fmt memberAccessExpression =
  match flatten_case_v memberAccessExpression with
  | [ [ "TypeAccE" ]; []; [] ], [ prefixedName; name ] ->
      F.fprintf fmt "%a.%a" pp_prefixedName prefixedName pp_name name
  | [ [ "ExprAccE" ]; []; [] ], [ expression; name ] ->
      F.fprintf fmt "%a.%a" pp_expression expression pp_name name
  | _ -> failwith "@pp_memberAccessExpression: unknown memberAccessExpression"

and pp_indexAccessExpression fmt indexAccessExpression =
  match flatten_case_v indexAccessExpression with
  | [ [ "ArrAccE" ]; []; [] ], [ expression_b; expression_i ] ->
      F.fprintf fmt "%a[%a]" pp_expression expression_b pp_expression
        expression_i
  | [ [ "BitAccE" ]; []; []; [] ], [ expression_b; expression_h; expression_l ]
    ->
      F.fprintf fmt "%a[%a:%a]" pp_expression expression_b pp_expression
        expression_h pp_expression expression_l
  | _ -> failwith "@pp_indexAccessExpression: unknown indexAccessExpression"

and pp_accessExpression fmt accessExpression =
  match flatten_case_v accessExpression with
  | [ [ "ErrAccE" ]; [] ], _ -> pp_errorAccessExpression fmt accessExpression
  | [ [ "TypeAccE" ]; []; [] ], _ | [ [ "ExprAccE" ]; []; [] ], _ ->
      pp_memberAccessExpression fmt accessExpression
  | [ [ "ArrAccE" ]; []; [] ], _ | [ [ "BitAccE" ]; []; []; [] ], _ ->
      pp_indexAccessExpression fmt accessExpression
  | _ -> failwith "@pp_accessExpression: unknown accessExpression"

(* Call expressions *)

and pp_callExpression fmt callExpression =
  match flatten_case_v callExpression with
  | ( [ [ "CallE" ]; []; []; [] ],
      [ routineTarget; typeArgumentList; argumentList ] ) ->
      F.fprintf fmt "%a%a%a" pp_expression routineTarget pp_typeArgumentList
        typeArgumentList pp_argumentList argumentList
  | [ [ "InstE" ]; []; [] ], [ namedType; argumentList ] ->
      F.fprintf fmt "%a%a" pp_namedType namedType pp_argumentList argumentList
  | _ -> failwith "@pp_callExpression: unknown callExpression"

(* Expressions *)

and pp_expression fmt expression =
  match flatten_case_v expression with
  | [ [ "BoolE" ]; [] ], _ | [ [ "NumE" ]; [] ], _ | [ [ "StrE" ]; [] ], _ ->
      pp_literalExpression fmt expression
  | [ [ "NameE" ]; [] ], _ -> pp_referenceExpression fmt expression
  | [ [ "DefaultE" ] ], _ -> pp_defaultExpression fmt expression
  | [ [ "UnE" ]; []; [] ], _ -> pp_unaryExpression fmt expression
  | [ [ "BinE" ]; []; []; [] ], _ -> pp_binaryExpression fmt expression
  | [ [ "TernE" ]; []; []; [] ], _ -> pp_ternaryExpression fmt expression
  | [ [ "CastE" ]; []; [] ], _ -> pp_castExpression fmt expression
  | [ [ "InvalidE" ] ], _
  | [ [ "SeqE" ]; [] ], _
  | [ [ "RecordE" ]; [] ], _
  | [ [ "RecordDefaultE" ]; [] ], _ ->
      pp_dataExpression fmt expression
  | [ [ "ErrAccE" ]; [] ], _
  | [ [ "TypeAccE" ]; []; [] ], _
  | [ [ "ExprAccE" ]; []; [] ], _
  | [ [ "ArrAccE" ]; []; [] ], _
  | [ [ "BitAccE" ]; []; []; [] ], _ ->
      pp_accessExpression fmt expression
  | [ [ "CallE" ]; []; []; [] ], _ | [ [ "InstE" ]; []; [] ], _ ->
      pp_callExpression fmt expression
  | _ -> failwith "@pp_expression: unknown expression"

and pp_expressionList fmt expressionList =
  match expressionList.it with
  | ListV expressions ->
      F.fprintf fmt "(%a)" (pp_list pp_expression ~sep:Comma) expressions
  | _ -> failwith "@pp_expressionList: expected ListV"

and pp_routineTarget fmt routineTarget = pp_expression fmt routineTarget

(* Keyset expressions *)

and pp_keysetExpression fmt keysetExpression =
  match flatten_case_v keysetExpression with
  | [ [ "ExprK" ]; [] ], [ expression ] -> pp_expression fmt expression
  | [ [ "MaskK" ]; []; [] ], [ expression_b; expression_m ] ->
      F.fprintf fmt "%a &&& %a" pp_expression expression_b pp_expression
        expression_m
  | [ [ "RangeK" ]; []; [] ], [ expression_l; expression_h ] ->
      F.fprintf fmt "%a .. %a" pp_expression expression_l pp_expression
        expression_h
  | [ [ "DefaultK" ] ], [] -> F.fprintf fmt "default"
  | [ [ "AnyK" ] ], [] -> F.fprintf fmt "_"
  | _ -> failwith "@pp_keysetExpression: unknown keysetExpression"

and pp_keysetExpressionList fmt keysetExpressionList =
  match keysetExpressionList.it with
  | ListV keysetExpressions ->
      F.fprintf fmt "(%a)"
        (pp_list pp_keysetExpression ~sep:Comma)
        keysetExpressions
  | _ -> failwith "@pp_keysetExpressionList: expected ListV"

(* Type arguments *)

and pp_typeArgument fmt typeArgument =
  match flatten_case_v typeArgument with
  | [ [ "BoolT" ] ], _
  | [ [ "ErrT" ] ], _
  | [ [ "MatchKindT" ] ], _
  | [ [ "StrT" ] ], _
  | [ [ "IntT" ] ], _
  | [ [ "FIntT" ]; [] ], _
  | [ [ "FBitT" ]; [] ], _
  | [ [ "VBitT" ]; [] ], _
  | [ [ "NameT" ]; [] ], _
  | [ [ "SpecT" ]; []; [] ], _
  | [ [ "HeaderStackT" ]; []; [] ], _
  | [ [ "ListT" ]; [] ], _
  | [ [ "TupleT" ]; [] ], _ ->
      pp_type fmt typeArgument
  | [ [ "VoidT" ] ], [] -> F.fprintf fmt "void"
  | [ [ "AnyT" ] ], [] -> F.fprintf fmt "_"
  | _ -> failwith "@pp_typeArgument: unknown typeArgument"

and pp_typeArgumentList fmt typeArgumentList =
  match typeArgumentList.it with
  | ListV [] -> ()
  | ListV typeArguments ->
      F.fprintf fmt "<%a>" (pp_list pp_typeArgument ~sep:Comma) typeArguments
  | _ -> failwith "@pp_typeArgumentList: expected ListV"

(* Arguments *)

and pp_argument fmt argument =
  match flatten_case_v argument with
  | [ [ "ExprA" ]; [] ], [ expression ] -> pp_expression fmt expression
  | [ [ "NameA" ]; []; [] ], [ name; expression ] ->
      F.fprintf fmt "%a = %a" pp_name name pp_expression expression
  | [ [ "NameAnyA" ]; [] ], [ name ] -> F.fprintf fmt "%a = _" pp_name name
  | [ [ "AnyA" ] ], [] -> F.fprintf fmt "_"
  | _ -> failwith "@pp_argument: unknown argument"

and pp_argumentList fmt argumentList =
  match argumentList.it with
  | ListV arguments ->
      F.fprintf fmt "(%a)" (pp_list pp_argument ~sep:Comma) arguments
  | _ -> failwith "@pp_argumentList: expected ListV"

(* L-values *)

and pp_lvalue fmt lvalue =
  match flatten_case_v lvalue with
  | [ [ "NameL" ]; [] ], [ prefixedName ] -> pp_prefixedName fmt prefixedName
  | [ [ "LvalueAccL" ]; []; [] ], [ lvalue; name ] ->
      F.fprintf fmt "%a.%a" pp_lvalue lvalue pp_name name
  | [ [ "ArrAccL" ]; []; [] ], [ lvalue; expression ] ->
      F.fprintf fmt "%a[%a]" pp_lvalue lvalue pp_expression expression
  | [ [ "BitAccL" ]; []; []; [] ], [ lvalue; expression_h; expression_l ] ->
      F.fprintf fmt "%a[%a:%a]" pp_lvalue lvalue pp_expression expression_h
        pp_expression expression_l
  | _ -> failwith "@pp_lvalue: unknown lvalue"

(* Empty statements *)

and pp_emptyStatement fmt emptyStatement =
  match flatten_case_v emptyStatement with
  | [ [ "EmptyS" ] ], [] -> F.fprintf fmt ";"
  | _ -> failwith "@pp_emptyStatement: unknown emptyStatement"

(* Assignment statements *)

and pp_assignmentStatement fmt assignmentStatement =
  match flatten_case_v assignmentStatement with
  | [ [ "AssignS" ]; []; [] ], [ lvalue; expression ] ->
      F.fprintf fmt "%a = %a;" pp_lvalue lvalue pp_expression expression
  | _ -> failwith "@pp_assignmentStatement: unknown assignmentStatement"

(* Call statements *)

and pp_callStatement fmt callStatement =
  match flatten_case_v callStatement with
  | [ [ "CallS" ]; []; []; [] ], [ lvalue; typeArgumentList; argumentList ] ->
      F.fprintf fmt "%a%a%a;" pp_lvalue lvalue pp_typeArgumentList
        typeArgumentList pp_argumentList argumentList
  | _ -> failwith "@pp_callStatement: unknown callStatement"

(* Direct application statements *)

and pp_directApplicationStatement fmt directApplicationStatement =
  match flatten_case_v directApplicationStatement with
  | [ [ "InstS" ]; []; [] ], [ namedType; argumentList ] ->
      F.fprintf fmt "%a.apply%a;" pp_namedType namedType pp_argumentList
        argumentList
  | _ ->
      failwith
        "@pp_directApplicationStatement: unknown directApplicationStatement"

(* Return statements *)

and pp_returnStatement fmt returnStatement =
  match flatten_case_v returnStatement with
  | [ [ "ReturnS" ]; [] ], [ expressionOpt ] -> (
      match expressionOpt.it with
      | OptV (Some expression) ->
          F.fprintf fmt "return %a;" pp_expression expression
      | OptV None -> F.fprintf fmt "return;"
      | _ -> failwith "@pp_returnStatement: expected OptV")
  | _ -> failwith "@pp_returnStatement: unknown returnStatement"

(* Exit statements *)

and pp_exitStatement fmt exitStatement =
  match flatten_case_v exitStatement with
  | [ [ "ExitS" ] ], [] -> F.fprintf fmt "exit;"
  | _ -> failwith "@pp_exitStatement: unknown exitStatement"

(* Block statements *)

and pp_blockStatement ?(level = 0) fmt blockStatement =
  match flatten_case_v blockStatement with
  | [ [ "BlockS" ]; [] ], [ blockElementStatementList ] ->
      F.fprintf fmt "{\n%a\n%s}"
        (pp_blockElementStatementList ~level:(level + 1))
        blockElementStatementList (indent level)
  | _ -> failwith "@pp_blockStatement: unknown blockStatement"

(* Conditional statements *)

and pp_conditionalStatement ?(level = 0) fmt conditionalStatement =
  match flatten_case_v conditionalStatement with
  | [ [ "IfS" ]; []; []; [] ], [ expression_c; statement_t; statement_f_opt ]
    -> (
      match statement_f_opt.it with
      | OptV (Some statement_f) ->
          F.fprintf fmt "if (%a) %a\n%selse %a" pp_expression expression_c
            (pp_statement ~level) statement_t (indent level)
            (pp_statement ~level) statement_f
      | OptV None ->
          F.fprintf fmt "if (%a) %a" pp_expression expression_c
            (pp_statement ~level) statement_t
      | _ -> failwith "@pp_conditionalStatement: expected OptV")
  | _ -> failwith "@pp_conditionalStatement: unknown conditionalStatement"

(* Switch statements *)

and pp_switchLabel fmt switchLabel =
  match flatten_case_v switchLabel with
  | [ [ "DefaultL" ] ], [] -> F.fprintf fmt "default"
  | [ [ "ExprL" ]; [] ], [ expression ] ->
      F.fprintf fmt "%a" pp_expression expression
  | _ -> failwith "@pp_switchLabel: unknown switchLabel"

and pp_switchCase ?(level = 0) fmt switchCase =
  match flatten_case_v switchCase with
  | [ [ "FallC" ]; [] ], [ switchLabel ] ->
      F.fprintf fmt "%a:" pp_switchLabel switchLabel
  | [ [ "MatchC" ]; []; [] ], [ switchLabel; blockStatement ] ->
      F.fprintf fmt "%a: %a" pp_switchLabel switchLabel
        (pp_blockStatement ~level:(level + 1))
        blockStatement
  | _ -> failwith "@pp_switchCase: unknown switchCase"

and pp_switchCaseList ?(level = 0) fmt switchCaseList =
  match switchCaseList.it with
  | ListV switchCases ->
      pp_list ~level (pp_switchCase ~level) ~sep:Nl fmt switchCases
  | _ -> failwith "@pp_switchCaseList: expected ListV"

and pp_switchStatement ?(level = 0) fmt switchStatement =
  match flatten_case_v switchStatement with
  | [ [ "SwitchS" ]; []; [] ], [ expression; switchCaseList ] ->
      F.fprintf fmt "switch (%a) {\n%a\n%s}" pp_expression expression
        (pp_switchCaseList ~level:(level + 1))
        switchCaseList (indent level)
  | _ -> failwith "@pp_switchStatement: unknown switchStatement"

(* Statement *)

and pp_statement ?(level = 0) fmt statement =
  match flatten_case_v statement with
  | [ [ "EmptyS" ] ], _ -> pp_emptyStatement fmt statement
  | [ [ "AssignS" ]; []; [] ], _ -> pp_assignmentStatement fmt statement
  | [ [ "CallS" ]; []; []; [] ], _ -> pp_callStatement fmt statement
  | [ [ "InstS" ]; []; [] ], _ -> pp_directApplicationStatement fmt statement
  | [ [ "ReturnS" ]; [] ], _ -> pp_returnStatement fmt statement
  | [ [ "ExitS" ] ], _ -> pp_exitStatement fmt statement
  | [ [ "BlockS" ]; [] ], _ -> pp_blockStatement ~level fmt statement
  | [ [ "IfS" ]; []; []; [] ], _ -> pp_conditionalStatement ~level fmt statement
  | [ [ "SwitchS" ]; []; [] ], _ -> pp_switchStatement ~level fmt statement
  | _ -> failwith "@pp_statement: unknown statement"

(* Constant and variable declarations *)

and pp_initialValue fmt initialValue = pp_expression fmt initialValue

and pp_initialValueOpt fmt initialValueOpt =
  match initialValueOpt.it with
  | OptV (Some initialValue) ->
      F.fprintf fmt " = %a" pp_initialValue initialValue
  | OptV None -> ()
  | _ -> failwith "@pp_initialValueOpt: expected OptV"

and pp_constantDeclaration fmt constantDeclaration =
  match flatten_case_v constantDeclaration with
  | [ [ "ConstD" ]; []; []; [] ], [ typ; name; initialValue ] ->
      F.fprintf fmt "const %a %a = %a;" pp_type typ pp_name name pp_initialValue
        initialValue
  | _ -> failwith "@pp_constantDeclaration: unknown constantDeclaration"

and pp_variableDeclaration fmt variableDeclaration =
  match flatten_case_v variableDeclaration with
  | [ [ "VarD" ]; []; []; [] ], [ typ; name; initialValueOpt ] ->
      F.fprintf fmt "%a %a%a;" pp_type typ pp_name name pp_initialValueOpt
        initialValueOpt
  | _ -> failwith "@pp_variableDeclaration: unknown variableDeclaration"

and pp_blockElementStatement ?(level = 0) fmt blockElementStatement =
  match flatten_case_v blockElementStatement with
  | [ [ "ConstD" ]; []; []; [] ], _ ->
      pp_constantDeclaration fmt blockElementStatement
  | [ [ "VarD" ]; []; []; [] ], _ ->
      pp_variableDeclaration fmt blockElementStatement
  | [ [ "EmptyS" ] ], _
  | [ [ "AssignS" ]; []; [] ], _
  | [ [ "CallS" ]; []; []; [] ], _
  | [ [ "InstS" ]; []; [] ], _
  | [ [ "ReturnS" ]; [] ], _
  | [ [ "ExitS" ] ], _
  | [ [ "BlockS" ]; [] ], _
  | [ [ "IfS" ]; []; []; [] ], _
  | [ [ "SwitchS" ]; []; [] ], _ ->
      pp_statement ~level fmt blockElementStatement
  | _ -> failwith "@pp_blockElementStatement: unknown blockElementStatement"

and pp_blockElementStatementList ?(level = 0) fmt blockElementStatementList =
  match blockElementStatementList.it with
  | ListV blockElementStatements ->
      pp_list ~level
        (pp_blockElementStatement ~level)
        ~sep:Nl fmt blockElementStatements
  | _ -> failwith "@pp_blockElementStatementList: expected ListV"

(* Function declarations *)

and pp_functionDeclaration ?(level = 0) fmt functionDeclaration =
  match flatten_case_v functionDeclaration with
  | ( [ [ "FuncD" ]; []; []; []; []; [] ],
      [ typeOrVoid; name; typeParameterList; parameterList; blockStatement ] )
    ->
      F.fprintf fmt "%a %a%a%a %a" pp_typeOrVoid typeOrVoid pp_name name
        pp_typeParameterList typeParameterList pp_parameterList parameterList
        (pp_blockStatement ~level:(level + 1))
        blockStatement
  | _ -> failwith "@pp_functionDeclaration: unknown functionDeclaration"

(* Action declarations *)

and pp_actionDeclaration ?(level = 0) fmt actionDeclaration =
  match flatten_case_v actionDeclaration with
  | [ [ "ActionD" ]; []; []; [] ], [ name; parameterList; blockStatement ] ->
      F.fprintf fmt "action %a%a %a" pp_name name pp_parameterList parameterList
        (pp_blockStatement ~level) blockStatement
  | _ -> failwith "@pp_actionDeclaration: unknown actionDeclaration"

(* Instantiations *)

and pp_instantiation ?(level = 0) fmt instantiation =
  match flatten_case_v instantiation with
  | ( [ [ "InstD" ]; []; []; []; [] ],
      [ typ; argumentList; name; objectInitializerOpt ] ) ->
      F.fprintf fmt "%a %a%a%a;" pp_type typ pp_name name pp_argumentList
        argumentList
        (pp_objectInitializerOpt ~level:(level + 1))
        objectInitializerOpt
  | _ -> failwith "@pp_instantiation: unknown instantiation"

and pp_objectDeclaration ?(level = 0) fmt objectDeclaration =
  match flatten_case_v objectDeclaration with
  | [ [ "FuncD" ]; []; []; []; []; [] ], _ ->
      pp_functionDeclaration ~level fmt objectDeclaration
  | [ [ "InstD" ]; []; []; []; [] ], _ ->
      pp_instantiation ~level fmt objectDeclaration
  | _ -> failwith "@pp_objectDeclaration: unknown objectDeclaration"

and pp_objectInitializer ?(level = 0) fmt objectInitializer =
  match objectInitializer.it with
  | ListV objectDeclarations ->
      F.fprintf fmt "{\n%a\n}"
        (pp_list ~level (pp_objectDeclaration ~level) ~sep:Nl)
        objectDeclarations
  | _ -> failwith "@pp_objectInitializer: expected ListV"

and pp_objectInitializerOpt ?(level = 0) fmt objectInitializerOpt =
  match objectInitializerOpt.it with
  | OptV (Some objectInitializer) ->
      F.fprintf fmt " = %a" (pp_objectInitializer ~level) objectInitializer
  | OptV None -> ()
  | _ -> failwith "@pp_objectInitializerOpt: expected OptV"

(* Error declarations *)

and pp_errorDeclaration fmt errorDeclaration =
  match flatten_case_v errorDeclaration with
  | [ [ "ErrD" ]; [] ], [ nameList ] ->
      F.fprintf fmt "error { %a };" pp_nameList nameList
  | _ -> failwith "@pp_errorDeclaration: unknown errorDeclaration"

(* Match kind declarations *)

and pp_matchKindDeclaration fmt matchKindDeclaration =
  match flatten_case_v matchKindDeclaration with
  | [ [ "MatchKindD" ]; [] ], [ nameList ] ->
      F.fprintf fmt "match_kind { %a };" pp_nameList nameList
  | _ -> failwith "@pp_matchKindDeclaration: unknown matchKindDeclaration"

(* Enum type declarations *)

and pp_enumTypeDeclaration fmt enumTypeDeclaration =
  match flatten_case_v enumTypeDeclaration with
  | [ [ "EnumD" ]; []; [] ], [ name; nameList ] ->
      F.fprintf fmt "enum %a { %a }" pp_name name pp_nameList nameList
  | [ [ "SEnumD" ]; []; []; [] ], [ typ; name; namedExpressionList ] ->
      F.fprintf fmt "enum %a %a { %a }" pp_type typ pp_name name
        pp_namedExpressionList namedExpressionList
  | _ -> failwith "@pp_enumTypeDeclaration: unknown enumTypeDeclaration"

(* Struct, header, and union type declarations *)

and pp_typeField fmt typeField =
  match flatten_case_v typeField with
  | [ []; []; [] ], [ typ; name ] ->
      F.fprintf fmt "%a %a;" pp_type typ pp_name name
  | _ -> failwith "@pp_typeField: unknown typeField"

and pp_typeFieldList ?(level = 0) fmt typeFieldList =
  match typeFieldList.it with
  | ListV typeFields -> pp_list ~level pp_typeField ~sep:Nl fmt typeFields
  | _ -> failwith "@pp_typeFieldList: expected ListV"

and pp_structTypeDeclaration ?(level = 0) fmt structTypeDeclaration =
  match flatten_case_v structTypeDeclaration with
  | [ [ "StructD" ]; []; []; [] ], [ name; typeParameterList; typeFieldList ] ->
      F.fprintf fmt "struct %a%a {\n%a\n}" pp_name name pp_typeParameterList
        typeParameterList
        (pp_typeFieldList ~level:(level + 1))
        typeFieldList
  | _ -> failwith "@pp_structTypeDeclaration: unknown structTypeDeclaration"

and pp_headerTypeDeclaration ?(level = 0) fmt headerTypeDeclaration =
  match flatten_case_v headerTypeDeclaration with
  | [ [ "HeaderD" ]; []; []; [] ], [ name; typeParameterList; typeFieldList ] ->
      F.fprintf fmt "header %a%a {\n%a\n}" pp_name name pp_typeParameterList
        typeParameterList
        (pp_typeFieldList ~level:(level + 1))
        typeFieldList
  | _ -> failwith "@pp_headerTypeDeclaration: unknown headerTypeDeclaration"

and pp_headerUnionTypeDeclaration ?(level = 0) fmt headerUnionTypeDeclaration =
  match flatten_case_v headerUnionTypeDeclaration with
  | ( [ [ "HeaderUnionD" ]; []; []; [] ],
      [ name; typeParameterList; typeFieldList ] ) ->
      F.fprintf fmt "header_union %a%a {\n%a\n}" pp_name name
        pp_typeParameterList typeParameterList
        (pp_typeFieldList ~level:(level + 1))
        typeFieldList
  | _ ->
      failwith
        "@pp_headerUnionTypeDeclaration: unknown headerUnionTypeDeclaration"

and pp_derivedTypeDeclaration ?(level = 0) fmt derivedTypeDeclaration =
  match flatten_case_v derivedTypeDeclaration with
  | [ [ "EnumD" ]; []; [] ], _ | [ [ "SEnumD" ]; []; []; [] ], _ ->
      pp_enumTypeDeclaration fmt derivedTypeDeclaration
  | [ [ "StructD" ]; []; []; [] ], _ ->
      pp_structTypeDeclaration ~level fmt derivedTypeDeclaration
  | [ [ "HeaderD" ]; []; []; [] ], _ ->
      pp_headerTypeDeclaration ~level fmt derivedTypeDeclaration
  | [ [ "HeaderUnionD" ]; []; []; [] ], _ ->
      pp_headerUnionTypeDeclaration ~level fmt derivedTypeDeclaration
  | _ -> failwith "@pp_derivedTypeDeclaration: unknown derivedTypeDeclaration"

(* Typedef and newtype declarations *)

and pp_typedefType ?(level = 0) fmt typedefType =
  match flatten_case_v typedefType with
  | [ [ "PlainT" ]; [] ], [ typ ] -> pp_type fmt typ
  | [ [ "DerivedT" ]; [] ], [ derivedTypeDeclaration ] ->
      pp_derivedTypeDeclaration ~level fmt derivedTypeDeclaration
  | _ -> failwith "@pp_typedefType: unknown typedefType"

and pp_typedefDeclaration ?(level = 0) fmt typedefDeclaration =
  match flatten_case_v typedefDeclaration with
  | [ [ "TypeDefD" ]; []; [] ], [ typedefType; name ] ->
      F.fprintf fmt "typedef %a %a;"
        (pp_typedefType ~level:(level + 1))
        typedefType pp_name name
  | [ [ "NewTypeD" ]; []; [] ], [ typ; name ] ->
      F.fprintf fmt "type %a %a;" pp_type typ pp_name name
  | _ -> failwith "@pp_typedefDeclaration: unknown typedefDeclaration"

(* Extern declarations *)

and pp_externFunctionDeclaration fmt externFunctionDeclaration =
  match flatten_case_v externFunctionDeclaration with
  | ( [ [ "ExternFuncD" ]; []; []; []; [] ],
      [ typeOrVoid; name; typeParameterList; parameterList ] ) ->
      F.fprintf fmt "extern %a %a%a%a;" pp_typeOrVoid typeOrVoid pp_name name
        pp_typeParameterList typeParameterList pp_parameterList parameterList
  | _ ->
      failwith
        "@pp_externFunctionDeclaration: unknown externFunctionDeclaration"

and pp_methodPrototype fmt methodPrototype =
  match flatten_case_v methodPrototype with
  | [ [ "ConsM" ]; []; [] ], [ name; constructorParameterList ] ->
      F.fprintf fmt "%a%a;" pp_name name pp_constructorParameterList
        constructorParameterList
  | ( [ [ "MethodM" ]; []; []; []; [] ],
      [ typeOrVoid; name; typeParameterList; parameterList ] ) ->
      F.fprintf fmt "%a %a%a%a;" pp_typeOrVoid typeOrVoid pp_name name
        pp_typeParameterList typeParameterList pp_parameterList parameterList
  | ( [ [ "AbstractMethodM" ]; []; []; []; [] ],
      [ typeOrVoid; name; typeParameterList; parameterList ] ) ->
      F.fprintf fmt "abstract %a %a%a%a;" pp_typeOrVoid typeOrVoid pp_name name
        pp_typeParameterList typeParameterList pp_parameterList parameterList
  | _ -> failwith "@pp_methodPrototype: unknown methodPrototype"

and pp_methodPrototypeList ?(level = 0) fmt methodPrototypeList =
  match methodPrototypeList.it with
  | ListV methodPrototypes ->
      pp_list ~level pp_methodPrototype ~sep:Nl fmt methodPrototypes
  | _ -> failwith "@pp_methodPrototypeList: expected ListV"

and pp_externObjectDeclaration ?(level = 0) fmt externObjectDeclaration =
  match flatten_case_v externObjectDeclaration with
  | ( [ [ "ExternObjectD" ]; []; []; [] ],
      [ name; typeParameterList; methodPrototypeList ] ) ->
      F.fprintf fmt "extern object %a%a {\n%a\n}" pp_name name
        pp_typeParameterList typeParameterList
        (pp_methodPrototypeList ~level:(level + 1))
        methodPrototypeList
  | _ -> failwith "@pp_externObjectDeclaration: unknown externObjectDeclaration"

and pp_externDeclaration ?(level = 0) fmt externDeclaration =
  match flatten_case_v externDeclaration with
  | [ [ "ExternFuncD" ]; []; []; []; [] ], _ ->
      pp_externFunctionDeclaration fmt externDeclaration
  | [ [ "ExternObjectD" ]; []; []; [] ], _ ->
      pp_externObjectDeclaration ~level fmt externDeclaration
  | _ -> failwith "@pp_externDeclaration: unknown externDeclaration"

(* Select cases *)

and pp_selectCase fmt selectCase =
  match flatten_case_v selectCase with
  | [ []; []; [] ], [ keysetExpressionList; name ] ->
      F.fprintf fmt "%a: %a;" pp_keysetExpressionList keysetExpressionList
        pp_name name
  | _ -> failwith "@pp_selectCase: unknown selectCase"

and pp_selectCaseList ?(level = 0) fmt selectCaseList =
  match selectCaseList.it with
  | ListV selectCases -> pp_list ~level pp_selectCase ~sep:Nl fmt selectCases
  | _ -> failwith "@pp_selectCaseList: expected ListV"

(* Transition statements *)

and pp_stateExpression ?(level = 0) fmt stateExpression =
  match flatten_case_v stateExpression with
  | [ [ "NameE" ]; [] ], [ name ] -> pp_name fmt name
  | [ [ "SelectE" ]; []; [] ], [ expressionList; selectCaseList ] ->
      F.fprintf fmt "select (%a) {\n%a\n}" pp_expressionList expressionList
        (pp_selectCaseList ~level:(level + 1))
        selectCaseList
  | _ -> failwith "@pp_stateExpression: unknown stateExpression"

and pp_transitionStatement ?(level = 0) fmt transitionStatement =
  match flatten_case_v transitionStatement with
  | [ [ "TransS" ]; [] ], [ stateExpression ] ->
      F.fprintf fmt "transition %a;" (pp_stateExpression ~level) stateExpression
  | _ -> failwith "@pp_transitionStatement: unknown transitionStatement"

and pp_transitionStatementOpt ?(level = 0) fmt transitionStatementOpt =
  match transitionStatementOpt.it with
  | OptV (Some transitionStatement) ->
      pp_transitionStatement ~level fmt transitionStatement
  | OptV None -> ()
  | _ -> failwith "@pp_transitionStatementOpt: expected OptV"

(* Value set declarations *)

and pp_valueSetType fmt valueSetType =
  match flatten_case_v valueSetType with
  | [ [ "BoolT" ] ], _
  | [ [ "ErrT" ] ], _
  | [ [ "MatchKindT" ] ], _
  | [ [ "StrT" ] ], _
  | [ [ "IntT" ] ], _
  | [ [ "FIntT" ]; [] ], _
  | [ [ "FBitT" ]; [] ], _
  | [ [ "VBitT" ]; [] ], _ ->
      pp_baseType fmt valueSetType
  | [ [ "TupleT" ]; [] ], _ -> pp_tupleType fmt valueSetType
  | [ [ "NameT" ]; [] ], _ -> pp_nameType fmt valueSetType
  | _ -> failwith "@pp_valueSetType: unknown valueSetType"

and pp_valueSetDeclaration fmt valueSetDeclaration =
  match flatten_case_v valueSetDeclaration with
  | [ [ "ValueSetD" ]; []; []; [] ], [ valueSetType; expression; name ] ->
      F.fprintf fmt "value_set<%a>(%a) %a;" pp_valueSetType valueSetType
        pp_expression expression pp_name name
  | _ -> failwith "@pp_valueSetDeclaration: unknown valueSetDeclaration"

(* Parser type declarations *)

and pp_parserTypeDeclaration fmt parserTypeDeclaration =
  match flatten_case_v parserTypeDeclaration with
  | ( [ [ "ParserTypeD" ]; []; []; [] ],
      [ name; typeParameterList; parameterList ] ) ->
      F.fprintf fmt "parser %a%a%a;" pp_name name pp_typeParameterList
        typeParameterList pp_parameterList parameterList
  | _ -> failwith "@pp_parserTypeDeclaration: unknown parserTypeDeclaration"

(* Parser declarations *)

and pp_parserBlockStatement ?(level = 0) fmt parserBlockStatement =
  match flatten_case_v parserBlockStatement with
  | [ [ "ParserBlockS" ]; [] ], [ parserStatementList ] ->
      F.fprintf fmt "{\n%a\n%s}"
        (pp_parserStatementList ~level:(level + 1))
        parserStatementList (indent level)
  | _ -> failwith "@pp_parserBlockStatement: unknown parserBlockStatement"

and pp_parserStatement ?(level = 0) fmt parserStatement =
  match flatten_case_v parserStatement with
  | [ [ "ConstD" ]; []; []; [] ], _ ->
      pp_constantDeclaration fmt parserStatement
  | [ [ "VarD" ]; []; []; [] ], _ -> pp_variableDeclaration fmt parserStatement
  | [ [ "EmptyS" ] ], _ -> pp_emptyStatement fmt parserStatement
  | [ [ "AssignS" ]; []; [] ], _ -> pp_assignmentStatement fmt parserStatement
  | [ [ "CallS" ]; []; []; [] ], _ -> pp_callStatement fmt parserStatement
  | [ [ "InstS" ]; []; [] ], _ ->
      pp_directApplicationStatement fmt parserStatement
  | [ [ "ParserBlockS" ]; [] ], _ ->
      pp_parserBlockStatement ~level fmt parserStatement
  | [ [ "IfS" ]; []; []; [] ], _ ->
      pp_conditionalStatement ~level fmt parserStatement
  | _ -> failwith "@pp_parserStatement: unknown parserStatement"

and pp_parserStatementList ?(level = 0) fmt parserStatementList =
  match parserStatementList.it with
  | ListV parserStatements ->
      pp_list ~level (pp_parserStatement ~level) ~sep:Nl fmt parserStatements
  | _ -> failwith "@pp_parserStatementList: expected ListV"

and pp_parserState ?(level = 0) fmt parserState =
  match flatten_case_v parserState with
  | [ []; []; []; [] ], [ name; parserStatementList; transitionStatementOpt ] ->
      F.fprintf fmt "state %a {\n%a\n%s%a\n}" pp_name name
        (pp_parserStatementList ~level:(level + 1))
        parserStatementList
        (indent (level + 1))
        (pp_transitionStatementOpt ~level:(level + 1))
        transitionStatementOpt
  | _ -> failwith "@pp_parserState: unknown parserState"

and pp_parserStateList ?(level = 0) fmt parserStateList =
  match parserStateList.it with
  | ListV parserStates ->
      pp_list ~level (pp_parserState ~level) ~sep:Nl fmt parserStates
  | _ -> failwith "@pp_parserStateList: expected ListV"

and pp_parserLocalDeclaration fmt parserLocalDeclaration =
  match flatten_case_v parserLocalDeclaration with
  | [ [ "ConstD" ]; []; []; [] ], _ ->
      pp_constantDeclaration fmt parserLocalDeclaration
  | [ [ "InstD" ]; []; []; []; [] ], _ ->
      pp_instantiation fmt parserLocalDeclaration
  | [ [ "VarD" ]; []; []; [] ], _ ->
      pp_variableDeclaration fmt parserLocalDeclaration
  | [ [ "ValueSetD" ]; []; []; [] ], _ ->
      pp_valueSetDeclaration fmt parserLocalDeclaration
  | _ -> failwith "@pp_parserLocalDeclaration: unknown parserLocalDeclaration"

and pp_parserLocalDeclarationList ?(level = 0) fmt parserLocalDeclarationList =
  match parserLocalDeclarationList.it with
  | ListV parserLocalDeclarations ->
      pp_list ~level pp_parserLocalDeclaration ~sep:Nl fmt
        parserLocalDeclarations
  | _ -> failwith "@pp_parserLocalDeclarationList: expected ListV"

and pp_parserDeclaration ?(level = 0) fmt parserDeclaration =
  match flatten_case_v parserDeclaration with
  | ( [ [ "ParserD" ]; []; []; []; []; []; [] ],
      [
        name;
        typeParameterList;
        parameterList;
        constructorParameterList;
        parserLocalDeclarationList;
        parserStateList;
      ] ) ->
      F.fprintf fmt "parser %a%a%a%a {\n%a\n%a\n}" pp_name name
        pp_typeParameterList typeParameterList pp_parameterList parameterList
        pp_constructorParameterList constructorParameterList
        (pp_parserLocalDeclarationList ~level:(level + 1))
        parserLocalDeclarationList
        (pp_parserStateList ~level:(level + 1))
        parserStateList
  | _ -> failwith "@pp_parserDeclaration: unknown parserDeclaration"

(* Table declarations *)

and pp_const fmt const =
  match flatten_case_v const with
  | [ [ "CONST" ] ], [] -> F.fprintf fmt "const"
  | _ -> failwith "@pp_const: unknown const"

and pp_constOpt fmt constOpt =
  match constOpt.it with
  | OptV (Some const) -> F.fprintf fmt "%a " pp_const const
  | OptV None -> ()
  | _ -> failwith "@pp_constOpt: expected OptV"

(* Table key property *)

and pp_tableKey fmt tableKey =
  match flatten_case_v tableKey with
  | [ []; []; [] ], [ expression; name ] ->
      F.fprintf fmt "%a : %a" pp_expression expression pp_name name
  | _ -> failwith "@pp_tableKey: unknown tableKey"

and pp_tableKeyList fmt tableKeyList =
  match tableKeyList.it with
  | ListV tableKeys -> pp_list pp_tableKey ~sep:Comma fmt tableKeys
  | _ -> failwith "@pp_tableKeyList: expected ListV"

(* Table actions property *)

and pp_tableActionReference fmt tableActionReference =
  match flatten_case_v tableActionReference with
  | [ []; []; [] ], [ prefixedName; argumentList ] ->
      F.fprintf fmt "%a%a" pp_prefixedName prefixedName pp_argumentList
        argumentList
  | _ -> failwith "@pp_tableActionReference: unknown tableActionReference"

and pp_tableAction fmt tableAction = pp_tableActionReference fmt tableAction

and pp_tableActionList fmt tableActionList =
  match tableActionList.it with
  | ListV tableActions -> pp_list pp_tableAction ~sep:Comma fmt tableActions
  | _ -> failwith "@pp_tableActionList: expected ListV"

(* Table entry property *)

and pp_tableEntryPriority fmt tableEntryPriority =
  pp_expression fmt tableEntryPriority

and pp_tableEntryPriorityOpt fmt tableEntryPriorityOpt =
  match tableEntryPriorityOpt.it with
  | OptV (Some tableEntryPriority) ->
      F.fprintf fmt "priority = %a : " pp_tableEntryPriority tableEntryPriority
  | OptV None -> ()
  | _ -> failwith "@pp_tableEntryPriorityOpt: expected OptV"

and pp_tableEntry fmt tableEntry =
  match flatten_case_v tableEntry with
  | ( [ []; []; []; []; [] ],
      [
        constOpt;
        tableEntryPriorityOpt;
        keysetExpressionList;
        tableActionReference;
      ] ) ->
      F.fprintf fmt "%a%a%a : %a" pp_constOpt constOpt pp_tableEntryPriorityOpt
        tableEntryPriorityOpt pp_keysetExpressionList keysetExpressionList
        pp_tableActionReference tableActionReference
  | _ -> failwith "@pp_tableEntry: unknown tableEntry"

and pp_tableEntryList ?(level = 0) fmt tableEntryList =
  match tableEntryList.it with
  | ListV tableEntries -> pp_list ~level pp_tableEntry ~sep:Nl fmt tableEntries
  | _ -> failwith "@pp_tableEntryList: expected ListV"

(* Table properties *)

and pp_tableProperty ?(level = 0) fmt tableProperty =
  match flatten_case_v tableProperty with
  | [ [ "KeyP" ]; [] ], [ tableKeyList ] ->
      F.fprintf fmt "key = { %a }" pp_tableKeyList tableKeyList
  | [ [ "ActionP" ]; [] ], [ tableActionList ] ->
      F.fprintf fmt "actions = { %a }" pp_tableActionList tableActionList
  | [ [ "EntryP" ]; []; [] ], [ constOpt; tableEntryList ] ->
      F.fprintf fmt "%aentries = {\n%a\n}" pp_constOpt constOpt
        (pp_tableEntryList ~level:(level + 1))
        tableEntryList
  | [ [ "CustomP" ]; []; []; [] ], [ constOpt; name; initialValue ] ->
      F.fprintf fmt "%a%a = %a;" pp_constOpt constOpt pp_name name
        pp_initialValue initialValue
  | _ -> failwith "@pp_tableProperty: unknown tableProperty"

and pp_tablePropertyList ?(level = 0) fmt tablePropertyList =
  match tablePropertyList.it with
  | ListV tableProperties ->
      pp_list ~level pp_tableProperty ~sep:Nl fmt tableProperties
  | _ -> failwith "@pp_tablePropertyList: expected ListV"

and pp_tableDeclaration ?(level = 0) fmt tableDeclaration =
  match flatten_case_v tableDeclaration with
  | [ [ "TableD" ]; []; [] ], [ name; tablePropertyList ] ->
      F.fprintf fmt "table %a {\n%a\n%s}" pp_name name
        (pp_tablePropertyList ~level:(level + 1))
        tablePropertyList (indent level)
  | _ -> failwith "@pp_tableDeclaration: unknown tableDeclaration"

(* Control type declarations *)

and pp_controlTypeDeclaration fmt controlTypeDeclaration =
  match flatten_case_v controlTypeDeclaration with
  | ( [ [ "ControlTypeD" ]; []; []; [] ],
      [ name; typeParameterList; parameterList ] ) ->
      F.fprintf fmt "control %a%a%a;" pp_name name pp_typeParameterList
        typeParameterList pp_parameterList parameterList
  | _ -> failwith "@pp_controlTypeDeclaration: unknown controlTypeDeclaration"

(* Control declarations *)

and pp_controlBody ?(level = 0) fmt controlBody =
  pp_blockStatement ~level fmt controlBody

and pp_controlLocalDeclaration ?(level = 0) fmt controlLocalDeclaration =
  match flatten_case_v controlLocalDeclaration with
  | [ [ "ConstD" ]; []; []; [] ], _ ->
      pp_constantDeclaration fmt controlLocalDeclaration
  | [ [ "InstD" ]; []; []; []; [] ], _ ->
      pp_instantiation ~level fmt controlLocalDeclaration
  | [ [ "VarD" ]; []; []; [] ], _ ->
      pp_variableDeclaration fmt controlLocalDeclaration
  | [ [ "ActionD" ]; []; []; [] ], _ ->
      pp_actionDeclaration ~level fmt controlLocalDeclaration
  | [ [ "TableD" ]; []; [] ], _ ->
      pp_tableDeclaration ~level fmt controlLocalDeclaration
  | _ -> failwith "@pp_controlLocalDeclaration: unknown controlLocalDeclaration"

and pp_controlLocalDeclarationList ?(level = 0) fmt controlLocalDeclarationList
    =
  match controlLocalDeclarationList.it with
  | ListV controlLocalDeclarations ->
      pp_list ~level
        (pp_controlLocalDeclaration ~level)
        ~sep:Nl fmt controlLocalDeclarations
  | _ -> failwith "@pp_controlLocalDeclarationList: expected ListV"

and pp_controlDeclaration ?(level = 0) fmt controlDeclaration =
  match flatten_case_v controlDeclaration with
  | ( [ [ "ControlD" ]; []; []; []; []; []; [] ],
      [
        name;
        typeParameterList;
        parameterList;
        constructorParameterList;
        controlLocalDeclarationList;
        controlBody;
      ] ) ->
      F.fprintf fmt "control %a%a%a%a {\n%a\n%sapply %a\n}" pp_name name
        pp_typeParameterList typeParameterList pp_parameterList parameterList
        pp_constructorParameterList constructorParameterList
        (pp_controlLocalDeclarationList ~level:(level + 1))
        controlLocalDeclarationList
        (indent (level + 1))
        (pp_controlBody ~level:(level + 1))
        controlBody
  | _ -> failwith "@pp_controlDeclaration: unknown controlDeclaration"

(* Package type declarations *)

and pp_packageTypeDeclaration fmt packageTypeDeclaration =
  match flatten_case_v packageTypeDeclaration with
  | ( [ [ "PackageTypeD" ]; []; []; [] ],
      [ name; typeParameterList; constructorParameterList ] ) ->
      F.fprintf fmt "package %a%a%a;" pp_name name pp_typeParameterList
        typeParameterList pp_constructorParameterList constructorParameterList
  | _ -> failwith "@pp_packageTypeDeclaration: unknown packageTypeDeclaration"

(* Type declarations *)

and pp_typeDeclaration ?(level = 0) fmt typeDeclaration =
  match flatten_case_v typeDeclaration with
  | [ [ "EnumD" ]; []; [] ], _
  | [ [ "SEnumD" ]; []; []; [] ], _
  | [ [ "StructD" ]; []; []; [] ], _
  | [ [ "HeaderD" ]; []; []; [] ], _
  | [ [ "HeaderUnionD" ]; []; []; [] ], _ ->
      pp_derivedTypeDeclaration fmt typeDeclaration
  | [ [ "TypeDefD" ]; []; [] ], _ | [ [ "NewTypeD" ]; []; [] ], _ ->
      pp_typedefDeclaration ~level fmt typeDeclaration
  | [ [ "ParserTypeD" ]; []; []; [] ], _ ->
      pp_parserTypeDeclaration fmt typeDeclaration
  | [ [ "ControlTypeD" ]; []; []; [] ], _ ->
      pp_controlTypeDeclaration fmt typeDeclaration
  | [ [ "PackageTypeD" ]; []; []; [] ], _ ->
      pp_packageTypeDeclaration fmt typeDeclaration
  | _ -> failwith "@pp_typeDeclaration: unknown typeDeclaration"

(* Declarations *)

and pp_declaration ?(level = 0) fmt declaration =
  match flatten_case_v declaration with
  | [ [ "ConstD" ]; []; []; [] ], _ -> pp_constantDeclaration fmt declaration
  | [ [ "InstD" ]; []; []; []; [] ], _ ->
      pp_instantiation ~level fmt declaration
  | [ [ "FuncD" ]; []; []; []; []; [] ], _ ->
      pp_functionDeclaration ~level fmt declaration
  | [ [ "ActionD" ]; []; []; [] ], _ ->
      pp_actionDeclaration ~level fmt declaration
  | [ [ "ErrD" ]; [] ], _ -> pp_errorDeclaration fmt declaration
  | [ [ "MatchKindD" ]; [] ], _ -> pp_matchKindDeclaration fmt declaration
  | [ [ "ExternFuncD" ]; []; []; []; [] ], _
  | [ [ "ExternObjectD" ]; []; []; [] ], _ ->
      pp_externDeclaration ~level fmt declaration
  | [ [ "ParserD" ]; []; []; []; []; []; [] ], _ ->
      pp_parserDeclaration ~level fmt declaration
  | [ [ "ControlD" ]; []; []; []; []; []; [] ], _ ->
      pp_controlDeclaration ~level fmt declaration
  | [ [ "EnumD" ]; []; [] ], _
  | [ [ "SEnumD" ]; []; []; [] ], _
  | [ [ "StructD" ]; []; []; [] ], _
  | [ [ "HeaderD" ]; []; []; [] ], _
  | [ [ "HeaderUnionD" ]; []; []; [] ], _
  | [ [ "TypeDefD" ]; []; [] ], _
  | [ [ "NewTypeD" ]; []; [] ], _
  | [ [ "ParserTypeD" ]; []; []; [] ], _
  | [ [ "ControlTypeD" ]; []; []; [] ], _
  | [ [ "PackageTypeD" ]; []; []; [] ], _ ->
      pp_typeDeclaration ~level fmt declaration
  | _ -> failwith "@pp_declaration: unknown declaration"

and pp_declarationList ?(level = 0) fmt declarationList =
  match declarationList.it with
  | ListV declarations ->
      pp_list ~level (pp_declaration ~level) ~sep:Nl fmt declarations
  | _ -> failwith "@pp_declarationList: expected ListV"

(* Program *)

let pp_program fmt program = pp_declarationList ~level:0 fmt program
