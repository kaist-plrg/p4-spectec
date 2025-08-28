open Il.Ast
open Flatten
open Util.Error
open Util.Source
module F = Format

let error_unexpected s n v =
  error_unparse
    (F.asprintf "@%s: expected %s but got %s" s n (Il.Print.string_of_value v))

let error_unknown s v =
  error_unparse (F.asprintf "@%s: unknown %s" s (Il.Print.string_of_value v))

(* Separator *)

type sep = Nl | Comma | Space

let is_nl = function Nl -> true | _ -> false

let pp_sep fmt = function
  | Nl -> F.fprintf fmt "\n"
  | Comma -> F.fprintf fmt ", "
  | Space -> F.fprintf fmt " "

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
  match flatten_case_v_opt number with
  | Some ([ [ "INT" ]; [] ], [ i ]) -> (
      match i.it with
      | NumV (`Nat n) -> Bigint.pp fmt n
      | NumV (`Int i) -> Bigint.pp fmt i
      | _ -> error_unknown "pp_number" number)
  | Some ([ [ "FINT" ]; []; [] ], [ w; i ]) -> (
      match (w.it, i.it) with
      | NumV (`Nat w), NumV (`Nat n) ->
          F.fprintf fmt "%as%a" Bigint.pp w Bigint.pp n
      | NumV (`Nat w), NumV (`Int i) ->
          F.fprintf fmt "%as%a" Bigint.pp w Bigint.pp i
      | _ -> error_unknown "pp_number" number)
  | Some ([ [ "FBIT" ]; []; [] ], [ w; i ]) -> (
      match (w.it, i.it) with
      | NumV (`Nat w), NumV (`Nat n) ->
          F.fprintf fmt "%aw%a" Bigint.pp w Bigint.pp n
      | NumV (`Nat w), NumV (`Int i) ->
          F.fprintf fmt "%aw%a" Bigint.pp w Bigint.pp i
      | _ -> error_unknown "pp_number" number)
  | _ -> error_unknown "pp_number" number

(* Identifiers *)

let pp_name fmt name =
  match name.it with
  | TextV s -> F.fprintf fmt "%s" s
  | _ -> error_unexpected "pp_name" "TextV" name

let pp_nameList fmt nameList =
  match nameList.it with
  | ListV names -> F.fprintf fmt "%a" (pp_list pp_name ~sep:Comma) names
  | _ -> error_unexpected "pp_nameList" "ListV" nameList

(* Variables (scoped identifiers) *)

let pp_prefixedName fmt prefixedName =
  match flatten_case_v_opt prefixedName with
  | Some ([ [ "TOP" ]; [] ], [ name ]) -> F.fprintf fmt ".%a" pp_name name
  | Some ([ [ "CURRENT" ]; [] ], [ name ]) -> F.fprintf fmt "%a" pp_name name
  | _ -> error_unknown "pp_prefixedName" prefixedName

(* Directions *)

let pp_direction fmt direction =
  match flatten_case_v_opt direction with
  | Some ([ [ "NO" ] ], []) -> ()
  | Some ([ [ "IN" ] ], []) -> F.fprintf fmt "in"
  | Some ([ [ "OUT" ] ], []) -> F.fprintf fmt "out"
  | Some ([ [ "INOUT" ] ], []) -> F.fprintf fmt "inout"
  | _ -> error_unknown "pp_direction" direction

(* Base types *)

let rec pp_baseType fmt baseType =
  match flatten_case_v_opt baseType with
  | Some ([ [ "BoolT" ] ], []) -> F.fprintf fmt "bool"
  | Some ([ [ "ErrT" ] ], []) -> F.fprintf fmt "error"
  | Some ([ [ "MatchKindT" ] ], []) -> F.fprintf fmt "match_kind"
  | Some ([ [ "StrT" ] ], []) -> F.fprintf fmt "string"
  | Some ([ [ "IntT" ] ], []) -> F.fprintf fmt "int"
  | Some ([ [ "FIntT" ]; [] ], [ expression ]) ->
      F.fprintf fmt "int<(%a)>" pp_expression expression
  | Some ([ [ "FBitT" ]; [] ], [ expression ]) ->
      F.fprintf fmt "bit<(%a)>" pp_expression expression
  | Some ([ [ "VBitT" ]; [] ], [ expression ]) ->
      F.fprintf fmt "varbit<(%a)>" pp_expression expression
  | _ -> error_unknown "pp_baseType" baseType

(* Named types *)

and pp_nameType fmt nameType =
  match flatten_case_v_opt nameType with
  | Some ([ [ "NameT" ]; [] ], [ prefixedName ]) ->
      pp_prefixedName fmt prefixedName
  | _ -> error_unknown "pp_nameType" nameType

and pp_specializedType fmt specializedType =
  match flatten_case_v_opt specializedType with
  | Some ([ [ "SpecT" ]; []; [] ], [ prefixedName; typeArgumentList ]) ->
      F.fprintf fmt "%a%a" pp_prefixedName prefixedName pp_typeArgumentList
        typeArgumentList
  | _ -> error_unknown "pp_specializedType" specializedType

and pp_namedType fmt namedType =
  match flatten_case_v_opt namedType with
  | Some ([ [ "NameT" ]; [] ], _) -> pp_nameType fmt namedType
  | Some ([ [ "SpecT" ]; []; [] ], _) -> pp_specializedType fmt namedType
  | _ -> error_unknown "pp_namedType" namedType

(* Header stack types *)

and pp_headerStackType fmt headerStackType =
  match flatten_case_v_opt headerStackType with
  | Some ([ [ "HeaderStackT" ]; []; [] ], [ namedType; expression ]) ->
      F.fprintf fmt "%a[%a]" pp_namedType namedType pp_expression expression
  | _ -> error_unknown "pp_headerStackType" headerStackType

(* List types *)

and pp_listType fmt listType =
  match flatten_case_v_opt listType with
  | Some ([ [ "ListT" ]; [] ], [ typeArgument ]) ->
      F.fprintf fmt "list<%a>" pp_typeArgument typeArgument
  | _ -> error_unknown "pp_listType" listType

(* Tuple types *)

and pp_tupleType fmt tupleType =
  match flatten_case_v_opt tupleType with
  | Some ([ [ "TupleT" ]; [] ], [ typeArgumentList ]) ->
      F.fprintf fmt "tuple%a" pp_typeArgumentList_strict typeArgumentList
  | _ -> error_unknown "pp_tupleType" tupleType

(* Types *)

and pp_type fmt typ =
  match flatten_case_v_opt typ with
  | Some ([ [ "BoolT" ] ], _)
  | Some ([ [ "ErrT" ] ], _)
  | Some ([ [ "MatchKindT" ] ], _)
  | Some ([ [ "StrT" ] ], _)
  | Some ([ [ "IntT" ] ], _)
  | Some ([ [ "FIntT" ]; [] ], _)
  | Some ([ [ "FBitT" ]; [] ], _)
  | Some ([ [ "VBitT" ]; [] ], _) ->
      pp_baseType fmt typ
  | Some ([ [ "NameT" ]; [] ], _) | Some ([ [ "SpecT" ]; []; [] ], _) ->
      pp_namedType fmt typ
  | Some ([ [ "HeaderStackT" ]; []; [] ], _) -> pp_headerStackType fmt typ
  | Some ([ [ "ListT" ]; [] ], _) -> pp_listType fmt typ
  | Some ([ [ "TupleT" ]; [] ], _) -> pp_tupleType fmt typ
  | _ -> error_unknown "pp_type" typ

and pp_typeOrVoid fmt typeOrVoid =
  match flatten_case_v_opt typeOrVoid with
  | Some ([ [ "BoolT" ] ], _)
  | Some ([ [ "ErrT" ] ], _)
  | Some ([ [ "MatchKindT" ] ], _)
  | Some ([ [ "StrT" ] ], _)
  | Some ([ [ "IntT" ] ], _)
  | Some ([ [ "FIntT" ]; [] ], _)
  | Some ([ [ "FBitT" ]; [] ], _)
  | Some ([ [ "VBitT" ]; [] ], _)
  | Some ([ [ "NameT" ]; [] ], _)
  | Some ([ [ "SpecT" ]; []; [] ], _)
  | Some ([ [ "HeaderStackT" ]; []; [] ], _)
  | Some ([ [ "ListT" ]; [] ], _)
  | Some ([ [ "TupleT" ]; [] ], _) ->
      pp_type fmt typeOrVoid
  | Some ([ [ "VoidT" ] ], []) -> F.fprintf fmt "void"
  | _ -> error_unknown "pp_typeOrVoid" typeOrVoid

(* Type parameters *)

and pp_typeParameter fmt typeParameter = pp_name fmt typeParameter

and pp_typeParameterList fmt typeParameterList =
  match typeParameterList.it with
  | ListV [] -> ()
  | ListV typeParameters ->
      F.fprintf fmt "<%a>" (pp_list pp_typeParameter ~sep:Comma) typeParameters
  | _ -> error_unexpected "pp_typeParameterList" "ListV" typeParameterList

(* Parameters *)

and pp_parameter fmt parameter =
  match flatten_case_v_opt parameter with
  | Some ([ []; []; []; []; [] ], [ direction; typ; name; initialValueOpt ]) ->
      F.fprintf fmt "%a %a %a%a" pp_direction direction pp_type typ pp_name name
        pp_initialValueOpt initialValueOpt
  | _ -> error_unknown "pp_parameter" parameter

and pp_parameterList fmt parameterList =
  match parameterList.it with
  | ListV parameters ->
      F.fprintf fmt "(%a)" (pp_list pp_parameter ~sep:Comma) parameters
  | _ -> error_unexpected "pp_parameterList" "ListV" parameterList

(* Constructor parameters *)

and pp_constructorParameter fmt constructorParameter =
  pp_parameter fmt constructorParameter

and pp_constructorParameterList fmt constructorParameterList =
  pp_parameterList fmt constructorParameterList

(* Expression key-value pairs *)

and pp_namedExpression fmt namedExpression =
  match flatten_case_v_opt namedExpression with
  | Some ([ []; []; [] ], [ name; expression ]) ->
      F.fprintf fmt "%a = %a" pp_name name pp_expression expression
  | _ -> error_unknown "pp_namedExpression" namedExpression

and pp_namedExpressionList fmt namedExpressionList =
  match namedExpressionList.it with
  | ListV namedExpressions ->
      F.fprintf fmt "%a"
        (pp_list pp_namedExpression ~sep:Comma)
        namedExpressions
  | _ -> error_unexpected "pp_namedExpressionList" "ListV" namedExpressionList

(* Literal expressions *)

and pp_literalExpression fmt literalExpression =
  match flatten_case_v_opt literalExpression with
  | Some ([ [ "BoolE" ]; [] ], [ b ]) -> (
      match b.it with
      | BoolV true -> F.fprintf fmt "true"
      | BoolV false -> F.fprintf fmt "false"
      | _ -> error_unknown "pp_literalExpression" literalExpression)
  | Some ([ [ "NumE" ]; [] ], [ number ]) -> pp_number fmt number
  | Some ([ [ "StrE" ]; [] ], [ s ]) -> (
      match s.it with
      | TextV s -> F.fprintf fmt "\"%s\"" s
      | _ -> error_unknown "pp_literalExpression" literalExpression)
  | _ -> error_unknown "pp_literalExpression" literalExpression

(* Reference expressions *)

and pp_referenceExpression fmt referenceExpression =
  match flatten_case_v_opt referenceExpression with
  | Some ([ [ "NameE" ]; [] ], [ prefixedName ]) ->
      pp_prefixedName fmt prefixedName
  | _ -> error_unknown "pp_referenceExpression" referenceExpression

(* Default expressions *)

and pp_defaultExpression fmt defaultExpression =
  match flatten_case_v_opt defaultExpression with
  | Some ([ [ "DefaultE" ] ], []) -> F.fprintf fmt "..."
  | _ -> error_unknown "pp_defaultExpression" defaultExpression

(* Unary, binary, and ternary expressions *)

and pp_unop fmt unop =
  match flatten_case_v_opt unop with
  | Some ([ [ "BNOT" ] ], []) -> F.fprintf fmt "~"
  | Some ([ [ "LNOT" ] ], []) -> F.fprintf fmt "!"
  | Some ([ [ "UPLUS" ] ], []) -> F.fprintf fmt "+"
  | Some ([ [ "UMINUS" ] ], []) -> F.fprintf fmt "-"
  | _ -> error_unknown "pp_unop" unop

and pp_unaryExpression fmt unaryExpression =
  match flatten_case_v_opt unaryExpression with
  | Some ([ [ "UnE" ]; []; [] ], [ unop; expression ]) ->
      F.fprintf fmt "%a%a" pp_unop unop pp_expression expression
  | _ -> error_unknown "pp_unaryExpression" unaryExpression

and pp_binop fmt binop =
  match flatten_case_v_opt binop with
  | Some ([ [ "PLUS" ] ], []) -> F.fprintf fmt "+"
  | Some ([ [ "SPLUS" ] ], []) -> F.fprintf fmt "|+|"
  | Some ([ [ "MINUS" ] ], []) -> F.fprintf fmt "-"
  | Some ([ [ "SMINUS" ] ], []) -> F.fprintf fmt "|-|"
  | Some ([ [ "MUL" ] ], []) -> F.fprintf fmt "*"
  | Some ([ [ "DIV" ] ], []) -> F.fprintf fmt "/"
  | Some ([ [ "MOD" ] ], []) -> F.fprintf fmt "%s" "%"
  | Some ([ [ "SHL" ] ], []) -> F.fprintf fmt "<<"
  | Some ([ [ "SHR" ] ], []) -> F.fprintf fmt ">>"
  | Some ([ [ "LE" ] ], []) -> F.fprintf fmt "<="
  | Some ([ [ "GE" ] ], []) -> F.fprintf fmt ">="
  | Some ([ [ "LT" ] ], []) -> F.fprintf fmt "<"
  | Some ([ [ "GT" ] ], []) -> F.fprintf fmt ">"
  | Some ([ [ "EQ" ] ], []) -> F.fprintf fmt "=="
  | Some ([ [ "NE" ] ], []) -> F.fprintf fmt "!="
  | Some ([ [ "BAND" ] ], []) -> F.fprintf fmt "&"
  | Some ([ [ "BXOR" ] ], []) -> F.fprintf fmt "^"
  | Some ([ [ "BOR" ] ], []) -> F.fprintf fmt "|"
  | Some ([ [ "CONCAT" ] ], []) -> F.fprintf fmt "++"
  | Some ([ [ "LAND" ] ], []) -> F.fprintf fmt "&&"
  | Some ([ [ "LOR" ] ], []) -> F.fprintf fmt "||"
  | _ -> error_unknown "pp_binop" binop

and pp_binaryExpression fmt binaryExpression =
  match flatten_case_v_opt binaryExpression with
  | Some ([ [ "BinE" ]; []; []; [] ], [ expression_l; binop; expression_r ]) ->
      F.fprintf fmt "((%a) %a (%a))" pp_expression expression_l pp_binop binop
        pp_expression expression_r
  | _ -> error_unknown "pp_binaryExpression" binaryExpression

and pp_ternaryExpression fmt ternaryExpression =
  match flatten_case_v_opt ternaryExpression with
  | Some
      ([ [ "TernE" ]; []; []; [] ], [ expression_c; expression_t; expression_f ])
    ->
      F.fprintf fmt "((%a) ? (%a) : (%a))" pp_expression expression_c
        pp_expression expression_t pp_expression expression_f
  | _ -> error_unknown "pp_ternaryExpression" ternaryExpression

(* Cast expressions *)

and pp_castExpression fmt castExpression =
  match flatten_case_v_opt castExpression with
  | Some ([ [ "CastE" ]; []; [] ], [ typ; expression ]) ->
      F.fprintf fmt "((%a) (%a))" pp_type typ pp_expression expression
  | _ -> error_unknown "pp_castExpression" castExpression

(* Data (aggregate) expressions *)

and pp_dataExpression fmt dataExpression =
  match flatten_case_v_opt dataExpression with
  | Some ([ [ "InvalidE" ] ], []) -> F.fprintf fmt "{#}"
  | Some ([ [ "SeqE" ]; [] ], [ expressionList ]) ->
      F.fprintf fmt "{ %a }" pp_expressionList expressionList
  | Some ([ [ "RecordE" ]; [] ], [ namedExpressionList ]) ->
      F.fprintf fmt "{ %a }" pp_namedExpressionList namedExpressionList
  | Some ([ [ "RecordDefaultE" ]; [] ], [ namedExpressionList ]) ->
      F.fprintf fmt "{ %a, ... }" pp_namedExpressionList namedExpressionList
  | _ -> error_unknown "pp_dataExpression" dataExpression

(* Member and index access expressions *)

and pp_errorAccessExpression fmt errorAccessExpression =
  match flatten_case_v_opt errorAccessExpression with
  | Some ([ [ "ErrAccE" ]; [] ], [ name ]) ->
      F.fprintf fmt "error.%a" pp_name name
  | _ -> error_unknown "pp_errorAccessExpression" errorAccessExpression

and pp_memberAccessExpression fmt memberAccessExpression =
  match flatten_case_v_opt memberAccessExpression with
  | Some ([ [ "TypeAccE" ]; []; [] ], [ prefixedName; name ]) ->
      F.fprintf fmt "%a.%a" pp_prefixedName prefixedName pp_name name
  | Some ([ [ "ExprAccE" ]; []; [] ], [ expression; name ]) ->
      F.fprintf fmt "%a.%a" pp_expression expression pp_name name
  | _ -> error_unknown "pp_memberAccessExpression" memberAccessExpression

and pp_indexAccessExpression fmt indexAccessExpression =
  match flatten_case_v_opt indexAccessExpression with
  | Some ([ [ "ArrAccE" ]; []; [] ], [ expression_b; expression_i ]) ->
      F.fprintf fmt "%a[%a]" pp_expression expression_b pp_expression
        expression_i
  | Some
      ( [ [ "BitAccE" ]; []; []; [] ],
        [ expression_b; expression_h; expression_l ] ) ->
      F.fprintf fmt "%a[%a:%a]" pp_expression expression_b pp_expression
        expression_h pp_expression expression_l
  | _ -> error_unknown "pp_indexAccessExpression" indexAccessExpression

and pp_accessExpression fmt accessExpression =
  match flatten_case_v_opt accessExpression with
  | Some ([ [ "ErrAccE" ]; [] ], _) ->
      pp_errorAccessExpression fmt accessExpression
  | Some ([ [ "TypeAccE" ]; []; [] ], _) | Some ([ [ "ExprAccE" ]; []; [] ], _)
    ->
      pp_memberAccessExpression fmt accessExpression
  | Some ([ [ "ArrAccE" ]; []; [] ], _) | Some ([ [ "BitAccE" ]; []; []; [] ], _)
    ->
      pp_indexAccessExpression fmt accessExpression
  | _ -> error_unknown "pp_accessExpression" accessExpression

(* Call expressions *)

and pp_callExpression fmt callExpression =
  match flatten_case_v_opt callExpression with
  | Some
      ( [ [ "CallE" ]; []; []; [] ],
        [ routineTarget; typeArgumentList; argumentList ] ) ->
      F.fprintf fmt "%a%a%a" pp_expression routineTarget pp_typeArgumentList
        typeArgumentList pp_argumentList argumentList
  | Some ([ [ "InstE" ]; []; [] ], [ namedType; argumentList ]) ->
      F.fprintf fmt "%a%a" pp_namedType namedType pp_argumentList argumentList
  | _ -> error_unknown "pp_callExpression" callExpression

(* Expressions *)

and pp_expression fmt expression =
  match flatten_case_v_opt expression with
  | Some ([ [ "BoolE" ]; [] ], _)
  | Some ([ [ "NumE" ]; [] ], _)
  | Some ([ [ "StrE" ]; [] ], _) ->
      pp_literalExpression fmt expression
  | Some ([ [ "NameE" ]; [] ], _) -> pp_referenceExpression fmt expression
  | Some ([ [ "DefaultE" ] ], _) -> pp_defaultExpression fmt expression
  | Some ([ [ "UnE" ]; []; [] ], _) -> pp_unaryExpression fmt expression
  | Some ([ [ "BinE" ]; []; []; [] ], _) -> pp_binaryExpression fmt expression
  | Some ([ [ "TernE" ]; []; []; [] ], _) -> pp_ternaryExpression fmt expression
  | Some ([ [ "CastE" ]; []; [] ], _) -> pp_castExpression fmt expression
  | Some ([ [ "InvalidE" ] ], _)
  | Some ([ [ "SeqE" ]; [] ], _)
  | Some ([ [ "RecordE" ]; [] ], _)
  | Some ([ [ "RecordDefaultE" ]; [] ], _) ->
      pp_dataExpression fmt expression
  | Some ([ [ "ErrAccE" ]; [] ], _)
  | Some ([ [ "TypeAccE" ]; []; [] ], _)
  | Some ([ [ "ExprAccE" ]; []; [] ], _)
  | Some ([ [ "ArrAccE" ]; []; [] ], _)
  | Some ([ [ "BitAccE" ]; []; []; [] ], _) ->
      pp_accessExpression fmt expression
  | Some ([ [ "CallE" ]; []; []; [] ], _) | Some ([ [ "InstE" ]; []; [] ], _) ->
      pp_callExpression fmt expression
  | _ -> error_unknown "pp_expression" expression

and pp_expressionList fmt expressionList =
  match expressionList.it with
  | ListV expressions ->
      F.fprintf fmt "%a" (pp_list pp_expression ~sep:Comma) expressions
  | _ -> error_unexpected "pp_expressionList" "ListV" expressionList

and pp_routineTarget fmt routineTarget = pp_expression fmt routineTarget

(* Keyset expressions *)

and pp_keysetExpression fmt keysetExpression =
  match flatten_case_v_opt keysetExpression with
  | Some ([ [ "ExprK" ]; [] ], [ expression ]) -> pp_expression fmt expression
  | Some ([ [ "MaskK" ]; []; [] ], [ expression_b; expression_m ]) ->
      F.fprintf fmt "%a &&& %a" pp_expression expression_b pp_expression
        expression_m
  | Some ([ [ "RangeK" ]; []; [] ], [ expression_l; expression_h ]) ->
      F.fprintf fmt "%a .. %a" pp_expression expression_l pp_expression
        expression_h
  | Some ([ [ "DefaultK" ] ], []) -> F.fprintf fmt "default"
  | Some ([ [ "AnyK" ] ], []) -> F.fprintf fmt "_"
  | _ -> error_unknown "pp_keysetExpression" keysetExpression

and pp_keysetExpressionList fmt keysetExpressionList =
  match keysetExpressionList.it with
  | ListV keysetExpressions ->
      F.fprintf fmt "(%a)"
        (pp_list pp_keysetExpression ~sep:Comma)
        keysetExpressions
  | _ -> error_unexpected "pp_keysetExpressionList" "ListV" keysetExpressionList

(* Type arguments *)

and pp_typeArgument fmt typeArgument =
  match flatten_case_v_opt typeArgument with
  | Some ([ [ "BoolT" ] ], _)
  | Some ([ [ "ErrT" ] ], _)
  | Some ([ [ "MatchKindT" ] ], _)
  | Some ([ [ "StrT" ] ], _)
  | Some ([ [ "IntT" ] ], _)
  | Some ([ [ "FIntT" ]; [] ], _)
  | Some ([ [ "FBitT" ]; [] ], _)
  | Some ([ [ "VBitT" ]; [] ], _)
  | Some ([ [ "NameT" ]; [] ], _)
  | Some ([ [ "SpecT" ]; []; [] ], _)
  | Some ([ [ "HeaderStackT" ]; []; [] ], _)
  | Some ([ [ "ListT" ]; [] ], _)
  | Some ([ [ "TupleT" ]; [] ], _) ->
      pp_type fmt typeArgument
  | Some ([ [ "VoidT" ] ], []) -> F.fprintf fmt "void"
  | Some ([ [ "AnyT" ] ], []) -> F.fprintf fmt "_"
  | _ -> error_unknown "pp_typeArgument" typeArgument

and pp_typeArgumentList fmt typeArgumentList =
  match typeArgumentList.it with
  | ListV [] -> ()
  | ListV typeArguments ->
      F.fprintf fmt "<%a>" (pp_list pp_typeArgument ~sep:Comma) typeArguments
  | _ -> error_unexpected "pp_typeArgumentList" "ListV" typeArgumentList

and pp_typeArgumentList_strict fmt typeArgumentList =
  match typeArgumentList.it with
  | ListV [] -> F.fprintf fmt "<>"
  | ListV typeArguments ->
      F.fprintf fmt "<%a>" (pp_list pp_typeArgument ~sep:Comma) typeArguments
  | _ -> error_unexpected "pp_typeArgumentList_strict" "ListV" typeArgumentList

(* Arguments *)

and pp_argument fmt argument =
  match flatten_case_v_opt argument with
  | Some ([ [ "ExprA" ]; [] ], [ expression ]) -> pp_expression fmt expression
  | Some ([ [ "NameA" ]; []; [] ], [ name; expression ]) ->
      F.fprintf fmt "%a = %a" pp_name name pp_expression expression
  | Some ([ [ "NameAnyA" ]; [] ], [ name ]) ->
      F.fprintf fmt "%a = _" pp_name name
  | Some ([ [ "AnyA" ] ], []) -> F.fprintf fmt "_"
  | _ -> error_unknown "pp_argument" argument

and pp_argumentList fmt argumentList =
  match argumentList.it with
  | ListV arguments ->
      F.fprintf fmt "(%a)" (pp_list pp_argument ~sep:Comma) arguments
  | _ -> error_unexpected "pp_argumentList" "ListV" argumentList

(* L-values *)

and pp_lvalue fmt lvalue =
  match flatten_case_v_opt lvalue with
  | Some ([ [ "NameL" ]; [] ], [ prefixedName ]) ->
      pp_prefixedName fmt prefixedName
  | Some ([ [ "LvalueAccL" ]; []; [] ], [ lvalue; name ]) ->
      F.fprintf fmt "%a.%a" pp_lvalue lvalue pp_name name
  | Some ([ [ "ArrAccL" ]; []; [] ], [ lvalue; expression ]) ->
      F.fprintf fmt "%a[%a]" pp_lvalue lvalue pp_expression expression
  | Some ([ [ "BitAccL" ]; []; []; [] ], [ lvalue; expression_h; expression_l ])
    ->
      F.fprintf fmt "%a[%a:%a]" pp_lvalue lvalue pp_expression expression_h
        pp_expression expression_l
  | _ -> error_unknown "pp_lvalue" lvalue

(* Empty statements *)

and pp_emptyStatement fmt emptyStatement =
  match flatten_case_v_opt emptyStatement with
  | Some ([ [ "EmptyS" ] ], []) -> F.fprintf fmt ";"
  | _ -> error_unknown "pp_emptyStatement" emptyStatement

(* Assignment statements *)

and pp_assignmentStatement fmt assignmentStatement =
  match flatten_case_v_opt assignmentStatement with
  | Some ([ [ "AssignS" ]; []; [] ], [ lvalue; expression ]) ->
      F.fprintf fmt "%a = %a;" pp_lvalue lvalue pp_expression expression
  | _ -> error_unknown "pp_assignmentStatement" assignmentStatement

(* Call statements *)

and pp_callStatement fmt callStatement =
  match flatten_case_v_opt callStatement with
  | Some
      ([ [ "CallS" ]; []; []; [] ], [ lvalue; typeArgumentList; argumentList ])
    ->
      F.fprintf fmt "%a%a%a;" pp_lvalue lvalue pp_typeArgumentList
        typeArgumentList pp_argumentList argumentList
  | _ -> error_unknown "pp_callStatement" callStatement

(* Direct application statements *)

and pp_directApplicationStatement fmt directApplicationStatement =
  match flatten_case_v_opt directApplicationStatement with
  | Some ([ [ "InstS" ]; []; [] ], [ namedType; argumentList ]) ->
      F.fprintf fmt "%a.apply%a;" pp_namedType namedType pp_argumentList
        argumentList
  | _ ->
      error_unknown "pp_directApplicationStatement" directApplicationStatement

(* Return statements *)

and pp_returnStatement fmt returnStatement =
  match flatten_case_v_opt returnStatement with
  | Some ([ [ "ReturnS" ]; [] ], [ expressionOpt ]) -> (
      match expressionOpt.it with
      | OptV (Some expression) ->
          F.fprintf fmt "return %a;" pp_expression expression
      | OptV None -> F.fprintf fmt "return;"
      | _ -> error_unknown "pp_returnStatement" returnStatement)
  | _ -> error_unknown "pp_returnStatement" returnStatement

(* Exit statements *)

and pp_exitStatement fmt exitStatement =
  match flatten_case_v_opt exitStatement with
  | Some ([ [ "ExitS" ] ], []) -> F.fprintf fmt "exit;"
  | _ -> error_unknown "pp_exitStatement" exitStatement

(* Block statements *)

and pp_blockStatement ?(level = 0) fmt blockStatement =
  match flatten_case_v_opt blockStatement with
  | Some ([ [ "BlockS" ]; [] ], [ blockElementStatementList ]) ->
      F.fprintf fmt "{\n%a\n%s}"
        (pp_blockElementStatementList ~level:(level + 1))
        blockElementStatementList (indent level)
  | _ -> error_unknown "pp_blockStatement" blockStatement

(* Conditional statements *)

and pp_conditionalStatement ?(level = 0) fmt conditionalStatement =
  match flatten_case_v_opt conditionalStatement with
  | Some
      ([ [ "IfS" ]; []; []; [] ], [ expression_c; statement_t; statement_f_opt ])
    -> (
      match statement_f_opt.it with
      | OptV (Some statement_f) ->
          F.fprintf fmt "if (%a) %a\n%selse %a" pp_expression expression_c
            (pp_statement ~level) statement_t (indent level)
            (pp_statement ~level) statement_f
      | OptV None ->
          F.fprintf fmt "if (%a) %a" pp_expression expression_c
            (pp_statement ~level) statement_t
      | _ -> error_unknown "pp_conditionalStatement" conditionalStatement)
  | _ -> error_unknown "pp_conditionalStatement" conditionalStatement

(* Switch statements *)

and pp_switchLabel fmt switchLabel =
  match flatten_case_v_opt switchLabel with
  | Some ([ [ "DefaultL" ] ], []) -> F.fprintf fmt "default"
  | Some ([ [ "ExprL" ]; [] ], [ expression ]) ->
      F.fprintf fmt "%a" pp_expression expression
  | _ -> error_unknown "pp_switchLabel" switchLabel

and pp_switchCase ?(level = 0) fmt switchCase =
  match flatten_case_v_opt switchCase with
  | Some ([ [ "FallC" ]; [] ], [ switchLabel ]) ->
      F.fprintf fmt "%a:" pp_switchLabel switchLabel
  | Some ([ [ "MatchC" ]; []; [] ], [ switchLabel; blockStatement ]) ->
      F.fprintf fmt "%a: %a" pp_switchLabel switchLabel
        (pp_blockStatement ~level:(level + 1))
        blockStatement
  | _ -> error_unknown "pp_switchCase" switchCase

and pp_switchCaseList ?(level = 0) fmt switchCaseList =
  match switchCaseList.it with
  | ListV switchCases ->
      pp_list ~level (pp_switchCase ~level) ~sep:Nl fmt switchCases
  | _ -> error_unexpected "pp_switchCaseList" "ListV" switchCaseList

and pp_switchStatement ?(level = 0) fmt switchStatement =
  match flatten_case_v_opt switchStatement with
  | Some ([ [ "SwitchS" ]; []; [] ], [ expression; switchCaseList ]) ->
      F.fprintf fmt "switch (%a) {\n%a\n%s}" pp_expression expression
        (pp_switchCaseList ~level:(level + 1))
        switchCaseList (indent level)
  | _ -> error_unknown "pp_switchStatement" switchStatement

(* Statement *)

and pp_statement ?(level = 0) fmt statement =
  match flatten_case_v_opt statement with
  | Some ([ [ "EmptyS" ] ], _) -> pp_emptyStatement fmt statement
  | Some ([ [ "AssignS" ]; []; [] ], _) -> pp_assignmentStatement fmt statement
  | Some ([ [ "CallS" ]; []; []; [] ], _) -> pp_callStatement fmt statement
  | Some ([ [ "InstS" ]; []; [] ], _) ->
      pp_directApplicationStatement fmt statement
  | Some ([ [ "ReturnS" ]; [] ], _) -> pp_returnStatement fmt statement
  | Some ([ [ "ExitS" ] ], _) -> pp_exitStatement fmt statement
  | Some ([ [ "BlockS" ]; [] ], _) -> pp_blockStatement ~level fmt statement
  | Some ([ [ "IfS" ]; []; []; [] ], _) ->
      pp_conditionalStatement ~level fmt statement
  | Some ([ [ "SwitchS" ]; []; [] ], _) ->
      pp_switchStatement ~level fmt statement
  | _ -> error_unknown "pp_statement" statement

(* Constant and variable declarations *)

and pp_initialValue fmt initialValue = pp_expression fmt initialValue

and pp_initialValueOpt fmt initialValueOpt =
  match initialValueOpt.it with
  | OptV (Some initialValue) ->
      F.fprintf fmt " = %a" pp_initialValue initialValue
  | OptV None -> ()
  | _ -> error_unexpected "pp_initialValueOpt" "OptV" initialValueOpt

and pp_constantDeclaration fmt constantDeclaration =
  match flatten_case_v_opt constantDeclaration with
  | Some ([ [ "ConstD" ]; []; []; [] ], [ typ; name; initialValue ]) ->
      F.fprintf fmt "const %a %a = %a;" pp_type typ pp_name name pp_initialValue
        initialValue
  | _ -> error_unknown "pp_constantDeclaration" constantDeclaration

and pp_variableDeclaration fmt variableDeclaration =
  match flatten_case_v_opt variableDeclaration with
  | Some ([ [ "VarD" ]; []; []; [] ], [ typ; name; initialValueOpt ]) ->
      F.fprintf fmt "%a %a%a;" pp_type typ pp_name name pp_initialValueOpt
        initialValueOpt
  | _ -> error_unknown "pp_variableDeclaration" variableDeclaration

and pp_blockElementStatement ?(level = 0) fmt blockElementStatement =
  match flatten_case_v_opt blockElementStatement with
  | Some ([ [ "ConstD" ]; []; []; [] ], _) ->
      pp_constantDeclaration fmt blockElementStatement
  | Some ([ [ "VarD" ]; []; []; [] ], _) ->
      pp_variableDeclaration fmt blockElementStatement
  | Some ([ [ "EmptyS" ] ], _)
  | Some ([ [ "AssignS" ]; []; [] ], _)
  | Some ([ [ "CallS" ]; []; []; [] ], _)
  | Some ([ [ "InstS" ]; []; [] ], _)
  | Some ([ [ "ReturnS" ]; [] ], _)
  | Some ([ [ "ExitS" ] ], _)
  | Some ([ [ "BlockS" ]; [] ], _)
  | Some ([ [ "IfS" ]; []; []; [] ], _)
  | Some ([ [ "SwitchS" ]; []; [] ], _) ->
      pp_statement ~level fmt blockElementStatement
  | _ -> error_unknown "pp_blockElementStatement" blockElementStatement

and pp_blockElementStatementList ?(level = 0) fmt blockElementStatementList =
  match blockElementStatementList.it with
  | ListV blockElementStatements ->
      pp_list ~level
        (pp_blockElementStatement ~level)
        ~sep:Nl fmt blockElementStatements
  | _ ->
      error_unexpected "pp_blockElementStatementList" "ListV"
        blockElementStatementList

(* Function declarations *)

and pp_functionDeclaration ?(level = 0) fmt functionDeclaration =
  match flatten_case_v_opt functionDeclaration with
  | Some
      ( [ [ "FuncD" ]; []; []; []; []; [] ],
        [ typeOrVoid; name; typeParameterList; parameterList; blockStatement ]
      ) ->
      F.fprintf fmt "%a %a%a%a %a" pp_typeOrVoid typeOrVoid pp_name name
        pp_typeParameterList typeParameterList pp_parameterList parameterList
        (pp_blockStatement ~level:(level + 1))
        blockStatement
  | _ -> error_unknown "pp_functionDeclaration" functionDeclaration

(* Action declarations *)

and pp_actionDeclaration ?(level = 0) fmt actionDeclaration =
  match flatten_case_v_opt actionDeclaration with
  | Some ([ [ "ActionD" ]; []; []; [] ], [ name; parameterList; blockStatement ])
    ->
      F.fprintf fmt "action %a%a %a" pp_name name pp_parameterList parameterList
        (pp_blockStatement ~level) blockStatement
  | _ -> error_unknown "pp_actionDeclaration" actionDeclaration

(* Instantiations *)

and pp_instantiation ?(level = 0) fmt instantiation =
  match flatten_case_v_opt instantiation with
  | Some
      ( [ [ "InstD" ]; []; []; []; [] ],
        [ typ; argumentList; name; objectInitializerOpt ] ) ->
      F.fprintf fmt "%a%a %a%a;" pp_type typ pp_argumentList argumentList
        pp_name name
        (pp_objectInitializerOpt ~level:(level + 1))
        objectInitializerOpt
  | _ -> error_unknown "pp_instantiation" instantiation

and pp_objectDeclaration ?(level = 0) fmt objectDeclaration =
  match flatten_case_v_opt objectDeclaration with
  | Some ([ [ "FuncD" ]; []; []; []; []; [] ], _) ->
      pp_functionDeclaration ~level fmt objectDeclaration
  | Some ([ [ "InstD" ]; []; []; []; [] ], _) ->
      pp_instantiation ~level fmt objectDeclaration
  | _ -> error_unknown "pp_objectDeclaration" objectDeclaration

and pp_objectInitializer ?(level = 0) fmt objectInitializer =
  match objectInitializer.it with
  | ListV objectDeclarations ->
      F.fprintf fmt "{\n%a\n}"
        (pp_list ~level (pp_objectDeclaration ~level) ~sep:Nl)
        objectDeclarations
  | _ -> error_unexpected "pp_objectInitializer" "ListV" objectInitializer

and pp_objectInitializerOpt ?(level = 0) fmt objectInitializerOpt =
  match objectInitializerOpt.it with
  | OptV (Some objectInitializer) ->
      F.fprintf fmt " = %a" (pp_objectInitializer ~level) objectInitializer
  | OptV None -> ()
  | _ -> error_unexpected "pp_objectInitializerOpt" "OptV" objectInitializerOpt

(* Error declarations *)

and pp_errorDeclaration fmt errorDeclaration =
  match flatten_case_v_opt errorDeclaration with
  | Some ([ [ "ErrD" ]; [] ], [ nameList ]) ->
      F.fprintf fmt "error { %a };" pp_nameList nameList
  | _ -> error_unknown "pp_errorDeclaration" errorDeclaration

(* Match kind declarations *)

and pp_matchKindDeclaration fmt matchKindDeclaration =
  match flatten_case_v_opt matchKindDeclaration with
  | Some ([ [ "MatchKindD" ]; [] ], [ nameList ]) ->
      F.fprintf fmt "match_kind { %a };" pp_nameList nameList
  | _ -> error_unknown "pp_matchKindDeclaration" matchKindDeclaration

(* Enum type declarations *)

and pp_enumTypeDeclaration fmt enumTypeDeclaration =
  match flatten_case_v_opt enumTypeDeclaration with
  | Some ([ [ "EnumD" ]; []; [] ], [ name; nameList ]) ->
      F.fprintf fmt "enum %a { %a }" pp_name name pp_nameList nameList
  | Some ([ [ "SEnumD" ]; []; []; [] ], [ typ; name; namedExpressionList ]) ->
      F.fprintf fmt "enum %a %a { %a }" pp_type typ pp_name name
        pp_namedExpressionList namedExpressionList
  | _ -> error_unknown "pp_enumTypeDeclaration" enumTypeDeclaration

(* Struct, header, and union type declarations *)

and pp_typeField fmt typeField =
  match flatten_case_v_opt typeField with
  | Some ([ []; []; [] ], [ typ; name ]) ->
      F.fprintf fmt "%a %a;" pp_type typ pp_name name
  | _ -> error_unknown "pp_typeField" typeField

and pp_typeFieldList ?(level = 0) fmt typeFieldList =
  match typeFieldList.it with
  | ListV typeFields -> pp_list ~level pp_typeField ~sep:Nl fmt typeFields
  | _ -> error_unexpected "pp_typeFieldList" "ListV" typeFieldList

and pp_structTypeDeclaration ?(level = 0) fmt structTypeDeclaration =
  match flatten_case_v_opt structTypeDeclaration with
  | Some
      ([ [ "StructD" ]; []; []; [] ], [ name; typeParameterList; typeFieldList ])
    ->
      F.fprintf fmt "struct %a%a {\n%a\n}" pp_name name pp_typeParameterList
        typeParameterList
        (pp_typeFieldList ~level:(level + 1))
        typeFieldList
  | _ -> error_unknown "pp_structTypeDeclaration" structTypeDeclaration

and pp_headerTypeDeclaration ?(level = 0) fmt headerTypeDeclaration =
  match flatten_case_v_opt headerTypeDeclaration with
  | Some
      ([ [ "HeaderD" ]; []; []; [] ], [ name; typeParameterList; typeFieldList ])
    ->
      F.fprintf fmt "header %a%a {\n%a\n}" pp_name name pp_typeParameterList
        typeParameterList
        (pp_typeFieldList ~level:(level + 1))
        typeFieldList
  | _ -> error_unknown "pp_headerTypeDeclaration" headerTypeDeclaration

and pp_headerUnionTypeDeclaration ?(level = 0) fmt headerUnionTypeDeclaration =
  match flatten_case_v_opt headerUnionTypeDeclaration with
  | Some
      ( [ [ "HeaderUnionD" ]; []; []; [] ],
        [ name; typeParameterList; typeFieldList ] ) ->
      F.fprintf fmt "header_union %a%a {\n%a\n}" pp_name name
        pp_typeParameterList typeParameterList
        (pp_typeFieldList ~level:(level + 1))
        typeFieldList
  | _ ->
      error_unknown "pp_headerUnionTypeDeclaration" headerUnionTypeDeclaration

and pp_derivedTypeDeclaration ?(level = 0) fmt derivedTypeDeclaration =
  match flatten_case_v_opt derivedTypeDeclaration with
  | Some ([ [ "EnumD" ]; []; [] ], _) | Some ([ [ "SEnumD" ]; []; []; [] ], _)
    ->
      pp_enumTypeDeclaration fmt derivedTypeDeclaration
  | Some ([ [ "StructD" ]; []; []; [] ], _) ->
      pp_structTypeDeclaration ~level fmt derivedTypeDeclaration
  | Some ([ [ "HeaderD" ]; []; []; [] ], _) ->
      pp_headerTypeDeclaration ~level fmt derivedTypeDeclaration
  | Some ([ [ "HeaderUnionD" ]; []; []; [] ], _) ->
      pp_headerUnionTypeDeclaration ~level fmt derivedTypeDeclaration
  | _ -> error_unknown "pp_derivedTypeDeclaration" derivedTypeDeclaration

(* Typedef and newtype declarations *)

and pp_typedefType ?(level = 0) fmt typedefType =
  match flatten_case_v_opt typedefType with
  | Some ([ [ "PlainT" ]; [] ], [ typ ]) -> pp_type fmt typ
  | Some ([ [ "DerivedT" ]; [] ], [ derivedTypeDeclaration ]) ->
      pp_derivedTypeDeclaration ~level fmt derivedTypeDeclaration
  | _ -> error_unknown "pp_typedefType" typedefType

and pp_typedefDeclaration ?(level = 0) fmt typedefDeclaration =
  match flatten_case_v_opt typedefDeclaration with
  | Some ([ [ "TypeDefD" ]; []; [] ], [ typedefType; name ]) ->
      F.fprintf fmt "typedef %a %a;"
        (pp_typedefType ~level:(level + 1))
        typedefType pp_name name
  | Some ([ [ "NewTypeD" ]; []; [] ], [ typ; name ]) ->
      F.fprintf fmt "type %a %a;" pp_type typ pp_name name
  | _ -> error_unknown "pp_typedefDeclaration" typedefDeclaration

(* Extern declarations *)

and pp_externFunctionDeclaration fmt externFunctionDeclaration =
  match flatten_case_v_opt externFunctionDeclaration with
  | Some
      ( [ [ "ExternFuncD" ]; []; []; []; [] ],
        [ typeOrVoid; name; typeParameterList; parameterList ] ) ->
      F.fprintf fmt "extern %a %a%a%a;" pp_typeOrVoid typeOrVoid pp_name name
        pp_typeParameterList typeParameterList pp_parameterList parameterList
  | _ -> error_unknown "pp_externFunctionDeclaration" externFunctionDeclaration

and pp_methodPrototype fmt methodPrototype =
  match flatten_case_v_opt methodPrototype with
  | Some ([ [ "ConsM" ]; []; [] ], [ name; constructorParameterList ]) ->
      F.fprintf fmt "%a%a;" pp_name name pp_constructorParameterList
        constructorParameterList
  | Some
      ( [ [ "MethodM" ]; []; []; []; [] ],
        [ typeOrVoid; name; typeParameterList; parameterList ] ) ->
      F.fprintf fmt "%a %a%a%a;" pp_typeOrVoid typeOrVoid pp_name name
        pp_typeParameterList typeParameterList pp_parameterList parameterList
  | Some
      ( [ [ "AbstractMethodM" ]; []; []; []; [] ],
        [ typeOrVoid; name; typeParameterList; parameterList ] ) ->
      F.fprintf fmt "abstract %a %a%a%a;" pp_typeOrVoid typeOrVoid pp_name name
        pp_typeParameterList typeParameterList pp_parameterList parameterList
  | _ -> error_unknown "pp_methodPrototype" methodPrototype

and pp_methodPrototypeList ?(level = 0) fmt methodPrototypeList =
  match methodPrototypeList.it with
  | ListV methodPrototypes ->
      pp_list ~level pp_methodPrototype ~sep:Nl fmt methodPrototypes
  | _ -> error_unexpected "pp_methodPrototypeList" "ListV" methodPrototypeList

and pp_externObjectDeclaration ?(level = 0) fmt externObjectDeclaration =
  match flatten_case_v_opt externObjectDeclaration with
  | Some
      ( [ [ "ExternObjectD" ]; []; []; [] ],
        [ name; typeParameterList; methodPrototypeList ] ) ->
      F.fprintf fmt "extern %a%a {\n%a\n}" pp_name name pp_typeParameterList
        typeParameterList
        (pp_methodPrototypeList ~level:(level + 1))
        methodPrototypeList
  | _ -> error_unknown "pp_externObjectDeclaration" externObjectDeclaration

and pp_externDeclaration ?(level = 0) fmt externDeclaration =
  match flatten_case_v_opt externDeclaration with
  | Some ([ [ "ExternFuncD" ]; []; []; []; [] ], _) ->
      pp_externFunctionDeclaration fmt externDeclaration
  | Some ([ [ "ExternObjectD" ]; []; []; [] ], _) ->
      pp_externObjectDeclaration ~level fmt externDeclaration
  | _ -> error_unknown "pp_externDeclaration" externDeclaration

(* Select cases *)

and pp_selectCase fmt selectCase =
  match flatten_case_v_opt selectCase with
  | Some ([ []; []; [] ], [ keysetExpressionList; name ]) ->
      F.fprintf fmt "%a: %a;" pp_keysetExpressionList keysetExpressionList
        pp_name name
  | _ -> error_unknown "pp_selectCase" selectCase

and pp_selectCaseList ?(level = 0) fmt selectCaseList =
  match selectCaseList.it with
  | ListV selectCases -> pp_list ~level pp_selectCase ~sep:Nl fmt selectCases
  | _ -> error_unexpected "pp_selectCaseList" "ListV" selectCaseList

(* Transition statements *)

and pp_stateExpression ?(level = 0) fmt stateExpression =
  match flatten_case_v_opt stateExpression with
  | Some ([ [ "NameE" ]; [] ], [ name ]) -> F.fprintf fmt "%a;" pp_name name
  | Some ([ [ "SelectE" ]; []; [] ], [ expressionList; selectCaseList ]) ->
      F.fprintf fmt "select (%a) {\n%a%s\n}" pp_expressionList expressionList
        (pp_selectCaseList ~level:(level + 1))
        selectCaseList (indent level)
  | _ -> error_unknown "pp_stateExpression" stateExpression

and pp_transitionStatement ?(level = 0) fmt transitionStatement =
  match flatten_case_v_opt transitionStatement with
  | Some ([ [ "TransS" ]; [] ], [ stateExpression ]) ->
      F.fprintf fmt "transition %a" (pp_stateExpression ~level) stateExpression
  | _ -> error_unknown "pp_transitionStatement" transitionStatement

and pp_transitionStatementOpt ?(level = 0) fmt transitionStatementOpt =
  match transitionStatementOpt.it with
  | OptV (Some transitionStatement) ->
      pp_transitionStatement ~level fmt transitionStatement
  | OptV None -> ()
  | _ ->
      error_unexpected "pp_transitionStatementOpt" "OptV" transitionStatementOpt

(* Value set declarations *)

and pp_valueSetType fmt valueSetType =
  match flatten_case_v_opt valueSetType with
  | Some ([ [ "BoolT" ] ], _)
  | Some ([ [ "ErrT" ] ], _)
  | Some ([ [ "MatchKindT" ] ], _)
  | Some ([ [ "StrT" ] ], _)
  | Some ([ [ "IntT" ] ], _)
  | Some ([ [ "FIntT" ]; [] ], _)
  | Some ([ [ "FBitT" ]; [] ], _)
  | Some ([ [ "VBitT" ]; [] ], _) ->
      pp_baseType fmt valueSetType
  | Some ([ [ "TupleT" ]; [] ], _) -> pp_tupleType fmt valueSetType
  | Some ([ [ "NameT" ]; [] ], _) -> pp_nameType fmt valueSetType
  | _ -> error_unknown "pp_valueSetType" valueSetType

and pp_valueSetDeclaration fmt valueSetDeclaration =
  match flatten_case_v_opt valueSetDeclaration with
  | Some ([ [ "ValueSetD" ]; []; []; [] ], [ valueSetType; expression; name ])
    ->
      F.fprintf fmt "value_set<%a>(%a) %a;" pp_valueSetType valueSetType
        pp_expression expression pp_name name
  | _ -> error_unknown "pp_valueSetDeclaration" valueSetDeclaration

(* Parser type declarations *)

and pp_parserTypeDeclaration fmt parserTypeDeclaration =
  match flatten_case_v_opt parserTypeDeclaration with
  | Some
      ( [ [ "ParserTypeD" ]; []; []; [] ],
        [ name; typeParameterList; parameterList ] ) ->
      F.fprintf fmt "parser %a%a%a;" pp_name name pp_typeParameterList
        typeParameterList pp_parameterList parameterList
  | _ -> error_unknown "pp_parserTypeDeclaration" parserTypeDeclaration

(* Parser declarations *)

and pp_parserBlockStatement ?(level = 0) fmt parserBlockStatement =
  match flatten_case_v_opt parserBlockStatement with
  | Some ([ [ "ParserBlockS" ]; [] ], [ parserStatementList ]) ->
      F.fprintf fmt "{\n%a\n%s}"
        (pp_parserStatementList ~level:(level + 1))
        parserStatementList (indent level)
  | _ -> error_unknown "pp_parserBlockStatement" parserBlockStatement

and pp_parserStatement ?(level = 0) fmt parserStatement =
  match flatten_case_v_opt parserStatement with
  | Some ([ [ "ConstD" ]; []; []; [] ], _) ->
      pp_constantDeclaration fmt parserStatement
  | Some ([ [ "VarD" ]; []; []; [] ], _) ->
      pp_variableDeclaration fmt parserStatement
  | Some ([ [ "EmptyS" ] ], _) -> pp_emptyStatement fmt parserStatement
  | Some ([ [ "AssignS" ]; []; [] ], _) ->
      pp_assignmentStatement fmt parserStatement
  | Some ([ [ "CallS" ]; []; []; [] ], _) ->
      pp_callStatement fmt parserStatement
  | Some ([ [ "InstS" ]; []; [] ], _) ->
      pp_directApplicationStatement fmt parserStatement
  | Some ([ [ "ParserBlockS" ]; [] ], _) ->
      pp_parserBlockStatement ~level fmt parserStatement
  | Some ([ [ "IfS" ]; []; []; [] ], _) ->
      pp_conditionalStatement ~level fmt parserStatement
  | _ -> error_unknown "pp_parserStatement" parserStatement

and pp_parserStatementList ?(level = 0) fmt parserStatementList =
  match parserStatementList.it with
  | ListV parserStatements ->
      pp_list ~level (pp_parserStatement ~level) ~sep:Nl fmt parserStatements
  | _ -> error_unexpected "pp_parserStatementList" "ListV" parserStatementList

and pp_parserState ?(level = 0) fmt parserState =
  match flatten_case_v_opt parserState with
  | Some
      ([ []; []; []; [] ], [ name; parserStatementList; transitionStatementOpt ])
    ->
      F.fprintf fmt "state %a {\n%a\n%s%a\n}" pp_name name
        (pp_parserStatementList ~level:(level + 1))
        parserStatementList
        (indent (level + 1))
        (pp_transitionStatementOpt ~level:(level + 1))
        transitionStatementOpt
  | _ -> error_unknown "pp_parserState" parserState

and pp_parserStateList ?(level = 0) fmt parserStateList =
  match parserStateList.it with
  | ListV parserStates ->
      pp_list ~level (pp_parserState ~level) ~sep:Nl fmt parserStates
  | _ -> error_unexpected "pp_parserStateList" "ListV" parserStateList

and pp_parserLocalDeclaration fmt parserLocalDeclaration =
  match flatten_case_v_opt parserLocalDeclaration with
  | Some ([ [ "ConstD" ]; []; []; [] ], _) ->
      pp_constantDeclaration fmt parserLocalDeclaration
  | Some ([ [ "InstD" ]; []; []; []; [] ], _) ->
      pp_instantiation fmt parserLocalDeclaration
  | Some ([ [ "VarD" ]; []; []; [] ], _) ->
      pp_variableDeclaration fmt parserLocalDeclaration
  | Some ([ [ "ValueSetD" ]; []; []; [] ], _) ->
      pp_valueSetDeclaration fmt parserLocalDeclaration
  | _ -> error_unknown "pp_parserLocalDeclaration" parserLocalDeclaration

and pp_parserLocalDeclarationList ?(level = 0) fmt parserLocalDeclarationList =
  match parserLocalDeclarationList.it with
  | ListV parserLocalDeclarations ->
      pp_list ~level pp_parserLocalDeclaration ~sep:Nl fmt
        parserLocalDeclarations
  | _ ->
      error_unexpected "pp_parserLocalDeclarationList" "ListV"
        parserLocalDeclarationList

and pp_parserDeclaration ?(level = 0) fmt parserDeclaration =
  match flatten_case_v_opt parserDeclaration with
  | Some
      ( [ [ "ParserD" ]; []; []; []; []; []; [] ],
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
  | _ -> error_unknown "pp_parserDeclaration" parserDeclaration

(* Table declarations *)

and pp_const fmt const =
  match flatten_case_v_opt const with
  | Some ([ [ "CONST" ] ], []) -> F.fprintf fmt "const"
  | _ -> error_unknown "pp_const" const

and pp_constOpt fmt constOpt =
  match constOpt.it with
  | OptV (Some const) -> F.fprintf fmt "%a " pp_const const
  | OptV None -> ()
  | _ -> error_unexpected "pp_constOpt" "OptV" constOpt

(* Table key property *)

and pp_tableKey fmt tableKey =
  match flatten_case_v_opt tableKey with
  | Some ([ []; []; [] ], [ expression; name ]) ->
      F.fprintf fmt "%a : %a;" pp_expression expression pp_name name
  | _ -> error_unknown "pp_tableKey" tableKey

and pp_tableKeyList fmt tableKeyList =
  match tableKeyList.it with
  | ListV tableKeys -> pp_list pp_tableKey ~sep:Space fmt tableKeys
  | _ -> error_unexpected "pp_tableKeyList" "ListV" tableKeyList

(* Table actions property *)

and pp_tableActionReference fmt tableActionReference =
  match flatten_case_v_opt tableActionReference with
  | Some ([ []; []; [] ], [ prefixedName; argumentList ]) ->
      F.fprintf fmt "%a%a;" pp_prefixedName prefixedName pp_argumentList
        argumentList
  | _ -> error_unknown "pp_tableActionReference" tableActionReference

and pp_tableAction fmt tableAction = pp_tableActionReference fmt tableAction

and pp_tableActionList fmt tableActionList =
  match tableActionList.it with
  | ListV tableActions -> pp_list pp_tableAction ~sep:Space fmt tableActions
  | _ -> error_unexpected "pp_tableActionList" "ListV" tableActionList

(* Table entry property *)

and pp_tableEntryPriority fmt tableEntryPriority =
  pp_expression fmt tableEntryPriority

and pp_tableEntryPriorityOpt fmt tableEntryPriorityOpt =
  match tableEntryPriorityOpt.it with
  | OptV (Some tableEntryPriority) ->
      F.fprintf fmt "priority = %a : " pp_tableEntryPriority tableEntryPriority
  | OptV None -> ()
  | _ ->
      error_unexpected "pp_tableEntryPriorityOpt" "OptV" tableEntryPriorityOpt

and pp_tableEntry fmt tableEntry =
  match flatten_case_v_opt tableEntry with
  | Some
      ( [ []; []; []; []; [] ],
        [
          constOpt;
          tableEntryPriorityOpt;
          keysetExpressionList;
          tableActionReference;
        ] ) ->
      F.fprintf fmt "%a%a%a : %a" pp_constOpt constOpt pp_tableEntryPriorityOpt
        tableEntryPriorityOpt pp_keysetExpressionList keysetExpressionList
        pp_tableActionReference tableActionReference
  | _ -> error_unknown "pp_tableEntry" tableEntry

and pp_tableEntryList ?(level = 0) fmt tableEntryList =
  match tableEntryList.it with
  | ListV tableEntries -> pp_list ~level pp_tableEntry ~sep:Nl fmt tableEntries
  | _ -> error_unexpected "pp_tableEntryList" "ListV" tableEntryList

(* Table properties *)

and pp_tableProperty ?(level = 0) fmt tableProperty =
  match flatten_case_v_opt tableProperty with
  | Some ([ [ "KeyP" ]; [] ], [ tableKeyList ]) ->
      F.fprintf fmt "key = { %a }" pp_tableKeyList tableKeyList
  | Some ([ [ "ActionP" ]; [] ], [ tableActionList ]) ->
      F.fprintf fmt "actions = { %a }" pp_tableActionList tableActionList
  | Some ([ [ "EntryP" ]; []; [] ], [ constOpt; tableEntryList ]) ->
      F.fprintf fmt "%aentries = {\n%a\n}" pp_constOpt constOpt
        (pp_tableEntryList ~level:(level + 1))
        tableEntryList
  | Some ([ [ "CustomP" ]; []; []; [] ], [ constOpt; name; initialValue ]) ->
      F.fprintf fmt "%a%a = %a;" pp_constOpt constOpt pp_name name
        pp_initialValue initialValue
  | _ -> error_unknown "pp_tableProperty" tableProperty

and pp_tablePropertyList ?(level = 0) fmt tablePropertyList =
  match tablePropertyList.it with
  | ListV tableProperties ->
      pp_list ~level pp_tableProperty ~sep:Nl fmt tableProperties
  | _ -> error_unexpected "pp_tablePropertyList" "ListV" tablePropertyList

and pp_tableDeclaration ?(level = 0) fmt tableDeclaration =
  match flatten_case_v_opt tableDeclaration with
  | Some ([ [ "TableD" ]; []; [] ], [ name; tablePropertyList ]) ->
      F.fprintf fmt "table %a {\n%a\n%s}" pp_name name
        (pp_tablePropertyList ~level:(level + 1))
        tablePropertyList (indent level)
  | _ -> error_unknown "pp_tableDeclaration" tableDeclaration

(* Control type declarations *)

and pp_controlTypeDeclaration fmt controlTypeDeclaration =
  match flatten_case_v_opt controlTypeDeclaration with
  | Some
      ( [ [ "ControlTypeD" ]; []; []; [] ],
        [ name; typeParameterList; parameterList ] ) ->
      F.fprintf fmt "control %a%a%a;" pp_name name pp_typeParameterList
        typeParameterList pp_parameterList parameterList
  | _ -> error_unknown "pp_controlTypeDeclaration" controlTypeDeclaration

(* Control declarations *)

and pp_controlBody ?(level = 0) fmt controlBody =
  pp_blockStatement ~level fmt controlBody

and pp_controlLocalDeclaration ?(level = 0) fmt controlLocalDeclaration =
  match flatten_case_v_opt controlLocalDeclaration with
  | Some ([ [ "ConstD" ]; []; []; [] ], _) ->
      pp_constantDeclaration fmt controlLocalDeclaration
  | Some ([ [ "InstD" ]; []; []; []; [] ], _) ->
      pp_instantiation ~level fmt controlLocalDeclaration
  | Some ([ [ "VarD" ]; []; []; [] ], _) ->
      pp_variableDeclaration fmt controlLocalDeclaration
  | Some ([ [ "ActionD" ]; []; []; [] ], _) ->
      pp_actionDeclaration ~level fmt controlLocalDeclaration
  | Some ([ [ "TableD" ]; []; [] ], _) ->
      pp_tableDeclaration ~level fmt controlLocalDeclaration
  | _ -> error_unknown "pp_controlLocalDeclaration" controlLocalDeclaration

and pp_controlLocalDeclarationList ?(level = 0) fmt controlLocalDeclarationList
    =
  match controlLocalDeclarationList.it with
  | ListV controlLocalDeclarations ->
      pp_list ~level
        (pp_controlLocalDeclaration ~level)
        ~sep:Nl fmt controlLocalDeclarations
  | _ ->
      error_unexpected "pp_controlLocalDeclarationList" "ListV"
        controlLocalDeclarationList

and pp_controlDeclaration ?(level = 0) fmt controlDeclaration =
  match flatten_case_v_opt controlDeclaration with
  | Some
      ( [ [ "ControlD" ]; []; []; []; []; []; [] ],
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
  | _ -> error_unknown "pp_controlDeclaration" controlDeclaration

(* Package type declarations *)

and pp_packageTypeDeclaration fmt packageTypeDeclaration =
  match flatten_case_v_opt packageTypeDeclaration with
  | Some
      ( [ [ "PackageTypeD" ]; []; []; [] ],
        [ name; typeParameterList; constructorParameterList ] ) ->
      F.fprintf fmt "package %a%a%a;" pp_name name pp_typeParameterList
        typeParameterList pp_constructorParameterList constructorParameterList
  | _ -> error_unknown "pp_packageTypeDeclaration" packageTypeDeclaration

(* Type declarations *)

and pp_typeDeclaration ?(level = 0) fmt typeDeclaration =
  match flatten_case_v_opt typeDeclaration with
  | Some ([ [ "EnumD" ]; []; [] ], _)
  | Some ([ [ "SEnumD" ]; []; []; [] ], _)
  | Some ([ [ "StructD" ]; []; []; [] ], _)
  | Some ([ [ "HeaderD" ]; []; []; [] ], _)
  | Some ([ [ "HeaderUnionD" ]; []; []; [] ], _) ->
      pp_derivedTypeDeclaration fmt typeDeclaration
  | Some ([ [ "TypeDefD" ]; []; [] ], _) | Some ([ [ "NewTypeD" ]; []; [] ], _)
    ->
      pp_typedefDeclaration ~level fmt typeDeclaration
  | Some ([ [ "ParserTypeD" ]; []; []; [] ], _) ->
      pp_parserTypeDeclaration fmt typeDeclaration
  | Some ([ [ "ControlTypeD" ]; []; []; [] ], _) ->
      pp_controlTypeDeclaration fmt typeDeclaration
  | Some ([ [ "PackageTypeD" ]; []; []; [] ], _) ->
      pp_packageTypeDeclaration fmt typeDeclaration
  | _ -> error_unknown "pp_typeDeclaration" typeDeclaration

(* Declarations *)

and pp_declaration ?(level = 0) fmt declaration =
  match flatten_case_v_opt declaration with
  | Some ([ [ "ConstD" ]; []; []; [] ], _) ->
      pp_constantDeclaration fmt declaration
  | Some ([ [ "InstD" ]; []; []; []; [] ], _) ->
      pp_instantiation ~level fmt declaration
  | Some ([ [ "FuncD" ]; []; []; []; []; [] ], _) ->
      pp_functionDeclaration ~level fmt declaration
  | Some ([ [ "ActionD" ]; []; []; [] ], _) ->
      pp_actionDeclaration ~level fmt declaration
  | Some ([ [ "ErrD" ]; [] ], _) -> pp_errorDeclaration fmt declaration
  | Some ([ [ "MatchKindD" ]; [] ], _) ->
      pp_matchKindDeclaration fmt declaration
  | Some ([ [ "ExternFuncD" ]; []; []; []; [] ], _)
  | Some ([ [ "ExternObjectD" ]; []; []; [] ], _) ->
      pp_externDeclaration ~level fmt declaration
  | Some ([ [ "ParserD" ]; []; []; []; []; []; [] ], _) ->
      pp_parserDeclaration ~level fmt declaration
  | Some ([ [ "ControlD" ]; []; []; []; []; []; [] ], _) ->
      pp_controlDeclaration ~level fmt declaration
  | Some ([ [ "EnumD" ]; []; [] ], _)
  | Some ([ [ "SEnumD" ]; []; []; [] ], _)
  | Some ([ [ "StructD" ]; []; []; [] ], _)
  | Some ([ [ "HeaderD" ]; []; []; [] ], _)
  | Some ([ [ "HeaderUnionD" ]; []; []; [] ], _)
  | Some ([ [ "TypeDefD" ]; []; [] ], _)
  | Some ([ [ "NewTypeD" ]; []; [] ], _)
  | Some ([ [ "ParserTypeD" ]; []; []; [] ], _)
  | Some ([ [ "ControlTypeD" ]; []; []; [] ], _)
  | Some ([ [ "PackageTypeD" ]; []; []; [] ], _) ->
      pp_typeDeclaration ~level fmt declaration
  | _ -> error_unknown "pp_declaration" declaration

and pp_declarationList ?(level = 0) fmt declarationList =
  match declarationList.it with
  | ListV declarations ->
      pp_list ~level (pp_declaration ~level) ~sep:Nl fmt declarations
  | _ -> error_unexpected "pp_declarationList" "ListV" declarationList

(* Program *)

let pp_program fmt program = pp_declarationList ~level:0 fmt program
