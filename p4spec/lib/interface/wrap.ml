open Domain.Atom
open Lang
open Il
open Util.Source

(* Atom generator *)

let wrap_atom (s : string) : atom =
  match s with
  | "<:" -> Sub $ no_region
  | ":>" -> Sup $ no_region
  | "|-" -> Turnstile $ no_region
  | "-|" -> Tilesturn $ no_region
  | "`" -> Tick $ no_region
  | "\"" -> DoubleQuote $ no_region
  | "_" -> Underscore $ no_region
  | "->" -> Arrow $ no_region
  | "->_" -> ArrowSub $ no_region
  | "=>" -> DoubleArrow $ no_region
  | "=>_" -> DoubleArrowSub $ no_region
  | "~>" -> SqArrow $ no_region
  | "~>*" -> SqArrowStar $ no_region
  | "." -> Dot $ no_region
  | ".." -> Dot2 $ no_region
  | "..." -> Dot3 $ no_region
  | "," -> Comma $ no_region
  | ";" -> Semicolon $ no_region
  | ":" -> Colon $ no_region
  | "#" -> Hash $ no_region
  | "$" -> Dollar $ no_region
  | "@" -> At $ no_region
  | "?" -> Quest $ no_region
  | "!" -> Bang $ no_region
  | "!=" -> BangEq $ no_region
  | "~" -> Tilde $ no_region
  | "~~" -> Tilde2 $ no_region
  | "<" -> LAngle $ no_region
  | "<<" -> LAngle2 $ no_region
  | "<=" -> LAngleEq $ no_region
  | "<<=" -> LAngle2Eq $ no_region
  | ">" -> RAngle $ no_region
  | ">>" -> RAngle2 $ no_region
  | ">=" -> RAngleEq $ no_region
  | ">>=" -> RAngle2Eq $ no_region
  | "(" -> LParen $ no_region
  | ")" -> RParen $ no_region
  | "[" -> LBrack $ no_region
  | "]" -> RBrack $ no_region
  | "{" -> LBrace $ no_region
  | "{#}" -> LBraceHashRBrace $ no_region
  | "}" -> RBrace $ no_region
  | "+" -> Plus $ no_region
  | "++" -> Plus2 $ no_region
  | "+=" -> PlusEq $ no_region
  | "-" -> Minus $ no_region
  | "-=" -> MinusEq $ no_region
  | "*" -> Star $ no_region
  | "*=" -> StarEq $ no_region
  | "/" -> Slash $ no_region
  | "/=" -> SlashEq $ no_region
  | "\\" -> Backslash $ no_region
  | "%" -> Percent $ no_region
  | "%=" -> PercentEq $ no_region
  | "=" -> Eq $ no_region
  | "==" -> Eq2 $ no_region
  | "&" -> Amp $ no_region
  | "&&" -> Amp2 $ no_region
  | "&&&" -> Amp3 $ no_region
  | "&=" -> AmpEq $ no_region
  | "^" -> Up $ no_region
  | "^=" -> UpEq $ no_region
  | "|" -> Bar $ no_region
  | "||" -> Bar2 $ no_region
  | "|=" -> BarEq $ no_region
  | "|+|" -> SPlus $ no_region
  | "|+|=" -> SPlusEq $ no_region
  | "|-|" -> SMinus $ no_region
  | "|-|=" -> SMinusEq $ no_region
  | s when String.get s 0 = '`' ->
      let s = String.sub s 1 (String.length s - 1) in
      SilentAtom s $ no_region
  | _ -> Atom s $ no_region

(* Type generators *)

let wrap_var_t (s : string) : typ' = VarT (s $ no_region, [])
let wrap_iter_t (i : iter) (t : typ') : typ' = IterT (t $ no_region, i)

(* Value note generators *)

let with_typ (typ : typ') (v : value') : value =
  Runtime.Dynamic_Il.Value.make typ v

(* Value generators *)

type symbol = NT of value | Term of string

let wrap_bool_v (b : bool) : value = BoolV b |> with_typ BoolT

let wrap_num_v_nat (n : Bigint.t) : value =
  NumV (`Nat n) |> with_typ (NumT `NatT)

let wrap_num_v_int (i : Bigint.t) : value =
  NumV (`Int i) |> with_typ (NumT `IntT)

let wrap_text_v (s : string) : value = TextV s |> with_typ TextT

let wrap_case_v (vs : symbol list) : value' =
  let rec build_mixop acc_mixop acc_terms = function
    | [] ->
        (* Always add the final group, even if empty *)
        acc_mixop @ [ acc_terms ]
    | Term s :: rest ->
        (* Accumulate terms *)
        build_mixop acc_mixop (acc_terms @ [ wrap_atom s ]) rest
    | NT _ :: rest ->
        (* When we hit a non-terminal, add accumulated terms to mixop and start new group *)
        let new_mixop = acc_mixop @ [ acc_terms ] in
        build_mixop new_mixop [] rest
  in
  let mixop = build_mixop [] [] vs in
  let values =
    vs
    |> List.filter (fun v -> match v with NT _ -> true | _ -> false)
    |> List.map (function NT v -> v | Term _ -> assert false)
  in
  CaseV (mixop, values)

let wrap_tuple_v (s : string) (vs : value list) : value =
  TupleV vs |> with_typ (wrap_var_t s)

let wrap_opt_v_typed (t : typ') (v : value option) : value =
  OptV v |> with_typ (wrap_iter_t Opt t)

let wrap_opt_v (s : string) (v : value option) : value =
  OptV v |> with_typ (wrap_iter_t Opt (wrap_var_t s))

let wrap_list_v (s : string) (vs : value list) : value =
  ListV vs |> with_typ (wrap_iter_t List (wrap_var_t s))

let wrap_list_v_typed (t : typ') (vs : value list) : value =
  ListV vs |> with_typ (wrap_iter_t List t)

let wrap_extern_v (s : string) (json : Yojson.Safe.t) : value =
  ExternV json |> with_typ (wrap_var_t s)

let ( #@ ) (vs : symbol list) (s : string) : value =
  vs |> wrap_case_v |> with_typ (wrap_var_t s)

let ( #@@ ) (v : value) (s : string) : value =
  { v with note = { v.note with typ = wrap_var_t s } }
