open Il.Ast
open Xl.Atom
open Util.Source

let wrap_atom (s : string) : atom =
  match s with
  | "<:" -> Sub $ no_region
  | ":>" -> Sup $ no_region
  | "|-" -> Turnstile $ no_region
  | "-|" -> Tilesturn $ no_region
  | "->" -> Arrow $ no_region
  | "->_" -> ArrowSub $ no_region
  | "~>" -> SqArrow $ no_region
  | "~>*" -> SqArrowStar $ no_region
  | "." -> Dot $ no_region
  | ".." -> Dot2 $ no_region
  | "..." -> Dot3 $ no_region
  | "," -> Comma $ no_region
  | ";" -> Semicolon $ no_region
  | ":" -> Colon $ no_region
  | "?" -> Quest $ no_region
  | "<" -> Less $ no_region
  | "<<" -> Prec $ no_region
  | "<=" -> LessEqual $ no_region
  | ">" -> Greater $ no_region
  | ">>" -> Succ $ no_region
  | ">=" -> GreaterEqual $ no_region
  | "(" -> LParen $ no_region
  | ")" -> RParen $ no_region
  | "[" -> LBrack $ no_region
  | "]" -> RBrack $ no_region
  | "{" -> LBrace $ no_region
  | "}" -> RBrace $ no_region
  | "++" -> Cat $ no_region
  | "*" -> Star $ no_region
  | "\\" -> Backslash $ no_region
  | "=" -> Equal $ no_region
  | "==" -> Equiv $ no_region
  | "|" -> Bar $ no_region
  | _ -> Atom s $ no_region

let wrap_var_t (s : string) : typ' = VarT (s $ no_region, [])
let wrap_iter_t (i : iter) (t : typ') : typ' = IterT (t $ no_region, i)

let with_fresh_val (typ : typ') : vnote =
  let vid = Runtime_dynamic.Value.fresh () in
  { vid; typ }

let with_typ (typ : typ') (v : value') : value = v $$$ with_fresh_val typ

type symbol = NT of value | Term of string

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

let wrap_opt_v (s : string) (v : value option) : value =
  OptV v |> with_typ (wrap_iter_t Opt (wrap_var_t s))

let wrap_list_v (s : string) (vs : value list) : value =
  ListV vs |> with_typ (wrap_iter_t List (wrap_var_t s))

let ( #@ ) (vs : symbol list) (s : string) : value =
  vs |> wrap_case_v |> with_typ (wrap_var_t s)

let ( #@@ ) (v : value) (s : string) : value =
  { v with note = { v.note with typ = wrap_var_t s } }
