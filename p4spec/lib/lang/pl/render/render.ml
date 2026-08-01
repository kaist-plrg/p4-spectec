open Domain
open Lib
open Xl
open Ast
open Util.Source
module F = Format
module Backtrack = Backtrack
open Utils

(* Render utils *)

(* Oxford-comma join over inline docs *)

let prose_of_list (items : Adoc.prose list) : Adoc.prose =
  match items with
  | [] -> Adoc.empty_prose
  | [ item ] -> item
  | [ item_a; item_b ] -> Adoc.(item_a ++ text " and " ++ item_b)
  | _ ->
      let items_rev = List.rev items in
      let items, item_last =
        (items_rev |> List.tl |> List.rev, items_rev |> List.hd)
      in
      Adoc.(
        seq_prose
          (List.mapi (fun i x -> if i = 0 then x else text ", " ++ x) items)
        ++ text ", and " ++ item_last)

(* Alternation *)

let alternate ?(caps = false) (hint : Hints.Alter.t)
    (base_text : string -> string) (render : 'a -> Adoc.prose) (items : 'a list)
    : Adoc.prose =
  let prose_alternated =
    Hints.Alter.alternate ~empty:Adoc.empty_prose
      ~text:(fun s ->
        match s with "" -> None | s -> Some (Adoc.text (base_text s)))
      ~atom:(fun (atom : atom) ->
        Adoc.code_prose (Adoc.token ("+" ^ Atom.string_of_atom atom.it ^ "+")))
      ~join:(fun (docs : Adoc.prose list) ->
        Adoc.seq_prose
          (List.mapi
             (fun i d -> if i = 0 then d else Adoc.(text " " ++ d))
             docs))
      ~fuse:(fun (a : Adoc.prose) (b : Adoc.prose) -> Adoc.(a ++ b))
      ~other:(fun (hintexp : El.exp) ->
        Adoc.text (El.Print.string_of_exp hintexp))
      hint render items
  in
  if caps then Adoc.capitalize_first prose_alternated else prose_alternated

(* Mixfix *)

let code_of_mixfix ~(atom : atom -> string) (mixop : Mixop.t)
    (args : Adoc.code list) : Adoc.code =
  Mixfix.assemble ~empty:Adoc.empty_code
    ~atom:(fun a -> match atom a with "" -> None | s -> Some (Adoc.token s))
    ~space:(Adoc.token " ") ~concat:Adoc.( ^^ ) (Mixfix.fill mixop args)

(* Numbers *)

let string_of_num (num : num) : string = Il.Print.string_of_num num

(* Texts *)

let string_of_text (text : text) : string = Il.Print.string_of_text text

(* Identifiers *)

let string_of_varid (varid : id) : string = Il.Print.string_of_varid varid
let string_of_relid (relid : id) : string = Il.Print.string_of_relid relid

let string_of_defid ?(link = false) (defid : id) : string =
  if link then Il.Print.string_of_varid defid
  else Il.Print.string_of_defid defid

let code_of_varid (id_var : id) : Adoc.code =
  if Id.is_underscored id_var then Adoc.token "++_++"
  else
    match String.split_on_char '_' id_var.it with
    | [] -> assert false
    | [ var_type ] -> Adoc.token var_type
    | var_type :: var_subscripts ->
        Adoc.token
          (var_type ^ (var_subscripts |> String.concat "_" |> adoc_subscript))

let string_of_rulegroupid (id_rel : string) (id_rulegroup : string) : string =
  let sanitize s = String.map (fun c -> if c = '/' then '-' else c) s in
  sanitize id_rel ^ "-" ^ sanitize id_rulegroup

(* Atoms *)

let string_of_atom (atom : atom) : string =
  match atom.it with
  | Atom.Tag _ -> ""
  | _ -> "+" ^ Atom.string_of_atom atom.it ^ "+"

let code_of_atom (atom : atom) : Adoc.code =
  atom |> string_of_atom |> Adoc.token

(* Mixfix operators *)

let code_of_mixop (mixop : mixop) : Adoc.code =
  let arity = Mixop.arity mixop in
  let placeholders = List.init arity (fun _ -> "%") in
  placeholders
  |> Mixop.assemble ~string_of_atom mixop
  |> String.trim |> Adoc.token

(* Iterators *)

let code_of_iter (iter : iter) : Adoc.code =
  match iter with
  | List -> "{asterisk}" |> adoc_superscript |> Adoc.token
  | Opt -> "?" |> adoc_superscript |> Adoc.token

let string_of_iter (iter : iter) : string =
  match iter with List -> "list" | Opt -> "option"

let code_of_iterexp (iterexp : iterexp) : Adoc.code =
  let iter, _ = iterexp in
  code_of_iter iter

(* Variables *)

let code_of_var (var : var) : Adoc.code =
  let id, _, iters = var in
  if Id.is_underscored id then Adoc.token "++_++"
  else Adoc.(code_of_varid id ^^ seq_code (List.map code_of_iter iters))

let prose_of_in_itervar (iter : iter) (var : var) : Adoc.prose =
  Adoc.(
    code_prose (code_of_var var)
    ++ text " in "
    ++ code_prose (code_of_var var ^^ code_of_iter iter))

let prose_of_in_itervars (iter : iter) (vars : var list) : Adoc.prose =
  vars |> List.map (prose_of_in_itervar iter) |> prose_of_list

let prose_of_out_itervars (iter : iter) (vars : var list) : Adoc.prose =
  vars
  |> List.filter_map (fun var ->
         let id, _, _ = var in
         if Id.is_underscored id then None
         else Some Adoc.(code_prose (code_of_var var ^^ code_of_iter iter)))
  |> prose_of_list

(* Types *)

let string_of_typ (typ : typ) : string = Sl.Print.string_of_typ typ
let code_of_typ (typ : typ) : Adoc.code = typ |> string_of_typ |> Adoc.token

let tid_of_typ (typ' : typ') : id option =
  match typ' with Il.VarT (id, _) -> Some id | _ -> None

(* Operators *)

let string_of_unop (unop : unop) : string = Sl.Print.string_of_unop unop

let string_of_binop (binop : binop) : string =
  match binop with
  | `AndOp -> "and"
  | `OrOp -> "or"
  | `ImplOp -> "implies"
  | `EquivOp -> "is equivalent to"
  | _ -> Sl.Print.string_of_binop binop

let string_of_cmpop (cmpop : cmpop) : string =
  match cmpop with
  | `EqOp -> "is equal to"
  | `NeOp -> "is not equal to"
  | `LtOp -> "is less than"
  | `GtOp -> "is greater than"
  | `LeOp -> "is less than or equal to"
  | `GeOp -> "is greater than or equal to"

(* Expressions, as code *)

let rec code_of_exp (exp : exp) : Adoc.code =
  match exp.node.it with
  | BoolE b -> code_of_bool_exp b
  | NumE n -> code_of_num_exp n
  | TextE text -> code_of_text_exp text
  | VarE id -> code_of_var_exp id
  | UnE (unop, _, exp) -> code_of_un_exp unop exp
  | BinE (binop, _, exp_l, exp_r) -> code_of_bin_exp binop exp_l exp_r
  | CmpE (cmpop, _, exp_l, exp_r) -> code_of_cmp_exp cmpop exp_l exp_r
  | UpCastE (_, exp) -> code_of_upcast_exp exp
  | DownCastE (_, exp) -> code_of_downcast_exp exp
  | SubE (exp, typ) -> code_of_sub_exp exp typ
  | MatchE (exp, pattern) -> code_of_match_exp exp pattern
  | TupleE exps -> code_of_tuple_exp exps
  | CaseE notexp -> code_of_case_exp notexp
  | StrE expfields -> code_of_str_exp expfields
  | OptE exp_opt -> code_of_opt_exp exp_opt
  | ListE exps -> code_of_list_exp exps
  | ConsE (exp_h, exp_t) -> code_of_cons_exp exp_h exp_t
  | CatE (exp_l, exp_r) -> code_of_cat_exp exp_l exp_r
  | MemE (exp_e, exp_s) -> code_of_mem_exp exp_e exp_s
  | LenE exp -> code_of_len_exp exp
  | DotE (exp_b, atom) -> code_of_dot_exp exp_b atom
  | IdxE (exp_b, exp_i) -> code_of_idx_exp exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> code_of_slice_exp exp_b exp_l exp_h
  | UpdE (exp_b, path, exp_f) -> code_of_upd_exp exp_b path exp_f
  | CallE (id, targs, args) -> code_of_call_exp id targs args
  | IterE (exp, iterexp) -> code_of_iter_exp exp iterexp

and code_of_exps ?(sep : string = ", ") (exps : exp list) : Adoc.code =
  Adoc.seq_code
    (List.mapi
       (fun i exp ->
         if i = 0 then code_of_exp exp else Adoc.(token sep ^^ code_of_exp exp))
       exps)

and code_of_notexp (notexp : notexp) : Adoc.code =
  let mixop, exps = Mixfix.split notexp in
  code_of_mixfix ~atom:string_of_atom mixop (List.map code_of_exp exps)

(* Boolean expression, as code *)

and code_of_bool_exp (b : bool) : Adoc.code = Adoc.token (string_of_bool b)

(* Numeric expression, as code *)

and code_of_num_exp (n : num) : Adoc.code = Adoc.token (string_of_num n)

(* Text expression, as code *)

and code_of_text_exp (text : text) : Adoc.code =
  Adoc.token ("\"" ^ String.escaped text ^ "\"")

(* Variable expression, as code *)

and code_of_var_exp (id : id) : Adoc.code = code_of_varid id

(* Unary expression, as code *)

and code_of_un_exp (unop : unop) (exp : exp) : Adoc.code =
  Adoc.(token (string_of_unop unop) ^^ code_of_exp exp)

(* Binary expression, as code *)

and code_of_bin_exp (binop : binop) (exp_l : exp) (exp_r : exp) : Adoc.code =
  Adoc.(
    code_of_exp exp_l
    ^^ token (" " ^ Sl.Print.string_of_binop binop ^ " ")
    ^^ code_of_exp exp_r)

(* Comparison expression, as code *)

and code_of_cmp_exp (cmpop : cmpop) (exp_l : exp) (exp_r : exp) : Adoc.code =
  Adoc.(
    code_of_exp exp_l
    ^^ token (" " ^ Sl.Print.string_of_cmpop cmpop ^ " ")
    ^^ code_of_exp exp_r)

(* Upcast expression, as code *)

and code_of_upcast_exp (exp : exp) : Adoc.code = code_of_exp exp

(* Downcast expression, as code *)

and code_of_downcast_exp (exp : exp) : Adoc.code = code_of_exp exp

(* Subtype check expression, as code *)

and code_of_sub_exp (exp : exp) (typ : typ) : Adoc.code =
  Adoc.(code_of_exp exp ^^ token " has type " ^^ code_of_typ typ)

(* Pattern match check expression, as code *)

and code_of_match_exp (exp : exp) (pattern : pattern) : Adoc.code =
  let code_scrut = code_of_exp exp in
  match pattern with
  | Il.CaseP mixop when Mixop.arity mixop = 0 ->
      Adoc.(code_scrut ^^ token " is " ^^ code_of_pattern (Il.CaseP mixop))
  | Il.ListP `Nil -> Adoc.(code_scrut ^^ token " is an empty list")
  | Il.ListP `Cons -> Adoc.(code_scrut ^^ token " is a non-empty list")
  | Il.ListP (`Fixed len) ->
      Adoc.(code_scrut ^^ token (F.asprintf " is a list of length %d" len))
  | Il.OptP `None -> Adoc.(code_scrut ^^ token " is none")
  | Il.OptP `Some -> Adoc.(code_scrut ^^ token " is defined")
  | pattern ->
      Adoc.(code_scrut ^^ token " matches pattern " ^^ code_of_pattern pattern)

(* Tuple expression, as code *)

and code_of_tuple_exp (exps : exp list) : Adoc.code =
  Adoc.(token "( " ^^ code_of_exps ~sep:", " exps ^^ token " )")

(* Case expression, as code *)

and code_of_case_exp (notexp : notexp) : Adoc.code = code_of_notexp notexp

(* Struct expression, as code *)

and code_of_str_exp (expfields : (atom * exp) list) : Adoc.code =
  Adoc.(
    token "+{+"
    ^^ seq_code
         (List.mapi
            (fun i (atom, exp_f) ->
              let code_field =
                code_of_atom atom ^^ token " " ^^ code_of_exp exp_f
              in
              if i = 0 then code_field else token ", " ^^ code_field)
            expfields)
    ^^ token "+}+")

(* Option expression, as code *)

and code_of_opt_exp (exp_opt : exp option) : Adoc.code =
  match exp_opt with None -> Adoc.token "·" | Some exp -> code_of_exp exp

(* List expression, as code *)

and code_of_list_exp (exps : exp list) : Adoc.code =
  match exps with
  | [] -> Adoc.token "·"
  | [ exp ] -> code_of_exp exp
  | exps -> Adoc.(token "+[+ " ^^ code_of_exps ~sep:", " exps ^^ token " +]+")

(* Cons expression, as code *)

and code_of_cons_exp (exp_h : exp) (exp_t : exp) : Adoc.code =
  Adoc.(code_of_exp exp_h ^^ token " {two-colons} " ^^ code_of_exp exp_t)

(* Concatenation expression, as code *)

and code_of_cat_exp (exp_l : exp) (exp_r : exp) : Adoc.code =
  Adoc.(code_of_exp exp_l ^^ token " {pp} " ^^ code_of_exp exp_r)

(* Membership expression, as code *)

and code_of_mem_exp (exp_e : exp) (exp_s : exp) : Adoc.code =
  Adoc.(code_of_exp exp_e ^^ token " is in " ^^ code_of_exp exp_s)

(* Length expression, as code *)

and code_of_len_exp (exp : exp) : Adoc.code =
  Adoc.(token "the length of " ^^ code_of_exp exp)

(* Dot expression, as code *)

and code_of_dot_exp (exp_b : exp) (atom : atom) : Adoc.code =
  Adoc.(code_of_exp exp_b ^^ token "." ^^ code_of_atom atom)

(* Index expression, as code *)

and code_of_idx_exp (exp_b : exp) (exp_i : exp) : Adoc.code =
  Adoc.(code_of_exp exp_b ^^ token "[" ^^ code_of_exp exp_i ^^ token "]")

(* Slice expression, as code *)

and code_of_slice_exp (exp_b : exp) (exp_l : exp) (exp_h : exp) : Adoc.code =
  Adoc.(
    code_of_exp exp_b ^^ token "[" ^^ code_of_exp exp_l ^^ token " : "
    ^^ code_of_exp exp_h ^^ token "]")

(* Update expression, as code *)

and code_of_upd_exp (exp_b : exp) (path : path) (exp_f : exp) : Adoc.code =
  Adoc.(
    code_of_exp exp_b ^^ token "[" ^^ code_of_path path ^^ token " = "
    ^^ code_of_exp exp_f ^^ token "]")

(* Function call expression, as code *)

and code_of_call_exp (id : id) (targs : targ list) (args : arg list) : Adoc.code
    =
  Adoc.link_code ~target:id.it
    Adoc.(
      token (string_of_defid id)
      ^^ token (string_of_targs targs)
      ^^ code_of_args args)

(* Iterated expression, as code *)

and code_of_iter_exp (exp_inner : exp) (iterexp : iterexp) : Adoc.code =
  match (exp_inner.node.it, iterexp) with
  | _, (_, []) -> code_of_exp exp_inner
  | (VarE _ | TupleE _), _ ->
      Adoc.(code_of_exp exp_inner ^^ code_of_iterexp iterexp)
  | _ ->
      let code_inner = code_of_exp exp_inner in
      let sexp = Adoc.ser_code code_inner in
      if String.contains sexp ' ' then
        Adoc.(token "( " ^^ code_inner ^^ token " )" ^^ code_of_iterexp iterexp)
      else Adoc.(code_inner ^^ code_of_iterexp iterexp)

(* Expressions, as prose *)

and prose_of_exp (exp : exp) : Adoc.prose =
  match exp.node.it with
  | BoolE b -> prose_of_bool_exp b
  | NumE n -> prose_of_num_exp n
  | TextE text -> prose_of_text_exp text
  | VarE id -> prose_of_var_exp id
  | UnE (unop, _, exp) -> prose_of_un_exp unop exp
  | BinE (binop, _, exp_l, exp_r) -> prose_of_bin_exp binop exp_l exp_r
  | CmpE (cmpop, _, exp_l, exp_r) -> prose_of_cmp_exp cmpop exp_l exp_r
  | UpCastE (_, exp) -> prose_of_upcast_exp exp
  | DownCastE (_, exp) -> prose_of_downcast_exp exp
  | SubE (exp, typ) -> prose_of_sub_exp exp typ
  | MatchE (exp, pattern) -> prose_of_match_exp exp pattern
  | TupleE exps -> prose_of_tuple_exp exps
  | CaseE notexp -> prose_of_case_exp exp notexp
  | StrE expfields -> prose_of_str_exp expfields
  | OptE exp_opt -> prose_of_opt_exp exp_opt
  | ListE exps -> prose_of_list_exp exps
  | ConsE (exp_h, exp_t) -> prose_of_cons_exp exp_h exp_t
  | CatE (exp_l, exp_r) -> prose_of_cat_exp exp_l exp_r
  | MemE (exp_e, exp_s) -> prose_of_mem_exp exp_e exp_s
  | LenE exp -> prose_of_len_exp exp
  | DotE (exp_b, atom) -> prose_of_dot_exp exp_b atom
  | IdxE (exp_b, exp_i) -> prose_of_idx_exp exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> prose_of_slice_exp exp_b exp_l exp_h
  | UpdE (exp_b, path, exp_f) -> prose_of_upd_exp exp_b path exp_f
  | CallE (id, _targs, args) -> prose_of_call_exp exp id args
  | IterE (exp, iterexp) -> prose_of_iter_exp exp iterexp

and prose_of_exps ?(sep : string option) (exps : exp list) : Adoc.prose =
  match sep with
  | Some sep ->
      Adoc.seq_prose
        (List.mapi
           (fun i exp ->
             if i = 0 then prose_of_exp exp
             else Adoc.(text sep ++ prose_of_exp exp))
           exps)
  | None -> prose_of_list (List.map prose_of_exp exps)

(* Boolean expression, as prose *)

and prose_of_bool_exp (b : bool) : Adoc.prose =
  Adoc.code_prose (code_of_bool_exp b)

(* Numeric expression, as prose *)

and prose_of_num_exp (n : num) : Adoc.prose =
  Adoc.code_prose (code_of_num_exp n)

(* Text expression, as prose *)

and prose_of_text_exp (text : text) : Adoc.prose =
  Adoc.code_prose (code_of_text_exp text)

(* Variable expression, as prose *)

and prose_of_var_exp (id : id) : Adoc.prose =
  Adoc.code_prose (code_of_var_exp id)

(* Unary expression, as prose *)

and prose_of_negated_exp_opt (exp : exp) : Adoc.prose option =
  match exp.node.it with
  | MatchE (exp_e, pattern) ->
      Some
        Adoc.(
          prose_of_exp exp_e
          ++ text " does not match pattern "
          ++ code_prose (code_of_pattern pattern))
  | SubE (exp_e, typ) ->
      Some
        Adoc.(
          code_prose (code_of_exp exp_e)
          ++ text " does not have type "
          ++ code_prose (code_of_typ typ))
  | MemE (exp_e, exp_s) ->
      Some
        Adoc.(
          code_prose (code_of_exp exp_e)
          ++ text " is not in "
          ++ code_prose (code_of_exp exp_s))
  | CallE (id, _targs, args) -> (
      match exp.hints.prose_false with
      | Some hints ->
          Some
            (Adoc.link_prose ~target:id.it
               (alternate hints (reindent_lines ~level:0) prose_of_arg args))
      | None ->
          Some
            (Adoc.code_prose
               Adoc.(token (string_of_unop `NotOp) ^^ code_of_exp exp)))
  | _ -> None

and prose_of_un_exp (unop : unop) (exp : exp) : Adoc.prose =
  match unop with
  | #Bool.unop -> (
      match prose_of_negated_exp_opt exp with
      | Some p -> p
      | None -> Adoc.code_prose (code_of_un_exp unop exp))
  | _ -> Adoc.code_prose (code_of_un_exp unop exp)

(* Binary expression, as prose *)

and prose_of_bin_exp (binop : binop) (exp_l : exp) (exp_r : exp) : Adoc.prose =
  match binop with
  | `ImplOp ->
      Adoc.(
        text "if " ++ prose_of_exp exp_l ++ text ", then " ++ prose_of_exp exp_r)
  | #Bool.binop as binop ->
      Adoc.(
        prose_of_exp exp_l
        ++ text (" " ^ string_of_binop binop ^ " ")
        ++ prose_of_exp exp_r)
  | #Num.binop -> Adoc.code_prose (code_of_bin_exp binop exp_l exp_r)

(* Comparison expression, as prose *)

and prose_of_cmp_exp (cmpop : cmpop) (exp_l : exp) (exp_r : exp) : Adoc.prose =
  Adoc.(
    prose_of_exp exp_l
    ++ text (" " ^ string_of_cmpop cmpop ^ " ")
    ++ prose_of_exp exp_r)

(* Upcast expression, as prose *)

and prose_of_upcast_exp (exp : exp) : Adoc.prose =
  Adoc.code_prose (code_of_upcast_exp exp)

(* Downcast expression, as prose *)

and prose_of_downcast_exp (exp : exp) : Adoc.prose =
  Adoc.code_prose (code_of_downcast_exp exp)

(* Subtype check expression, as prose *)

and prose_of_sub_exp (exp : exp) (typ : typ) : Adoc.prose =
  Adoc.(
    code_prose (code_of_exp exp)
    ++ text " has type "
    ++ code_prose (code_of_typ typ))

(* Pattern match check expression, as prose *)

and prose_of_match_exp (exp : exp) (pattern : pattern) : Adoc.prose =
  let prose_scrut = prose_of_exp exp in
  let pat p = Adoc.code_prose (code_of_pattern p) in
  match pattern with
  | Il.CaseP mixop when Mixop.arity mixop = 0 ->
      Adoc.(prose_scrut ++ text " is " ++ pat (Il.CaseP mixop))
  | Il.ListP `Nil -> Adoc.(prose_scrut ++ text " is an empty list")
  | Il.ListP `Cons -> Adoc.(prose_scrut ++ text " is a non-empty list")
  | Il.ListP (`Fixed len) ->
      Adoc.(prose_scrut ++ text (F.asprintf " is a list of length %d" len))
  | Il.OptP `None -> Adoc.(prose_scrut ++ text " is none")
  | Il.OptP `Some -> Adoc.(prose_scrut ++ text " is defined")
  | pattern -> Adoc.(prose_scrut ++ text " matches pattern " ++ pat pattern)

(* Tuple expression, as prose *)

and prose_of_tuple_exp (exps : exp list) : Adoc.prose =
  Adoc.(text "( " ++ prose_of_exps ~sep:", " exps ++ text " )")

(* Case expression, as prose *)

and prose_of_case_exp (exp : exp) (notexp : notexp) : Adoc.prose =
  let hint_opt = exp.hints.prose in
  let link_opt = tid_of_typ exp.node.note in
  match (hint_opt, link_opt) with
  | Some hints, Some tid ->
      Adoc.link_prose ~target:tid.it
        (alternate hints (reindent_lines ~level:0) prose_of_exp
           (Mixfix.args notexp))
  | _ -> Adoc.code_prose (code_of_notexp notexp)

(* Struct expression, as prose *)

and prose_of_str_exp (expfields : (atom * exp) list) : Adoc.prose =
  Adoc.(
    text "+{+"
    ++ seq_prose
         (List.mapi
            (fun i (atom, exp_f) ->
              let prose_field =
                text (string_of_atom atom) ++ text " " ++ prose_of_exp exp_f
              in
              if i = 0 then prose_field else text ", " ++ prose_field)
            expfields)
    ++ text "+}+")

(* Option expression, as prose *)

and prose_of_opt_exp (exp_opt : exp option) : Adoc.prose =
  match exp_opt with
  | None -> Adoc.code_prose (code_of_opt_exp None)
  | Some exp -> prose_of_exp exp

(* List expression, as prose *)

and prose_of_list_exp (exps : exp list) : Adoc.prose =
  Adoc.code_prose (code_of_list_exp exps)

(* Cons expression, as prose *)

and prose_of_cons_exp (exp_h : exp) (exp_t : exp) : Adoc.prose =
  Adoc.code_prose (code_of_cons_exp exp_h exp_t)

(* Concatenation expression, as prose *)

and prose_of_cat_exp (exp_l : exp) (exp_r : exp) : Adoc.prose =
  Adoc.(prose_of_exp exp_l ++ text " concatenated with " ++ prose_of_exp exp_r)

(* Membership expression, as prose *)

and prose_of_mem_exp (exp_e : exp) (exp_s : exp) : Adoc.prose =
  Adoc.(prose_of_exp exp_e ++ text " is in " ++ prose_of_exp exp_s)

(* Length expression, as prose *)

and prose_of_len_exp (exp : exp) : Adoc.prose =
  Adoc.(text "the length of " ++ prose_of_exp exp)

(* Dot expression, as prose *)

and prose_of_dot_exp (exp_b : exp) (atom : atom) : Adoc.prose =
  Adoc.code_prose (code_of_dot_exp exp_b atom)

(* Index expression, as prose *)

and prose_of_idx_exp (exp_b : exp) (exp_i : exp) : Adoc.prose =
  Adoc.code_prose (code_of_idx_exp exp_b exp_i)

(* Slice expression, as prose *)

and prose_of_slice_exp (exp_b : exp) (exp_l : exp) (exp_h : exp) : Adoc.prose =
  Adoc.code_prose (code_of_slice_exp exp_b exp_l exp_h)

(* Update expression, as prose *)

and prose_of_upd_exp (exp_b : exp) (path : path) (exp_f : exp) : Adoc.prose =
  Adoc.(
    code_prose (code_of_exp exp_b)
    ++ text " with "
    ++ code_prose (code_of_path path)
    ++ text " set to "
    ++ code_prose (code_of_exp exp_f))

(* Function call expression, as prose *)

and prose_of_call_exp (exp : exp) (id : id) (args : arg list) : Adoc.prose =
  let hint_in = exp.hints.prose_in in
  let hint_true = exp.hints.prose_true in
  match (hint_in, hint_true) with
  | Some hints, _ | _, Some hints ->
      Adoc.link_prose ~target:id.it
        (alternate hints (reindent_lines ~level:0) prose_of_arg args)
  | None, None -> Adoc.code_prose (code_of_exp exp)

(* Iterated expression, as prose *)

and prose_of_iter_exp (exp : exp) (iterexp : iterexp) : Adoc.prose =
  match iterexp with
  | _, [] -> prose_of_exp exp
  | _ -> Adoc.code_prose (code_of_iter_exp exp iterexp)

(* Patterns *)

and code_of_pattern (pattern : pattern) : Adoc.code =
  match pattern with
  | Il.CaseP mixop -> code_of_mixop mixop
  | Il.ListP `Cons -> Adoc.token "_ :: _"
  | Il.ListP (`Fixed len) -> Adoc.token (Format.asprintf "[ _/%d ]" len)
  | Il.ListP `Nil -> Adoc.token "[]"
  | Il.OptP `Some -> Adoc.token "(_)"
  | Il.OptP `None -> Adoc.token "()"

(* Path *)

and code_of_path (path : path) : Adoc.code =
  match path.it with
  | RootP -> Adoc.empty_code
  | IdxP (path, exp) ->
      Adoc.(code_of_path path ^^ token "[" ^^ code_of_exp exp ^^ token "]")
  | SliceP (path, exp_l, exp_h) ->
      Adoc.(
        code_of_path path ^^ token "[" ^^ code_of_exp exp_l ^^ token " : "
        ^^ code_of_exp exp_h ^^ token "]")
  | DotP ({ it = RootP; _ }, atom) -> code_of_atom atom
  | DotP (path, atom) ->
      Adoc.(code_of_path path ^^ token "." ^^ code_of_atom atom)

(* Type parameters *)

(* Parameters, as code *)

and code_of_param (param : param) : Adoc.code =
  match param.it with
  | ExpP (_, exp) -> code_of_exp exp
  | DefP (defid, _, _, _) -> defid |> string_of_defid |> Adoc.token

and code_of_params (params : param list) : Adoc.code =
  match params with
  | [] -> Adoc.empty_code
  | params ->
      Adoc.(
        token "("
        ^^ seq_code
             (List.mapi
                (fun i param ->
                  if i = 0 then code_of_param param
                  else token ", " ^^ code_of_param param)
                params)
        ^^ token ")")

(* Parameters, as prose *)

and prose_of_param (param : param) : Adoc.prose =
  match param.it with
  | ExpP (_, exp) -> prose_of_exp exp
  | DefP (defid, _, _, _) ->
      defid |> string_of_defid |> Adoc.token |> Adoc.code_prose

and prose_of_params (params : param list) : Adoc.prose =
  match params with
  | [] -> Adoc.empty_prose
  | params ->
      Adoc.(
        text "("
        ++ seq_prose
             (List.mapi
                (fun i param ->
                  if i = 0 then prose_of_param param
                  else text ", " ++ prose_of_param param)
                params)
        ++ text ")")

(* Type arguments *)

and string_of_targs (targs : targ list) : string =
  Sl.Print.string_of_targs targs

(* Arguments, as code *)

and code_of_arg (arg : arg) : Adoc.code =
  match arg.it with
  | ExpA exp -> code_of_exp exp
  | DefA defid -> defid |> string_of_defid |> Adoc.token

and code_of_args (args : arg list) : Adoc.code =
  match args with
  | [] -> Adoc.empty_code
  | args ->
      Adoc.(
        token "("
        ^^ seq_code
             (List.mapi
                (fun i a ->
                  if i = 0 then code_of_arg a else token ", " ^^ code_of_arg a)
                args)
        ^^ token ")")

(* Arguments, as prose *)

and prose_of_arg (arg : arg) : Adoc.prose =
  match arg.it with
  | ExpA exp -> prose_of_exp exp
  | DefA defid -> defid |> string_of_defid |> Adoc.token |> Adoc.code_prose

(* Case analysis *)

let prose_of_guard (exp_scrut : exp) (guard : guard) : Adoc.prose =
  match guard with
  | BoolG true -> prose_of_exp exp_scrut
  | BoolG false ->
      let node_scrut = exp_scrut.node in
      let neg_inner =
        UnE (`NotOp, `BoolT, exp_scrut) $$ (node_scrut.at, node_scrut.note)
      in
      prose_of_exp (Annot.no_hints neg_inner)
  | CmpG (cmpop, _, exp) ->
      Adoc.(
        prose_of_exp exp_scrut
        ++ text (" " ^ string_of_cmpop cmpop ^ " ")
        ++ prose_of_exp exp)
  | SubG typ ->
      Adoc.(
        code_prose (code_of_exp exp_scrut)
        ++ text " has type "
        ++ code_prose (code_of_typ typ))
  | MatchG pattern ->
      Adoc.(
        prose_of_exp exp_scrut ++ text " matches pattern "
        ++ code_prose (code_of_pattern pattern))
  | MemG exp ->
      Adoc.(prose_of_exp exp_scrut ++ text " is in " ++ prose_of_exp exp)
  | CheckLetSubG (_, target) | CheckLetMatchG (_, target) ->
      Adoc.(
        text "let "
        ++ code_prose (code_of_exp target)
        ++ text " be " ++ prose_of_exp exp_scrut)

(* Instructions *)

let rec render_instr ?(level : int = 0)
    ?(backtrack : Backtrack.ctx option = None) ?(dispatcher : bool = false)
    (instr : instr) : Adoc.block =
  let hints = instr.hints in
  match instr.node.it with
  | IfI (cond, iterexps, block_then, _) ->
      render_if_instr ~level ~backtrack ~dispatcher cond iterexps block_then
  | HoldI (id_rel, notexp, iterexps, holdcase) ->
      render_hold_instr ~level ~backtrack ~dispatcher hints id_rel notexp
        iterexps holdcase
  | CaseI (exp_scrut, cases, dangle) ->
      render_case_instr ~level ~backtrack ~dispatcher exp_scrut cases dangle
  | GroupI (id_rulegroup, id_rel, rel_signature, exps, block) ->
      if dispatcher then render_group_instr_dispatch ~level id_rel id_rulegroup
      else
        render_group_instr ~level ~backtrack hints id_rel rel_signature exps
          block
  | TryI arms -> render_try_instr ~level ~dispatcher arms
  | LetI (exp_l, exp_r, iterinstrs) ->
      render_let_instr ~level ~backtrack exp_l exp_r iterinstrs
  | RuleI (id_rel, notexp, hint_input, iterinstrs) ->
      render_rule_instr ~level ~backtrack hints id_rel notexp hint_input
        iterinstrs
  | ResultI (rel_signature, exps) ->
      render_result_instr ~level hints rel_signature exps
  | ReturnI exp -> render_return_instr ~level exp
  | DebugI exp -> render_debug_instr ~level exp
  | DestructI (fields, exp_source) ->
      render_destruct_instr ~level fields exp_source
  | CheckLetSubI (_, exp_l, exp_r, block_inner)
  | CheckLetMatchI (_, exp_l, exp_r, block_inner) ->
      render_check_let_instr ~level ~backtrack ~dispatcher exp_l exp_r
        block_inner
  | OptionGetI (exp_l, exp_r, block_inner) ->
      render_option_get_instr ~level ~backtrack ~dispatcher exp_l exp_r
        block_inner

and render_instrs ?(level : int = 0) ?(head : Adoc.block option = None)
    ?(backtrack : Backtrack.ctx option = None) ?(dispatcher : bool = false)
    (instrs : block) : Adoc.block =
  match instrs with
  | [ ({ node = { it = ReturnI exp; _ }; _ } : instr) ]
    when Adoc.width_prose (prose_of_exp exp) <= adoc_width_short -> (
      let prose_return =
        Adoc.(text " return " ++ prose_of_exp exp ++ text ".")
      in
      match head with
      | Some head -> Adoc.concat_block [ head; Adoc.inline_block prose_return ]
      | None -> Adoc.inline_block prose_return)
  | [ ({ node = { it = ResultI (rel_signature, exps); _ }; hints } : instr) ]
    when Adoc.width_prose (prose_of_result hints rel_signature exps)
         <= adoc_width_short -> (
      let prose_result =
        Adoc.(text " " ++ prose_of_result hints rel_signature exps)
      in
      match head with
      | Some head -> Adoc.concat_block [ head; Adoc.inline_block prose_result ]
      | None -> Adoc.inline_block prose_result)
  | _ -> (
      let blocks =
        List.map (render_instr ~level ~backtrack ~dispatcher) instrs
      in
      match head with
      | Some head -> Adoc.seq_block (head :: blocks)
      | None -> Adoc.concat_block [ Adoc.raw_block "\n"; Adoc.seq_block blocks ]
      )

and render_elseblock (elseblock_opt : elseblock option) : string =
  match elseblock_opt with
  | None | Some [] -> ""
  | Some block ->
      "\n\n" ^ adoc_ordered_bullet 0 ^ "Otherwise:"
      ^ Adoc.ser_block (render_instrs ~level:1 block)

(* Iterations *)

and prose_of_iterexp_suffix (iterexps : iterexp list) : Adoc.prose =
  let proses =
    List.concat_map
      (fun (iter, vars) -> List.map (prose_of_in_itervar iter) vars)
      iterexps
  in
  match proses with
  | [] -> Adoc.empty_prose
  | _ -> Adoc.(text ", for all " ++ prose_of_list proses)

and prose_of_iterinstr_suffix (iterinstrs : iterinstr list) : Adoc.prose =
  let proses =
    List.concat_map
      (fun (iter, vars_in, _) -> List.map (prose_of_in_itervar iter) vars_in)
      iterinstrs
  in
  match proses with
  | [] -> Adoc.empty_prose
  | _ -> Adoc.(text ", for each " ++ prose_of_list proses)

and render_iterinstrs ~(level : int) ~(prose_fallthrough : Adoc.prose)
    (iterinstrs : iterinstr list) (render_body : int -> Adoc.block) : Adoc.block
    =
  let rec render ~(outermost : bool) (level : int) (levels : iterinstr list) :
      Adoc.block =
    match levels with
    | [] -> render_body level
    | (iter, vars_in, vars_out) :: iterinstrs_t ->
        let vars_out_visible =
          List.filter (fun (id, _, _) -> not (Id.is_underscored id)) vars_out
        in
        let block_inner = render ~outermost:false (level + 1) iterinstrs_t in
        let block_head =
          Adoc.bullet_inline_block (`Ordered level)
            Adoc.(
              text "For each " ++ prose_of_in_itervars iter vars_in ++ text ":")
        in
        let prose_fallthrough =
          if outermost then prose_fallthrough else Adoc.empty_prose
        in
        if vars_out_visible = [] then
          Adoc.concat_block
            [
              block_head;
              Adoc.raw_block "\n+\n--\n";
              block_inner;
              Adoc.raw_block "\n--\n";
            ]
        else
          let noun = string_of_iter iter in
          Adoc.concat_block
            [
              block_head;
              Adoc.raw_block "\n+\n--\n";
              block_inner;
              Adoc.raw_block "\n--\n+\n";
              Adoc.inline_block
                Adoc.(
                  text "Let "
                  ++ prose_of_out_itervars iter vars_out_visible
                  ++ text
                       (if List.length vars_out_visible > 1 then
                          Printf.sprintf " be the resulting %ss." noun
                        else Printf.sprintf " be the resulting %s." noun)
                  ++ prose_fallthrough);
            ]
  in
  render ~outermost:true level (List.rev iterinstrs)

(* If instruction rendering *)

and render_if_instr ~(level : int) ~(backtrack : Backtrack.ctx option)
    ?(dispatcher : bool = false) (cond : exp) (iterexps : iterexp list)
    (block_then : block) : Adoc.block =
  let prose_fallthrough = Backtrack.prose_of_fallthrough_link backtrack in
  let block_head =
    Adoc.bullet_inline_block (`Ordered level)
      Adoc.(
        text "Check that " ++ prose_of_exp cond
        ++ prose_of_iterexp_suffix iterexps
        ++ text "." ++ prose_fallthrough)
  in
  if block_then = [] then block_head
  else
    Adoc.seq_block
      (block_head
      :: List.map (render_instr ~level ~backtrack ~dispatcher) block_then)

(* Hold instruction rendering *)

and render_hold_instr ~(level : int) ~(backtrack : Backtrack.ctx option)
    ?(dispatcher : bool = false) (hints : Annot.hints) (id_rel : id)
    (notexp : notexp) (iterexps : iterexp list) (holdcase : holdcase) :
    Adoc.block =
  let exps = Mixfix.args notexp in
  let hint_true = hints.prose_true in
  let hint_false = hints.prose_false in
  let iter_suffix = Adoc.ser_prose (prose_of_iterexp_suffix iterexps) in
  let render_head ~(hold : bool) : string =
    let hint_opt = if hold then hint_true else hint_false in
    let fallback_verb = if hold then " holds" else " does not hold" in
    match hint_opt with
    | Some hint ->
        Adoc.ser_prose
          (Adoc.link_prose ~target:(string_of_relid id_rel)
             (alternate hint (reindent_lines ~level:0) prose_of_exp exps))
    | None ->
        let math =
          Adoc.ser_prose
            (Adoc.link_prose ~target:(string_of_relid id_rel)
               (Adoc.code_prose (code_of_notexp notexp)))
        in
        math ^ fallback_verb
  in
  let if_head ~hold =
    Adoc.bullet_inline_block (`Ordered level)
      Adoc.(
        text "If " ++ text (render_head ~hold) ++ text iter_suffix ++ text ":")
  in
  match holdcase with
  | HoldH (block, _dangle) ->
      render_instrs
        ~head:(Some (if_head ~hold:true))
        ~level:(level + 1) ~backtrack ~dispatcher block
  | NotHoldH (block, _dangle) ->
      render_instrs
        ~head:(Some (if_head ~hold:false))
        ~level:(level + 1) ~backtrack ~dispatcher block
  | BothH (block_hold, block_nothold) ->
      Adoc.seq_block
        [
          render_instrs
            ~head:(Some (if_head ~hold:true))
            ~level:(level + 1) ~backtrack ~dispatcher block_hold;
          render_instrs
            ~head:
              (Some
                 (Adoc.bullet_inline_block (`Ordered level) (Adoc.text "Else:")))
            ~level:(level + 1) ~backtrack ~dispatcher block_nothold;
        ]

(* Case analysis instruction rendering *)

and render_case_instr ~(level : int) ~(backtrack : Backtrack.ctx option)
    ?(dispatcher : bool = false) (exp_scrut : exp) (cases : case list)
    (dangle : dangle) : Adoc.block =
  let total = not dangle in
  let n = List.length cases in
  match cases with
  | [ (guard, block_then) ] ->
      let block_head =
        Adoc.bullet_inline_block (`Ordered level)
          Adoc.(
            text "Check that " ++ prose_of_guard exp_scrut guard ++ text ".")
      in
      if block_then = [] then block_head
      else
        Adoc.seq_block
          (block_head
          :: List.map (render_instr ~level ~backtrack ~dispatcher) block_then)
  | _ ->
      Adoc.seq_block
        (cases
        |> List.mapi (fun idx (guard, block_then) ->
               if idx = n - 1 && total then
                 let block_else =
                   Adoc.bullet_inline_block (`Ordered level) (Adoc.text "Else:")
                 in
                 match guard with
                 | CheckLetSubG _ | CheckLetMatchG _ ->
                     let prose_bind = prose_of_guard exp_scrut guard in
                     let block_bind =
                       Adoc.bullet_inline_block
                         (`Ordered (level + 1))
                         Adoc.(capitalize_first prose_bind ++ text ".")
                     in
                     Adoc.seq_block
                       (block_else :: block_bind
                       :: List.map
                            (render_instr ~level:(level + 1) ~backtrack
                               ~dispatcher)
                            block_then)
                 | _ ->
                     render_instrs ~head:(Some block_else) ~level:(level + 1)
                       ~backtrack ~dispatcher block_then
               else
                 let keyword = if idx = 0 then "If" else "Else if" in
                 render_instrs
                   ~head:
                     (Some
                        (Adoc.bullet_inline_block (`Ordered level)
                           Adoc.(
                             text (keyword ^ " ")
                             ++ prose_of_guard exp_scrut guard
                             ++ text ":")))
                   ~level:(level + 1) ~backtrack ~dispatcher block_then))

(* Group instruction rendering *)

and render_group_instr ~(level : int) ~(backtrack : Backtrack.ctx option)
    (hints : Annot.hints) (id_rel : id) (rel_signature : rel_signature)
    (exps : exp list) (block : block) : Adoc.block =
  let hint_in = hints.prose_in in
  let hint_true = hints.prose_true in
  let prose_title =
    match (hint_in, hint_true) with
    | Some hint, _ | _, Some hint ->
        Adoc.link_prose ~target:(string_of_relid id_rel)
          (alternate ~caps:true hint (reindent_lines ~level:0) prose_of_exp exps)
    | None, None ->
        Adoc.link_prose ~target:(string_of_relid id_rel)
          (prose_of_rel_title_math rel_signature exps)
  in
  render_instrs
    ~head:
      (Some
         (Adoc.bullet_inline_block (`Ordered level)
            Adoc.(prose_title ++ text ":")))
    ~level:(level + 1) ~backtrack block

and render_group_instr_dispatch ~(level : int) (id_rel : id) (id_rulegroup : id)
    : Adoc.block =
  let name = string_of_relid id_rulegroup in
  let target = string_of_rulegroupid (string_of_relid id_rel) name in
  Adoc.bullet_inline_block (`Ordered level)
    Adoc.(text "goto " ++ link_prose ~target (text name))

(* Try instruction rendering *)

and render_try_instr ~(level : int) ?(dispatcher : bool = false)
    (arms : arm list) : Adoc.block =
  let label = Backtrack.Label.fresh () in
  let level_arm = level + 1 in
  let level_body = level + 2 in
  let total = List.length arms in
  let render_arm idx arm =
    let backtrack = Backtrack.update ~label ~level:level_arm ~total idx in
    let prose_anchor =
      Backtrack.prose_of_arm_anchor ~label ~level:level_arm idx
    in
    render_instrs
      ~head:
        (Some
           (Adoc.bullet_inline_block (`Ordered level_arm)
              Adoc.(text "{empty}" ++ prose_anchor)))
      ~level:level_body ~backtrack:(Some backtrack) ~dispatcher arm
  in
  let block_head =
    Adoc.bullet_inline_block (`Ordered level)
      Adoc.(text "Try " ++ Backtrack.prose_of_label label ++ text ":")
  in
  Adoc.seq_block (block_head :: List.mapi render_arm arms)

(* Let instruction rendering *)

and render_let_instr ~(level : int) ~(backtrack : Backtrack.ctx option)
    (exp_l : exp) (exp_r : exp) (iterinstrs : iterinstr list) : Adoc.block =
  let prose_fallthrough = Backtrack.prose_of_fallthrough_link backtrack in
  let vars_out_visible =
    iterinstrs
    |> List.concat_map (fun (_, _, vars_out) -> vars_out)
    |> List.filter (fun (id, _, _) -> not (Id.is_underscored id))
  in
  if vars_out_visible = [] then
    Adoc.bullet_inline_block (`Ordered level)
      Adoc.(
        text "Let "
        ++ code_prose (code_of_exp exp_l)
        ++ text " be " ++ prose_of_exp exp_r
        ++ prose_of_iterinstr_suffix iterinstrs
        ++ text "." ++ prose_fallthrough)
  else
    let render_body level =
      Adoc.bullet_inline_block (`Unordered level)
        Adoc.(
          text "Let "
          ++ code_prose (code_of_exp exp_l)
          ++ text " be " ++ prose_of_exp exp_r ++ text ".")
    in
    render_iterinstrs ~level ~prose_fallthrough iterinstrs render_body

(* Rule instruction rendering *)

and render_rule_instr ~(level : int) ~(backtrack : Backtrack.ctx option)
    (hints : Annot.hints) (id_rel : id) (notexp : notexp)
    (hint_input : Hints.Input.t) (iterinstrs : iterinstr list) : Adoc.block =
  let exps = Mixfix.args notexp in
  let prose_fallthrough = Backtrack.prose_of_fallthrough_link backtrack in
  let exps_in, exps_out = Hints.Input.split hint_input exps in
  let hint_in = hints.prose_in in
  let hint_out = hints.prose_out in
  let vars_out_visible =
    iterinstrs
    |> List.concat_map (fun (_, _, vars_out) -> vars_out)
    |> List.filter (fun (id, _, _) -> not (Id.is_underscored id))
  in
  let rule_body =
    match (hint_in, hint_out) with
    | Some hint_in, Some hint_out ->
        let prose_out =
          Adoc.ser_prose_in_link
            (alternate hint_out unindent_lines prose_of_exp exps_out)
        in
        let prose_in_typed =
          Adoc.link_prose ~target:(string_of_relid id_rel)
            (alternate hint_in unindent_lines prose_of_exp exps_in)
        in
        let prose_in = Adoc.ser_prose prose_in_typed in
        F.asprintf "Let %s be the result of %s" prose_out prose_in
    | _ ->
        F.asprintf "Let %s"
          (Adoc.ser_prose
             (Adoc.link_prose ~target:(string_of_relid id_rel)
                (Adoc.code_prose (code_of_notexp notexp))))
  in
  if vars_out_visible = [] then
    Adoc.concat_block
      [
        Adoc.bullet_block (`Ordered level);
        Adoc.raw_block rule_body;
        Adoc.inline_block
          Adoc.(
            prose_of_iterinstr_suffix iterinstrs
            ++ text "." ++ prose_fallthrough);
      ]
  else
    let render_body level =
      Adoc.concat_block
        [
          Adoc.bullet_block (`Unordered level);
          Adoc.raw_block rule_body;
          Adoc.raw_block ".";
        ]
    in
    render_iterinstrs ~level ~prose_fallthrough iterinstrs render_body

(* Result instruction rendering *)

and prose_of_result (hints : Annot.hints) (rel_signature : rel_signature)
    (exps : exp list) : Adoc.prose =
  let nottyp, hint_input = rel_signature in
  let typs = Mixfix.args nottyp.it in
  let is_conditional = Hints.Input.is_conditional hint_input typs in
  if is_conditional then Adoc.text "then, the relation holds."
  else
    match (hints.prose_out, exps) with
    | Some hint, _ ->
        Adoc.(
          text "the result is "
          ++ alternate hint (reindent_lines ~level:0) prose_of_exp exps
          ++ text ".")
    | None, [] -> Adoc.text "the relation holds."
    | None, _ -> Adoc.(text "the result is " ++ prose_of_exps exps ++ text ".")

and render_result_instr ~(level : int) (hints : Annot.hints)
    (rel_signature : rel_signature) (exps : exp list) : Adoc.block =
  Adoc.bullet_inline_block (`Ordered level)
    (Adoc.capitalize_first (prose_of_result hints rel_signature exps))

(* Return instruction rendering *)

and render_return_instr ~(level : int) (exp : exp) : Adoc.block =
  Adoc.bullet_inline_block (`Ordered level)
    Adoc.(text "Return " ++ prose_of_exp exp ++ text ".")

(* Debug instruction rendering *)

and render_debug_instr ~(level : int) (exp : exp) : Adoc.block =
  Adoc.bullet_inline_block (`Ordered level)
    Adoc.(text "(debug: " ++ prose_of_exp exp ++ text ")")

(* Destruct instruction rendering *)

and render_destruct_instr ~(level : int) (fields : (string option * exp) list)
    (exp_source : exp) : Adoc.block =
  let projections =
    List.filter_map
      (fun (name_opt, exp_target) ->
        Option.map (fun name -> (name, exp_target)) name_opt)
      fields
  in
  let line = Adoc.bullet_inline_block (`Ordered level) in
  match projections with
  | [ (name, exp_target) ] ->
      line
        Adoc.(
          text "Let " ++ prose_of_exp exp_target
          ++ text (F.asprintf " be the %s of " name)
          ++ prose_of_exp exp_source ++ text ".")
  | _ ->
      let names, exps_target = List.split projections in
      line
        Adoc.(
          text "Let " ++ prose_of_exps exps_target ++ text " be "
          ++ prose_of_list (List.map (fun s -> text ("the " ^ s)) names)
          ++ text " of " ++ prose_of_exp exp_source ++ text ".")

(* Check-let instruction rendering (CheckLetSubI / CheckLetMatchI) *)

and render_check_let_instr ~(level : int) ~(backtrack : Backtrack.ctx option)
    ?(dispatcher : bool = false) (exp_l : exp) (exp_r : exp)
    (block_inner : block) : Adoc.block =
  let prose_fallthrough = Backtrack.prose_of_fallthrough_link backtrack in
  let block_head =
    Adoc.bullet_inline_block (`Ordered level)
      Adoc.(
        text "Let!~type~ "
        ++ code_prose (code_of_exp exp_l)
        ++ text " be " ++ prose_of_exp exp_r ++ text "." ++ prose_fallthrough)
  in
  if block_inner = [] then block_head
  else
    Adoc.seq_block
      (block_head
      :: List.map (render_instr ~level ~backtrack ~dispatcher) block_inner)

(* Option-get instruction rendering *)

and render_option_get_instr ~(level : int) ~(backtrack : Backtrack.ctx option)
    ?(dispatcher : bool = false) (exp_l : exp) (exp_r : exp)
    (block_inner : block) : Adoc.block =
  let prose_fallthrough = Backtrack.prose_of_fallthrough_link backtrack in
  let block_head =
    Adoc.bullet_inline_block (`Ordered level)
      Adoc.(
        text "Let "
        ++ code_prose (code_of_exp exp_l)
        ++ text " be "
        ++ text (adoc_link ~link:"option_get" "*!*")
        ++ text " " ++ prose_of_exp exp_r ++ text "." ++ prose_fallthrough)
  in
  if block_inner = [] then block_head
  else
    Adoc.seq_block
      (block_head
      :: List.map (render_instr ~level ~backtrack ~dispatcher) block_inner)

(* Relations *)

and lift_synthesized_exp (exp : Sl.exp) : exp =
  let it' =
    match exp.it with
    | Il.VarE id -> VarE id
    | Il.IterE (exp_inner, (iter, vars)) ->
        IterE (lift_synthesized_exp exp_inner, (iter, vars))
    | _ -> assert false
  in
  Annot.no_hints (it' $$ (exp.at, exp.note))

and prose_of_rel_title_math (rel_signature : rel_signature) (exps : exp list) :
    Adoc.prose =
  let nottyp, inputs = rel_signature in
  let mixop = Mixfix.to_mixop nottyp.it in
  let dexps = List.map code_of_exp exps in
  let num_outputs = Mixop.arity mixop - List.length dexps in
  let code_holes = List.init num_outputs (fun _ -> Adoc.token "%") in
  let padded = Hints.Input.combine inputs dexps code_holes in
  Adoc.code_prose (code_of_mixfix ~atom:string_of_atom mixop padded)

and render_rel_title_block (hints : Annot.hints) (id_rel : id)
    (rel_signature : rel_signature) (exps : exp list) : Adoc.block =
  let exps_in_title =
    match hints.prose_input_exps with
    | Some exps_in_sl -> List.map lift_synthesized_exp exps_in_sl
    | None -> exps
  in
  let prose_title =
    Adoc.link_prose ~target:(string_of_relid id_rel)
      (Adoc.text (Sl.Print.string_of_relid id_rel))
  in
  let block_title_header =
    Adoc.concat_block
      [
        Adoc.inline_block Adoc.(prose_title ++ text ":"); Adoc.raw_block "\n\n";
      ]
  in
  match
    (hints.prose_in, hints.prose_out, hints.prose_output_exps, hints.prose_true)
  with
  | Some _, Some _, None, _ -> assert false
  | Some hint_in, Some hint_out, Some exps_out_sl, _ ->
      let exps_out = List.map lift_synthesized_exp exps_out_sl in
      Adoc.concat_block
        [
          block_title_header;
          Adoc.bullet_inline_block (`Unordered 0)
            (alternate ~caps:true hint_in (reindent_lines ~level:1) prose_of_exp
               exps_in_title);
          Adoc.raw_block ":\n";
          Adoc.bullet_inline_block (`Unordered 0)
            Adoc.(
              text "The result is "
              ++ alternate ~caps:false hint_out (reindent_lines ~level:1)
                   prose_of_exp exps_out);
          Adoc.raw_block ".";
        ]
  | Some hint_in, _, _, _ ->
      Adoc.concat_block
        [
          block_title_header;
          Adoc.bullet_inline_block (`Unordered 0)
            (alternate ~caps:true hint_in (reindent_lines ~level:1) prose_of_exp
               exps_in_title);
          Adoc.raw_block ".";
        ]
  | _, _, _, Some hint_true ->
      Adoc.concat_block
        [
          block_title_header;
          Adoc.bullet_inline_block (`Unordered 0)
            (alternate ~caps:true hint_true (reindent_lines ~level:0)
               prose_of_exp exps);
        ]
  | _ ->
      Adoc.inline_block
        (Adoc.link_prose ~target:(string_of_relid id_rel)
           Adoc.(
             text (Sl.Print.string_of_relid id_rel ^ ": ")
             ++ prose_of_rel_title_math rel_signature exps))

and render_rel_title_adoc (hints : Annot.hints) (id_rel : id)
    (rel_signature : rel_signature) (exps : exp list) : string =
  Adoc.ser_block (render_rel_title_block hints id_rel rel_signature exps)

(* Extern relations *)

let render_extern_rel_def_block (hints : Annot.hints) (externrel : externrel) :
    Adoc.block =
  let id_rel, rel_signature, exps = externrel in
  render_rel_title_block hints id_rel rel_signature exps

let render_extern_rel_def (hints : Annot.hints) (externrel : externrel) : string
    =
  Adoc.ser_block (render_extern_rel_def_block hints externrel)

(* Defined relations *)

let collect_groups (block : block) : instr list =
  let rec collect_instr (instr : instr) : instr list =
    match instr.node.it with
    | IfI (_, _, block_then, _) -> collect_block block_then
    | HoldI (_, _, _, holdcase) -> (
        match holdcase with
        | BothH (block_hold, block_nothold) ->
            collect_block block_hold @ collect_block block_nothold
        | HoldH (block_hold, _) -> collect_block block_hold
        | NotHoldH (block_nothold, _) -> collect_block block_nothold)
    | CaseI (_, cases, _) ->
        cases |> List.concat_map (fun (_, block) -> collect_block block)
    | TryI arms -> arms |> List.concat_map collect_block
    | GroupI _ -> [ instr ]
    | LetI _ | RuleI _ | ResultI _ | ReturnI _ | DebugI _ | DestructI _ -> []
    | CheckLetSubI (_, _, _, block_then)
    | CheckLetMatchI (_, _, _, block_then)
    | OptionGetI (_, _, block_then) ->
        collect_block block_then
  and collect_block (block : block) : instr list =
    block |> List.concat_map collect_instr
  in
  collect_block block

let render_rulegroup (hints : Annot.hints) (_id_rulegroup : id) (id_rel : id)
    (rel_signature : rel_signature) (exps : exp list) (block : block) : string =
  let hint_in = hints.prose_in in
  let hint_true = hints.prose_true in
  let title =
    match (hint_in, hint_true) with
    | Some hint, _ | _, Some hint ->
        Adoc.ser_prose
          (Adoc.link_prose ~target:(string_of_relid id_rel)
             (alternate ~caps:true hint (reindent_lines ~level:0) prose_of_exp
                exps))
    | None, None ->
        Adoc.ser_prose
          (Adoc.link_prose ~target:(string_of_relid id_rel)
             (prose_of_rel_title_math rel_signature exps))
  in
  title ^ ":\n" ^ Adoc.ser_block (render_instrs block)

let render_defined_rel_def_dispatch
    ((id_rel, _rel_signature, _exps, block, _elseblock_opt) : rel) : string =
  let head =
    Adoc.inline_block Adoc.(text (string_of_relid id_rel) ++ text " dispatch:")
  in
  Adoc.ser_block
    (render_instrs ~head:(Some head) ~level:0 ~dispatcher:true block)

let render_defined_rel_def_block (hints : Annot.hints) (rel : rel) : Adoc.block
    =
  let id_rel, rel_signature, exps, block, elseblock_opt = rel in
  Adoc.concat_block
    [
      render_rel_title_block hints id_rel rel_signature exps;
      Adoc.raw_block "\n\n";
      Adoc.raw_block
        (block |> collect_groups
        |> List.map (fun (instr : instr) ->
               match instr.node.it with
               | GroupI (id_rulegroup, id_rel, rel_signature, exps, block) ->
                   render_rulegroup instr.hints id_rulegroup id_rel
                     rel_signature exps block
               | _ -> assert false)
        |> String.concat "\n\n");
      Adoc.raw_block (render_elseblock elseblock_opt);
      Adoc.raw_block ("\n\n" ^ render_defined_rel_def_dispatch rel);
    ]

let render_defined_rel_def (hints : Annot.hints) (rel : rel) : string =
  Adoc.ser_block (render_defined_rel_def_block hints rel)

(* Functions *)

let render_func_title_block (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : Adoc.block =
  let prose_title =
    Adoc.link_prose
      ~target:(string_of_defid ~link:true id_func)
      (Adoc.text (string_of_defid id_func))
  in
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      Adoc.concat_block
        [
          Adoc.inline_block Adoc.(prose_title ++ text ":");
          Adoc.raw_block "\n\n";
          Adoc.bullet_inline_block (`Unordered 0)
            (alternate ~caps:true hint (reindent_lines ~level:0) prose_of_param
               params);
        ]
  | None, None ->
      Adoc.concat_block
        [
          Adoc.inline_block prose_title;
          Adoc.raw_block (Sl.Print.string_of_tparams tparams);
          Adoc.raw_block (Adoc.ser_code (code_of_params params));
        ]

let render_func_title (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : string =
  Adoc.ser_block (render_func_title_block hints id_func tparams params)

let render_func_header_block (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : Adoc.block =
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      Adoc.inline_block
        (Adoc.link_prose
           ~target:(string_of_defid ~link:true id_func)
           (Adoc.text
              (Adoc.ser_prose
                 (alternate ~caps:true hint (reindent_lines ~level:0)
                    prose_of_param params))))
  | None, None ->
      Adoc.inline_block
        (Adoc.link_prose
           ~target:(string_of_defid ~link:true id_func)
           (Adoc.text
              (string_of_defid id_func
              ^ Sl.Print.string_of_tparams tparams
              ^ Adoc.ser_code (code_of_params params))))

let render_func_header (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : string =
  Adoc.ser_block (render_func_header_block hints id_func tparams params)

(* Extern functions *)

let render_extern_func_def (hints : Annot.hints) (externfunc : externfunc) :
    string =
  let id_func, tparams, params, _ = externfunc in
  render_func_header hints id_func tparams params

(* Builtin functions *)

let render_builtin_func_def (hints : Annot.hints) (builtinfunc : builtinfunc) :
    string =
  let id_func, tparams, params, _ = builtinfunc in
  render_func_header hints id_func tparams params

(* Table functions *)

let render_table_func_def_block (hints : Annot.hints) (tablefunc : tablefunc) :
    Adoc.block =
  let id_func, params, _, tablerows = tablefunc in
  let block_table =
    Adoc.table_block
      ~cols:(List.length params + 1)
      ~header:[ prose_of_params params; Adoc.text "Result" ]
      (tablerows
      |> List.map (fun tablerow ->
             let exps_sig, exp_res, _ = tablerow in
             [
               Adoc.ser_code (code_of_exps exps_sig);
               Adoc.ser_code (code_of_exp exp_res);
             ]))
  in
  Adoc.concat_block
    [
      render_func_header_block hints id_func [] params;
      Adoc.raw_block ":\n";
      block_table;
    ]

let render_table_func_def (hints : Annot.hints) (tablefunc : tablefunc) : string
    =
  Adoc.ser_block (render_table_func_def_block hints tablefunc)

(* Defined functions *)

let render_defined_func_def_block (hints : Annot.hints) (func : definedfunc) :
    Adoc.block =
  let id_func, tparams, params, _typ, block, elseblock_opt = func in
  let block_body =
    match block with
    | [
     ({ node = { it = ReturnI ({ node = { it = BoolE _; _ }; _ } as e); _ }; _ } :
       instr);
    ] ->
        Adoc.inline_block
          Adoc.(text " return " ++ code_prose (code_of_exp e) ++ text ".")
    | _ ->
        Adoc.seq_block (List.map (render_instr ~level:0 ~backtrack:None) block)
  in
  Adoc.concat_block
    [
      render_func_header_block hints id_func tparams params;
      Adoc.raw_block "\n\n";
      block_body;
      Adoc.raw_block (render_elseblock elseblock_opt);
    ]

let render_defined_func_def (hints : Annot.hints) (func : definedfunc) : string
    =
  Adoc.ser_block (render_defined_func_def_block hints func)

(* Definitions *)

let id_of_def (def : def) : string option =
  match def.node.it with
  | ExternTypD _ | TypD _ | VarD _ -> None
  | ExternRelD (id, _, _)
  | RelD (id, _, _, _, _)
  | ExternDecD (id, _, _, _)
  | BuiltinDecD (id, _, _, _)
  | TableDecD (id, _, _, _)
  | FuncDecD (id, _, _, _, _, _) ->
      Some id.it

let render_def (def : def) : string option =
  def |> id_of_def |> Option.iter Backtrack.Label.set_namespace;
  let wrap_some s = Some s in
  let hints = def.hints in
  match def.node.it with
  | ExternTypD _ | TypD _ | VarD _ -> None
  | ExternRelD externrel -> render_extern_rel_def hints externrel |> wrap_some
  | RelD rel -> render_defined_rel_def hints rel |> wrap_some
  | ExternDecD externfunc ->
      render_extern_func_def hints externfunc |> wrap_some
  | BuiltinDecD builtinfunc ->
      render_builtin_func_def hints builtinfunc |> wrap_some
  | TableDecD tablefunc -> render_table_func_def hints tablefunc |> wrap_some
  | FuncDecD func -> render_defined_func_def hints func |> wrap_some

let render_defs (defs : def list) : string =
  defs |> List.filter_map render_def |> String.concat "\n\n"

(* Spec *)

let render_spec (spec : spec) : string = render_defs spec
