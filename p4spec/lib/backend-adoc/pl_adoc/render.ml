open Domain
open Lib
open Lang
open Xl
open Pl
open Util.Source
module F = Format
module Fallthrough = Fallthrough
open Utils

(* Render utils *)

type anchors = Adoc.anchor

let anchors ~(func : string -> string option) ~(rel : string -> string option) :
    anchors =
  Adoc.anchor ~func ~rel

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

let escape_plus (text : string) : string =
  text |> String.split_on_char '+' |> String.concat "{plus}"

let escape_apostrophe (text : string) : string =
  text |> String.split_on_char '\'' |> String.concat "{apos}"

let string_of_atom (atom : atom) : string =
  match atom.it with
  | Atom.Tag s_tag -> "{nbsp}" ^ adoc_subscript s_tag
  | atom ->
      let text = Atom.string_of_atom atom in
      if String.contains text '+' then text |> escape_plus |> escape_apostrophe
      else "+" ^ text ^ "+"

(* Alternation *)

let alternate ?(caps = false) (hint : Hints.Alter.t)
    (base_text : string -> string) (render : 'a -> Adoc.prose) (items : 'a list)
    : Adoc.prose =
  let prose_alternated =
    Hints.Alter.alternate ~empty:Adoc.empty_prose
      ~text:(fun s ->
        match s with "" -> None | s -> Some (Adoc.text (base_text s)))
      ~atom:(fun (atom : atom) ->
        Adoc.code_prose (Adoc.token (string_of_atom atom)))
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
  if caps then Adoc.capitalize_first_prose prose_alternated
  else prose_alternated

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

(* Atoms *)

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

(* Dispatch by constructor; code view of prose_of_exp *)

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
  | SubE (exp, typ, _) -> code_of_sub_exp exp typ
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

(* List of expressions, as code

     x, y, z *)

and code_of_exps ?(sep : string = ", ") (exps : exp list) : Adoc.code =
  Adoc.seq_code
    (List.mapi
       (fun i exp ->
         if i = 0 then code_of_exp exp else Adoc.(token sep ^^ code_of_exp exp))
       exps)

(* Relation notation, as code

     G |- e : t *)

and code_of_notexp (notexp : notexp) : Adoc.code =
  let mixop, exps = Mixfix.split notexp in
  code_of_mixfix ~atom:string_of_atom mixop (List.map code_of_exp exps)

(* Boolean literal, as code

     true *)

and code_of_bool_exp (b : bool) : Adoc.code = Adoc.token (string_of_bool b)

(* Numeric literal, as code

     42 *)

and code_of_num_exp (n : num) : Adoc.code = Adoc.token (string_of_num n)

(* Text literal, as code

     "abc" *)

and code_of_text_exp (text : text) : Adoc.code =
  Adoc.token ("\"" ^ String.escaped text ^ "\"")

(* Variable, as code

     x *)

and code_of_var_exp (id : id) : Adoc.code = code_of_varid id

(* Unary op, as code

     -n *)

and code_of_un_exp (unop : unop) (exp : exp) : Adoc.code =
  Adoc.(token (string_of_unop unop) ^^ code_of_exp exp)

(* Binary op, as code

     x + y *)

and code_of_bin_exp (binop : binop) (exp_l : exp) (exp_r : exp) : Adoc.code =
  Adoc.(
    code_of_exp exp_l
    ^^ token (" " ^ escape_plus (Sl.Print.string_of_binop binop) ^ " ")
    ^^ code_of_exp exp_r)

(* Comparison, as code

     x < y *)

and code_of_cmp_exp (cmpop : cmpop) (exp_l : exp) (exp_r : exp) : Adoc.code =
  Adoc.(
    code_of_exp exp_l
    ^^ token (" " ^ Sl.Print.string_of_cmpop cmpop ^ " ")
    ^^ code_of_exp exp_r)

(* Upcast, as code: operand alone, cast implicit

     x *)

and code_of_upcast_exp (exp : exp) : Adoc.code = code_of_exp exp

(* Downcast, as code: operand alone, cast implicit

     x *)

and code_of_downcast_exp (exp : exp) : Adoc.code = code_of_exp exp

(* Subtype check, as code

     x has type nat *)

and code_of_sub_exp (exp : exp) (typ : typ) : Adoc.code =
  Adoc.(code_of_exp exp ^^ token " has type " ^^ code_of_typ typ)

(* Pattern-match check, as code

     x is none
     xs is a non-empty list
     xs is a list of length 3
     x matches pattern A y *)

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

(* Tuple, as code

     ( x, y ) *)

and code_of_tuple_exp (exps : exp list) : Adoc.code =
  Adoc.(token "( " ^^ code_of_exps ~sep:", " exps ^^ token " )")

(* Case, as code

     A x *)

and code_of_case_exp (notexp : notexp) : Adoc.code = code_of_notexp notexp

(* Struct, as code

     { a x, b y } *)

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

(* Option, as code: none | some

     ·
     x *)

and code_of_opt_exp (exp_opt : exp option) : Adoc.code =
  match exp_opt with None -> Adoc.token "·" | Some exp -> code_of_exp exp

(* List, as code: empty | singleton | many

     ·
     x
     [ x, y ] *)

and code_of_list_exp (exps : exp list) : Adoc.code =
  match exps with
  | [] -> Adoc.token "·"
  | [ exp ] -> code_of_exp exp
  | exps -> Adoc.(token "+[+ " ^^ code_of_exps ~sep:", " exps ^^ token " +]+")

(* Cons, as code

     x :: xs *)

and code_of_cons_exp (exp_h : exp) (exp_t : exp) : Adoc.code =
  Adoc.(code_of_exp exp_h ^^ token " {two-colons} " ^^ code_of_exp exp_t)

(* Concatenation, as code

     xs ++ ys *)

and code_of_cat_exp (exp_l : exp) (exp_r : exp) : Adoc.code =
  Adoc.(code_of_exp exp_l ^^ token " {pp} " ^^ code_of_exp exp_r)

(* Membership, as code

     x is in s *)

and code_of_mem_exp (exp_e : exp) (exp_s : exp) : Adoc.code =
  Adoc.(code_of_exp exp_e ^^ token " is in " ^^ code_of_exp exp_s)

(* Length, as code

     the length of xs *)

and code_of_len_exp (exp : exp) : Adoc.code =
  Adoc.(token "the length of " ^^ code_of_exp exp)

(* Field access, as code

     p.x *)

and code_of_dot_exp (exp_b : exp) (atom : atom) : Adoc.code =
  Adoc.(code_of_exp exp_b ^^ token "." ^^ code_of_atom atom)

(* Indexing, as code

     xs[i] *)

and code_of_idx_exp (exp_b : exp) (exp_i : exp) : Adoc.code =
  Adoc.(code_of_exp exp_b ^^ token "[" ^^ code_of_exp exp_i ^^ token "]")

(* Slice, as code

     xs[0 : n] *)

and code_of_slice_exp (exp_b : exp) (exp_l : exp) (exp_h : exp) : Adoc.code =
  Adoc.(
    code_of_exp exp_b ^^ token "[" ^^ code_of_exp exp_l ^^ token " : "
    ^^ code_of_exp exp_h ^^ token "]")

(* Update, as code

     s[.x = v] *)

and code_of_upd_exp (exp_b : exp) (path : path) (exp_f : exp) : Adoc.code =
  Adoc.(
    code_of_exp exp_b ^^ token "[" ^^ code_of_path path ^^ token " = "
    ^^ code_of_exp exp_f ^^ token "]")

(* Function call, as code: linked callee

     $lookup(g, x) *)

and code_of_call_exp (id : id) (targs : targ list) (args : arg list) : Adoc.code
    =
  Adoc.link_subject_code (Adoc.Function id.it)
    Adoc.(
      token (string_of_defid id)
      ^^ token (string_of_targs targs)
      ^^ code_of_args args)

(* Iterated, as code: multi-token inner parenthesized

     x^*^
     ( x + y )^*^ *)

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

(* Dispatch by constructor; readable-prose view of code_of_exp, falling back to
   the code form where no distinct prose exists *)

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
  | SubE (exp, typ, _) -> prose_of_sub_exp exp typ
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

(* List of expressions, as prose

     x, y, and z *)

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

(* Boolean literal, as prose (code form, inline span)

     true *)

and prose_of_bool_exp (b : bool) : Adoc.prose =
  Adoc.code_prose (code_of_bool_exp b)

(* Numeric literal, as prose (code form, inline span)

     42 *)

and prose_of_num_exp (n : num) : Adoc.prose =
  Adoc.code_prose (code_of_num_exp n)

(* Text literal, as prose (code form, inline span)

     "abc" *)

and prose_of_text_exp (text : text) : Adoc.prose =
  Adoc.code_prose (code_of_text_exp text)

(* Variable, as prose (code form, inline span)

     x *)

and prose_of_var_exp (id : id) : Adoc.prose =
  Adoc.code_prose (code_of_var_exp id)

(* Negated check, as prose; None when [exp] has no readable negation

     x does not match pattern p
     x does not have type t
     x is not in s *)

and prose_of_negated_exp_opt (exp : exp) : Adoc.prose option =
  match exp.node.it with
  | MatchE (exp_e, pattern) ->
      Some
        Adoc.(
          prose_of_exp exp_e
          ++ text " does not match pattern "
          ++ code_prose (code_of_pattern pattern))
  | SubE (exp_e, typ, _) ->
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
            (Adoc.link_subject_prose (Adoc.Function id.it)
               (alternate hints (reindent_lines ~level:0) prose_of_arg args))
      | None ->
          Some
            (Adoc.code_prose
               Adoc.(token (string_of_unop `NotOp) ^^ code_of_exp exp)))
  | _ -> None

(* Unary op, as prose: boolean "not" uses the negated form when available

     x does not match pattern p *)

and prose_of_un_exp (unop : unop) (exp : exp) : Adoc.prose =
  match unop with
  | #Bool.unop -> (
      match prose_of_negated_exp_opt exp with
      | Some p -> p
      | None -> Adoc.code_prose (code_of_un_exp unop exp))
  | _ -> Adoc.code_prose (code_of_un_exp unop exp)

(* Binary op, as prose: boolean spelled out, numeric falls back to code

     x and y
     if p, then q *)

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

(* Comparison, as prose: operator spelled out

     x is equal to y *)

and prose_of_cmp_exp (cmpop : cmpop) (exp_l : exp) (exp_r : exp) : Adoc.prose =
  Adoc.(
    prose_of_exp exp_l
    ++ text (" " ^ string_of_cmpop cmpop ^ " ")
    ++ prose_of_exp exp_r)

(* Upcast, as prose: operand alone

     x *)

and prose_of_upcast_exp (exp : exp) : Adoc.prose =
  Adoc.code_prose (code_of_upcast_exp exp)

(* Downcast, as prose: operand alone

     x *)

and prose_of_downcast_exp (exp : exp) : Adoc.prose =
  Adoc.code_prose (code_of_downcast_exp exp)

(* Subtype check, as prose

     x has type nat *)

and prose_of_sub_exp (exp : exp) (typ : typ) : Adoc.prose =
  Adoc.(
    code_prose (code_of_exp exp)
    ++ text " has type "
    ++ code_prose (code_of_typ typ))

(* Pattern-match check, as prose

     x is none
     xs is a non-empty list
     xs is a list of length 3
     x matches pattern A y *)

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

(* Tuple, as prose

     ( x, y ) *)

and prose_of_tuple_exp (exps : exp list) : Adoc.prose =
  Adoc.(text "( " ++ prose_of_exps ~sep:", " exps ++ text " )")

(* Case, as prose: type prose hint (linked) when available, else code

     the header of x
     A x *)

and prose_of_case_exp (exp : exp) (notexp : notexp) : Adoc.prose =
  let hint_opt = exp.hints.prose in
  let link_opt = tid_of_typ exp.node.note in
  match (hint_opt, link_opt) with
  | Some hints, Some tid ->
      Adoc.link_prose ~target:tid.it
        (alternate hints (reindent_lines ~level:0) prose_of_exp
           (Mixfix.args notexp))
  | _ -> Adoc.code_prose (code_of_notexp notexp)

(* Struct, as prose

     { a x, b y } *)

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

(* Option, as prose: none | some

     ·
     x *)

and prose_of_opt_exp (exp_opt : exp option) : Adoc.prose =
  match exp_opt with
  | None -> Adoc.code_prose (code_of_opt_exp None)
  | Some exp -> prose_of_exp exp

(* List, as prose

     [ x, y ] *)

and prose_of_list_exp (exps : exp list) : Adoc.prose =
  Adoc.code_prose (code_of_list_exp exps)

(* Cons, as prose

     x :: xs *)

and prose_of_cons_exp (exp_h : exp) (exp_t : exp) : Adoc.prose =
  Adoc.code_prose (code_of_cons_exp exp_h exp_t)

(* Concatenation, as prose: spelled out (code: xs ++ ys)

     xs concatenated with ys *)

and prose_of_cat_exp (exp_l : exp) (exp_r : exp) : Adoc.prose =
  Adoc.(prose_of_exp exp_l ++ text " concatenated with " ++ prose_of_exp exp_r)

(* Membership, as prose

     x is in s *)

and prose_of_mem_exp (exp_e : exp) (exp_s : exp) : Adoc.prose =
  Adoc.(prose_of_exp exp_e ++ text " is in " ++ prose_of_exp exp_s)

(* Length, as prose

     the length of xs *)

and prose_of_len_exp (exp : exp) : Adoc.prose =
  Adoc.(text "the length of " ++ prose_of_exp exp)

(* Field access, as prose

     p.x *)

and prose_of_dot_exp (exp_b : exp) (atom : atom) : Adoc.prose =
  Adoc.code_prose (code_of_dot_exp exp_b atom)

(* Indexing, as prose

     xs[i] *)

and prose_of_idx_exp (exp_b : exp) (exp_i : exp) : Adoc.prose =
  Adoc.code_prose (code_of_idx_exp exp_b exp_i)

(* Slice, as prose

     xs[0 : n] *)

and prose_of_slice_exp (exp_b : exp) (exp_l : exp) (exp_h : exp) : Adoc.prose =
  Adoc.code_prose (code_of_slice_exp exp_b exp_l exp_h)

(* Update, as prose: spelled out (code: s[.x = v])

     s with .x set to v *)

and prose_of_upd_exp (exp_b : exp) (path : path) (exp_f : exp) : Adoc.prose =
  Adoc.(
    code_prose (code_of_exp exp_b)
    ++ text " with "
    ++ code_prose (code_of_path path)
    ++ text " set to "
    ++ code_prose (code_of_exp exp_f))

(* Function call, as prose: prose hint (linked) when available, else code

     the lookup of x in g
     $lookup(g, x) *)

and prose_of_call_exp (exp : exp) (id : id) (args : arg list) : Adoc.prose =
  let hint_in = exp.hints.prose_in in
  let hint_true = exp.hints.prose_true in
  match (hint_in, hint_true) with
  | Some hints, _ | _, Some hints ->
      Adoc.link_subject_prose (Adoc.Function id.it)
        (alternate hints (reindent_lines ~level:0) prose_of_arg args)
  | None, None -> Adoc.code_prose (code_of_exp exp)

(* Iterated, as prose

     x^*^ *)

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

(* Case guard against scrutinee, as prose

     x is equal to y
     x has type t
     x matches pattern p
     x is in s
     let y be x *)

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
  | SubG (typ, _) ->
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
  | CheckLetSubG (_, _, target) | CheckLetMatchG (_, target) ->
      Adoc.(
        text "let "
        ++ code_prose (code_of_exp target)
        ++ text " be " ++ prose_of_exp exp_scrut)

(* Instructions *)

(* What a tier renderer produces.

   Composition with the enclosing head
   -- inline fold, goto fold + capitalize, or nesting --
   is centralized in [compose] *)

type rendered =
  | Inline of Adoc.prose
  | InlineGoto of Adoc.prose
  | Nested of Adoc.block

(* Renders a tier's own instruction: its shared node plus the tier payload *)

type 'instr_tier render_instr_tier =
  level:int ->
  ctx_fallthrough:Fallthrough.ctx ->
  singleton:bool ->
  'instr_tier instr ->
  'instr_tier ->
  rendered

let compose ~(block_head : Adoc.block option) ~(singleton : bool) :
    rendered -> Adoc.block = function
  | Inline prose -> (
      match block_head with
      | Some block_head ->
          Adoc.concat_block [ block_head; Adoc.inline_block prose ]
      | None -> Adoc.inline_block prose)
  | InlineGoto prose ->
      let bullet =
        match block_head with
        | Some block_head ->
            Adoc.concat_block [ block_head; Adoc.inline_block prose ]
        | None -> Adoc.inline_block prose
      in
      Adoc.capitalize_first_block bullet
  | Nested block when not singleton -> block
  | Nested block -> (
      match block_head with
      | Some block_head -> Adoc.seq_block [ block_head; block ]
      | None ->
          Adoc.concat_block [ Adoc.raw_block "\n"; Adoc.seq_block [ block ] ])

let rec render_instr ?(level : int = 0) ~(ctx_fallthrough : Fallthrough.ctx)
    (render_instr_tier : 'instr_tier render_instr_tier)
    (instr : 'instr_tier instr) : Adoc.block =
  match instr.node.it with
  | IfI (cond, iterexps, block_then, _) ->
      render_if_instr ~level ~ctx_fallthrough render_instr_tier instr cond
        iterexps block_then
  | HoldI (id_rel, notexp, iterexps, holdcase) ->
      render_hold_instr ~level ~ctx_fallthrough render_instr_tier instr
        instr.hints id_rel notexp iterexps holdcase
  | CaseI (exp_scrut, cases, dangle) ->
      render_case_instr ~level ~ctx_fallthrough render_instr_tier instr
        exp_scrut cases dangle
  | LetI (exp_l, exp_r, iterinstrs) ->
      render_let_instr ~level ~ctx_fallthrough instr exp_l exp_r iterinstrs
  | DebugI exp -> render_debug_instr ~level ~ctx_fallthrough instr exp
  | DestructI (fields, exp_source) ->
      render_destruct_instr ~level ~ctx_fallthrough instr fields exp_source
  | CheckLetSubI (_, _, exp_l, exp_r, block_inner)
  | CheckLetMatchI (_, exp_l, exp_r, block_inner) ->
      render_check_let_instr ~level ~ctx_fallthrough render_instr_tier instr
        exp_l exp_r block_inner
  | OptionGetI (exp_l, exp_r, block_inner) ->
      render_option_get_instr ~level ~ctx_fallthrough render_instr_tier instr
        exp_l exp_r block_inner
  | TierI instr_tier ->
      compose ~block_head:None ~singleton:false
        (render_instr_tier ~level ~ctx_fallthrough ~singleton:false instr
           instr_tier)

(* Instructions under an optional head; a lone tier may fold onto the head
   (via [render_instr_tier]), else nests below

     . If x holds: return y. *)

and render_instrs ?(level : int = 0) ?(block_head : Adoc.block option = None)
    ~(ctx_fallthrough : Fallthrough.ctx) render_instr_tier
    (instrs : 'instr_tier block) : Adoc.block =
  match instrs with
  | [
   ({ node = { it = TierI instr_tier; _ }; _ } as instr : 'instr_tier instr);
  ] ->
      compose ~block_head ~singleton:true
        (render_instr_tier ~level ~ctx_fallthrough ~singleton:true instr
           instr_tier)
  | _ -> (
      let blocks =
        List.map (render_instr ~level ~ctx_fallthrough render_instr_tier) instrs
      in
      match block_head with
      | Some block_head -> Adoc.seq_block (block_head :: blocks)
      | None -> Adoc.concat_block [ Adoc.raw_block "\n"; Adoc.seq_block blocks ]
      )

(* Relation/function fallback as a top-level bullet, tagged with the else anchor
   that [-> ⋅] labels link to; None / Some [] emit nothing

     . Otherwise: return false. *)

and render_elseblock ?(anchor_else : string option = None)
    ?(anchors : anchors = Adoc.subject_name)
    ~(ctx_fallthrough : Fallthrough.ctx) render_instr_tier
    (elseblock_opt : 'instr_tier block option) : string =
  match elseblock_opt with
  | None | Some [] -> ""
  | Some block ->
      let anchor_prose =
        match anchor_else with
        | Some a -> F.asprintf "+++<span id=\"%s\"></span>+++" a
        | None -> ""
      in
      "\n\n" ^ adoc_ordered_bullet 0 ^ anchor_prose ^ "Otherwise:"
      ^ Adoc.ser_block ~anchor:anchors
          (render_instrs ~level:1 ~ctx_fallthrough render_instr_tier block)

(* Iterations *)

(* Trailing "for all" clause of an iterated premise; empty with no iter vars

     , for all x in x^*^ *)

and prose_of_iterexp_suffix (iterexps : iterexp list) : Adoc.prose =
  let proses =
    List.concat_map
      (fun (iter, vars) -> List.map (prose_of_in_itervar iter) vars)
      iterexps
  in
  match proses with
  | [] -> Adoc.empty_prose
  | _ -> Adoc.(text ", for all " ++ prose_of_list proses)

(* Trailing "for each" clause of an iterated let/rule step; empty with no iter
   vars

     , for each x in x^*^ *)

and prose_of_iterinstr_suffix (iterinstrs : iterinstr list) : Adoc.prose =
  let proses =
    List.concat_map
      (fun (iter, vars_in, _) -> List.map (prose_of_in_itervar iter) vars_in)
      iterinstrs
  in
  match proses with
  | [] -> Adoc.empty_prose
  | _ -> Adoc.(text ", for each " ++ prose_of_list proses)

(* Body wrapped in nested "For each" open-blocks (innermost first); a level
   binding visible outputs appends "Let ... be the resulting ..."

     . For each x in x^*^:
       <body>
     . Let y^*^ be the resulting list. *)

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
        let prose_head =
          Adoc.(
            text "For each " ++ prose_of_in_itervars iter vars_in ++ text ":")
        in
        let prose_fallthrough =
          if outermost then prose_fallthrough else Adoc.empty_prose
        in
        let block_body =
          if vars_out_visible = [] then
            Adoc.concat_block
              [ Adoc.raw_block "+\n--\n"; block_inner; Adoc.raw_block "\n--\n" ]
          else
            let noun = string_of_iter iter in
            Adoc.concat_block
              [
                Adoc.raw_block "+\n--\n";
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
        Adoc.item_ordered_block ~level ~block_body prose_head
  in
  render ~outermost:true level (List.rev iterinstrs)

(* If (check) instruction

     . Check that x is equal to y. [FAIL]
     . Return true. *)

and render_if_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    render_instr_tier (instr : _ instr) (cond : exp) (iterexps : iterexp list)
    (block_then : 'instr_tier block) : Adoc.block =
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  let block_head =
    Adoc.item_ordered_block ~level
      Adoc.(
        text "Check that " ++ prose_of_exp cond
        ++ prose_of_iterexp_suffix iterexps
        ++ text "." ++ prose_fallthrough)
  in
  if block_then = [] then block_head
  else
    Adoc.seq_block
      (block_head
      :: List.map
           (render_instr ~level ~ctx_fallthrough render_instr_tier)
           block_then)

(* Hold instruction; BothH also emits the complementary "Else:" branch

     . If x is well-typed: [-> b]
       <block>
     . Else:
       <block> *)

and render_hold_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    render_instr_tier (instr : _ instr) (hints : Annot.hints) (id_rel : id)
    (notexp : notexp) (iterexps : iterexp list)
    (holdcase : 'instr_tier holdcase) : Adoc.block =
  let exps = Mixfix.args notexp in
  let hint_true = hints.prose_true in
  let hint_false = hints.prose_false in
  let iter_suffix = prose_of_iterexp_suffix iterexps in
  let prose_of_cond ~(hold : bool) : Adoc.prose =
    let hint_opt = if hold then hint_true else hint_false in
    let fallback_verb = if hold then " holds" else " does not hold" in
    match hint_opt with
    | Some hint ->
        Adoc.link_subject_prose
          (Adoc.Relation (string_of_relid id_rel))
          (alternate hint (reindent_lines ~level:0) prose_of_exp exps)
    | None ->
        Adoc.(
          link_subject_prose
            (Adoc.Relation (string_of_relid id_rel))
            (code_prose (code_of_notexp notexp))
          ++ text fallback_verb)
  in
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  let block_head ~(hold : bool) : Adoc.block =
    Adoc.item_ordered_block ~level
      Adoc.(
        text "If " ++ prose_of_cond ~hold ++ iter_suffix ++ text ":"
        ++ prose_fallthrough)
  in
  match holdcase with
  | HoldH (block, _dangle) ->
      let head = block_head ~hold:true in
      render_instrs ~block_head:(Some head) ~level:(level + 1) ~ctx_fallthrough
        render_instr_tier block
  | NotHoldH (block, _dangle) ->
      let head = block_head ~hold:false in
      render_instrs ~block_head:(Some head) ~level:(level + 1) ~ctx_fallthrough
        render_instr_tier block
  | BothH (block_hold, block_nothold) ->
      let head_hold = block_head ~hold:true in
      let head_else = Adoc.item_ordered_block ~level (Adoc.text "Else:") in
      Adoc.seq_block
        [
          render_instrs ~block_head:(Some head_hold) ~level:(level + 1)
            ~ctx_fallthrough render_instr_tier block_hold;
          render_instrs ~block_head:(Some head_else) ~level:(level + 1)
            ~ctx_fallthrough render_instr_tier block_nothold;
        ]

(* Case analysis: single case as a Check bullet, else an If/Else-if/Else ladder;
   a total analysis makes the last case "Else:"

     . If t matches pattern A: return 1.
     . Else if t matches pattern B: return 2.
     . Else: return 3. *)

and render_case_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    render_instr_tier (instr : _ instr) (exp_scrut : exp)
    (cases : 'instr_tier case list) (dangle : dangle) : Adoc.block =
  let total = not dangle in
  let n = List.length cases in
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  match cases with
  | [ (guard, block_then) ] ->
      let block_head =
        Adoc.item_ordered_block ~level
          Adoc.(
            text "Check that "
            ++ prose_of_guard exp_scrut guard
            ++ text "." ++ prose_fallthrough)
      in
      if block_then = [] then block_head
      else
        Adoc.seq_block
          (block_head
          :: List.map
               (render_instr ~level ~ctx_fallthrough render_instr_tier)
               block_then)
  | _ ->
      Adoc.seq_block
        (cases
        |> List.mapi (fun idx (guard, block_then) ->
               if idx = n - 1 && total then
                 let block_else =
                   Adoc.item_ordered_block ~level (Adoc.text "Else:")
                 in
                 match guard with
                 | CheckLetSubG _ | CheckLetMatchG _ ->
                     let prose_bind = prose_of_guard exp_scrut guard in
                     let block_bind =
                       Adoc.item_ordered_block ~level:(level + 1)
                         Adoc.(capitalize_first_prose prose_bind ++ text ".")
                     in
                     Adoc.seq_block
                       (block_else :: block_bind
                       :: List.map
                            (render_instr ~level:(level + 1) ~ctx_fallthrough
                               render_instr_tier)
                            block_then)
                 | _ ->
                     render_instrs ~block_head:(Some block_else)
                       ~level:(level + 1) ~ctx_fallthrough render_instr_tier
                       block_then
               else
                 let keyword = if idx = 0 then "If" else "Else if" in
                 let label =
                   if
                     Partial.is_partial_guard guard
                     || (idx = 0 && Partial.is_partial_exp exp_scrut)
                   then prose_fallthrough
                   else Adoc.empty_prose
                 in
                 let block_head =
                   Adoc.item_ordered_block ~level
                     Adoc.(
                       text (keyword ^ " ")
                       ++ prose_of_guard exp_scrut guard
                       ++ text ":" ++ label)
                 in
                 render_instrs ~block_head:(Some block_head) ~level:(level + 1)
                   ~ctx_fallthrough render_instr_tier block_then))

(* Cross-group edge, as an inline goto link into that group's dispatch anchor

     goto newTypeIR *)

and prose_of_group_dispatch (id_rel : id) (id_rulegroup : id) : Adoc.prose =
  let name = string_of_relid id_rulegroup in
  let target = Fallthrough.anchor_of_group (string_of_relid id_rel) name in
  Adoc.(text "goto " ++ link_prose ~target (text name))

(* Standalone dispatch goto as its own bullet, capitalized to lead the line

     . Goto newTypeIR *)

and render_group_instr_dispatch ~(level : int) (id_rel : id) (id_rulegroup : id)
    : Adoc.block =
  Adoc.item_ordered_block ~level
    (Adoc.capitalize_first_prose (prose_of_group_dispatch id_rel id_rulegroup))

(* Let binding; label present when the bound expression can backtrack

     . Let x be $f(y). [FAIL] *)

and render_let_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    (instr : _ instr) (exp_l : exp) (exp_r : exp) (iterinstrs : iterinstr list)
    : Adoc.block =
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  let vars_out_visible =
    iterinstrs
    |> List.concat_map (fun (_, _, vars_out) -> vars_out)
    |> List.filter (fun (id, _, _) -> not (Id.is_underscored id))
  in
  if vars_out_visible = [] then
    Adoc.item_ordered_block ~level
      Adoc.(
        text "Let "
        ++ code_prose (code_of_exp exp_l)
        ++ text " be " ++ prose_of_exp exp_r
        ++ prose_of_iterinstr_suffix iterinstrs
        ++ text "." ++ prose_fallthrough)
  else
    let render_body level =
      Adoc.item_unordered_block ~level
        Adoc.(
          text "Let "
          ++ code_prose (code_of_exp exp_l)
          ++ text " be " ++ prose_of_exp exp_r ++ text ".")
    in
    render_iterinstrs ~level ~prose_fallthrough iterinstrs render_body

(* Rule application (or bare "Let <rel>" without hints)

     . Let v be the result of evaluating e. [FAIL] *)

and render_rule_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    (instr : _ instr) (hints : Annot.hints) (id_rel : id) (notexp : notexp)
    (hint_input : Hints.Input.t) (iterinstrs : iterinstr list) : Adoc.block =
  let exps = Mixfix.args notexp in
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
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
          Adoc.link_subject_prose
            (Adoc.Relation (string_of_relid id_rel))
            (alternate hint_in unindent_lines prose_of_exp exps_in)
        in
        Adoc.(
          text "Let " ++ text prose_out ++ text " be the result of "
          ++ prose_in_typed)
    | _ ->
        Adoc.(
          text "Let "
          ++ link_subject_prose
               (Adoc.Relation (string_of_relid id_rel))
               (code_prose (code_of_notexp notexp)))
  in
  if vars_out_visible = [] then
    Adoc.item_ordered_block ~level
      Adoc.(
        rule_body
        ++ prose_of_iterinstr_suffix iterinstrs
        ++ text "." ++ prose_fallthrough)
  else
    let render_body level =
      Adoc.item_unordered_block ~level Adoc.(rule_body ++ text ".")
    in
    render_iterinstrs ~level ~prose_fallthrough iterinstrs render_body

(* Result clause of a relation

     the result is v.
     the relation holds.
     then, the relation holds. *)

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

(* Result instruction: prose_of_result as a bullet

     . The result is v. [FAIL] *)

and render_result_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    (instr : _ instr) (hints : Annot.hints) (rel_signature : rel_signature)
    (exps : exp list) : Adoc.block =
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  Adoc.item_ordered_block ~level
    Adoc.(
      capitalize_first_prose (prose_of_result hints rel_signature exps)
      ++ prose_fallthrough)

(* Return instruction; label present when the returned expression can backtrack

     . Return true. [FAIL] *)

and render_return_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    (instr : _ instr) (exp : exp) : Adoc.block =
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  Adoc.item_ordered_block ~level
    Adoc.(text "Return " ++ prose_of_exp exp ++ text "." ++ prose_fallthrough)

(* Debug instruction

     . (debug: x) *)

and render_debug_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    (instr : _ instr) (exp : exp) : Adoc.block =
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  Adoc.item_ordered_block ~level
    Adoc.(text "(debug: " ++ prose_of_exp exp ++ text ")" ++ prose_fallthrough)

(* Destruct instruction: named projections of a source value

     . Let k be the key of e.
     . Let k, v be the key, and the value of e. *)

and render_destruct_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    (instr : _ instr) (fields : (string option * exp) list) (exp_source : exp) :
    Adoc.block =
  let projections =
    List.filter_map
      (fun (name_opt, exp_target) ->
        Option.map (fun name -> (name, exp_target)) name_opt)
      fields
  in
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  let block_line = Adoc.item_ordered_block ~level in
  match projections with
  | [ (name, exp_target) ] ->
      block_line
        Adoc.(
          text "Let " ++ prose_of_exp exp_target
          ++ text (F.asprintf " be the %s of " name)
          ++ prose_of_exp exp_source ++ text "." ++ prose_fallthrough)
  | _ ->
      let names, exps_target = List.split projections in
      block_line
        Adoc.(
          text "Let " ++ prose_of_exps exps_target ++ text " be "
          ++ prose_of_list (List.map (fun s -> text ("the " ^ s)) names)
          ++ text " of " ++ prose_of_exp exp_source ++ text "."
          ++ prose_fallthrough)

(* Check-let instruction: a partial binding that may fail the match

     . Let!~type~ `A x` be e. [FAIL] *)

and render_check_let_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    render_instr_tier (instr : _ instr) (exp_l : exp) (exp_r : exp)
    (block_inner : 'instr_tier block) : Adoc.block =
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  let block_head =
    Adoc.item_ordered_block ~level
      Adoc.(
        text "Let!~type~ "
        ++ code_prose (code_of_exp exp_l)
        ++ text " be " ++ prose_of_exp exp_r ++ text "." ++ prose_fallthrough)
  in
  if block_inner = [] then block_head
  else
    Adoc.seq_block
      (block_head
      :: List.map
           (render_instr ~level ~ctx_fallthrough render_instr_tier)
           block_inner)

(* Option-get instruction: forces an option that may be none

     . Let x be *!* xs[0]. [FAIL] *)

and render_option_get_instr ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    render_instr_tier (instr : _ instr) (exp_l : exp) (exp_r : exp)
    (block_inner : 'instr_tier block) : Adoc.block =
  let prose_fallthrough =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  let block_head =
    Adoc.item_ordered_block ~level
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
      :: List.map
           (render_instr ~level ~ctx_fallthrough render_instr_tier)
           block_inner)

(* Relations *)

(* Lifts a synthesized SL output (a var, possibly iterated) into a PL exp for
   rendering in a relation title *)

and lift_synthesized_exp (exp : Sl.exp) : exp =
  let it' =
    match exp.it with
    | Il.VarE id -> VarE id
    | Il.IterE (exp_inner, (iter, vars)) ->
        IterE (lift_synthesized_exp exp_inner, (iter, vars))
    | _ -> assert false
  in
  Annot.no_hints (it' $$ (exp.at, exp.note))

(* Relation title, math form: mixfix with input holes filled, outputs as "%"

     |- e : % *)

and prose_of_rel_title_math (rel_signature : rel_signature) (exps : exp list) :
    Adoc.prose =
  let nottyp, inputs = rel_signature in
  let mixop = Mixfix.to_mixop nottyp.it in
  let dexps = List.map code_of_exp exps in
  let num_outputs = Mixop.arity mixop - List.length dexps in
  let code_holes = List.init num_outputs (fun _ -> Adoc.token "%") in
  let padded = Hints.Input.combine inputs dexps code_holes in
  Adoc.code_prose (code_of_mixfix ~atom:string_of_atom mixop padded)

(* Relation heading: linked name, then a prose statement chosen by hints (input
   +output, input-only, plain truth, or math fallback)

     Type:
     * e has type t *)

and render_rel_title_block (hints : Annot.hints) (id_rel : id)
    (rel_signature : rel_signature) (exps : exp list) : Adoc.block =
  let exps_in_title =
    match hints.prose_input_exps with
    | Some exps_in_sl -> List.map lift_synthesized_exp exps_in_sl
    | None -> exps
  in
  let prose_title =
    Adoc.link_subject_prose
      (Adoc.Relation (string_of_relid id_rel))
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
          Adoc.item_unordered_block ~level:0
            (alternate ~caps:true hint_in (reindent_lines ~level:1) prose_of_exp
               exps_in_title);
          Adoc.raw_block ":\n";
          Adoc.item_unordered_block ~level:0
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
          Adoc.item_unordered_block ~level:0
            (alternate ~caps:true hint_in (reindent_lines ~level:1) prose_of_exp
               exps_in_title);
          Adoc.raw_block ".";
        ]
  | _, _, _, Some hint_true ->
      Adoc.concat_block
        [
          block_title_header;
          Adoc.item_unordered_block ~level:0
            (alternate ~caps:true hint_true (reindent_lines ~level:0)
               prose_of_exp exps);
        ]
  | _ ->
      Adoc.inline_block
        (Adoc.link_subject_prose
           (Adoc.Relation (string_of_relid id_rel))
           Adoc.(
             text (Sl.Print.string_of_relid id_rel ^ ": ")
             ++ prose_of_rel_title_math rel_signature exps))

(* Serialized form of [render_rel_title_block]. *)

and render_rel_title_adoc ?(anchors = Adoc.subject_name) (hints : Annot.hints)
    (id_rel : id) (rel_signature : rel_signature) (exps : exp list) : string =
  Adoc.ser_block ~anchor:anchors
    (render_rel_title_block hints id_rel rel_signature exps)

(* Extern relations *)

(* Extern relation: title block only (defined outside the spec) *)

let render_extern_rel_def_block (hints : Annot.hints) (externrel : externrel) :
    Adoc.block =
  let id_rel, rel_signature, exps = externrel in
  render_rel_title_block hints id_rel rel_signature exps

(* Serialized form of [render_extern_rel_def_block]. *)

let render_extern_rel_def ?(anchors = Adoc.subject_name) (hints : Annot.hints)
    (externrel : externrel) : string =
  Adoc.ser_block ~anchor:anchors (render_extern_rel_def_block hints externrel)

(* Tier renderers -- each only decides the [rendered] shape; joining it to the
   enclosing head is [compose]'s job. *)

(* Shared backtracking scaffolding: each arm's head, anchor, and fallthrough
   (next arm, last inheriting the ambient one). How an arm body renders is left
   to [render_arm_body]. Used by [BacktrackI] and [RouteI]. *)

let render_block_arms ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    (render_arm_body : ctx_fallthrough:Fallthrough.ctx -> 'arm -> Adoc.block)
    (arms : 'arm list) : Adoc.block =
  let anchor_block = Fallthrough.fresh_block_anchor ctx_fallthrough.namespace in
  let count_arm = List.length arms in
  let render_arm idx_arm arm =
    let anchor_next_opt =
      if idx_arm + 1 < count_arm then
        Some (Fallthrough.anchor_of_arm anchor_block (idx_arm + 1))
      else ctx_fallthrough.next
    in
    let ctx_fallthrough = { ctx_fallthrough with next = anchor_next_opt } in
    let anchor_arm = Fallthrough.anchor_of_arm anchor_block idx_arm in
    let prose_head =
      if idx_arm = 0 then Adoc.text "Try:" else Adoc.text "Then, try:"
    in
    let block_body = render_arm_body ~ctx_fallthrough arm in
    Adoc.item_ordered_block ~level ~anchor:anchor_arm ~block_body prose_head
  in
  Adoc.seq_block (List.mapi render_arm arms)

(* Group-body tier: result/return/rule, or a backtracking [.bk-arm] block. A
   short return/result in a singleton block folds inline onto the head. *)

let rec render_instr_group ~(level : int) ~(ctx_fallthrough : Fallthrough.ctx)
    ~(singleton : bool) (instr : instr_group instr) (instr_group : instr_group)
    : rendered =
  let hints = instr.hints in
  let prose_fallthrough () =
    Fallthrough.prose_of_fallthrough_link ~ctx_fallthrough instr
  in
  match instr_group with
  | ReturnI exp
    when singleton && Adoc.width_prose (prose_of_exp exp) <= adoc_width_short ->
      Inline
        Adoc.(
          text " return " ++ prose_of_exp exp ++ text "."
          ++ prose_fallthrough ())
  | ResultI (rel_signature, exps)
    when singleton
         && Adoc.width_prose (prose_of_result hints rel_signature exps)
            <= adoc_width_short ->
      Inline
        Adoc.(
          text " "
          ++ prose_of_result hints rel_signature exps
          ++ prose_fallthrough ())
  | ResultI (rel_signature, exps) ->
      Nested
        (render_result_instr ~level ~ctx_fallthrough instr hints rel_signature
           exps)
  | ReturnI exp ->
      Nested (render_return_instr ~level ~ctx_fallthrough instr exp)
  | RuleI (id_rel, notexp, hint_input, iterinstrs) ->
      Nested
        (render_rule_instr ~level ~ctx_fallthrough instr hints id_rel notexp
           hint_input iterinstrs)
  | BacktrackI arms ->
      let level_body = level + 1 in
      Nested
        (render_block_arms ~level ~ctx_fallthrough
           (fun ~ctx_fallthrough arm ->
             Adoc.seq_block
               (List.map
                  (render_instr ~level:level_body ~ctx_fallthrough
                     render_instr_group)
                  arm))
           arms)

(* Dispatch tier: a rule group as a goto-xref link, or a routing block whose
   arms are rendered inline (leading to goto edges). *)

let rec render_instr_dispatch ~(level : int)
    ~(ctx_fallthrough : Fallthrough.ctx) ~(singleton : bool)
    (_instr : instr_dispatch instr) (instr_dispatch : instr_dispatch) : rendered
    =
  match instr_dispatch with
  | GroupI (id_rulegroup, id_rel, _, _, _) ->
      if singleton then
        InlineGoto
          Adoc.(text " " ++ prose_of_group_dispatch id_rel id_rulegroup)
      else Nested (render_group_instr_dispatch ~level id_rel id_rulegroup)
  | RouteI arms ->
      let level_body = level + 1 in
      Nested
        (render_block_arms ~level ~ctx_fallthrough
           (fun ~ctx_fallthrough arm ->
             Adoc.seq_block
               (List.map
                  (render_instr ~level:level_body ~ctx_fallthrough
                     render_instr_dispatch)
                  arm))
           arms)

(* Dispatch tier, inline mode: a group's own title + body, used for the rel
   elseblock where the else-group shows its body rather than a goto link. *)

let render_instr_dispatch_inline ~(level : int)
    ~(ctx_fallthrough : Fallthrough.ctx) ~(singleton : bool)
    (instr : instr_dispatch instr) (instr_dispatch : instr_dispatch) : rendered
    =
  match instr_dispatch with
  | GroupI (_id_rulegroup, id_rel, rel_signature, exps, block) ->
      let hints = instr.hints in
      let hint_in = hints.prose_in in
      let hint_true = hints.prose_true in
      let prose_title =
        match (hint_in, hint_true) with
        | Some hint, _ | _, Some hint ->
            Adoc.link_subject_prose
              (Adoc.Relation (string_of_relid id_rel))
              (alternate ~caps:true hint (reindent_lines ~level:0) prose_of_exp
                 exps)
        | None, None ->
            Adoc.link_subject_prose
              (Adoc.Relation (string_of_relid id_rel))
              (prose_of_rel_title_math rel_signature exps)
      in
      let block_head_title =
        Adoc.item_ordered_block ~level Adoc.(prose_title ++ text ":")
      in
      Nested
        (render_instrs ~block_head:(Some block_head_title) ~level:(level + 1)
           ~ctx_fallthrough render_instr_group block)
  | RouteI _ ->
      render_instr_dispatch ~level ~ctx_fallthrough ~singleton instr
        instr_dispatch

(* Defined relations *)

(* One rule group of a defined relation: title line, then group body; the unit
   the rulegroup splicer emits into the doc

     x reduces to v:
       <arms> *)

let render_rulegroup ?(anchors = Adoc.subject_name) (hints : Annot.hints)
    (_id_rulegroup : id) (id_rel : id) (rel_signature : rel_signature)
    (exps : exp list) (block : block_group) : string =
  let hint_in = hints.prose_in in
  let hint_true = hints.prose_true in
  let title =
    match (hint_in, hint_true) with
    | Some hint, _ | _, Some hint ->
        Adoc.ser_prose ~anchor:anchors
          (Adoc.link_subject_prose
             (Adoc.Relation (string_of_relid id_rel))
             (alternate ~caps:true hint (reindent_lines ~level:0) prose_of_exp
                exps))
    | None, None ->
        Adoc.ser_prose ~anchor:anchors
          (Adoc.link_subject_prose
             (Adoc.Relation (string_of_relid id_rel))
             (prose_of_rel_title_math rel_signature exps))
  in
  let ctx_fallthrough =
    Fallthrough.{ namespace = string_of_relid id_rel; next = None }
  in
  let body = render_instrs ~ctx_fallthrough render_instr_group block in
  title ^ ":\n" ^ Adoc.ser_block ~anchor:anchors body

let render_rulegroup_else ?(anchors : anchors = Adoc.subject_name) (id_rel : id)
    (elseblock : block_dispatch) : string =
  render_elseblock
    ~anchor_else:(Some (Fallthrough.anchor_of_else (string_of_relid id_rel)))
    ~anchors
    ~ctx_fallthrough:
      Fallthrough.{ namespace = string_of_relid id_rel; next = None }
    render_instr_dispatch_inline (Some elseblock)
  |> String.trim

(* Dispatch tree of a defined relation: block rendered as goto edges between
   groups

     Type dispatch:
       <goto tree> *)

let render_defined_rel_def_dispatch ?(anchors = Adoc.subject_name)
    ((id_rel, _rel_signature, _exps, block, _elseblock_opt) : rel) : string =
  let ctx_fallthrough =
    Fallthrough.{ namespace = string_of_relid id_rel; next = None }
  in
  string_of_relid id_rel ^ " dispatch:\n"
  ^ Adoc.ser_block ~anchor:anchors
      (render_instrs ~level:0 ~ctx_fallthrough render_instr_dispatch block)

(* Full defined relation: title, rule groups in order, "Otherwise" fallback
   (when a non-empty else block exists), then the dispatch tree

     <title>
     <group 1> ... <group n>
     . Otherwise: ...
     Type dispatch: ... *)

let render_defined_rel_def_block (hints : Annot.hints) (rel : rel) : Adoc.block
    =
  let id_rel, rel_signature, exps, block, elseblock_opt = rel in
  let has_elseblock =
    match elseblock_opt with Some (_ :: _) -> true | _ -> false
  in
  let groups = block |> Group.collect_groups in
  let anchor_else =
    if has_elseblock then
      Some (Fallthrough.anchor_of_else (string_of_relid id_rel))
    else None
  in
  Adoc.concat_block
    [
      render_rel_title_block hints id_rel rel_signature exps;
      Adoc.raw_block "\n\n";
      Adoc.raw_block
        (groups
        |> List.map (fun (group : Group.t) ->
               render_rulegroup group.hints group.id_rulegroup group.id_rel
                 group.rel_signature group.exps group.body)
        |> String.concat "\n\n");
      Adoc.raw_block
        (render_elseblock ~anchor_else
           ~ctx_fallthrough:
             Fallthrough.{ namespace = string_of_relid id_rel; next = None }
           render_instr_dispatch_inline elseblock_opt);
      Adoc.raw_block ("\n\n" ^ render_defined_rel_def_dispatch rel);
    ]

(* Serialized form of [render_defined_rel_def_block]. *)

let render_defined_rel_def (hints : Annot.hints) (rel : rel) : string =
  Adoc.ser_block (render_defined_rel_def_block hints rel)

(* Functions *)

(* Function title: linked name, then prose input phrase (hinted) or signature

     $f:
     * the lookup of x in g
     $f(g, x) *)

let render_func_title_block (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : Adoc.block =
  let prose_title =
    Adoc.link_subject_prose (Adoc.Function id_func.it)
      (Adoc.text (string_of_defid id_func))
  in
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      Adoc.concat_block
        [
          Adoc.inline_block Adoc.(prose_title ++ text ":");
          Adoc.raw_block "\n\n";
          Adoc.item_unordered_block ~level:0
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

(* Serialized form of [render_func_title_block]. *)

let render_func_title ?(anchors = Adoc.subject_name) (hints : Annot.hints)
    (id_func : id) (tparams : tparam list) (params : param list) : string =
  Adoc.ser_block ~anchor:anchors
    (render_func_title_block hints id_func tparams params)

(* Function header: the function inline as one linked phrase (hinted phrase or
   signature); lead-in before a body or table

     $f(g, x) *)

let render_func_header_block (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : Adoc.block =
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      Adoc.inline_block
        (Adoc.link_subject_prose (Adoc.Function id_func.it)
           (Adoc.text
              (Adoc.ser_prose
                 (alternate ~caps:true hint (reindent_lines ~level:0)
                    prose_of_param params))))
  | None, None ->
      Adoc.inline_block
        (Adoc.link_subject_prose (Adoc.Function id_func.it)
           (Adoc.text
              (string_of_defid id_func
              ^ Sl.Print.string_of_tparams tparams
              ^ Adoc.ser_code (code_of_params params))))

(* Serialized form of [render_func_header_block]. *)

let render_func_header ?(anchors = Adoc.subject_name) (hints : Annot.hints)
    (id_func : id) (tparams : tparam list) (params : param list) : string =
  Adoc.ser_block ~anchor:anchors
    (render_func_header_block hints id_func tparams params)

(* Extern functions *)

(* Extern function: header only (body defined outside the spec) *)

let render_extern_func_def ?(anchors = Adoc.subject_name) (hints : Annot.hints)
    (externfunc : externfunc) : string =
  let id_func, tparams, params, _ = externfunc in
  render_func_header ~anchors hints id_func tparams params

(* Builtin functions *)

(* Builtin function: header only (implemented by the interpreter) *)

let render_builtin_func_def ?(anchors = Adoc.subject_name) (hints : Annot.hints)
    (builtinfunc : builtinfunc) : string =
  let id_func, tparams, params, _ = builtinfunc in
  render_func_header ~anchors hints id_func tparams params

(* Table functions *)

(* Table function: header, then a table mapping argument tuples to results

     $f:
     | x | Result |
     | 0 | a      | *)

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

(* Serialized form of [render_table_func_def_block]. *)

let render_table_func_def ?(anchors = Adoc.subject_name) (hints : Annot.hints)
    (tablefunc : tablefunc) : string =
  Adoc.ser_block ~anchor:anchors (render_table_func_def_block hints tablefunc)

(* Defined functions *)

(* Full defined function: header, body, "Otherwise" fallback; a lone boolean
   Return folds inline, a lone Block renders its arms

     $f(x)
     . Check that ...
     . Return true.
     . Otherwise: return false. *)

let render_defined_func_def_block (hints : Annot.hints) (func : definedfunc) :
    Adoc.block =
  let id_func, tparams, params, _typ, block, elseblock_opt = func in
  let has_elseblock =
    match elseblock_opt with Some (_ :: _) -> true | _ -> false
  in
  let ctx_fallthrough = Fallthrough.{ namespace = id_func.it; next = None } in
  let block_body, anchor_else =
    match block with
    | [
     {
       node =
         { it = TierI (ReturnI ({ node = { it = BoolE _; _ }; _ } as e)); _ };
       _;
     };
    ] ->
        ( Adoc.inline_block
            Adoc.(text " return " ++ code_prose (code_of_exp e) ++ text "."),
          None )
    | [ ({ node = { it = TierI (BacktrackI _); _ }; _ } as instr) ]
      when has_elseblock ->
        let anchor_else = Fallthrough.anchor_of_else id_func.it in
        ( render_instr ~level:0 ~ctx_fallthrough render_instr_group instr,
          Some anchor_else )
    | _ ->
        let anchor_else =
          if has_elseblock then Some (Fallthrough.anchor_of_else id_func.it)
          else None
        in
        ( Adoc.seq_block
            (List.map
               (render_instr ~level:0 ~ctx_fallthrough render_instr_group)
               block),
          anchor_else )
  in
  Adoc.concat_block
    [
      render_func_header_block hints id_func tparams params;
      Adoc.raw_block "\n\n";
      block_body;
      Adoc.raw_block
        (render_elseblock ~anchor_else ~ctx_fallthrough render_instr_group
           elseblock_opt);
    ]

(* Serialized form of [render_defined_func_def_block]. *)

let render_defined_func_def ?(anchors = Adoc.subject_name) (hints : Annot.hints)
    (func : definedfunc) : string =
  Adoc.ser_block ~anchor:anchors (render_defined_func_def_block hints func)

(* Definitions *)

(* Renders one top-level definition to its doc string, dispatching by kind;
   type/var declarations render nothing (None). *)

let render_def (def : def) : string option =
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

(* Renders every definition, dropping the empty ones, joined by blank lines. *)

let render_defs (defs : def list) : string =
  defs |> List.filter_map render_def |> String.concat "\n\n"

(* Spec *)

(* Entry point: renders a whole spec. *)

let render_spec (spec : spec) : string = render_defs spec
