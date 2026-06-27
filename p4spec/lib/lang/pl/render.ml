open Domain
open Lib
open Xl
open Ast
open Util.Source
module F = Format

(* Asciidoc rendering context *)

module Ctx = struct
  type t = { in_code : bool; in_link : bool }

  let in_prose = { in_code = false; in_link = false }
  let in_code = { in_code = true; in_link = false }
  let in_link = { in_code = false; in_link = true }
  let code (context : t) = { context with in_code = true }
  let link (context : t) = { context with in_link = true }
end

open Ctx

(* Backtrack utils *)

module Backtrack = struct
  (* Namespaced anchor ids and display labels for backtracking blocks *)

  module BlockLabel : sig
    type t = { id : string; display : string }

    val set_namespace : string -> unit
    val fresh : unit -> t
  end = struct
    type t = { id : string; display : string }

    (* Current namespace prefix for generated ids *)

    let namespace : string option ref = ref None

    (* Per-namespace counter for unique label numbers *)

    let counters : (string, int) Hashtbl.t = Hashtbl.create 64

    (* Set the namespace prefix for ids generated afterwards *)

    let set_namespace (namespace_ : string) = namespace := Some namespace_

    (* Fresh label, counted within the current namespace *)

    let fresh () =
      let namespace = !namespace in
      let key = Option.value namespace ~default:"" in
      let name = (try Hashtbl.find counters key with Not_found -> 0) + 1 in
      Hashtbl.replace counters key name;
      let id =
        match namespace with
        | None -> F.asprintf "bk-%d" name
        | Some namespace -> F.asprintf "bk-%s-%d" namespace name
      in
      { id; display = F.asprintf "#%d" name }
  end

  (* Where a failed instruction backtracks to: the next arm, or out of the block *)

  type target = NextArm of string | OutOfBlock

  (* A backtracking block paired with the target of its current arm *)

  type ctx = { block : BlockLabel.t; target : target }

  (* Asciidoctor ordered-list styles, cycled by nesting depth. *)

  type ordered_style =
    | Arabic
    | Loweralpha
    | Lowerroman
    | Upperalpha
    | Upperroman

  (* The list style asciidoctor uses at the given nesting depth *)

  let style_at_level (level : int) : ordered_style =
    let cycle = [| Arabic; Loweralpha; Lowerroman; Upperalpha; Upperroman |] in
    cycle.(((level mod 5) + 5) mod 5)

  (* The arm's list marker as asciidoctor renders it at this level and index *)

  let arm_letter (level : int) (idx : int) : string =
    let to_roman ?(upper = false) (n : int) =
      let units =
        [| ""; "i"; "ii"; "iii"; "iv"; "v"; "vi"; "vii"; "viii"; "ix" |]
      in
      let tens =
        [| ""; "x"; "xx"; "xxx"; "xl"; "l"; "lx"; "lxx"; "lxxx"; "xc" |]
      in
      let n = max 1 n in
      let s = tens.(n / 10 mod 10) ^ units.(n mod 10) in
      if upper then String.uppercase_ascii s else s
    in
    let n = idx + 1 in
    match style_at_level level with
    | Arabic -> string_of_int n
    | Loweralpha when idx < 26 -> String.make 1 (Char.chr (Char.code 'a' + idx))
    | Upperalpha when idx < 26 -> String.make 1 (Char.chr (Char.code 'A' + idx))
    | Lowerroman -> to_roman n
    | Upperroman -> to_roman ~upper:true n
    | Loweralpha | Upperalpha -> F.asprintf "arm%d" n

  (* Trails the bullet's first line, which must start with plain text *)

  let render_block_label (block : BlockLabel.t) : string =
    F.asprintf "pass:[<strong id=\"%s\" class=\"bk-label\">%s</strong>]"
      block.id block.display

  (* +++...+++, not pass:[...]: content has literal '[' / ']' *)

  let render_fallthrough_link (backtrack : ctx option) : string =
    match backtrack with
    | None -> ""
    | Some { block; target } ->
        let text, id_target =
          match target with
          | NextArm letter ->
              ( F.asprintf "else %s-%s" block.display letter,
                F.asprintf "%s-%s" block.id letter )
          | OutOfBlock -> (F.asprintf "fail %s" block.display, block.id)
        in
        F.asprintf "+++<sub class=\"bk-mark\">[<a href=\"#%s\">%s</a>]</sub>+++"
          id_target text

  (* Backtrack target for arm [idx]: the next arm, or out of the block *)

  let arm_backtrack_ctx ~(block : BlockLabel.t) ~(level_arm : int)
      ~(total : int) (idx : int) : ctx =
    let target =
      if idx + 1 < total then NextArm (arm_letter level_arm (idx + 1))
      else OutOfBlock
    in
    { block; target }

  (* Trails the arm's first line; bk-arm-anchor sets scroll-margin-top so
     fragment links land on the arm header *)

  let arm_anchor ~(block : BlockLabel.t) ~(level_arm : int) (idx : int) : string
      =
    F.asprintf "+++<span class=\"bk-arm-anchor\" id=\"%s-%s\"></span>+++"
      block.id (arm_letter level_arm idx)
end

(* Asciidoc utils *)

let rec adoc_escape (c : char) (text : string) =
  match String.index_opt text c with
  | None -> text
  | Some idx ->
      let text_before = String.sub text 0 idx in
      let text_after =
        String.sub text (idx + 1) (String.length text - idx - 1)
      in
      text_before ^ "+" ^ String.make 1 c ^ "+" ^ adoc_escape c text_after

let adoc_width_short = 30
let adoc_fits_in_width_short (s : string) = String.length s <= adoc_width_short
let adoc_subscript (s : string) = "~" ^ s ^ "~"
let adoc_superscript (s : string) = "^" ^ s ^ "^"
let adoc_bold (s : string) = "*" ^ s ^ "*"
let adoc_mono (s : string) = "``" ^ s ^ "``"

let adoc_mono_chopped (s : string) =
  s |> String.split_on_char ' ' |> List.map adoc_mono |> String.concat " "

let adoc_as_code (ctx : t) (s : string) : string =
  if ctx.in_code then s else adoc_mono_chopped s

let adoc_ordered_bullet (level : int) =
  Format.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '.')

let adoc_unordered_bullet (level : int) =
  Format.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '*')

let adoc_link ~(link : string) (text : string) : string =
  let brackets = String.contains text '[' || String.contains text ']' in
  let angles = String.contains text '<' || String.contains text '>' in
  match (brackets, angles) with
  | false, false | false, true -> "xref:" ^ link ^ "[" ^ text ^ "]"
  | true, false -> "<<" ^ link ^ "," ^ text ^ ">>"
  | true, true ->
      Format.eprintf
        "Warning: Asciidoc link text contains both brackets and angle \
         brackets. Link may not render correctly.\n\
         \t%s\n"
        text;
      text

let adoc_as_link (ctx : t) ~link (s : string) : string =
  if ctx.in_link then s else adoc_link ~link s

let adoc_attach_block = "+\n"
let adoc_open_block (s : string) = F.asprintf "--\n%s\n--" s

let reindent_lines ?(level = 0) (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n" ^ adoc_unordered_bullet level) lines

let unindent_lines (s : string) : string =
  s |> String.split_on_char '\n' |> String.concat ""

(* Render utils *)

let render_list (items : string list) : string =
  match items with
  | [] -> ""
  | [ item ] -> item
  | [ item_a; item_b ] -> item_a ^ " and " ^ item_b
  | _ ->
      let items_rev = List.rev items in
      let items, item_last =
        (items_rev |> List.tl |> List.rev, items_rev |> List.hd)
      in
      String.concat ", " items ^ ", and " ^ item_last

let render_alter_hint ?(caps = false) (ctx : t) (hints : Hints.Alter.t)
    (render_base : string -> string) (render : t -> 'a -> string)
    (items : 'a list) : string =
  let render_atom (atom : atom) : string =
    "+" ^ (atom.it |> Atom.string_of_atom) ^ "+" |> adoc_as_code ctx
  in
  items
  |> Hints.Alter.alternate ~base_text:render_base ~base_atom:render_atom hints
       (fun a -> render ctx a)
  |> fun s -> if caps then String.capitalize_ascii s else s

(* Numbers *)

let string_of_num (num : num) = Il.Print.string_of_num num

(* Texts *)

let string_of_text (text : text) = Il.Print.string_of_text text

(* Identifiers *)

let string_of_varid (varid : id) = Il.Print.string_of_varid varid
let string_of_relid (relid : id) = Il.Print.string_of_relid relid

let string_of_defid ?(link = false) (defid : id) =
  if link then Il.Print.string_of_varid defid
  else Il.Print.string_of_defid defid

let render_varid (ctx : t) (id_var : id) =
  if Id.is_underscored id_var then "++_++" |> adoc_as_code ctx
  else
    let var_slices = String.split_on_char '_' id_var.it in
    match var_slices with
    | [] -> assert false
    | [ var_type ] -> var_type |> adoc_as_code ctx
    | var_type :: var_subscripts ->
        var_type ^ (var_subscripts |> String.concat "_" |> adoc_subscript)
        |> adoc_as_code ctx

(* Atoms *)

let code_of_atom (atom : atom) =
  match atom.it with
  | Atom.Tick -> ""
  | _ -> "+" ^ Atom.string_of_atom atom.it ^ "+"

let code_of_atoms (atoms : atom list) =
  atoms |> List.map code_of_atom |> String.concat " "

(* Mixfix operators *)

let code_of_mixop (mixop : mixop) =
  let arity = Mixop.arity mixop in
  let placeholders = List.init arity (fun _ -> "%") in
  Mixop.assemble ~string_of_atom:code_of_atom mixop placeholders |> String.trim

(* Iterators *)

let code_of_iter (iter : iter) =
  match iter with
  | List -> "{asterisk}" |> adoc_superscript
  | Opt -> "?" |> adoc_superscript

let code_of_iterexp ((iter, _) : iterexp) = code_of_iter iter

(* Variables *)

let render_var (ctx : t) ((id, _typ, iters) : var) =
  if Id.is_underscored id then "++_++" |> adoc_as_code ctx
  else render_varid ctx id ^ String.concat "" (List.map code_of_iter iters)

let render_in_itervars (ctx : t) (vars : var list) : string =
  let render_in_var var =
    F.asprintf "%s in %s"
      (render_var in_code var |> adoc_as_code ctx)
      (render_var in_code var ^ code_of_iter List |> adoc_as_code ctx)
  in
  vars |> List.map render_in_var |> render_list

let render_out_itervars (ctx : t) (vars : var list) : string =
  vars
  |> List.filter_map (fun var ->
         let id, _, _ = var in
         if Id.is_underscored id then None
         else
           Some
             (F.asprintf "%s be the list"
                (render_var in_code var ^ code_of_iter List |> adoc_as_code ctx)))
  |> render_list

(* Types *)

let code_of_typ (ctx : t) (typ : typ) : string =
  Sl.Print.string_of_typ typ |> adoc_as_code ctx

let tid_of_typ (typ' : typ') : id option =
  match typ' with Il.VarT (id, _) -> Some id | _ -> None

(* Operators *)

let render_unop = Sl.Print.string_of_unop

let render_binop (ctx : t) (binop : binop) =
  if ctx.in_code then Sl.Print.string_of_binop binop
  else
    match binop with
    | `AndOp -> "and"
    | `OrOp -> "or"
    | `ImplOp -> "implies"
    | `EquivOp -> "is equivalent to"
    | _ -> Sl.Print.string_of_binop binop

let render_cmpop (ctx : t) (cmpop : cmpop) =
  if ctx.in_code then Sl.Print.string_of_cmpop cmpop
  else
    match cmpop with
    | `EqOp -> "is equal to"
    | `NeOp -> "is not equal to"
    | `LtOp -> "is less than"
    | `GtOp -> "is greater than"
    | `LeOp -> "is less than or equal to"
    | `GeOp -> "is greater than or equal to"

(* Expressions *)

let rec render_exp (ctx : t) (exp : exp) : string =
  match exp.node.it with
  | BoolE b -> render_bool_exp ctx b
  | NumE n -> render_num_exp ctx n
  | TextE text -> render_text_exp ctx text
  | VarE id_var -> render_var_exp ctx id_var
  | UnE (unop, _, exp_inner) -> render_un_exp ctx unop exp_inner
  | BinE (binop, _, exp_l, exp_r) -> render_bin_exp ctx binop exp_l exp_r
  | CmpE (cmpop, _, exp_l, exp_r) -> render_cmp_exp ctx cmpop exp_l exp_r
  | UpCastE (_typ, exp_inner) | DownCastE (_typ, exp_inner) ->
      render_cast_exp ctx exp_inner
  | SubE (exp_inner, typ) -> render_sub_exp ctx exp_inner typ
  | MatchE (exp_inner, pattern) -> render_match_exp ctx exp_inner pattern
  | TupleE exps -> render_tuple_exp ctx exps
  | CaseE notexp -> render_case_exp ctx exp notexp
  | StrE expfields -> render_str_exp ctx expfields
  | OptE exp_opt -> render_opt_exp ctx exp_opt
  | ListE exps -> render_list_exp ctx exps
  | ConsE (exp_h, exp_t) -> render_cons_exp ctx exp_h exp_t
  | CatE (exp_l, exp_r) -> render_cat_exp ctx exp_l exp_r
  | MemE (exp_e, exp_s) -> render_mem_exp ctx exp_e exp_s
  | LenE exp_inner -> render_len_exp ctx exp_inner
  | DotE (exp_b, atom) -> render_dot_exp ctx exp_b atom
  | IdxE (exp_b, exp_i) -> render_idx_exp ctx exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> render_slice_exp ctx exp_b exp_l exp_h
  | UpdE (exp_b, path, exp_f) -> render_upd_exp ctx exp_b path exp_f
  | CallE (id, targs, args) -> render_call_exp ctx exp id targs args
  | IterE (exp_inner, iterexp) -> render_iter_exp ctx exp_inner iterexp

and render_exps (ctx : t) ?(sep : string option) (exps : exp list) =
  match (ctx.in_code, sep) with
  | _, Some s -> String.concat s (List.map (render_exp ctx) exps)
  | true, None -> String.concat ", " (List.map (render_exp ctx) exps)
  | false, None -> render_list (List.map (render_exp ctx) exps)

and render_exp_as_code (ctx : t) (exp : exp) =
  render_exp (code ctx) exp |> adoc_as_code ctx

and code_of_notexp (ctx : t) (notexp : notexp) =
  let mixop, exps = Mixfix.split notexp in
  let sexps = List.map (render_exp in_code) exps in
  Mixop.assemble ~string_of_atom:code_of_atom mixop sexps |> adoc_as_code ctx

(* Boolean expression rendering *)

and render_bool_exp (ctx : t) (b : bool) : string =
  string_of_bool b |> adoc_as_code ctx

(* Numeric expression rendering *)

and render_num_exp (ctx : t) (n : num) : string =
  string_of_num n |> adoc_as_code ctx

(* Text expression rendering *)

and render_text_exp (ctx : t) (text : string) : string =
  "\"" ^ String.escaped text ^ "\"" |> adoc_as_code ctx

(* Variable expression rendering *)

and render_var_exp (ctx : t) (id_var : id) : string =
  render_varid (code ctx) id_var |> adoc_as_code ctx

(* Unary expression rendering *)

and render_negated_exp_opt (ctx : t) (exp_inner : exp) : string option =
  match exp_inner.node.it with
  | MatchE (exp_e, pattern) ->
      Some
        (F.asprintf "%s does not match pattern %s" (render_exp ctx exp_e)
           (code_of_pattern pattern |> adoc_as_code ctx))
  | SubE (exp_e, typ) ->
      Some
        (F.asprintf "%s does not have type %s"
           (render_exp_as_code ctx exp_e)
           (code_of_typ ctx typ))
  | MemE (exp_e, exp_s) ->
      Some
        (F.asprintf "%s is not in %s"
           (render_exp_as_code ctx exp_e)
           (render_exp_as_code ctx exp_s))
  | CallE (id, _targs, args) when not ctx.in_code -> (
      let hint_false_opt = exp_inner.hints.Annot.prose_false in
      match hint_false_opt with
      | Some hints ->
          Some
            (render_alter_hint (link ctx) hints (reindent_lines ~level:0)
               render_arg args
            |> adoc_as_link ctx ~link:id.it)
      | None ->
          Some
            (render_unop `NotOp ^ render_exp (code ctx) exp_inner
            |> adoc_as_code ctx))
  | _ -> None

and render_un_exp (ctx : t) (unop : unop) (exp_inner : exp) : string =
  match unop with
  | #Bool.unop -> (
      match render_negated_exp_opt ctx exp_inner with
      | Some s -> s
      | None ->
          render_unop unop ^ render_exp (code ctx) exp_inner |> adoc_as_code ctx
      )
  | _ -> render_unop unop ^ render_exp (code ctx) exp_inner |> adoc_as_code ctx

(* Binary expression rendering *)

and render_bin_exp (ctx : t) (binop : binop) (exp_l : exp) (exp_r : exp) :
    string =
  match binop with
  | `ImplOp when not ctx.in_code ->
      "if " ^ render_exp ctx exp_l ^ ", then " ^ render_exp ctx exp_r
  | #Bool.binop ->
      render_exp ctx exp_l ^ " " ^ render_binop ctx binop ^ " "
      ^ render_exp ctx exp_r
  | #Num.binop ->
      render_exp (code ctx) exp_l
      ^ " "
      ^ render_binop (code ctx) binop
      ^ " "
      ^ render_exp (code ctx) exp_r
      |> adoc_as_code ctx

(* Comparison expression rendering *)

and render_cmp_exp (ctx : t) (cmpop : cmpop) (exp_l : exp) (exp_r : exp) :
    string =
  render_exp ctx exp_l ^ " " ^ render_cmpop ctx cmpop ^ " "
  ^ render_exp ctx exp_r

(* Cast expression rendering *)

and render_cast_exp (ctx : t) (exp_inner : exp) : string =
  render_exp_as_code ctx exp_inner

(* Subtype check expression rendering *)

and render_sub_exp (ctx : t) (exp_inner : exp) (typ : typ) : string =
  F.asprintf "%s has type %s"
    (render_exp_as_code ctx exp_inner)
    (code_of_typ ctx typ)

(* Pattern match check expression rendering *)

and render_match_exp (ctx : t) (exp_inner : exp) (pattern : pattern) : string =
  match pattern with
  | Il.CaseP mixop when Mixop.arity mixop = 0 ->
      F.asprintf "%s is %s" (render_exp ctx exp_inner)
        (code_of_pattern (Il.CaseP mixop) |> adoc_as_code ctx)
  | Il.ListP `Nil -> F.asprintf "%s is an empty list" (render_exp ctx exp_inner)
  | Il.ListP `Cons ->
      F.asprintf "%s is a non-empty list" (render_exp ctx exp_inner)
  | Il.ListP (`Fixed len) ->
      F.asprintf "%s is a list of length %d" (render_exp ctx exp_inner) len
  | Il.OptP `None -> F.asprintf "%s is none" (render_exp ctx exp_inner)
  | Il.OptP `Some -> F.asprintf "%s is defined" (render_exp ctx exp_inner)
  | pattern ->
      F.asprintf "%s matches pattern %s" (render_exp ctx exp_inner)
        (code_of_pattern pattern |> adoc_as_code ctx)

(* Tuple expression rendering *)

and render_tuple_exp (ctx : t) (exps : exp list) : string =
  "( " ^ render_exps ctx ~sep:", " exps ^ " )"

(* Case expression rendering *)

and render_case_exp (ctx : t) (exp : exp) (notexp : notexp) : string =
  if ctx.in_code then code_of_notexp ctx notexp
  else
    let hint_opt = exp.hints.Annot.prose in
    let link_opt = tid_of_typ exp.node.note in
    match (hint_opt, link_opt) with
    | Some hints, Some tid ->
        render_alter_hint (ctx |> link) hints (reindent_lines ~level:0)
          render_exp (Mixfix.args notexp)
        |> adoc_as_link ctx ~link:tid.it
    | _ -> code_of_notexp ctx notexp

(* Struct expression rendering *)

and render_str_exp (ctx : t) (expfields : (atom * exp) list) : string =
  "+{+"
  ^ String.concat ", "
      (List.map
         (fun (atom, exp_f) -> code_of_atom atom ^ " " ^ render_exp ctx exp_f)
         expfields)
  ^ "+}+"

(* Option expression rendering *)

and render_opt_exp (ctx : t) (exp_opt : exp option) : string =
  match exp_opt with
  | Some exp_inner -> "" ^ render_exp ctx exp_inner ^ ""
  | None -> "·" |> adoc_as_code ctx

(* List expression rendering *)

and render_list_exp (ctx : t) (exps : exp list) : string =
  match exps with
  | [] -> "·" |> adoc_as_code ctx
  | [ exp_inner ] -> render_exp (code ctx) exp_inner |> adoc_as_code ctx
  | exps ->
      "+[+ " ^ render_exps (code ctx) ~sep:", " exps ^ " +]+"
      |> adoc_as_code ctx

(* Cons expression rendering *)

and render_cons_exp (ctx : t) (exp_h : exp) (exp_t : exp) : string =
  render_exp (code ctx) exp_h ^ " {two-colons} " ^ render_exp (code ctx) exp_t
  |> adoc_as_code ctx

(* Concatenation expression rendering *)

and render_cat_exp (ctx : t) (exp_l : exp) (exp_r : exp) : string =
  if ctx.in_code then render_exp ctx exp_l ^ " {pp} " ^ render_exp ctx exp_r
  else render_exp ctx exp_l ^ " concatenated with " ^ render_exp ctx exp_r

(* Membership expression rendering *)

and render_mem_exp (ctx : t) (exp_e : exp) (exp_s : exp) : string =
  render_exp ctx exp_e ^ " is in " ^ render_exp ctx exp_s

(* Length expression rendering *)

and render_len_exp (ctx : t) (exp_inner : exp) : string =
  "the length of " ^ render_exp ctx exp_inner

(* Dot expression rendering *)

and render_dot_exp (ctx : t) (exp_b : exp) (atom : atom) : string =
  render_exp (code ctx) exp_b ^ "." ^ code_of_atom atom |> adoc_as_code ctx

(* Index expression rendering *)

and render_idx_exp (ctx : t) (exp_b : exp) (exp_i : exp) : string =
  render_exp (code ctx) exp_b ^ "[" ^ render_exp (code ctx) exp_i ^ "]"
  |> adoc_as_code ctx

(* Slice expression rendering *)

and render_slice_exp (ctx : t) (exp_b : exp) (exp_l : exp) (exp_h : exp) :
    string =
  render_exp (code ctx) exp_b
  ^ "["
  ^ render_exp (code ctx) exp_l
  ^ " : "
  ^ render_exp (code ctx) exp_h
  ^ "]"
  |> adoc_as_code ctx

(* Update expression rendering *)

and render_upd_exp (ctx : t) (exp_b : exp) (path : path) (exp_f : exp) : string
    =
  if ctx.in_code then
    render_exp (code ctx) exp_b
    ^ "["
    ^ render_path (code ctx) path
    ^ " = "
    ^ render_exp (code ctx) exp_f
    ^ "]"
    |> adoc_as_code ctx
  else
    (render_exp (code ctx) exp_b |> adoc_as_code ctx)
    ^ " with "
    ^ (render_path (code ctx) path |> adoc_as_code ctx)
    ^ " set to "
    ^ (render_exp (code ctx) exp_f |> adoc_as_code ctx)

(* Function call expression rendering *)

and render_call_exp (ctx : t) (exp : exp) (id : id) (targs : targ list)
    (args : arg list) : string =
  let hint_in = exp.hints.Annot.prose_in in
  let hint_true = exp.hints.Annot.prose_true in
  if ctx.in_code then
    string_of_defid id ^ string_of_targs targs
    ^ render_args (ctx |> link |> code) args
    |> adoc_as_link ctx ~link:id.it
    |> adoc_as_code ctx
  else
    match (hint_in, hint_true) with
    | Some hints, _ | _, Some hints ->
        render_alter_hint (link ctx) hints (reindent_lines ~level:0) render_arg
          args
        |> adoc_as_link ctx ~link:id.it
    | None, None ->
        string_of_defid id ^ string_of_targs targs
        ^ render_args (ctx |> link |> code) args
        |> adoc_as_link ctx ~link:id.it
        |> adoc_as_code ctx

(* Iterated expression rendering *)

and render_iter_exp (ctx : t) (exp_inner : exp) (iterexp : iterexp) : string =
  match (exp_inner.node.it, iterexp) with
  | _, (_, []) -> render_exp ctx exp_inner
  | (VarE _ | TupleE _), _ ->
      render_exp (code ctx) exp_inner ^ code_of_iterexp iterexp
      |> adoc_as_code ctx
  | _ ->
      let sexp = render_exp (code ctx) exp_inner in
      if String.contains sexp ' ' then
        "( " ^ sexp ^ " )" ^ code_of_iterexp iterexp |> adoc_as_code ctx
      else sexp ^ code_of_iterexp iterexp |> adoc_as_code ctx

(* Patterns *)

and code_of_pattern (pattern : pattern) =
  match pattern with
  | Il.CaseP mixop -> code_of_mixop mixop
  | Il.ListP `Cons -> "_ :: _"
  | Il.ListP (`Fixed len) -> Format.asprintf "[ _/%d ]" len
  | Il.ListP `Nil -> "[]"
  | Il.OptP `Some -> "(_)"
  | Il.OptP `None -> "()"

(* Path *)

and render_path (ctx : t) (path : path) =
  match path.it with
  | RootP -> ""
  | IdxP (path, e) -> render_path ctx path ^ "[" ^ render_exp ctx e ^ "]"
  | SliceP (path, e_l, e_h) ->
      render_path ctx path ^ "[" ^ render_exp ctx e_l ^ " : "
      ^ render_exp ctx e_h ^ "]"
  | DotP ({ it = RootP; _ }, atom) -> code_of_atom atom
  | DotP (path, atom) -> render_path ctx path ^ "." ^ code_of_atom atom

(* Parameters *)

and render_param (ctx : t) (param : param) =
  match param.it with
  | ExpP (_typ, exp) -> render_exp ctx exp
  | DefP (defid, _, _, _) -> string_of_defid defid |> adoc_as_code ctx

and render_params (ctx : t) (params : param list) =
  match params with
  | [] -> ""
  | params ->
      "(" ^ String.concat ", " (List.map (render_param ctx) params) ^ ")"

(* Type arguments *)

and string_of_targs (targs : targ list) = Sl.Print.string_of_targs targs

(* Arguments *)

and render_arg (ctx : t) (arg : arg) =
  match arg.it with
  | ExpA exp -> render_exp ctx exp
  | DefA defid -> string_of_defid defid |> adoc_as_code ctx

and render_args (ctx : t) (args : arg list) =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map (render_arg ctx) args) ^ ")"

(* Case analysis *)

let render_guard (ctx : t) (exp_scrut : exp) (guard : guard) : string =
  match guard with
  | BoolG true -> render_exp ctx exp_scrut
  | BoolG false ->
      let node_scrut = exp_scrut.node in
      let neg_inner =
        UnE (`NotOp, `BoolT, exp_scrut) $$ (node_scrut.at, node_scrut.note)
      in
      render_exp ctx (Annot.no_hints neg_inner)
  | CmpG (cmpop, _, exp) ->
      render_exp ctx exp_scrut ^ " " ^ render_cmpop ctx cmpop ^ " "
      ^ render_exp ctx exp
  | SubG typ ->
      F.asprintf "%s has type %s"
        (render_exp_as_code ctx exp_scrut)
        (code_of_typ ctx typ)
  | MatchG pattern ->
      F.asprintf "%s matches pattern %s" (render_exp ctx exp_scrut)
        (code_of_pattern pattern |> adoc_as_code ctx)
  | MemG exp -> render_exp ctx exp_scrut ^ " is in " ^ render_exp ctx exp
  | CheckLetSubG (_, target) | CheckLetMatchG (_, target) ->
      F.asprintf "let %s be %s"
        (render_exp_as_code ctx target)
        (render_exp ctx exp_scrut)

(* Instructions *)

let rec render_instr ?(level : int = 0) ?(unordered : bool = false)
    ?(backtrack : Backtrack.ctx option = None) (instr : instr) : string =
  let bullet =
    if unordered then adoc_unordered_bullet level else adoc_ordered_bullet level
  in
  let hints = instr.hints in
  match instr.node.it with
  | IfI (cond, iterexps, block_then, _) ->
      render_if_instr ~level ~bullet ~backtrack cond iterexps block_then
  | HoldI (id_rel, notexp, iterexps, holdcase) ->
      render_hold_instr ~level ~bullet ~backtrack hints id_rel notexp iterexps
        holdcase
  | CaseI (exp_scrut, cases, dangle) ->
      render_case_instr ~level ~bullet ~backtrack exp_scrut cases dangle
  | GroupI (_id_rulegroup, id_rel, rel_signature, exps, block) ->
      render_group_instr ~level ~bullet ~backtrack hints id_rel rel_signature
        exps block
  | TryI arms -> render_try_instr ~level ~bullet arms
  | LetI (exp_l, exp_r, iterinstrs) ->
      render_let_instr ~level ~bullet ~backtrack exp_l exp_r iterinstrs
  | RuleI (id_rel, notexp, hint_input, iterinstrs) ->
      render_rule_instr ~level ~bullet ~backtrack hints id_rel notexp hint_input
        iterinstrs
  | ResultI (rel_signature, exps) ->
      render_result_instr ~bullet hints rel_signature exps
  | ReturnI exp -> render_return_instr ~bullet exp
  | DebugI exp -> render_debug_instr ~bullet exp
  | DestructI (fields, exp_source) ->
      render_destruct_instr ~bullet fields exp_source
  | CheckLetSubI (_, exp_l, exp_r, block_inner)
  | CheckLetMatchI (_, exp_l, exp_r, block_inner) ->
      render_check_let_instr ~level ~bullet ~backtrack exp_l exp_r block_inner
  | OptionGetI (exp_l, exp_r, block_inner) ->
      render_option_get_instr ~level ~bullet ~backtrack exp_l exp_r block_inner

and render_instrs ?(level : int = 0) ?(backtrack : Backtrack.ctx option = None)
    (instrs : block) : string =
  match instrs with
  | [
   ({ node = { it = ReturnI ({ node = { it = BoolE _; _ }; _ } as e); _ }; _ } :
     instr);
  ] ->
      F.asprintf " return %s." (render_exp_as_code in_prose e)
  | _ ->
      "\n"
      ^ (List.map (render_instr ~level ~backtrack) instrs |> String.concat "\n")

and render_iterexp_suffix (ctx : t) (iterexps : iterexp list) : string =
  match iterexps with
  | [] -> ""
  | _ ->
      let vars = List.concat_map (fun (_, vars) -> vars) iterexps in
      if vars = [] then ""
      else F.asprintf ", for all %s" (render_in_itervars ctx vars)

and render_iterinstr_suffix (ctx : t) (iterinstrs : iterinstr list) : string =
  match iterinstrs with
  | [] -> ""
  | _ ->
      let vars =
        List.concat_map (fun (_, vars_in, _vars_out) -> vars_in) iterinstrs
      in
      if vars = [] then ""
      else F.asprintf ", for each %s" (render_in_itervars ctx vars)

(* If instruction rendering *)

and render_if_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (cond : exp) (iterexps : iterexp list)
    (block_then : block) : string =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let check_line =
    F.asprintf "%sCheck that %s%s.%s" bullet (render_exp in_prose cond)
      (render_iterexp_suffix in_prose iterexps)
      fallthrough
  in
  if block_then = [] then check_line
  else
    check_line ^ "\n"
    ^ (block_then
      |> List.map (render_instr ~level ~backtrack)
      |> String.concat "\n")

(* Hold instruction rendering *)

and render_hold_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (hints : Annot.hints) (id_rel : id)
    (notexp : notexp) (iterexps : iterexp list) (holdcase : holdcase) : string =
  let exps = Mixfix.args notexp in
  let hint_true = hints.Annot.prose_true in
  let hint_false = hints.Annot.prose_false in
  let iter_suffix = render_iterexp_suffix in_prose iterexps in
  let render_head ~(hold : bool) : string =
    let hint_opt = if hold then hint_true else hint_false in
    let fallback_verb = if hold then " holds" else " does not hold" in
    match hint_opt with
    | Some hint ->
        render_alter_hint in_link hint (reindent_lines ~level:0) render_exp exps
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
    | None ->
        let math =
          code_of_notexp in_prose notexp
          |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
        in
        math ^ fallback_verb
  in
  match holdcase with
  | HoldH (block, _dangle) ->
      F.asprintf "%sIf %s%s:%s" bullet (render_head ~hold:true) iter_suffix
        (render_instrs ~level:(level + 1) ~backtrack block)
  | NotHoldH (block, _dangle) ->
      F.asprintf "%sIf %s%s:%s" bullet (render_head ~hold:false) iter_suffix
        (render_instrs ~level:(level + 1) ~backtrack block)
  | BothH (block_hold, block_nothold) ->
      F.asprintf "%sIf %s%s:%s\n%sElse:%s" bullet (render_head ~hold:true)
        iter_suffix
        (render_instrs ~level:(level + 1) ~backtrack block_hold)
        bullet
        (render_instrs ~level:(level + 1) ~backtrack block_nothold)

(* Case analysis instruction rendering *)

and render_case_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_scrut : exp) (cases : case list)
    (dangle : dangle) : string =
  let total = not dangle in
  let n = List.length cases in
  match cases with
  | [ (guard, block_then) ] ->
      let check_line =
        F.asprintf "%sCheck that %s." bullet
          (render_guard in_prose exp_scrut guard)
      in
      if block_then = [] then check_line
      else
        check_line ^ "\n"
        ^ (block_then
          |> List.map (render_instr ~level ~backtrack)
          |> String.concat "\n")
  | _ ->
      cases
      |> List.mapi (fun idx (guard, block_then) ->
             if idx = n - 1 && total then
               F.asprintf "%sElse:%s" bullet
                 (render_instrs ~level:(level + 1) ~backtrack block_then)
             else
               let keyword = if idx = 0 then "If" else "Else if" in
               F.asprintf "%s%s %s:%s" bullet keyword
                 (render_guard in_prose exp_scrut guard)
                 (render_instrs ~level:(level + 1) ~backtrack block_then))
      |> String.concat "\n"

(* Group instruction rendering *)

and render_group_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (hints : Annot.hints) (id_rel : id)
    (rel_signature : rel_signature) (exps : exp list) (block : block) : string =
  let hint_in = hints.Annot.prose_in in
  let hint_true = hints.Annot.prose_true in
  let title =
    match (hint_in, hint_true) with
    | Some hint, _ | _, Some hint ->
        render_alter_hint ~caps:true in_link hint (reindent_lines ~level:0)
          render_exp exps
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
    | None, None ->
        render_rel_title_math in_prose rel_signature exps
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
  in
  F.asprintf "%s%s:%s" bullet title
    (render_instrs ~level:(level + 1) ~backtrack block)

(* Try instruction rendering *)

and render_try_instr ~(level : int) ~(bullet : string) (arms : arm list) :
    string =
  let block = Backtrack.BlockLabel.fresh () in
  let level_arm = level + 1 in
  let level_body = level + 2 in
  let total = List.length arms in
  let render_arm idx arm =
    let backtrack = Backtrack.arm_backtrack_ctx ~block ~level_arm ~total idx in
    let anchor = Backtrack.arm_anchor ~block ~level_arm idx in
    F.asprintf "%s{empty}%s%s"
      (adoc_ordered_bullet level_arm)
      anchor
      (render_instrs ~level:level_body ~backtrack:(Some backtrack) arm)
  in
  F.asprintf "%sTry %s:\n%s" bullet
    (Backtrack.render_block_label block)
    (String.concat "\n" (List.mapi render_arm arms))

(* Let instruction rendering *)

and render_let_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_l : exp) (exp_r : exp)
    (iterinstrs : iterinstr list) : string =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let vars_out_visible =
    iterinstrs
    |> List.concat_map (fun (_, _, vars_out) -> vars_out)
    |> List.filter (fun (id, _, _) -> not (Id.is_underscored id))
  in
  let vars_in_all =
    List.concat_map (fun (_, vars_in, _) -> vars_in) iterinstrs
  in
  if vars_out_visible = [] then
    F.asprintf "%sLet %s be %s%s.%s" bullet
      (render_exp_as_code in_prose exp_l)
      (render_exp in_prose exp_r)
      (render_iterinstr_suffix in_prose iterinstrs)
      fallthrough
  else
    let bullet_inner = adoc_unordered_bullet (level + 1) in
    let body =
      F.asprintf "%sLet %s be %s." bullet_inner
        (render_exp_as_code in_prose exp_l)
        (render_exp in_prose exp_r)
    in
    F.asprintf
      "%sLet %s obtained by repeating:\n+\n--\n%s\n--\n+\nfor each %s.%s" bullet
      (render_out_itervars in_prose vars_out_visible)
      body
      (render_in_itervars in_prose vars_in_all)
      fallthrough

(* Rule instruction rendering *)

and render_rule_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (hints : Annot.hints) (id_rel : id)
    (notexp : notexp) (hint_input : Hints.Input.t) (iterinstrs : iterinstr list)
    : string =
  let exps = Mixfix.args notexp in
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let exps_in, exps_out = Hints.Input.split hint_input exps in
  let hint_in = hints.Annot.prose_in in
  let hint_out = hints.Annot.prose_out in
  let vars_out_visible =
    iterinstrs
    |> List.concat_map (fun (_, _, vars_out) -> vars_out)
    |> List.filter (fun (id, _, _) -> not (Id.is_underscored id))
  in
  let vars_in_all =
    List.concat_map (fun (_, vars_in, _) -> vars_in) iterinstrs
  in
  let rule_body =
    match (hint_in, hint_out) with
    | Some hint_in, Some hint_out ->
        let prose_out =
          render_alter_hint in_link hint_out unindent_lines render_exp exps_out
        in
        let prose_in =
          render_alter_hint in_link hint_in unindent_lines render_exp exps_in
          |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
        in
        if adoc_fits_in_width_short prose_in then
          F.asprintf "Let %s be the result of %s" prose_out prose_in
        else
          F.asprintf "Let %s be\n%sthe result of %s" prose_out
            (adoc_unordered_bullet level)
            prose_in
    | _ ->
        F.asprintf "Let %s"
          (code_of_notexp in_prose notexp
          |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
  in
  if vars_out_visible = [] then
    F.asprintf "%s%s%s.%s" bullet rule_body
      (render_iterinstr_suffix in_prose iterinstrs)
      fallthrough
  else
    let bullet_inner = adoc_unordered_bullet (level + 1) in
    F.asprintf
      "%sLet %s obtained by repeating:\n+\n--\n%s%s.\n--\n+\nfor each %s.%s"
      bullet
      (render_out_itervars in_prose vars_out_visible)
      bullet_inner rule_body
      (render_in_itervars in_prose vars_in_all)
      fallthrough

(* Result instruction rendering *)

and render_result_instr ~(bullet : string) (hints : Annot.hints)
    (rel_signature : rel_signature) (exps : exp list) : string =
  let nottyp, hint_input = rel_signature in
  let typs = Mixfix.args nottyp.it in
  let is_conditional = Hints.Input.is_conditional hint_input typs in
  if is_conditional then bullet ^ "Then, the relation holds."
  else
    match (hints.Annot.prose_out, exps) with
    | Some hint, _ ->
        F.asprintf "%sResult in %s." bullet
          (render_alter_hint in_prose hint (reindent_lines ~level:0) render_exp
             exps)
    | None, [] -> bullet ^ "The relation holds."
    | None, _ -> F.asprintf "%sResult in %s." bullet (render_exps in_prose exps)

(* Return instruction rendering *)

and render_return_instr ~(bullet : string) (exp : exp) : string =
  F.asprintf "%sReturn %s." bullet (render_exp in_prose exp)

(* Debug instruction rendering *)

and render_debug_instr ~(bullet : string) (exp : exp) : string =
  F.asprintf "%s(debug: %s)" bullet (render_exp in_prose exp)

(* Destruct instruction rendering *)

and render_destruct_instr ~(bullet : string)
    (fields : (string option * exp) list) (exp_source : exp) : string =
  let projections =
    List.filter_map
      (fun (name_opt, exp_target) ->
        Option.map (fun name -> (name, exp_target)) name_opt)
      fields
  in
  match projections with
  | [ (name, exp_target) ] ->
      F.asprintf "%sLet %s be the %s of %s." bullet
        (render_exp in_prose exp_target)
        name
        (render_exp in_prose exp_source)
  | _ ->
      let names, exps_target = List.split projections in
      F.asprintf "%sLet %s be %s of %s." bullet
        (render_exps in_prose exps_target)
        (render_list (List.map (fun s -> "the " ^ s) names))
        (render_exp in_prose exp_source)

(* Check-let instruction rendering (CheckLetSubI / CheckLetMatchI) *)

and render_check_let_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_l : exp) (exp_r : exp)
    (block_inner : block) : string =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let head =
    F.asprintf "%sLet!~type~ %s be %s.%s" bullet
      (render_exp_as_code in_prose exp_l)
      (render_exp in_prose exp_r)
      fallthrough
  in
  if block_inner = [] then head
  else
    head ^ "\n"
    ^ (block_inner
      |> List.map (render_instr ~level ~backtrack)
      |> String.concat "\n")

(* Option-get instruction rendering *)

and render_option_get_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_l : exp) (exp_r : exp)
    (block_inner : block) : string =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let head =
    F.asprintf "%sLet %s be %s %s.%s" bullet
      (render_exp_as_code in_prose exp_l)
      (adoc_link ~link:"option_get" "*!*")
      (render_exp in_prose exp_r)
      fallthrough
  in
  if block_inner = [] then head
  else
    head ^ "\n"
    ^ (block_inner
      |> List.map (render_instr ~level ~backtrack)
      |> String.concat "\n")

(* Definitions *)

and strip_leading_newline (s : string) : string =
  if String.length s > 0 && s.[0] = '\n' then
    String.sub s 1 (String.length s - 1)
  else s

and collect_groups (block : block) : instr list =
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

and render_group (instr : instr) : string =
  let hints = instr.hints in
  match instr.node.it with
  | GroupI (_, id_rel, rel_signature, exps, block) ->
      let hint_in = hints.Annot.prose_in in
      let hint_true = hints.Annot.prose_true in
      let title =
        match (hint_in, hint_true) with
        | Some hint, _ | _, Some hint ->
            render_alter_hint ~caps:true in_link hint (reindent_lines ~level:0)
              render_exp exps
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
        | None, None ->
            render_rel_title_math in_prose rel_signature exps
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
      in
      title ^ ":\n" ^ render_instrs block
  | _ -> assert false

and render_elseblock (elseblock_opt : elseblock option) : string =
  match elseblock_opt with
  | None | Some [] -> ""
  | Some block ->
      "\n\n" ^ adoc_ordered_bullet 0 ^ "Otherwise:"
      ^ render_instrs ~level:1 block

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

and render_rel_title_math (ctx : t) (rel_signature : rel_signature)
    (exps : exp list) : string =
  let nottyp, inputs = rel_signature in
  let mixop = Mixfix.to_mixop nottyp.it in
  let sexps = List.map (render_exp in_code) exps in
  let num_outputs = Mixop.arity mixop - List.length sexps in
  let holes = List.init num_outputs (fun _ -> "%") in
  let padded = Hints.Input.combine inputs sexps holes in
  Mixop.assemble ~string_of_atom:code_of_atom mixop padded |> adoc_as_code ctx

and render_rel_title_adoc (hints : Annot.hints) (id_rel : id)
    (rel_signature : rel_signature) (exps : exp list) : string =
  let exps_in_title =
    match hints.Annot.prose_input_exps with
    | Some exps_in_sl -> List.map lift_synthesized_exp exps_in_sl
    | None -> exps
  in
  match
    (hints.prose_in, hints.prose_out, hints.prose_output_exps, hints.prose_true)
  with
  | Some _, Some _, None, _ -> assert false
  | Some hint_in, Some hint_out, Some exps_out_sl, _ ->
      let exps_out = List.map lift_synthesized_exp exps_out_sl in
      F.asprintf "%s:\n\n%s%s:\n%s%s."
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose hint_in (reindent_lines ~level:1)
           render_exp exps_in_title)
        (adoc_unordered_bullet 0)
        ("Result in "
        ^ render_alter_hint ~caps:false in_prose hint_out
            (reindent_lines ~level:1) render_exp exps_out)
  | Some hint_in, _, _, _ ->
      F.asprintf "%s:\n\n%s%s."
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose hint_in (reindent_lines ~level:1)
           render_exp exps_in_title)
  | _, _, _, Some hint_true ->
      F.asprintf "%s:\n\n%s%s"
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose hint_true
           (reindent_lines ~level:0) render_exp exps)
  | _ ->
      F.asprintf "%s: %s"
        (Sl.Print.string_of_relid id_rel)
        (render_rel_title_math in_prose rel_signature exps)
      |> adoc_as_link in_prose ~link:(string_of_relid id_rel)

let render_extern_rel_def (hints : Annot.hints) (externrel : externrel) : string
    =
  let id_rel, rel_signature, exps = externrel in
  render_rel_title_adoc hints id_rel rel_signature exps

let render_defined_rel_def (hints : Annot.hints) (rel : rel) : string =
  let id_rel, rel_signature, exps, block, elseblock_opt = rel in
  render_rel_title_adoc hints id_rel rel_signature exps
  ^ "\n\n"
  ^ (collect_groups block |> List.map render_group |> String.concat "\n\n")
  ^ render_elseblock elseblock_opt

(* Functions *)

let render_func_title_adoc (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : string =
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      F.asprintf "%s:\n\n%s%s"
        (string_of_defid id_func
        |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose hint (reindent_lines ~level:0)
           render_param params)
  | None, None ->
      (string_of_defid id_func
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func))
      ^ Sl.Print.string_of_tparams tparams
      ^ render_params (in_link |> code) params

let render_func_header (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : string =
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      render_alter_hint ~caps:true in_prose hint (reindent_lines ~level:0)
        render_param params
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func)
  | None, None ->
      string_of_defid id_func
      ^ Sl.Print.string_of_tparams tparams
      ^ render_params (in_link |> code) params
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func)

let render_extern_func_def (hints : Annot.hints) (externfunc : externfunc) :
    string =
  let id_func, tparams, params, _ = externfunc in
  render_func_header hints id_func tparams params

let render_builtin_func_def (hints : Annot.hints) (builtinfunc : builtinfunc) :
    string =
  let id_func, tparams, params, _ = builtinfunc in
  render_func_header hints id_func tparams params

let render_table_func_def (hints : Annot.hints) (tablefunc : tablefunc) : string
    =
  let id_func, params, _, tablerows = tablefunc in
  let table_meta =
    "[cols=\""
    ^ string_of_int (List.length params + 1)
    ^ "\", options=\"header\"]\n"
  in
  let table_header =
    "|===" ^ "\n" ^ "| " ^ render_params in_prose params ^ " | " ^ "Result \n\n"
  in
  let table_rows =
    tablerows
    |> List.map (fun tablerow ->
           let exps_sig, exp_res, _ = tablerow in
           let row_output = render_exp in_code exp_res in
           let row_input = render_exps in_code exps_sig in
           "| " ^ row_input ^ " | " ^ row_output)
    |> String.concat "\n"
  in
  let table_footer = "\n\n|===" in
  render_func_header hints id_func [] params
  ^ ":\n" ^ table_meta ^ table_header ^ table_rows ^ table_footer

let render_defined_func_def (hints : Annot.hints) (func : definedfunc) : string
    =
  let id_func, tparams, params, _typ, block, elseblock_opt = func in
  render_func_header hints id_func tparams params
  ^ "\n\n"
  ^ strip_leading_newline (render_instrs block)
  ^ render_elseblock elseblock_opt

(* Definitions *)

let id_of_def (def : def) : string option =
  match def.node.it with
  | ExternTypD _ | TypD _ | VarD _ -> None
  | ExternRelD (id, _, _) | RelD (id, _, _, _, _) -> Some id.it
  | ExternDecD (id, _, _, _)
  | BuiltinDecD (id, _, _, _)
  | TableDecD (id, _, _, _)
  | FuncDecD (id, _, _, _, _, _) ->
      Some id.it

let render_def (def : def) : string option =
  def |> id_of_def |> Option.iter Backtrack.BlockLabel.set_namespace;
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
