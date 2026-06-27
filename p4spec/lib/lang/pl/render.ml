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

(* Inline document

   A structured representation of inline asciidoc content, serialized once by
   [to_adoc]. A [Code]/[Link] wrapper is emitted only when one of the same kind
   is not already open, so nested wrappers collapse structurally -- this
   replaces the old [in_code]/[in_link] suppression booleans. *)

module Inline = struct
  type t =
    | Text of string
    | Seq of t list
    | Code of t
    | Link of string * t (* target, body *)

  let text (s : string) : t = Text s
  let seq (ts : t list) : t = Seq ts
  let code (t : t) : t = Code t
  let link ~(target : string) (t : t) : t = Link (target, t)
  let empty : t = Seq []
  let ( ++ ) (a : t) (b : t) : t = Seq [ a; b ]

  let rec serialize ~(in_code : bool) ~(in_link : bool) (t : t) : string =
    match t with
    | Text s -> s
    | Seq ts -> String.concat "" (List.map (serialize ~in_code ~in_link) ts)
    | Code inner ->
        let s = serialize ~in_code:true ~in_link inner in
        if in_code then s else adoc_mono_chopped s
    | Link (target, inner) ->
        let s = serialize ~in_code ~in_link:true inner in
        if in_link then s else adoc_link ~link:target s

  (* Serialize at the top level: neither a code span nor a link is open. *)
  let to_adoc (t : t) : string = serialize ~in_code:false ~in_link:false t

  (* Serialize as if already inside a code span (no extra mono-wrapping of the
     whole, leaves stay raw) -- the structural equivalent of the old
     [render_exp in_code] used to build raw fragments for string assembly. *)
  let to_adoc_code (t : t) : string = serialize ~in_code:true ~in_link:false t

  (* Serialize as if already inside a link (nested links suppressed) -- the
     structural equivalent of the old [render_exp in_link] used inside hint
     alternations whose whole result is then wrapped in one outer link. *)
  let to_adoc_in_link (t : t) : string = serialize ~in_code:false ~in_link:true t
end

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

(* Rendering mode: prose words vs. math/code symbols (the one remaining
   contextual concern after wrapping became structural via [Inline]). *)

type mode = Prose | Code

(* Wrap as a code span only in prose mode; in code mode the value is already
   inside an enclosing code span, so leave it alone. The structural equivalent
   of the old [_ |> adoc_as_code ctx] on an [Inline.t] that must not be
   unconditionally re-wrapped. *)

let as_code_in (mode : mode) (t : Inline.t) : Inline.t =
  match mode with Prose -> Inline.code t | Code -> t

let render_alter_hint ?(caps = false) (mode : mode) (hints : Hints.Alter.t)
    (render_base : string -> string) (render : 'a -> string) (items : 'a list) :
    string =
  let render_atom (atom : atom) : string =
    let raw = "+" ^ (atom.it |> Atom.string_of_atom) ^ "+" in
    match mode with Code -> raw | Prose -> adoc_mono_chopped raw
  in
  items
  |> Hints.Alter.alternate ~base_text:render_base ~base_atom:render_atom hints
       render
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

let render_varid (_mode : mode) (id_var : id) : Inline.t =
  if Id.is_underscored id_var then Inline.code (Inline.text "++_++")
  else
    let var_slices = String.split_on_char '_' id_var.it in
    match var_slices with
    | [] -> assert false
    | [ var_type ] -> Inline.code (Inline.text var_type)
    | var_type :: var_subscripts ->
        Inline.code
          (Inline.text
             (var_type ^ (var_subscripts |> String.concat "_" |> adoc_subscript)))

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

let render_var (mode : mode) ((id, _typ, iters) : var) : Inline.t =
  if Id.is_underscored id then Inline.code (Inline.text "++_++")
  else
    Inline.code
      (Inline.seq
         [ render_varid mode id;
           Inline.text (String.concat "" (List.map code_of_iter iters)) ])

let render_in_itervars (mode : mode) (vars : var list) : string =
  let render_in_var var =
    F.asprintf "%s in %s"
      (Inline.to_adoc (as_code_in mode (render_var Code var)))
      (Inline.to_adoc
         (as_code_in mode
            (Inline.seq
               [ render_var Code var; Inline.text (code_of_iter List) ])))
  in
  vars |> List.map render_in_var |> render_list

let render_out_itervars (mode : mode) (vars : var list) : string =
  vars
  |> List.filter_map (fun var ->
         let id, _, _ = var in
         if Id.is_underscored id then None
         else
           Some
             (F.asprintf "%s be the list"
                (Inline.to_adoc
                   (as_code_in mode
                      (Inline.seq
                         [ render_var Code var; Inline.text (code_of_iter List) ])))))
  |> render_list

(* Types *)

let code_of_typ (_mode : mode) (typ : typ) : Inline.t =
  Inline.code (Inline.text (Sl.Print.string_of_typ typ))

let tid_of_typ (typ' : typ') : id option =
  match typ' with Il.VarT (id, _) -> Some id | _ -> None

(* Operators *)

let render_unop = Sl.Print.string_of_unop

let render_binop_word (mode : mode) (binop : binop) : string =
  match mode with
  | Code -> Sl.Print.string_of_binop binop
  | Prose -> (
      match binop with
      | `AndOp -> "and"
      | `OrOp -> "or"
      | `ImplOp -> "implies"
      | `EquivOp -> "is equivalent to"
      | _ -> Sl.Print.string_of_binop binop)

let render_cmpop_word (mode : mode) (cmpop : cmpop) : string =
  match mode with
  | Code -> Sl.Print.string_of_cmpop cmpop
  | Prose -> (
      match cmpop with
      | `EqOp -> "is equal to"
      | `NeOp -> "is not equal to"
      | `LtOp -> "is less than"
      | `GtOp -> "is greater than"
      | `LeOp -> "is less than or equal to"
      | `GeOp -> "is greater than or equal to")

(* Expressions *)

let rec render_exp (mode : mode) (exp : exp) : Inline.t =
  match exp.node.it with
  | BoolE b -> render_bool_exp mode b
  | NumE n -> render_num_exp mode n
  | TextE text -> render_text_exp mode text
  | VarE id_var -> render_var_exp mode id_var
  | UnE (unop, _, exp_inner) -> render_un_exp mode unop exp_inner
  | BinE (binop, _, exp_l, exp_r) -> render_bin_exp mode binop exp_l exp_r
  | CmpE (cmpop, _, exp_l, exp_r) -> render_cmp_exp mode cmpop exp_l exp_r
  | UpCastE (_typ, exp_inner) | DownCastE (_typ, exp_inner) ->
      render_cast_exp mode exp_inner
  | SubE (exp_inner, typ) -> render_sub_exp mode exp_inner typ
  | MatchE (exp_inner, pattern) -> render_match_exp mode exp_inner pattern
  | TupleE exps -> render_tuple_exp mode exps
  | CaseE notexp -> render_case_exp mode exp notexp
  | StrE expfields -> render_str_exp mode expfields
  | OptE exp_opt -> render_opt_exp mode exp_opt
  | ListE exps -> render_list_exp mode exps
  | ConsE (exp_h, exp_t) -> render_cons_exp mode exp_h exp_t
  | CatE (exp_l, exp_r) -> render_cat_exp mode exp_l exp_r
  | MemE (exp_e, exp_s) -> render_mem_exp mode exp_e exp_s
  | LenE exp_inner -> render_len_exp mode exp_inner
  | DotE (exp_b, atom) -> render_dot_exp mode exp_b atom
  | IdxE (exp_b, exp_i) -> render_idx_exp mode exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> render_slice_exp mode exp_b exp_l exp_h
  | UpdE (exp_b, path, exp_f) -> render_upd_exp mode exp_b path exp_f
  | CallE (id, targs, args) -> render_call_exp mode exp id targs args
  | IterE (exp_inner, iterexp) -> render_iter_exp mode exp_inner iterexp

and render_exps (mode : mode) ?(sep : string option) (exps : exp list) :
    Inline.t =
  match (mode, sep) with
  | _, Some s ->
      Inline.seq
        (List.mapi
           (fun i e ->
             if i = 0 then render_exp mode e
             else Inline.seq [ Inline.text s; render_exp mode e ])
           exps)
  | Code, None ->
      Inline.seq
        (List.mapi
           (fun i e ->
             if i = 0 then render_exp mode e
             else Inline.seq [ Inline.text ", "; render_exp mode e ])
           exps)
  | Prose, None ->
      Inline.text
        (render_list (List.map (fun e -> Inline.to_adoc (render_exp mode e)) exps))

and render_exp_as_code (_mode : mode) (exp : exp) : Inline.t =
  Inline.code (render_exp Code exp)

and code_of_notexp (_mode : mode) (notexp : notexp) : Inline.t =
  let mixop, exps = Mixfix.split notexp in
  let sexps = List.map (fun e -> Inline.to_adoc_code (render_exp Code e)) exps in
  Inline.code
    (Inline.text (Mixop.assemble ~string_of_atom:code_of_atom mixop sexps))

(* Boolean expression rendering *)

and render_bool_exp (_mode : mode) (b : bool) : Inline.t =
  Inline.code (Inline.text (string_of_bool b))

(* Numeric expression rendering *)

and render_num_exp (_mode : mode) (n : num) : Inline.t =
  Inline.code (Inline.text (string_of_num n))

(* Text expression rendering *)

and render_text_exp (_mode : mode) (text : string) : Inline.t =
  Inline.code (Inline.text ("\"" ^ String.escaped text ^ "\""))

(* Variable expression rendering *)

and render_var_exp (_mode : mode) (id_var : id) : Inline.t =
  Inline.code (render_varid Code id_var)

(* Unary expression rendering *)

and render_negated_exp_opt (mode : mode) (exp_inner : exp) : Inline.t option =
  match exp_inner.node.it with
  | MatchE (exp_e, pattern) ->
      Some
        (Inline.seq
           [ render_exp mode exp_e;
             Inline.text " does not match pattern ";
             Inline.code (Inline.text (code_of_pattern pattern)) ])
  | SubE (exp_e, typ) ->
      Some
        (Inline.seq
           [ render_exp_as_code mode exp_e;
             Inline.text " does not have type "; code_of_typ mode typ ])
  | MemE (exp_e, exp_s) ->
      Some
        (Inline.seq
           [ render_exp_as_code mode exp_e; Inline.text " is not in ";
             render_exp_as_code mode exp_s ])
  | CallE (id, _targs, args) when mode = Prose -> (
      let hint_false_opt = exp_inner.hints.Annot.prose_false in
      match hint_false_opt with
      | Some hints ->
          Some
            (Inline.link ~target:id.it
               (Inline.text
                  (render_alter_hint mode hints (reindent_lines ~level:0)
                     (fun a -> Inline.to_adoc_in_link (render_arg Prose a))
                     args)))
      | None ->
          Some
            (Inline.code
               (Inline.seq
                  [ Inline.text (render_unop `NotOp);
                    render_exp Code exp_inner ])))
  | _ -> None

and render_un_exp (mode : mode) (unop : unop) (exp_inner : exp) : Inline.t =
  match unop with
  | #Bool.unop -> (
      match render_negated_exp_opt mode exp_inner with
      | Some t -> t
      | None ->
          Inline.code
            (Inline.seq
               [ Inline.text (render_unop unop); render_exp Code exp_inner ]))
  | _ ->
      Inline.code
        (Inline.seq [ Inline.text (render_unop unop); render_exp Code exp_inner ])

(* Binary expression rendering *)

and render_bin_exp (mode : mode) (binop : binop) (exp_l : exp) (exp_r : exp) :
    Inline.t =
  match binop with
  | `ImplOp when mode = Prose ->
      Inline.seq
        [ Inline.text "if "; render_exp mode exp_l; Inline.text ", then ";
          render_exp mode exp_r ]
  | #Bool.binop ->
      Inline.seq
        [ render_exp mode exp_l; Inline.text " ";
          Inline.text (render_binop_word mode binop); Inline.text " ";
          render_exp mode exp_r ]
  | #Num.binop ->
      Inline.code
        (Inline.seq
           [ render_exp Code exp_l;
             Inline.text (" " ^ Sl.Print.string_of_binop binop ^ " ");
             render_exp Code exp_r ])

(* Comparison expression rendering *)

and render_cmp_exp (mode : mode) (cmpop : cmpop) (exp_l : exp) (exp_r : exp) :
    Inline.t =
  Inline.seq
    [ render_exp mode exp_l; Inline.text " ";
      Inline.text (render_cmpop_word mode cmpop); Inline.text " ";
      render_exp mode exp_r ]

(* Cast expression rendering *)

and render_cast_exp (mode : mode) (exp_inner : exp) : Inline.t =
  render_exp_as_code mode exp_inner

(* Subtype check expression rendering *)

and render_sub_exp (mode : mode) (exp_inner : exp) (typ : typ) : Inline.t =
  Inline.seq
    [ render_exp_as_code mode exp_inner; Inline.text " has type ";
      code_of_typ mode typ ]

(* Pattern match check expression rendering *)

and render_match_exp (mode : mode) (exp_inner : exp) (pattern : pattern) :
    Inline.t =
  match pattern with
  | Il.CaseP mixop when Mixop.arity mixop = 0 ->
      Inline.seq
        [ render_exp mode exp_inner; Inline.text " is ";
          Inline.code (Inline.text (code_of_pattern (Il.CaseP mixop))) ]
  | Il.ListP `Nil ->
      Inline.seq [ render_exp mode exp_inner; Inline.text " is an empty list" ]
  | Il.ListP `Cons ->
      Inline.seq
        [ render_exp mode exp_inner; Inline.text " is a non-empty list" ]
  | Il.ListP (`Fixed len) ->
      Inline.seq
        [ render_exp mode exp_inner;
          Inline.text (F.asprintf " is a list of length %d" len) ]
  | Il.OptP `None ->
      Inline.seq [ render_exp mode exp_inner; Inline.text " is none" ]
  | Il.OptP `Some ->
      Inline.seq [ render_exp mode exp_inner; Inline.text " is defined" ]
  | pattern ->
      Inline.seq
        [ render_exp mode exp_inner; Inline.text " matches pattern ";
          Inline.code (Inline.text (code_of_pattern pattern)) ]

(* Tuple expression rendering *)

and render_tuple_exp (mode : mode) (exps : exp list) : Inline.t =
  Inline.seq
    [ Inline.text "( "; render_exps mode ~sep:", " exps; Inline.text " )" ]

(* Case expression rendering *)

and render_case_exp (mode : mode) (exp : exp) (notexp : notexp) : Inline.t =
  match mode with
  | Code -> code_of_notexp mode notexp
  | Prose -> (
      let hint_opt = exp.hints.Annot.prose in
      let link_opt = tid_of_typ exp.node.note in
      match (hint_opt, link_opt) with
      | Some hints, Some tid ->
          Inline.link ~target:tid.it
            (Inline.text
               (render_alter_hint mode hints (reindent_lines ~level:0)
                  (fun e -> Inline.to_adoc_in_link (render_exp Prose e))
                  (Mixfix.args notexp)))
      | _ -> code_of_notexp mode notexp)

(* Struct expression rendering *)

and render_str_exp (mode : mode) (expfields : (atom * exp) list) : Inline.t =
  Inline.seq
    [ Inline.text "+{+";
      Inline.seq
        (List.mapi
           (fun i (atom, exp_f) ->
             let field =
               Inline.seq
                 [ Inline.text (code_of_atom atom); Inline.text " ";
                   render_exp mode exp_f ]
             in
             if i = 0 then field else Inline.seq [ Inline.text ", "; field ])
           expfields);
      Inline.text "+}+" ]

(* Option expression rendering *)

and render_opt_exp (mode : mode) (exp_opt : exp option) : Inline.t =
  match exp_opt with
  | Some exp_inner -> render_exp mode exp_inner
  | None -> Inline.code (Inline.text "·")

(* List expression rendering *)

and render_list_exp (_mode : mode) (exps : exp list) : Inline.t =
  match exps with
  | [] -> Inline.code (Inline.text "·")
  | [ exp_inner ] -> Inline.code (render_exp Code exp_inner)
  | exps ->
      Inline.code
        (Inline.seq
           [ Inline.text "+[+ "; render_exps Code ~sep:", " exps;
             Inline.text " +]+" ])

(* Cons expression rendering *)

and render_cons_exp (_mode : mode) (exp_h : exp) (exp_t : exp) : Inline.t =
  Inline.code
    (Inline.seq
       [ render_exp Code exp_h; Inline.text " {two-colons} ";
         render_exp Code exp_t ])

(* Concatenation expression rendering *)

and render_cat_exp (mode : mode) (exp_l : exp) (exp_r : exp) : Inline.t =
  match mode with
  | Code ->
      Inline.seq
        [ render_exp mode exp_l; Inline.text " {pp} "; render_exp mode exp_r ]
  | Prose ->
      Inline.seq
        [ render_exp mode exp_l; Inline.text " concatenated with ";
          render_exp mode exp_r ]

(* Membership expression rendering *)

and render_mem_exp (mode : mode) (exp_e : exp) (exp_s : exp) : Inline.t =
  Inline.seq
    [ render_exp mode exp_e; Inline.text " is in "; render_exp mode exp_s ]

(* Length expression rendering *)

and render_len_exp (mode : mode) (exp_inner : exp) : Inline.t =
  Inline.seq [ Inline.text "the length of "; render_exp mode exp_inner ]

(* Dot expression rendering *)

and render_dot_exp (_mode : mode) (exp_b : exp) (atom : atom) : Inline.t =
  Inline.code
    (Inline.seq
       [ render_exp Code exp_b; Inline.text "."; Inline.text (code_of_atom atom) ])

(* Index expression rendering *)

and render_idx_exp (_mode : mode) (exp_b : exp) (exp_i : exp) : Inline.t =
  Inline.code
    (Inline.seq
       [ render_exp Code exp_b; Inline.text "["; render_exp Code exp_i;
         Inline.text "]" ])

(* Slice expression rendering *)

and render_slice_exp (_mode : mode) (exp_b : exp) (exp_l : exp) (exp_h : exp) :
    Inline.t =
  Inline.code
    (Inline.seq
       [ render_exp Code exp_b; Inline.text "["; render_exp Code exp_l;
         Inline.text " : "; render_exp Code exp_h; Inline.text "]" ])

(* Update expression rendering *)

and render_upd_exp (mode : mode) (exp_b : exp) (path : path) (exp_f : exp) :
    Inline.t =
  match mode with
  | Code ->
      Inline.code
        (Inline.seq
           [ render_exp Code exp_b; Inline.text "["; render_path Code path;
             Inline.text " = "; render_exp Code exp_f; Inline.text "]" ])
  | Prose ->
      Inline.seq
        [ Inline.code (render_exp Code exp_b); Inline.text " with ";
          Inline.code (render_path Code path); Inline.text " set to ";
          Inline.code (render_exp Code exp_f) ]

(* Function call expression rendering *)

and render_call_exp (mode : mode) (exp : exp) (id : id) (targs : targ list)
    (args : arg list) : Inline.t =
  let hint_in = exp.hints.Annot.prose_in in
  let hint_true = exp.hints.Annot.prose_true in
  match mode with
  | Code ->
      Inline.code
        (Inline.link ~target:id.it
           (Inline.seq
              [ Inline.text (string_of_defid id);
                Inline.text (string_of_targs targs); render_args Code args ]))
  | Prose -> (
      match (hint_in, hint_true) with
      | Some hints, _ | _, Some hints ->
          Inline.link ~target:id.it
            (Inline.text
               (render_alter_hint mode hints (reindent_lines ~level:0)
                  (fun a -> Inline.to_adoc_in_link (render_arg Prose a)) args))
      | None, None ->
          Inline.code
            (Inline.link ~target:id.it
               (Inline.seq
                  [ Inline.text (string_of_defid id);
                    Inline.text (string_of_targs targs); render_args Code args ])))

(* Iterated expression rendering *)

and render_iter_exp (mode : mode) (exp_inner : exp) (iterexp : iterexp) :
    Inline.t =
  match (exp_inner.node.it, iterexp) with
  | _, (_, []) -> render_exp mode exp_inner
  | (VarE _ | TupleE _), _ ->
      Inline.code
        (Inline.seq
           [ render_exp Code exp_inner; Inline.text (code_of_iterexp iterexp) ])
  | _ ->
      let inner = render_exp Code exp_inner in
      (* The space-test only decides parenthesization; the inner is emitted
         structurally so the surrounding link/code context is preserved. *)
      let sexp = Inline.to_adoc_code inner in
      if String.contains sexp ' ' then
        Inline.code
          (Inline.seq
             [ Inline.text "( "; inner;
               Inline.text (" )" ^ code_of_iterexp iterexp) ])
      else
        Inline.code
          (Inline.seq [ inner; Inline.text (code_of_iterexp iterexp) ])

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

and render_path (mode : mode) (path : path) : Inline.t =
  match path.it with
  | RootP -> Inline.empty
  | IdxP (path, e) ->
      Inline.seq
        [ render_path mode path; Inline.text "["; render_exp mode e;
          Inline.text "]" ]
  | SliceP (path, e_l, e_h) ->
      Inline.seq
        [ render_path mode path; Inline.text "["; render_exp mode e_l;
          Inline.text " : "; render_exp mode e_h; Inline.text "]" ]
  | DotP ({ it = RootP; _ }, atom) -> Inline.text (code_of_atom atom)
  | DotP (path, atom) ->
      Inline.seq
        [ render_path mode path; Inline.text "."; Inline.text (code_of_atom atom) ]

(* Parameters *)

and render_param (mode : mode) (param : param) : Inline.t =
  match param.it with
  | ExpP (_typ, exp) -> render_exp mode exp
  | DefP (defid, _, _, _) -> Inline.code (Inline.text (string_of_defid defid))

and render_params (mode : mode) (params : param list) : Inline.t =
  match params with
  | [] -> Inline.empty
  | params ->
      Inline.seq
        [ Inline.text "(";
          Inline.seq
            (List.mapi
               (fun i p ->
                 if i = 0 then render_param mode p
                 else Inline.seq [ Inline.text ", "; render_param mode p ])
               params);
          Inline.text ")" ]

(* Type arguments *)

and string_of_targs (targs : targ list) = Sl.Print.string_of_targs targs

(* Arguments *)

and render_arg (mode : mode) (arg : arg) : Inline.t =
  match arg.it with
  | ExpA exp -> render_exp mode exp
  | DefA defid -> Inline.code (Inline.text (string_of_defid defid))

and render_args (mode : mode) (args : arg list) : Inline.t =
  match args with
  | [] -> Inline.empty
  | args ->
      Inline.seq
        [ Inline.text "(";
          Inline.seq
            (List.mapi
               (fun i a ->
                 if i = 0 then render_arg mode a
                 else Inline.seq [ Inline.text ", "; render_arg mode a ])
               args);
          Inline.text ")" ]

(* Case analysis *)

let render_guard (mode : mode) (exp_scrut : exp) (guard : guard) : Inline.t =
  match guard with
  | BoolG true -> render_exp mode exp_scrut
  | BoolG false ->
      let node_scrut = exp_scrut.node in
      let neg_inner =
        UnE (`NotOp, `BoolT, exp_scrut) $$ (node_scrut.at, node_scrut.note)
      in
      render_exp mode (Annot.no_hints neg_inner)
  | CmpG (cmpop, _, exp) ->
      Inline.seq
        [ render_exp mode exp_scrut; Inline.text " ";
          Inline.text (render_cmpop_word mode cmpop); Inline.text " ";
          render_exp mode exp ]
  | SubG typ ->
      Inline.seq
        [ render_exp_as_code mode exp_scrut; Inline.text " has type ";
          code_of_typ mode typ ]
  | MatchG pattern ->
      Inline.seq
        [ render_exp mode exp_scrut; Inline.text " matches pattern ";
          Inline.code (Inline.text (code_of_pattern pattern)) ]
  | MemG exp ->
      Inline.seq
        [ render_exp mode exp_scrut; Inline.text " is in "; render_exp mode exp ]
  | CheckLetSubG (_, target) | CheckLetMatchG (_, target) ->
      Inline.seq
        [ Inline.text "let "; render_exp_as_code mode target; Inline.text " be ";
          render_exp mode exp_scrut ]

(* Boundary shims: serialize an [Inline.t] to a string for the not-yet-migrated
   instruction and definition renderers (removed in a later session). *)

let render_exp_s (mode : mode) (exp : exp) : string =
  Inline.to_adoc (render_exp mode exp)

let render_exps_s (mode : mode) ?(sep : string option) (exps : exp list) :
    string =
  Inline.to_adoc (render_exps mode ?sep exps)

let render_exp_as_code_s (mode : mode) (exp : exp) : string =
  Inline.to_adoc (render_exp_as_code mode exp)

let render_guard_s (mode : mode) (exp_scrut : exp) (guard : guard) : string =
  Inline.to_adoc (render_guard mode exp_scrut guard)

let render_param_s (mode : mode) (param : param) : string =
  Inline.to_adoc (render_param mode param)

let render_params_s (mode : mode) (params : param list) : string =
  Inline.to_adoc (render_params mode params)

let code_of_notexp_s (mode : mode) (notexp : notexp) : string =
  Inline.to_adoc (code_of_notexp mode notexp)

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
      F.asprintf " return %s." (render_exp_as_code_s Prose e)
  | _ ->
      "\n"
      ^ (List.map (render_instr ~level ~backtrack) instrs |> String.concat "\n")

and render_iterexp_suffix (mode : mode) (iterexps : iterexp list) : string =
  match iterexps with
  | [] -> ""
  | _ ->
      let vars = List.concat_map (fun (_, vars) -> vars) iterexps in
      if vars = [] then ""
      else F.asprintf ", for all %s" (render_in_itervars mode vars)

and render_iterinstr_suffix (mode : mode) (iterinstrs : iterinstr list) : string =
  match iterinstrs with
  | [] -> ""
  | _ ->
      let vars =
        List.concat_map (fun (_, vars_in, _vars_out) -> vars_in) iterinstrs
      in
      if vars = [] then ""
      else F.asprintf ", for each %s" (render_in_itervars mode vars)

(* If instruction rendering *)

and render_if_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (cond : exp) (iterexps : iterexp list)
    (block_then : block) : string =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let check_line =
    F.asprintf "%sCheck that %s%s.%s" bullet (render_exp_s Prose cond)
      (render_iterexp_suffix Prose iterexps)
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
  let iter_suffix = render_iterexp_suffix Prose iterexps in
  let render_head ~(hold : bool) : string =
    let hint_opt = if hold then hint_true else hint_false in
    let fallback_verb = if hold then " holds" else " does not hold" in
    match hint_opt with
    | Some hint ->
        Inline.to_adoc
          (Inline.link ~target:(string_of_relid id_rel)
             (Inline.text
                (render_alter_hint Prose hint (reindent_lines ~level:0)
                   (fun e -> Inline.to_adoc_in_link (render_exp Prose e)) exps)))
    | None ->
        let math =
          Inline.to_adoc
            (Inline.link ~target:(string_of_relid id_rel)
               (Inline.text (code_of_notexp_s Prose notexp)))
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
          (render_guard_s Prose exp_scrut guard)
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
                 (render_guard_s Prose exp_scrut guard)
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
        Inline.to_adoc
          (Inline.link ~target:(string_of_relid id_rel)
             (Inline.text
                (render_alter_hint ~caps:true Prose hint (reindent_lines ~level:0)
                   (fun e -> Inline.to_adoc_in_link (render_exp Prose e)) exps)))
    | None, None ->
        Inline.to_adoc
          (Inline.link ~target:(string_of_relid id_rel)
             (Inline.text (render_rel_title_math Prose rel_signature exps)))
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
      (render_exp_as_code_s Prose exp_l)
      (render_exp_s Prose exp_r)
      (render_iterinstr_suffix Prose iterinstrs)
      fallthrough
  else
    let bullet_inner = adoc_unordered_bullet (level + 1) in
    let body =
      F.asprintf "%sLet %s be %s." bullet_inner
        (render_exp_as_code_s Prose exp_l)
        (render_exp_s Prose exp_r)
    in
    F.asprintf
      "%sLet %s obtained by repeating:\n+\n--\n%s\n--\n+\nfor each %s.%s" bullet
      (render_out_itervars Prose vars_out_visible)
      body
      (render_in_itervars Prose vars_in_all)
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
          render_alter_hint Prose hint_out unindent_lines
            (fun e -> Inline.to_adoc_in_link (render_exp Prose e)) exps_out
        in
        let prose_in =
          Inline.to_adoc
            (Inline.link ~target:(string_of_relid id_rel)
               (Inline.text
                  (render_alter_hint Prose hint_in unindent_lines
                     (fun e -> Inline.to_adoc_in_link (render_exp Prose e))
                     exps_in)))
        in
        if adoc_fits_in_width_short prose_in then
          F.asprintf "Let %s be the result of %s" prose_out prose_in
        else
          F.asprintf "Let %s be\n%sthe result of %s" prose_out
            (adoc_unordered_bullet level)
            prose_in
    | _ ->
        F.asprintf "Let %s"
          (Inline.to_adoc
             (Inline.link ~target:(string_of_relid id_rel)
                (Inline.text (code_of_notexp_s Prose notexp))))
  in
  if vars_out_visible = [] then
    F.asprintf "%s%s%s.%s" bullet rule_body
      (render_iterinstr_suffix Prose iterinstrs)
      fallthrough
  else
    let bullet_inner = adoc_unordered_bullet (level + 1) in
    F.asprintf
      "%sLet %s obtained by repeating:\n+\n--\n%s%s.\n--\n+\nfor each %s.%s"
      bullet
      (render_out_itervars Prose vars_out_visible)
      bullet_inner rule_body
      (render_in_itervars Prose vars_in_all)
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
          (render_alter_hint Prose hint (reindent_lines ~level:0)
             (fun e -> render_exp_s Prose e) exps)
    | None, [] -> bullet ^ "The relation holds."
    | None, _ -> F.asprintf "%sResult in %s." bullet (render_exps_s Prose exps)

(* Return instruction rendering *)

and render_return_instr ~(bullet : string) (exp : exp) : string =
  F.asprintf "%sReturn %s." bullet (render_exp_s Prose exp)

(* Debug instruction rendering *)

and render_debug_instr ~(bullet : string) (exp : exp) : string =
  F.asprintf "%s(debug: %s)" bullet (render_exp_s Prose exp)

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
        (render_exp_s Prose exp_target)
        name
        (render_exp_s Prose exp_source)
  | _ ->
      let names, exps_target = List.split projections in
      F.asprintf "%sLet %s be %s of %s." bullet
        (render_exps_s Prose exps_target)
        (render_list (List.map (fun s -> "the " ^ s) names))
        (render_exp_s Prose exp_source)

(* Check-let instruction rendering (CheckLetSubI / CheckLetMatchI) *)

and render_check_let_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_l : exp) (exp_r : exp)
    (block_inner : block) : string =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let head =
    F.asprintf "%sLet!~type~ %s be %s.%s" bullet
      (render_exp_as_code_s Prose exp_l)
      (render_exp_s Prose exp_r)
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
      (render_exp_as_code_s Prose exp_l)
      (adoc_link ~link:"option_get" "*!*")
      (render_exp_s Prose exp_r)
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
            Inline.to_adoc
              (Inline.link ~target:(string_of_relid id_rel)
                 (Inline.text
                    (render_alter_hint ~caps:true Prose hint
                       (reindent_lines ~level:0)
                       (fun e -> Inline.to_adoc_in_link (render_exp Prose e))
                       exps)))
        | None, None ->
            Inline.to_adoc
              (Inline.link ~target:(string_of_relid id_rel)
                 (Inline.text (render_rel_title_math Prose rel_signature exps)))
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

and render_rel_title_math (mode : mode) (rel_signature : rel_signature)
    (exps : exp list) : string =
  let nottyp, inputs = rel_signature in
  let mixop = Mixfix.to_mixop nottyp.it in
  let sexps = List.map (fun e -> Inline.to_adoc_code (render_exp Code e)) exps in
  let num_outputs = Mixop.arity mixop - List.length sexps in
  let holes = List.init num_outputs (fun _ -> "%") in
  let padded = Hints.Input.combine inputs sexps holes in
  Inline.to_adoc
    (as_code_in mode
       (Inline.text (Mixop.assemble ~string_of_atom:code_of_atom mixop padded)))

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
        (render_alter_hint ~caps:true Prose hint_in (reindent_lines ~level:1)
           (fun e -> render_exp_s Prose e) exps_in_title)
        (adoc_unordered_bullet 0)
        ("Result in "
        ^ render_alter_hint ~caps:false Prose hint_out
            (reindent_lines ~level:1) (fun e -> render_exp_s Prose e) exps_out)
  | Some hint_in, _, _, _ ->
      F.asprintf "%s:\n\n%s%s."
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true Prose hint_in (reindent_lines ~level:1)
           (fun e -> render_exp_s Prose e) exps_in_title)
  | _, _, _, Some hint_true ->
      F.asprintf "%s:\n\n%s%s"
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true Prose hint_true
           (reindent_lines ~level:0) (fun e -> render_exp_s Prose e) exps)
  | _ ->
      F.asprintf "%s: %s"
        (Sl.Print.string_of_relid id_rel)
        (render_rel_title_math Prose rel_signature exps)
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
        (render_alter_hint ~caps:true Prose hint (reindent_lines ~level:0)
           (fun p -> render_param_s Prose p) params)
  | None, None ->
      (string_of_defid id_func
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func))
      ^ Sl.Print.string_of_tparams tparams
      ^ Inline.serialize ~in_code:true ~in_link:true (render_params Code params)

let render_func_header (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : string =
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      render_alter_hint ~caps:true Prose hint (reindent_lines ~level:0)
        (fun p -> render_param_s Prose p) params
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func)
  | None, None ->
      string_of_defid id_func
      ^ Sl.Print.string_of_tparams tparams
      ^ Inline.serialize ~in_code:true ~in_link:true (render_params Code params)
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
    "|===" ^ "\n" ^ "| " ^ render_params_s Prose params ^ " | " ^ "Result \n\n"
  in
  let table_rows =
    tablerows
    |> List.map (fun tablerow ->
           let exps_sig, exp_res, _ = tablerow in
           let row_output = Inline.to_adoc_code (render_exp Code exp_res) in
           let row_input = Inline.to_adoc_code (render_exps Code exps_sig) in
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
