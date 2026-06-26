open Domain
open Lib
open Xl
open Ast
open Util.Source
module F = Format

(* Asciidoc rendering context *)

type context = { in_code : bool; in_link : bool }

let in_prose = { in_code = false; in_link = false }
let in_code = { in_code = true; in_link = false }
let in_link = { in_code = false; in_link = true }
let code context = { context with in_code = true }
let link context = { context with in_link = true }

(* Asciidoc utils *)

let rec adoc_escape c text =
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

let adoc_as_code (ctx : context) (s : string) : string =
  if ctx.in_code then s else adoc_mono_chopped s

let adoc_ordered_bullet level =
  Format.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '.')

let adoc_unordered_bullet level =
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

let adoc_as_link (ctx : context) ~link (s : string) : string =
  if ctx.in_link then s else adoc_link ~link s

let adoc_attach_block = "+\n"
let adoc_open_block s = F.asprintf "--\n%s\n--" s

let reindent_lines ?(level = 0) (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n" ^ adoc_unordered_bullet level) lines

let unindent_lines (s : string) : string =
  s |> String.split_on_char '\n' |> String.concat ""

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

let string_of_num num = Il.Print.string_of_num num
let string_of_text text = Il.Print.string_of_text text
let string_of_varid varid = Il.Print.string_of_varid varid
let string_of_relid relid = Il.Print.string_of_relid relid

let string_of_defid ?(link = false) defid =
  if link then Il.Print.string_of_varid defid
  else Il.Print.string_of_defid defid

let render_varid (ctx : context) (id_var : id) =
  if Id.is_underscored id_var then "++_++" |> adoc_as_code ctx
  else
    let var_slices = String.split_on_char '_' id_var.it in
    match var_slices with
    | [] -> assert false
    | [ var_type ] -> var_type |> adoc_as_code ctx
    | var_type :: var_subscripts ->
        var_type ^ (var_subscripts |> String.concat "_" |> adoc_subscript)
        |> adoc_as_code ctx

let code_of_atom atom =
  match atom.it with
  | Atom.Tick -> ""
  | _ -> "+" ^ Atom.string_of_atom atom.it ^ "+"

let code_of_atoms atoms = atoms |> List.map code_of_atom |> String.concat " "

let code_of_mixop mixop =
  let arity = Mixop.arity mixop in
  let placeholders = List.init arity (fun _ -> "%") in
  Mixop.assemble ~string_of_atom:code_of_atom mixop placeholders |> String.trim

let code_of_iter (iter : iter) =
  match iter with
  | List -> "{asterisk}" |> adoc_superscript
  | Opt -> "?" |> adoc_superscript

let code_of_iterexp (iter, _) = code_of_iter iter

let render_var ctx (id, _typ, iters) =
  if Id.is_underscored id then "++_++" |> adoc_as_code ctx
  else render_varid ctx id ^ String.concat "" (List.map code_of_iter iters)

let render_in_itervars ctx vars : string =
  let render_in_var var =
    F.asprintf "%s in %s"
      (render_var in_code var |> adoc_as_code ctx)
      (render_var in_code var ^ code_of_iter List |> adoc_as_code ctx)
  in
  List.map render_in_var vars |> render_list

(* For block-form iteration: the output-list variables, formatted as
   "X^{asterisk}^ be the list" / "X^{asterisk}^ and Y^{asterisk}^ be the lists". *)
let render_out_itervars ctx vars : string =
  vars
  |> List.filter_map (fun ((id, _, _) as var) ->
         if Id.is_underscored id then None
         else
           Some
             (F.asprintf "%s be the list"
                (render_var in_code var ^ code_of_iter List |> adoc_as_code ctx)))
  |> render_list

let code_of_typ (ctx : context) (typ : typ) : string =
  Sl.Print.string_of_typ typ |> adoc_as_code ctx

let tid_of_typ (typ' : typ') : id option =
  match typ' with Il.VarT (id, _) -> Some id | _ -> None

let render_unop = Sl.Print.string_of_unop

let render_binop ctx binop =
  if ctx.in_code then Sl.Print.string_of_binop binop
  else
    match binop with
    | `AndOp -> "and"
    | `OrOp -> "or"
    | `ImplOp -> "implies"
    | `EquivOp -> "is equivalent to"
    | _ -> Sl.Print.string_of_binop binop

let render_cmpop ctx cmpop =
  if ctx.in_code then Sl.Print.string_of_cmpop cmpop
  else
    match cmpop with
    | `EqOp -> "is equal to"
    | `NeOp -> "is not equal to"
    | `LtOp -> "is less than"
    | `GtOp -> "is greater than"
    | `LeOp -> "is less than or equal to"
    | `GeOp -> "is greater than or equal to"

let render_alter_hint ?(caps = false) (ctx : context) (hints : Hints.Alter.t)
    (render_base : string -> string) (render : context -> 'a -> string)
    (items : 'a list) : string =
  let render_atom (atom : atom) : string =
    "+" ^ (atom.it |> Atom.string_of_atom) ^ "+" |> adoc_as_code ctx
  in
  items
  |> Hints.Alter.alternate ~base_text:render_base ~base_atom:render_atom hints
       (fun a -> render ctx a)
  |> fun s -> if caps then String.capitalize_ascii s else s

(* Expressions *)

let rec render_exp ctx (exp : exp) : string =
  let in_code_ctx = code ctx in
  match exp.node.it with
  | BoolE b -> string_of_bool b |> adoc_as_code ctx
  | NumE n -> string_of_num n |> adoc_as_code ctx
  | TextE text -> "\"" ^ String.escaped text ^ "\"" |> adoc_as_code ctx
  | VarE id_var -> render_varid in_code_ctx id_var |> adoc_as_code ctx
  | UnE ((#Bool.unop as unop), _, exp_inner) -> (
      match render_negated_exp_opt ctx exp_inner with
      | Some s -> s
      | None ->
          render_unop unop ^ render_exp in_code_ctx exp_inner
          |> adoc_as_code ctx)
  | UnE (unop, _, exp_inner) ->
      render_unop unop ^ render_exp in_code_ctx exp_inner |> adoc_as_code ctx
  | BinE (`ImplOp, _, exp_l, exp_r) when not ctx.in_code ->
      "if " ^ render_exp ctx exp_l ^ ", then " ^ render_exp ctx exp_r
  | BinE ((#Bool.binop as binop), _, exp_l, exp_r) ->
      render_exp ctx exp_l ^ " " ^ render_binop ctx binop ^ " "
      ^ render_exp ctx exp_r
  | BinE ((#Num.binop as binop), _, exp_l, exp_r) ->
      render_exp in_code_ctx exp_l
      ^ " "
      ^ render_binop in_code_ctx binop
      ^ " "
      ^ render_exp in_code_ctx exp_r
      |> adoc_as_code ctx
  | CmpE (cmpop, _, exp_l, exp_r) ->
      render_exp ctx exp_l ^ " " ^ render_cmpop ctx cmpop ^ " "
      ^ render_exp ctx exp_r
  | UpCastE (_typ, exp_inner) | DownCastE (_typ, exp_inner) ->
      render_exp_as_code ctx exp_inner
  | SubE (exp_inner, typ) ->
      F.asprintf "%s has type %s"
        (render_exp_as_code ctx exp_inner)
        (code_of_typ ctx typ)
  | MatchE (exp_inner, Il.CaseP mixop) when Mixop.arity mixop = 0 ->
      F.asprintf "%s is %s" (render_exp ctx exp_inner)
        (code_of_pattern (Il.CaseP mixop) |> adoc_as_code ctx)
  | MatchE (exp_inner, Il.ListP `Nil) ->
      F.asprintf "%s is an empty list" (render_exp ctx exp_inner)
  | MatchE (exp_inner, Il.ListP `Cons) ->
      F.asprintf "%s is a non-empty list" (render_exp ctx exp_inner)
  | MatchE (exp_inner, Il.ListP (`Fixed len)) ->
      F.asprintf "%s is a list of length %d" (render_exp ctx exp_inner) len
  | MatchE (exp_inner, Il.OptP `None) ->
      F.asprintf "%s is none" (render_exp ctx exp_inner)
  | MatchE (exp_inner, Il.OptP `Some) ->
      F.asprintf "%s is defined" (render_exp ctx exp_inner)
  | MatchE (exp_inner, pattern) ->
      F.asprintf "%s matches pattern %s" (render_exp ctx exp_inner)
        (code_of_pattern pattern |> adoc_as_code ctx)
  | TupleE exps -> "( " ^ render_exps ctx ~sep:", " exps ^ " )"
  | CaseE notexp -> (
      if ctx.in_code then code_of_notexp ctx notexp
      else
        let hint_opt = exp.hints.Annot.prose in
        let link_opt = tid_of_typ exp.node.note in
        match (hint_opt, link_opt) with
        | Some hints, Some tid ->
            render_alter_hint (ctx |> link) hints (reindent_lines ~level:0)
              render_exp (Mixfix.args notexp)
            |> adoc_as_link ctx ~link:tid.it
        | _ -> code_of_notexp ctx notexp)
  | StrE expfields ->
      "+{+"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp_f) ->
               code_of_atom atom ^ " " ^ render_exp ctx exp_f)
             expfields)
      ^ "+}+"
  | OptE (Some exp_inner) -> "" ^ render_exp ctx exp_inner ^ ""
  | OptE None -> "·" |> adoc_as_code ctx
  | ListE [] -> "·" |> adoc_as_code ctx
  | ListE [ exp_inner ] -> render_exp in_code_ctx exp_inner |> adoc_as_code ctx
  | ListE exps ->
      "+[+ " ^ render_exps in_code_ctx ~sep:", " exps ^ " +]+"
      |> adoc_as_code ctx
  | ConsE (exp_h, exp_t) ->
      render_exp in_code_ctx exp_h
      ^ " {two-colons} "
      ^ render_exp in_code_ctx exp_t
      |> adoc_as_code ctx
  | CatE (exp_l, exp_r) ->
      if ctx.in_code then render_exp ctx exp_l ^ " {pp} " ^ render_exp ctx exp_r
      else render_exp ctx exp_l ^ " concatenated with " ^ render_exp ctx exp_r
  | MemE (exp_e, exp_s) ->
      render_exp ctx exp_e ^ " is in " ^ render_exp ctx exp_s
  | LenE exp_inner -> "the length of " ^ render_exp ctx exp_inner
  | DotE (exp_b, atom) ->
      render_exp in_code_ctx exp_b ^ "." ^ code_of_atom atom |> adoc_as_code ctx
  | IdxE (exp_b, exp_i) ->
      render_exp in_code_ctx exp_b ^ "[" ^ render_exp in_code_ctx exp_i ^ "]"
      |> adoc_as_code ctx
  | SliceE (exp_b, exp_l, exp_h) ->
      render_exp in_code_ctx exp_b
      ^ "["
      ^ render_exp in_code_ctx exp_l
      ^ " : "
      ^ render_exp in_code_ctx exp_h
      ^ "]"
      |> adoc_as_code ctx
  | UpdE (exp_b, path, exp_f) ->
      if ctx.in_code then
        render_exp in_code_ctx exp_b
        ^ "["
        ^ render_path in_code_ctx path
        ^ " = "
        ^ render_exp in_code_ctx exp_f
        ^ "]"
        |> adoc_as_code ctx
      else
        (render_exp in_code_ctx exp_b |> adoc_as_code ctx)
        ^ " with "
        ^ (render_path in_code_ctx path |> adoc_as_code ctx)
        ^ " set to "
        ^ (render_exp in_code_ctx exp_f |> adoc_as_code ctx)
  | CallE (id, targs, args) -> (
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
            render_alter_hint (link ctx) hints (reindent_lines ~level:0)
              render_arg args
            |> adoc_as_link ctx ~link:id.it
        | None, None ->
            string_of_defid id ^ string_of_targs targs
            ^ render_args (ctx |> link |> code) args
            |> adoc_as_link ctx ~link:id.it
            |> adoc_as_code ctx)
  | IterE (exp_inner, (_, [])) -> render_exp ctx exp_inner
  | IterE (({ node = { it = VarE _; _ }; _ } as exp_inner), iterexp)
  | IterE (({ node = { it = TupleE _; _ }; _ } as exp_inner), iterexp) ->
      render_exp in_code_ctx exp_inner ^ code_of_iterexp iterexp
      |> adoc_as_code ctx
  | IterE (exp_inner, iterexp) ->
      let sexp = render_exp in_code_ctx exp_inner in
      if String.contains sexp ' ' then
        "( " ^ sexp ^ " )" ^ code_of_iterexp iterexp |> adoc_as_code ctx
      else sexp ^ code_of_iterexp iterexp |> adoc_as_code ctx

and render_negated_exp_opt ctx (exp_inner : exp) : string option =
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

and render_exp_as_code ctx (exp : exp) =
  render_exp (code ctx) exp |> adoc_as_code ctx

and render_exps ctx ?sep (exps : exp list) =
  match (ctx.in_code, sep) with
  | _, Some s -> String.concat s (List.map (render_exp ctx) exps)
  | true, None -> String.concat ", " (List.map (render_exp ctx) exps)
  | false, None -> render_list (List.map (render_exp ctx) exps)

and code_of_notexp ctx notexp =
  let mixop, exps = Mixfix.split notexp in
  let sexps = List.map (render_exp in_code) exps in
  Mixop.assemble ~string_of_atom:code_of_atom mixop sexps |> adoc_as_code ctx

and code_of_pattern (pattern : pattern) =
  match pattern with
  | Il.CaseP mixop -> code_of_mixop mixop
  | Il.ListP `Cons -> "_ :: _"
  | Il.ListP (`Fixed len) -> Format.asprintf "[ _/%d ]" len
  | Il.ListP `Nil -> "[]"
  | Il.OptP `Some -> "(_)"
  | Il.OptP `None -> "()"

and render_path ctx path =
  match path.it with
  | RootP -> ""
  | IdxP (path, e) -> render_path ctx path ^ "[" ^ render_exp ctx e ^ "]"
  | SliceP (path, e_l, e_h) ->
      render_path ctx path ^ "[" ^ render_exp ctx e_l ^ " : "
      ^ render_exp ctx e_h ^ "]"
  | DotP ({ it = RootP; _ }, atom) -> code_of_atom atom
  | DotP (path, atom) -> render_path ctx path ^ "." ^ code_of_atom atom

and render_arg ctx (arg : arg) =
  match arg.it with
  | ExpA exp -> render_exp ctx exp
  | DefA defid -> string_of_defid defid |> adoc_as_code ctx

and render_args ctx args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map (render_arg ctx) args) ^ ")"

and string_of_targs targs = Sl.Print.string_of_targs targs

(* Parameters *)

and render_param ctx (param : param) =
  match param.it with
  | ExpP (_typ, exp) -> render_exp ctx exp
  | DefP (defid, _, _, _) -> string_of_defid defid |> adoc_as_code ctx

and render_params ctx params =
  match params with
  | [] -> ""
  | params ->
      "(" ^ String.concat ", " (List.map (render_param ctx) params) ^ ")"

(* Backtracking blocks (CaseI / TryI).
   Fallthrough markers emitted only on instructions that may raise Unmatch:
   IfI, LetI, RuleI, CheckLetSubI, CheckLetMatchI, and OptionGetI. *)

module BlockLabel : sig
  type t = { id : string; display : string }

  val set_namespace : string -> unit
  val fresh : unit -> t
end = struct
  type t = { id : string; display : string }

  let namespace : string option ref = ref None
  let counters : (string, int) Hashtbl.t = Hashtbl.create 64
  let set_namespace ns = namespace := Some ns

  let fresh () =
    let ns = !namespace in
    let key = Option.value ns ~default:"" in
    let n = (try Hashtbl.find counters key with Not_found -> 0) + 1 in
    Hashtbl.replace counters key n;
    let id =
      match ns with
      | None -> F.asprintf "bk-%d" n
      | Some ns -> F.asprintf "bk-%s-%d" ns n
    in
    { id; display = F.asprintf "#%d" n }
end

type backtrack_target = NextArm of string | OutOfBlock
type backtrack_ctx = { block : BlockLabel.t; target : backtrack_target }

(* Asciidoctor's default ordered-list style cycle, indexed by nesting depth. *)
type ordered_style =
  | Arabic
  | Loweralpha
  | Lowerroman
  | Upperalpha
  | Upperroman

let style_at_level (level : int) : ordered_style =
  let cycle = [| Arabic; Loweralpha; Lowerroman; Upperalpha; Upperroman |] in
  cycle.(((level mod 5) + 5) mod 5)

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

(* Sits at the end of the bullet's first line so the line still starts with
   plain text -- otherwise asciidoctor fails to recognise it as a list item. *)
let render_block_label (block : BlockLabel.t) : string =
  F.asprintf "pass:[<strong id=\"%s\" class=\"bk-label\">%s</strong>]" block.id
    block.display

(* +++...+++ passthrough rather than pass:[...] because the content contains
   '[' and ']' literally, which would terminate pass:[...]. *)
let render_fallthrough_link (bt : backtrack_ctx option) : string =
  match bt with
  | None -> ""
  | Some { block; target } ->
      let text, target_id =
        match target with
        | NextArm letter ->
            ( F.asprintf "else %s-%s" block.display letter,
              F.asprintf "%s-%s" block.id letter )
        | OutOfBlock -> (F.asprintf "fail %s" block.display, block.id)
      in
      F.asprintf "+++<sub class=\"bk-mark\">[<a href=\"#%s\">%s</a>]</sub>+++"
        target_id text

let arm_backtrack_ctx ~(block : BlockLabel.t) ~arm_level ~total (idx : int) :
    backtrack_ctx =
  let target =
    if idx + 1 < total then NextArm (arm_letter arm_level (idx + 1))
    else OutOfBlock
  in
  { block; target }

(* Sits at the end of the arm's first line so the line still starts with
   plain text -- otherwise asciidoctor drops the list-item marker for
   arms whose first instruction is If / If-let. The bk-arm-anchor class
   supplies scroll-margin-top so fragment links land looking at the arm
   header rather than past it. *)
let arm_anchor ~(block : BlockLabel.t) ~arm_level (idx : int) : string =
  F.asprintf "+++<span class=\"bk-arm-anchor\" id=\"%s-%s\"></span>+++" block.id
    (arm_letter arm_level idx)

(* Guards, iterators, instructions *)

let render_guard ctx (exp_scrut : exp) (guard : guard) : string =
  match guard with
  | BoolG true -> render_exp ctx exp_scrut
  | BoolG false ->
      (* Mirror prose/prosify.ml:473-488: rebuild the scrutinee as a NotE so
         render_exp's UnE handlers (prose_false hint, "is not in", "does not
         match pattern", …) fire instead of dropping it as bare "false". *)
      let scrut_node = exp_scrut.node in
      let neg_inner =
        UnE (`NotOp, `BoolT, exp_scrut) $$ (scrut_node.at, scrut_node.note)
      in
      render_exp ctx (Annot.no_hints neg_inner)
  | CmpG (cmpop, _, e) ->
      render_exp ctx exp_scrut ^ " " ^ render_cmpop ctx cmpop ^ " "
      ^ render_exp ctx e
  | SubG typ ->
      F.asprintf "%s has type %s"
        (render_exp_as_code ctx exp_scrut)
        (code_of_typ ctx typ)
  | MatchG pattern ->
      F.asprintf "%s matches pattern %s" (render_exp ctx exp_scrut)
        (code_of_pattern pattern |> adoc_as_code ctx)
  | MemG e -> render_exp ctx exp_scrut ^ " is in " ^ render_exp ctx e
  | CheckLetSubG (_, target) | CheckLetMatchG (_, target) ->
      F.asprintf "let %s be %s"
        (render_exp_as_code ctx target)
        (render_exp ctx exp_scrut)

let render_iterexp_suffix ctx (iterexps : iterexp list) : string =
  match iterexps with
  | [] -> ""
  | _ ->
      let vars = List.concat_map (fun (_, vars) -> vars) iterexps in
      if vars = [] then ""
      else F.asprintf ", for all %s" (render_in_itervars ctx vars)

let render_iterinstr_suffix ctx (iterinstrs : iterinstr list) : string =
  match iterinstrs with
  | [] -> ""
  | _ ->
      let vars =
        List.concat_map (fun (_, vars_in, _vars_out) -> vars_in) iterinstrs
      in
      if vars = [] then ""
      else F.asprintf ", for each %s" (render_in_itervars ctx vars)

let render_rel_title_math ctx (rel_signature : rel_signature) (exps : exp list)
    : string =
  let nottyp, inputs = rel_signature in
  let mixop = Mixfix.to_mixop nottyp.it in
  let sexps = List.map (render_exp in_code) exps in
  let num_outputs = Mixop.arity mixop - List.length sexps in
  let holes = List.init num_outputs (fun _ -> "%") in
  let padded = Hints.Input.combine inputs sexps holes in
  Mixop.assemble ~string_of_atom:code_of_atom mixop padded |> adoc_as_code ctx

let rec render_instr ?(level = 0) ?(unordered = false)
    ?(backtrack : backtrack_ctx option = None) (instr : instr) : string =
  let bullet =
    if unordered then adoc_unordered_bullet level else adoc_ordered_bullet level
  in
  let hints = instr.hints in
  match instr.node.it with
  | IfI (cond, iterexps, block_then, _) ->
      let fallthrough = render_fallthrough_link backtrack in
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
  | HoldI (id_rel, notexp, iterexps, holdcase) -> (
      let exps = Mixfix.args notexp in
      let hint_true = hints.Annot.prose_true in
      let hint_false = hints.Annot.prose_false in
      let iter_suffix = render_iterexp_suffix in_prose iterexps in
      let render_head ~(hold : bool) : string =
        let h_opt = if hold then hint_true else hint_false in
        let fallback_verb = if hold then " holds" else " does not hold" in
        match h_opt with
        | Some h ->
            render_alter_hint in_link h (reindent_lines ~level:0) render_exp
              exps
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
            (render_instrs ~level:(level + 1) ~backtrack block_nothold))
  | CaseI (exp_scrut, cases, dangle) -> (
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
          |> String.concat "\n")
  | GroupI (_id_rulegroup, id_rel, rel_signature, exps, block) ->
      let hint_in = hints.Annot.prose_in in
      let hint_true = hints.Annot.prose_true in
      let title =
        match (hint_in, hint_true) with
        | Some h, _ | _, Some h ->
            render_alter_hint ~caps:true in_link h (reindent_lines ~level:0)
              render_exp exps
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
        | None, None ->
            render_rel_title_math in_prose rel_signature exps
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
      in
      F.asprintf "%s%s:%s" bullet title
        (render_instrs ~level:(level + 1) ~backtrack block)
  | TryI arms ->
      let block = BlockLabel.fresh () in
      let arm_level = level + 1 in
      let body_level = level + 2 in
      let total = List.length arms in
      let render_arm idx arm =
        let bt = arm_backtrack_ctx ~block ~arm_level ~total idx in
        let anchor = arm_anchor ~block ~arm_level idx in
        F.asprintf "%s{empty}%s%s"
          (adoc_ordered_bullet arm_level)
          anchor
          (render_instrs ~level:body_level ~backtrack:(Some bt) arm)
      in
      F.asprintf "%sTry %s:\n%s" bullet (render_block_label block)
        (String.concat "\n" (List.mapi render_arm arms))
  | LetI (exp_l, exp_r, iterinstrs) ->
      (* RHS can contain a CallE to a partial defined function, which
         backtracks via Unmatch. *)
      let fallthrough = render_fallthrough_link backtrack in
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
        let inner_bullet = adoc_unordered_bullet (level + 1) in
        let body =
          F.asprintf "%sLet %s be %s." inner_bullet
            (render_exp_as_code in_prose exp_l)
            (render_exp in_prose exp_r)
        in
        F.asprintf
          "%sLet %s obtained by repeating:\n+\n--\n%s\n--\n+\nfor each %s.%s"
          bullet
          (render_out_itervars in_prose vars_out_visible)
          body
          (render_in_itervars in_prose vars_in_all)
          fallthrough
  | RuleI (id_rel, notexp, input_hint, iterinstrs) ->
      let exps = Mixfix.args notexp in
      let fallthrough = render_fallthrough_link backtrack in
      let exps_in, exps_out = Hints.Input.split input_hint exps in
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
        | Some h_in, Some h_out ->
            let prose_out =
              render_alter_hint in_link h_out unindent_lines render_exp exps_out
            in
            let prose_in =
              render_alter_hint in_link h_in unindent_lines render_exp exps_in
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
        let inner_bullet = adoc_unordered_bullet (level + 1) in
        F.asprintf
          "%sLet %s obtained by repeating:\n+\n--\n%s%s.\n--\n+\nfor each %s.%s"
          bullet
          (render_out_itervars in_prose vars_out_visible)
          inner_bullet rule_body
          (render_in_itervars in_prose vars_in_all)
          fallthrough
  | ResultI (rel_signature, exps) -> (
      (* Mirrors the old prosify decision (prosify.ml ~628):
           is_conditional       → "Then, the relation holds."
           prose_out hint set   → "Result in <alter-hinted exps>."
           neither, no exps     → "The relation holds."
           neither, has exps    → "Result in <exps>." *)
      let nottyp, input_hint = rel_signature in
      let typs = Mixfix.args nottyp.it in
      let is_conditional = Hints.Input.is_conditional input_hint typs in
      if is_conditional then bullet ^ "Then, the relation holds."
      else
        match (hints.Annot.prose_out, exps) with
        | Some h, _ ->
            F.asprintf "%sResult in %s." bullet
              (render_alter_hint in_prose h (reindent_lines ~level:0) render_exp
                 exps)
        | None, [] -> bullet ^ "The relation holds."
        | None, _ ->
            F.asprintf "%sResult in %s." bullet (render_exps in_prose exps))
  | ReturnI e -> F.asprintf "%sReturn %s." bullet (render_exp in_prose e)
  | DebugI e -> F.asprintf "%s(debug: %s)" bullet (render_exp in_prose e)
  | DestructI (fields, exp_source) -> (
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
          let names = List.map fst projections in
          let exps_target = List.map snd projections in
          F.asprintf "%sLet %s be %s of %s." bullet
            (render_exps in_prose exps_target)
            (render_list (List.map (fun s -> "the " ^ s) names))
            (render_exp in_prose exp_source))
  | CheckLetSubI (_, exp_l, exp_r, block_inner)
  | CheckLetMatchI (_, exp_l, exp_r, block_inner) ->
      let fallthrough = render_fallthrough_link backtrack in
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
  | OptionGetI (exp_l, exp_r, block_inner) ->
      let fallthrough = render_fallthrough_link backtrack in
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

and render_instrs ?(level = 0) ?(backtrack : backtrack_ctx option = None)
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

(* Definitions *)

let rec lift_synthesized_exp (exp : Sl.exp) : exp =
  let it' =
    match exp.it with
    | Il.VarE id -> VarE id
    | Il.IterE (exp_inner, (iter, vars)) ->
        IterE (lift_synthesized_exp exp_inner, (iter, vars))
    | _ -> assert false
  in
  Annot.no_hints (it' $$ (exp.at, exp.note))

let render_rel_title_adoc (hints : Annot.hints) (id_rel : id)
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
  | Some h_in, Some h_out, Some exps_out_sl, _ ->
      let exps_out = List.map lift_synthesized_exp exps_out_sl in
      F.asprintf "%s:\n\n%s%s:\n%s%s."
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose h_in (reindent_lines ~level:1)
           render_exp exps_in_title)
        (adoc_unordered_bullet 0)
        ("Result in "
        ^ render_alter_hint ~caps:false in_prose h_out (reindent_lines ~level:1)
            render_exp exps_out)
  | Some h_in, _, _, _ ->
      F.asprintf "%s:\n\n%s%s."
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose h_in (reindent_lines ~level:1)
           render_exp exps_in_title)
  | _, _, _, Some h_true ->
      F.asprintf "%s:\n\n%s%s"
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose h_true (reindent_lines ~level:0)
           render_exp exps)
  | _ ->
      F.asprintf "%s: %s"
        (Sl.Print.string_of_relid id_rel)
        (render_rel_title_math in_prose rel_signature exps)
      |> adoc_as_link in_prose ~link:(string_of_relid id_rel)

let render_func_title_adoc (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : string =
  match (hints.prose_in, hints.prose_true) with
  | Some h, _ | _, Some h ->
      F.asprintf "%s:\n\n%s%s"
        (string_of_defid id_func
        |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose h (reindent_lines ~level:0)
           render_param params)
  | None, None ->
      (string_of_defid id_func
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func))
      ^ Sl.Print.string_of_tparams tparams
      ^ render_params (in_link |> code) params

let render_func_header (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : string =
  match (hints.prose_in, hints.prose_true) with
  | Some h, _ | _, Some h ->
      render_alter_hint ~caps:true in_prose h (reindent_lines ~level:0)
        render_param params
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func)
  | None, None ->
      string_of_defid id_func
      ^ Sl.Print.string_of_tparams tparams
      ^ render_params (in_link |> code) params
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func)

let render_extern_rel_def (hints : Annot.hints) (externrel : externrel) : string
    =
  let id_rel, rel_signature, exps = externrel in
  render_rel_title_adoc hints id_rel rel_signature exps

let strip_leading_newline (s : string) : string =
  if String.length s > 0 && s.[0] = '\n' then
    String.sub s 1 (String.length s - 1)
  else s

let rec collect_groups_instr (instr : instr) : instr list =
  match instr.node.it with
  | IfI (_, _, block_then, _) -> collect_groups_block block_then
  | HoldI (_, _, _, holdcase) -> (
      match holdcase with
      | BothH (b1, b2) -> collect_groups_block b1 @ collect_groups_block b2
      | HoldH (b, _) | NotHoldH (b, _) -> collect_groups_block b)
  | CaseI (_, cases, _) ->
      cases
      |> List.concat_map (fun (_guard, block) -> collect_groups_block block)
  | TryI arms -> arms |> List.concat_map collect_groups_block
  | CheckLetSubI (_, _, _, block_inner) | CheckLetMatchI (_, _, _, block_inner)
    ->
      collect_groups_block block_inner
  | OptionGetI (_, _, block_inner) -> collect_groups_block block_inner
  | GroupI _ -> [ instr ]
  | LetI _ | RuleI _ | ResultI _ | ReturnI _ | DebugI _ | DestructI _ -> []

and collect_groups_block (block : block) : instr list =
  block |> List.concat_map collect_groups_instr

let render_group (instr : instr) : string =
  let hints = instr.hints in
  match instr.node.it with
  | GroupI (_id_rulegroup, id_rel, rel_signature, exps, block) ->
      let hint_in = hints.Annot.prose_in in
      let hint_true = hints.Annot.prose_true in
      let title =
        match (hint_in, hint_true) with
        | Some h, _ | _, Some h ->
            render_alter_hint ~caps:true in_link h (reindent_lines ~level:0)
              render_exp exps
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
        | None, None ->
            render_rel_title_math in_prose rel_signature exps
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
      in
      title ^ ":\n" ^ render_instrs block
  | _ -> assert false

let render_elseblock (elseblock_opt : elseblock option) : string =
  match elseblock_opt with
  | None | Some [] -> ""
  | Some block ->
      "\n\n" ^ adoc_ordered_bullet 0 ^ "Otherwise:"
      ^ render_instrs ~level:1 block

let render_defined_rel_def (hints : Annot.hints) (rel : rel) : string =
  let id_rel, rel_signature, exps, block, elseblock_opt = rel in
  render_rel_title_adoc hints id_rel rel_signature exps
  ^ "\n\n"
  ^ (collect_groups_block block |> List.map render_group |> String.concat "\n\n")
  ^ render_elseblock elseblock_opt

let render_extern_func_def (hints : Annot.hints) (externfunc : externfunc) :
    string =
  let id_func, tparams, params, _typ = externfunc in
  render_func_header hints id_func tparams params

let render_builtin_func_def (hints : Annot.hints) (builtinfunc : builtinfunc) :
    string =
  let id_func, tparams, params, _typ = builtinfunc in
  render_func_header hints id_func tparams params

let render_table_func_def (hints : Annot.hints) (tablefunc : tablefunc) : string
    =
  let id_func, params, _typ, tablerows = tablefunc in
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
           let exps_sig, exp_res, _block = tablerow in
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

let def_id_for_labels (def : def) : string option =
  match def.node.it with
  | ExternTypD _ | TypD _ | VarD _ -> None
  | ExternRelD (id, _, _) | RelD (id, _, _, _, _) -> Some id.it
  | ExternDecD (id, _, _, _)
  | BuiltinDecD (id, _, _, _)
  | TableDecD (id, _, _, _)
  | FuncDecD (id, _, _, _, _, _) ->
      Some id.it

let render_def (def : def) : string option =
  Option.iter BlockLabel.set_namespace (def_id_for_labels def);
  let hints = def.hints in
  match def.node.it with
  | ExternTypD _ | TypD _ | VarD _ -> None
  | ExternRelD externrel -> Some (render_extern_rel_def hints externrel)
  | RelD rel -> Some (render_defined_rel_def hints rel)
  | ExternDecD externfunc -> Some (render_extern_func_def hints externfunc)
  | BuiltinDecD builtinfunc -> Some (render_builtin_func_def hints builtinfunc)
  | TableDecD tablefunc -> Some (render_table_func_def hints tablefunc)
  | FuncDecD func -> Some (render_defined_func_def hints func)

let render_defs (defs : def list) : string =
  defs |> List.filter_map render_def |> String.concat "\n\n"

let render_spec (spec : spec) : string = render_defs spec
