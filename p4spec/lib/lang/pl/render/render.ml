open Domain
open Lib
open Xl
open Ast
open Util.Source
module F = Format
module Backtrack = Backtrack
open Adoc

(* Rendering mode: prose words or math/code symbols *)

type mode = Prose | Code

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

(* Oxford-comma join over inline documents (the [Doc.t] analogue of
   [render_list]). *)

let render_list_doc (items : Doc.t list) : Doc.t =
  match items with
  | [] -> Doc.empty
  | [ item ] -> item
  | [ item_a; item_b ] -> Doc.seq [ item_a; Doc.text " and "; item_b ]
  | _ ->
      let items_rev = List.rev items in
      let items, item_last =
        (items_rev |> List.tl |> List.rev, items_rev |> List.hd)
      in
      Doc.seq
        [
          Doc.seq
            (List.mapi
               (fun i x -> if i = 0 then x else Doc.seq [ Doc.text ", "; x ])
               items);
          Doc.text ", and "; item_last;
        ]

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

let render_varid (_mode : mode) (id_var : id) : Doc.t =
  if Id.is_underscored id_var then Doc.code (Doc.text "++_++")
  else
    let var_slices = String.split_on_char '_' id_var.it in
    match var_slices with
    | [] -> assert false
    | [ var_type ] -> Doc.code (Doc.text var_type)
    | var_type :: var_subscripts ->
        Doc.code
          (Doc.text
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

let render_var (mode : mode) ((id, _typ, iters) : var) : Doc.t =
  if Id.is_underscored id then Doc.code (Doc.text "++_++")
  else
    Doc.code
      (Doc.seq
         [
           render_varid mode id;
           Doc.text (String.concat "" (List.map code_of_iter iters));
         ])

let render_in_itervars (vars : var list) : Doc.t =
  let render_in_var var =
    Doc.seq
      [
        Doc.code (render_var Code var);
        Doc.text " in ";
        Doc.code
          (Doc.seq [ render_var Code var; Doc.text (code_of_iter List) ]);
      ]
  in
  render_list_doc (List.map render_in_var vars)

let render_out_itervars (vars : var list) : Doc.t =
  vars
  |> List.filter_map (fun var ->
         let id, _, _ = var in
         if Id.is_underscored id then None
         else
           Some
             (Doc.seq
                [
                  Doc.code
                    (Doc.seq
                       [ render_var Code var; Doc.text (code_of_iter List) ]);
                  Doc.text " be the list";
                ]))
  |> render_list_doc

(* Types *)

let code_of_typ (_mode : mode) (typ : typ) : Doc.t =
  Doc.code (Doc.text (Sl.Print.string_of_typ typ))

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

let rec render_exp (mode : mode) (exp : exp) : Doc.t =
  match exp.node.it with
  | BoolE b -> render_bool_exp b
  | NumE n -> render_num_exp n
  | TextE text -> render_text_exp text
  | VarE id_var -> render_var_exp id_var
  | UnE (unop, _, exp_inner) -> render_un_exp mode unop exp_inner
  | BinE (binop, _, exp_l, exp_r) -> render_bin_exp mode binop exp_l exp_r
  | CmpE (cmpop, _, exp_l, exp_r) -> render_cmp_exp mode cmpop exp_l exp_r
  | UpCastE (_, exp_inner) | DownCastE (_, exp_inner) ->
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

and render_exps (mode : mode) ?(sep : string option) (exps : exp list) : Doc.t =
  match (mode, sep) with
  | _, Some sep ->
      Doc.seq
        (List.mapi
           (fun i exp ->
             if i = 0 then render_exp mode exp
             else Doc.seq [ Doc.text sep; render_exp mode exp ])
           exps)
  | Code, None ->
      Doc.seq
        (List.mapi
           (fun i exp ->
             if i = 0 then render_exp mode exp
             else Doc.seq [ Doc.text ", "; render_exp mode exp ])
           exps)
  | Prose, None ->
      Doc.text
        (render_list
           (List.map (fun exp -> Doc.to_adoc (render_exp mode exp)) exps))

and render_exp_as_code (_mode : mode) (exp : exp) : Doc.t =
  Doc.code (render_exp Code exp)

and code_of_notexp (notexp : notexp) : Doc.t =
  let mixop, exps = Mixfix.split notexp in
  let sexps = List.map (fun e -> Doc.to_adoc_code (render_exp Code e)) exps in
  Doc.code (Doc.text (Mixop.assemble ~string_of_atom:code_of_atom mixop sexps))

(* Boolean expression rendering *)

and render_bool_exp (b : bool) : Doc.t = Doc.code (Doc.text (string_of_bool b))

(* Numeric expression rendering *)

and render_num_exp (n : num) : Doc.t = Doc.code (Doc.text (string_of_num n))

(* Text expression rendering *)

and render_text_exp (text : string) : Doc.t =
  Doc.code (Doc.text ("\"" ^ String.escaped text ^ "\""))

(* Variable expression rendering *)

and render_var_exp (id_var : id) : Doc.t = Doc.code (render_varid Code id_var)

(* Unary expression rendering *)

and render_negated_exp_opt (mode : mode) (exp : exp) : Doc.t option =
  match exp.node.it with
  | MatchE (exp_e, pattern) ->
      Some
        (Doc.seq
           [
             render_exp mode exp_e;
             Doc.text " does not match pattern ";
             Doc.code (Doc.text (code_of_pattern pattern));
           ])
  | SubE (exp_e, typ) ->
      Some
        (Doc.seq
           [
             render_exp_as_code mode exp_e;
             Doc.text " does not have type ";
             code_of_typ mode typ;
           ])
  | MemE (exp_e, exp_s) ->
      Some
        (Doc.seq
           [
             render_exp_as_code mode exp_e;
             Doc.text " is not in ";
             render_exp_as_code mode exp_s;
           ])
  | CallE (id, _targs, args) when mode = Prose -> (
      let hint_false_opt = exp.hints.Annot.prose_false in
      match hint_false_opt with
      | Some hints ->
          Some
            (Doc.link ~target:id.it
               (Doc.text
                  (render_alter_hint mode hints (reindent_lines ~level:0)
                     (fun a -> Doc.to_adoc_in_link (render_arg Prose a))
                     args)))
      | None ->
          Some
            (Doc.code
               (Doc.seq [ Doc.text (render_unop `NotOp); render_exp Code exp ]))
      )
  | _ -> None

and render_un_exp (mode : mode) (unop : unop) (exp : exp) : Doc.t =
  match unop with
  | #Bool.unop -> (
      match render_negated_exp_opt mode exp with
      | Some t -> t
      | None ->
          Doc.code
            (Doc.seq [ Doc.text (render_unop unop); render_exp Code exp ]))
  | _ -> Doc.code (Doc.seq [ Doc.text (render_unop unop); render_exp Code exp ])

(* Binary expression rendering *)

and render_bin_exp (mode : mode) (binop : binop) (exp_l : exp) (exp_r : exp) :
    Doc.t =
  match binop with
  | `ImplOp when mode = Prose ->
      Doc.seq
        [
          Doc.text "if ";
          render_exp mode exp_l;
          Doc.text ", then ";
          render_exp mode exp_r;
        ]
  | #Bool.binop ->
      Doc.seq
        [
          render_exp mode exp_l;
          Doc.text " ";
          Doc.text (render_binop_word mode binop);
          Doc.text " ";
          render_exp mode exp_r;
        ]
  | #Num.binop ->
      Doc.code
        (Doc.seq
           [
             render_exp Code exp_l;
             Doc.text (" " ^ Sl.Print.string_of_binop binop ^ " ");
             render_exp Code exp_r;
           ])

(* Comparison expression rendering *)

and render_cmp_exp (mode : mode) (cmpop : cmpop) (exp_l : exp) (exp_r : exp) :
    Doc.t =
  Doc.seq
    [
      render_exp mode exp_l;
      Doc.text " ";
      Doc.text (render_cmpop_word mode cmpop);
      Doc.text " ";
      render_exp mode exp_r;
    ]

(* Cast expression rendering *)

and render_cast_exp (mode : mode) (exp_inner : exp) : Doc.t =
  render_exp_as_code mode exp_inner

(* Subtype check expression rendering *)

and render_sub_exp (mode : mode) (exp_inner : exp) (typ : typ) : Doc.t =
  Doc.seq
    [
      render_exp_as_code mode exp_inner;
      Doc.text " has type ";
      code_of_typ mode typ;
    ]

(* Pattern match check expression rendering *)

and render_match_exp (mode : mode) (exp_inner : exp) (pattern : pattern) : Doc.t
    =
  match pattern with
  | Il.CaseP mixop when Mixop.arity mixop = 0 ->
      Doc.seq
        [
          render_exp mode exp_inner;
          Doc.text " is ";
          Doc.code (Doc.text (code_of_pattern (Il.CaseP mixop)));
        ]
  | Il.ListP `Nil ->
      Doc.seq [ render_exp mode exp_inner; Doc.text " is an empty list" ]
  | Il.ListP `Cons ->
      Doc.seq [ render_exp mode exp_inner; Doc.text " is a non-empty list" ]
  | Il.ListP (`Fixed len) ->
      Doc.seq
        [
          render_exp mode exp_inner;
          Doc.text (F.asprintf " is a list of length %d" len);
        ]
  | Il.OptP `None -> Doc.seq [ render_exp mode exp_inner; Doc.text " is none" ]
  | Il.OptP `Some ->
      Doc.seq [ render_exp mode exp_inner; Doc.text " is defined" ]
  | pattern ->
      Doc.seq
        [
          render_exp mode exp_inner;
          Doc.text " matches pattern ";
          Doc.code (Doc.text (code_of_pattern pattern));
        ]

(* Tuple expression rendering *)

and render_tuple_exp (mode : mode) (exps : exp list) : Doc.t =
  Doc.seq [ Doc.text "( "; render_exps mode ~sep:", " exps; Doc.text " )" ]

(* Case expression rendering *)

and render_case_exp (mode : mode) (exp : exp) (notexp : notexp) : Doc.t =
  match mode with
  | Code -> code_of_notexp notexp
  | Prose -> (
      let hint_opt = exp.hints.Annot.prose in
      let link_opt = tid_of_typ exp.node.note in
      match (hint_opt, link_opt) with
      | Some hints, Some tid ->
          Doc.link ~target:tid.it
            (Doc.text
               (render_alter_hint mode hints (reindent_lines ~level:0)
                  (fun e -> Doc.to_adoc_in_link (render_exp Prose e))
                  (Mixfix.args notexp)))
      | _ -> code_of_notexp notexp)

(* Struct expression rendering *)

and render_str_exp (mode : mode) (expfields : (atom * exp) list) : Doc.t =
  Doc.seq
    [
      Doc.text "+{+";
      Doc.seq
        (List.mapi
           (fun i (atom, exp_f) ->
             let field =
               Doc.seq
                 [
                   Doc.text (code_of_atom atom);
                   Doc.text " ";
                   render_exp mode exp_f;
                 ]
             in
             if i = 0 then field else Doc.seq [ Doc.text ", "; field ])
           expfields);
      Doc.text "+}+";
    ]

(* Option expression rendering *)

and render_opt_exp (mode : mode) (exp_opt : exp option) : Doc.t =
  match exp_opt with
  | Some exp_inner -> render_exp mode exp_inner
  | None -> Doc.code (Doc.text "·")

(* List expression rendering *)

and render_list_exp (_mode : mode) (exps : exp list) : Doc.t =
  match exps with
  | [] -> Doc.code (Doc.text "·")
  | [ exp_inner ] -> Doc.code (render_exp Code exp_inner)
  | exps ->
      Doc.code
        (Doc.seq
           [ Doc.text "+[+ "; render_exps Code ~sep:", " exps; Doc.text " +]+" ])

(* Cons expression rendering *)

and render_cons_exp (_mode : mode) (exp_h : exp) (exp_t : exp) : Doc.t =
  Doc.code
    (Doc.seq
       [
         render_exp Code exp_h; Doc.text " {two-colons} "; render_exp Code exp_t;
       ])

(* Concatenation expression rendering *)

and render_cat_exp (mode : mode) (exp_l : exp) (exp_r : exp) : Doc.t =
  match mode with
  | Code ->
      Doc.seq
        [ render_exp mode exp_l; Doc.text " {pp} "; render_exp mode exp_r ]
  | Prose ->
      Doc.seq
        [
          render_exp mode exp_l;
          Doc.text " concatenated with ";
          render_exp mode exp_r;
        ]

(* Membership expression rendering *)

and render_mem_exp (mode : mode) (exp_e : exp) (exp_s : exp) : Doc.t =
  Doc.seq [ render_exp mode exp_e; Doc.text " is in "; render_exp mode exp_s ]

(* Length expression rendering *)

and render_len_exp (mode : mode) (exp_inner : exp) : Doc.t =
  Doc.seq [ Doc.text "the length of "; render_exp mode exp_inner ]

(* Dot expression rendering *)

and render_dot_exp (_mode : mode) (exp_b : exp) (atom : atom) : Doc.t =
  Doc.code
    (Doc.seq
       [ render_exp Code exp_b; Doc.text "."; Doc.text (code_of_atom atom) ])

(* Index expression rendering *)

and render_idx_exp (_mode : mode) (exp_b : exp) (exp_i : exp) : Doc.t =
  Doc.code
    (Doc.seq
       [
         render_exp Code exp_b;
         Doc.text "[";
         render_exp Code exp_i;
         Doc.text "]";
       ])

(* Slice expression rendering *)

and render_slice_exp (_mode : mode) (exp_b : exp) (exp_l : exp) (exp_h : exp) :
    Doc.t =
  Doc.code
    (Doc.seq
       [
         render_exp Code exp_b;
         Doc.text "[";
         render_exp Code exp_l;
         Doc.text " : ";
         render_exp Code exp_h;
         Doc.text "]";
       ])

(* Update expression rendering *)

and render_upd_exp (mode : mode) (exp_b : exp) (path : path) (exp_f : exp) :
    Doc.t =
  match mode with
  | Code ->
      Doc.code
        (Doc.seq
           [
             render_exp Code exp_b;
             Doc.text "[";
             render_path Code path;
             Doc.text " = ";
             render_exp Code exp_f;
             Doc.text "]";
           ])
  | Prose ->
      Doc.seq
        [
          Doc.code (render_exp Code exp_b);
          Doc.text " with ";
          Doc.code (render_path Code path);
          Doc.text " set to ";
          Doc.code (render_exp Code exp_f);
        ]

(* Function call expression rendering *)

and render_call_exp (mode : mode) (exp : exp) (id : id) (targs : targ list)
    (args : arg list) : Doc.t =
  let hint_in = exp.hints.Annot.prose_in in
  let hint_true = exp.hints.Annot.prose_true in
  match mode with
  | Code ->
      Doc.code
        (Doc.link ~target:id.it
           (Doc.seq
              [
                Doc.text (string_of_defid id);
                Doc.text (string_of_targs targs);
                render_args Code args;
              ]))
  | Prose -> (
      match (hint_in, hint_true) with
      | Some hints, _ | _, Some hints ->
          Doc.link ~target:id.it
            (Doc.text
               (render_alter_hint mode hints (reindent_lines ~level:0)
                  (fun a -> Doc.to_adoc_in_link (render_arg Prose a))
                  args))
      | None, None ->
          Doc.code
            (Doc.link ~target:id.it
               (Doc.seq
                  [
                    Doc.text (string_of_defid id);
                    Doc.text (string_of_targs targs);
                    render_args Code args;
                  ])))

(* Iterated expression rendering *)

and render_iter_exp (mode : mode) (exp_inner : exp) (iterexp : iterexp) : Doc.t
    =
  match (exp_inner.node.it, iterexp) with
  | _, (_, []) -> render_exp mode exp_inner
  | (VarE _ | TupleE _), _ ->
      Doc.code
        (Doc.seq
           [ render_exp Code exp_inner; Doc.text (code_of_iterexp iterexp) ])
  | _ ->
      let inner = render_exp Code exp_inner in
      let sexp = Doc.to_adoc_code inner in
      if String.contains sexp ' ' then
        Doc.code
          (Doc.seq
             [ Doc.text "( "; inner; Doc.text (" )" ^ code_of_iterexp iterexp) ])
      else Doc.code (Doc.seq [ inner; Doc.text (code_of_iterexp iterexp) ])

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

and render_path (mode : mode) (path : path) : Doc.t =
  match path.it with
  | RootP -> Doc.empty
  | IdxP (path, exp) ->
      Doc.seq
        [
          render_path mode path; Doc.text "["; render_exp mode exp; Doc.text "]";
        ]
  | SliceP (path, exp_l, exp_h) ->
      Doc.seq
        [
          render_path mode path;
          Doc.text "[";
          render_exp mode exp_l;
          Doc.text " : ";
          render_exp mode exp_h;
          Doc.text "]";
        ]
  | DotP ({ it = RootP; _ }, atom) -> Doc.text (code_of_atom atom)
  | DotP (path, atom) ->
      Doc.seq
        [ render_path mode path; Doc.text "."; Doc.text (code_of_atom atom) ]

(* Parameters *)

and render_param (mode : mode) (param : param) : Doc.t =
  match param.it with
  | ExpP (_, exp) -> render_exp mode exp
  | DefP (defid, _, _, _) -> Doc.code (Doc.text (string_of_defid defid))

and render_params (mode : mode) (params : param list) : Doc.t =
  match params with
  | [] -> Doc.empty
  | params ->
      Doc.seq
        [
          Doc.text "(";
          Doc.seq
            (List.mapi
               (fun i param ->
                 if i = 0 then render_param mode param
                 else Doc.seq [ Doc.text ", "; render_param mode param ])
               params);
          Doc.text ")";
        ]

(* Type arguments *)

and string_of_targs (targs : targ list) = Sl.Print.string_of_targs targs

(* Arguments *)

and render_arg (mode : mode) (arg : arg) : Doc.t =
  match arg.it with
  | ExpA exp -> render_exp mode exp
  | DefA defid -> Doc.code (Doc.text (string_of_defid defid))

and render_args (mode : mode) (args : arg list) : Doc.t =
  match args with
  | [] -> Doc.empty
  | args ->
      Doc.seq
        [
          Doc.text "(";
          Doc.seq
            (List.mapi
               (fun i a ->
                 if i = 0 then render_arg mode a
                 else Doc.seq [ Doc.text ", "; render_arg mode a ])
               args);
          Doc.text ")";
        ]

(* Case analysis *)

let render_guard (mode : mode) (exp_scrut : exp) (guard : guard) : Doc.t =
  match guard with
  | BoolG true -> render_exp mode exp_scrut
  | BoolG false ->
      let node_scrut = exp_scrut.node in
      let neg_inner =
        UnE (`NotOp, `BoolT, exp_scrut) $$ (node_scrut.at, node_scrut.note)
      in
      render_exp mode (Annot.no_hints neg_inner)
  | CmpG (cmpop, _, exp) ->
      Doc.seq
        [
          render_exp mode exp_scrut;
          Doc.text " ";
          Doc.text (render_cmpop_word mode cmpop);
          Doc.text " ";
          render_exp mode exp;
        ]
  | SubG typ ->
      Doc.seq
        [
          render_exp_as_code mode exp_scrut;
          Doc.text " has type ";
          code_of_typ mode typ;
        ]
  | MatchG pattern ->
      Doc.seq
        [
          render_exp mode exp_scrut;
          Doc.text " matches pattern ";
          Doc.code (Doc.text (code_of_pattern pattern));
        ]
  | MemG exp ->
      Doc.seq
        [ render_exp mode exp_scrut; Doc.text " is in "; render_exp mode exp ]
  | CheckLetSubG (_, target) | CheckLetMatchG (_, target) ->
      Doc.seq
        [
          Doc.text "let ";
          render_exp_as_code mode target;
          Doc.text " be ";
          render_exp mode exp_scrut;
        ]

(* Instructions *)

let rec render_instr ?(level : int = 0) ?(unordered : bool = false)
    ?(backtrack : Backtrack.ctx option = None) (instr : instr) : string =
  let bullet =
    if unordered then adoc_unordered_bullet level else adoc_ordered_bullet level
  in
  let hints = instr.hints in
  match instr.node.it with
  | IfI (cond, iterexps, block_then, _) ->
      Block.serialize
        (render_if_instr ~level ~bullet ~backtrack cond iterexps block_then)
  | HoldI (id_rel, notexp, iterexps, holdcase) ->
      Block.serialize
        (render_hold_instr ~level ~bullet ~backtrack hints id_rel notexp iterexps
           holdcase)
  | CaseI (exp_scrut, cases, dangle) ->
      Block.serialize
        (render_case_instr ~level ~bullet ~backtrack exp_scrut cases dangle)
  | GroupI (_id_rulegroup, id_rel, rel_signature, exps, block) ->
      Block.serialize
        (render_group_instr ~level ~bullet ~backtrack hints id_rel rel_signature
           exps block)
  | TryI arms -> Block.serialize (render_try_instr ~level ~bullet arms)
  | LetI (exp_l, exp_r, iterinstrs) ->
      Block.serialize
        (render_let_instr ~level ~bullet ~backtrack exp_l exp_r iterinstrs)
  | RuleI (id_rel, notexp, hint_input, iterinstrs) ->
      Block.serialize
        (render_rule_instr ~level ~bullet ~backtrack hints id_rel notexp
           hint_input iterinstrs)
  | ResultI (rel_signature, exps) ->
      Block.serialize (render_result_instr ~bullet hints rel_signature exps)
  | ReturnI exp -> Block.serialize (render_return_instr ~bullet exp)
  | DebugI exp -> Block.serialize (render_debug_instr ~bullet exp)
  | DestructI (fields, exp_source) ->
      Block.serialize (render_destruct_instr ~bullet fields exp_source)
  | CheckLetSubI (_, exp_l, exp_r, block_inner)
  | CheckLetMatchI (_, exp_l, exp_r, block_inner) ->
      Block.serialize
        (render_check_let_instr ~level ~bullet ~backtrack exp_l exp_r block_inner)
  | OptionGetI (exp_l, exp_r, block_inner) ->
      Block.serialize
        (render_option_get_instr ~level ~bullet ~backtrack exp_l exp_r block_inner)

and render_instrs ?(level : int = 0) ?(backtrack : Backtrack.ctx option = None)
    (instrs : block) : string =
  match instrs with
  | [
   ({ node = { it = ReturnI ({ node = { it = BoolE _; _ }; _ } as e); _ }; _ } :
     instr);
  ] ->
      F.asprintf " return %s." (Doc.to_adoc (render_exp_as_code Prose e))
  | _ ->
      "\n"
      ^ (List.map (render_instr ~level ~backtrack) instrs |> String.concat "\n")

and render_iterexp_suffix (iterexps : iterexp list) : Doc.t =
  match iterexps with
  | [] -> Doc.empty
  | _ ->
      let vars = List.concat_map (fun (_, vars) -> vars) iterexps in
      if vars = [] then Doc.empty
      else Doc.seq [ Doc.text ", for all "; render_in_itervars vars ]

and render_iterinstr_suffix (iterinstrs : iterinstr list) : Doc.t =
  match iterinstrs with
  | [] -> Doc.empty
  | _ ->
      let vars =
        List.concat_map (fun (_, vars_in, _vars_out) -> vars_in) iterinstrs
      in
      if vars = [] then Doc.empty
      else Doc.seq [ Doc.text ", for each "; render_in_itervars vars ]

(* If instruction rendering *)

and render_children ~(level : int) ~(backtrack : Backtrack.ctx option)
    (block : block) : Block.t =
  Block.raw
    (block |> List.map (render_instr ~level ~backtrack) |> String.concat "\n")

and render_if_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (cond : exp) (iterexps : iterexp list)
    (block_then : block) : Block.t =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let head =
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.seq
             [
               Doc.text "Check that ";
               render_exp Prose cond;
               render_iterexp_suffix iterexps;
               Doc.text ".";
               Doc.text fallthrough;
             ]);
      ]
  in
  if block_then = [] then head
  else
    Block.concat
      [ head; Block.raw "\n"; render_children ~level ~backtrack block_then ]

(* Hold instruction rendering *)

and render_hold_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (hints : Annot.hints) (id_rel : id)
    (notexp : notexp) (iterexps : iterexp list) (holdcase : holdcase) : Block.t =
  let exps = Mixfix.args notexp in
  let hint_true = hints.Annot.prose_true in
  let hint_false = hints.Annot.prose_false in
  let iter_suffix = Doc.to_adoc (render_iterexp_suffix iterexps) in
  let render_head ~(hold : bool) : string =
    let hint_opt = if hold then hint_true else hint_false in
    let fallback_verb = if hold then " holds" else " does not hold" in
    match hint_opt with
    | Some hint ->
        Doc.to_adoc
          (Doc.link ~target:(string_of_relid id_rel)
             (Doc.text
                (render_alter_hint Prose hint (reindent_lines ~level:0)
                   (fun e -> Doc.to_adoc_in_link (render_exp Prose e))
                   exps)))
    | None ->
        let math =
          Doc.to_adoc
            (Doc.link ~target:(string_of_relid id_rel)
               (Doc.text (Doc.to_adoc (code_of_notexp notexp))))
        in
        math ^ fallback_verb
  in
  let if_head ~hold =
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.seq
             [
               Doc.text "If ";
               Doc.text (render_head ~hold);
               Doc.text iter_suffix;
               Doc.text ":";
             ]);
      ]
  in
  match holdcase with
  | HoldH (block, _dangle) ->
      Block.concat
        [
          if_head ~hold:true;
          Block.raw (render_instrs ~level:(level + 1) ~backtrack block);
        ]
  | NotHoldH (block, _dangle) ->
      Block.concat
        [
          if_head ~hold:false;
          Block.raw (render_instrs ~level:(level + 1) ~backtrack block);
        ]
  | BothH (block_hold, block_nothold) ->
      Block.concat
        [
          if_head ~hold:true;
          Block.raw (render_instrs ~level:(level + 1) ~backtrack block_hold);
          Block.raw "\n";
          Block.raw bullet;
          Block.inline (Doc.text "Else:");
          Block.raw (render_instrs ~level:(level + 1) ~backtrack block_nothold);
        ]

(* Case analysis instruction rendering *)

and render_case_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_scrut : exp) (cases : case list)
    (dangle : dangle) : Block.t =
  let total = not dangle in
  let n = List.length cases in
  match cases with
  | [ (guard, block_then) ] ->
      let head =
        Block.concat
          [
            Block.raw bullet;
            Block.inline
              (Doc.seq
                 [
                   Doc.text "Check that ";
                   render_guard Prose exp_scrut guard;
                   Doc.text ".";
                 ]);
          ]
      in
      if block_then = [] then head
      else
        Block.concat
          [ head; Block.raw "\n"; render_children ~level ~backtrack block_then ]
  | _ ->
      Block.vseq
        (cases
        |> List.mapi (fun idx (guard, block_then) ->
               if idx = n - 1 && total then
                 Block.concat
                   [
                     Block.raw bullet;
                     Block.inline (Doc.text "Else:");
                     Block.raw
                       (render_instrs ~level:(level + 1) ~backtrack block_then);
                   ]
               else
                 let keyword = if idx = 0 then "If" else "Else if" in
                 Block.concat
                   [
                     Block.raw bullet;
                     Block.inline
                       (Doc.seq
                          [
                            Doc.text (keyword ^ " ");
                            render_guard Prose exp_scrut guard;
                            Doc.text ":";
                          ]);
                     Block.raw
                       (render_instrs ~level:(level + 1) ~backtrack block_then);
                   ]))

(* Group instruction rendering *)

and render_group_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (hints : Annot.hints) (id_rel : id)
    (rel_signature : rel_signature) (exps : exp list) (block : block) : Block.t =
  let hint_in = hints.Annot.prose_in in
  let hint_true = hints.Annot.prose_true in
  let title =
    match (hint_in, hint_true) with
    | Some hint, _ | _, Some hint ->
        Doc.link ~target:(string_of_relid id_rel)
          (Doc.text
             (render_alter_hint ~caps:true Prose hint (reindent_lines ~level:0)
                (fun e -> Doc.to_adoc_in_link (render_exp Prose e))
                exps))
    | None, None ->
        Doc.link ~target:(string_of_relid id_rel)
          (render_rel_title_math rel_signature exps)
  in
  Block.concat
    [
      Block.raw bullet;
      Block.inline (Doc.seq [ title; Doc.text ":" ]);
      Block.raw (render_instrs ~level:(level + 1) ~backtrack block);
    ]

(* Try instruction rendering *)

and render_try_instr ~(level : int) ~(bullet : string) (arms : arm list) :
    Block.t =
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
  Block.concat
    [
      Block.raw bullet;
      Block.inline
        (Doc.seq
           [
             Doc.text "Try ";
             Doc.text (Backtrack.render_block_label block);
             Doc.text ":";
           ]);
      Block.raw "\n";
      Block.raw (String.concat "\n" (List.mapi render_arm arms));
    ]

(* Let instruction rendering *)

and render_let_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_l : exp) (exp_r : exp)
    (iterinstrs : iterinstr list) : Block.t =
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
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.seq
             [
               Doc.text "Let ";
               render_exp_as_code Prose exp_l;
               Doc.text " be ";
               render_exp Prose exp_r;
               render_iterinstr_suffix iterinstrs;
               Doc.text ".";
               Doc.text fallthrough;
             ]);
      ]
  else
    let bullet_inner = adoc_unordered_bullet (level + 1) in
    let body =
      Block.concat
        [
          Block.raw bullet_inner;
          Block.inline
            (Doc.seq
               [
                 Doc.text "Let ";
                 render_exp_as_code Prose exp_l;
                 Doc.text " be ";
                 render_exp Prose exp_r;
                 Doc.text ".";
               ]);
        ]
    in
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.seq
             [
               Doc.text "Let ";
               render_out_itervars vars_out_visible;
               Doc.text " obtained by repeating:";
             ]);
        Block.raw "\n+\n--\n";
        body;
        Block.raw "\n--\n+\nfor each ";
        Block.inline
          (Doc.seq
             [ render_in_itervars vars_in_all; Doc.text "."; Doc.text fallthrough ]);
      ]

(* Rule instruction rendering *)

and render_rule_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (hints : Annot.hints) (id_rel : id)
    (notexp : notexp) (hint_input : Hints.Input.t) (iterinstrs : iterinstr list)
    : Block.t =
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
            (fun e -> Doc.to_adoc_in_link (render_exp Prose e))
            exps_out
        in
        let prose_in =
          Doc.to_adoc
            (Doc.link ~target:(string_of_relid id_rel)
               (Doc.text
                  (render_alter_hint Prose hint_in unindent_lines
                     (fun e -> Doc.to_adoc_in_link (render_exp Prose e))
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
          (Doc.to_adoc
             (Doc.link ~target:(string_of_relid id_rel)
                (Doc.text (Doc.to_adoc (code_of_notexp notexp)))))
  in
  if vars_out_visible = [] then
    Block.concat
      [
        Block.raw bullet;
        Block.raw rule_body;
        Block.inline
          (Doc.seq
             [ render_iterinstr_suffix iterinstrs; Doc.text "."; Doc.text fallthrough ]);
      ]
  else
    let bullet_inner = adoc_unordered_bullet (level + 1) in
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.seq
             [
               Doc.text "Let ";
               render_out_itervars vars_out_visible;
               Doc.text " obtained by repeating:";
             ]);
        Block.raw "\n+\n--\n";
        Block.raw bullet_inner;
        Block.raw rule_body;
        Block.raw ".";
        Block.raw "\n--\n+\nfor each ";
        Block.inline
          (Doc.seq
             [ render_in_itervars vars_in_all; Doc.text "."; Doc.text fallthrough ]);
      ]

(* Result instruction rendering *)

and render_result_instr ~(bullet : string) (hints : Annot.hints)
    (rel_signature : rel_signature) (exps : exp list) : Block.t =
  let nottyp, hint_input = rel_signature in
  let typs = Mixfix.args nottyp.it in
  let is_conditional = Hints.Input.is_conditional hint_input typs in
  let line doc = Block.concat [ Block.raw bullet; Block.inline doc ] in
  if is_conditional then line (Doc.text "Then, the relation holds.")
  else
    match (hints.Annot.prose_out, exps) with
    | Some hint, _ ->
        line
          (Doc.seq
             [
               Doc.text "Result in ";
               Doc.text
                 (render_alter_hint Prose hint (reindent_lines ~level:0)
                    (fun e -> Doc.to_adoc (render_exp Prose e))
                    exps);
               Doc.text ".";
             ])
    | None, [] -> line (Doc.text "The relation holds.")
    | None, _ ->
        line
          (Doc.seq
             [ Doc.text "Result in "; render_exps Prose exps; Doc.text "." ])

(* Return instruction rendering *)

and render_return_instr ~(bullet : string) (exp : exp) : Block.t =
  Block.concat
    [
      Block.raw bullet;
      Block.inline
        (Doc.seq [ Doc.text "Return "; render_exp Prose exp; Doc.text "." ]);
    ]

(* Debug instruction rendering *)

and render_debug_instr ~(bullet : string) (exp : exp) : Block.t =
  Block.concat
    [
      Block.raw bullet;
      Block.inline
        (Doc.seq [ Doc.text "(debug: "; render_exp Prose exp; Doc.text ")" ]);
    ]

(* Destruct instruction rendering *)

and render_destruct_instr ~(bullet : string)
    (fields : (string option * exp) list) (exp_source : exp) : Block.t =
  let projections =
    List.filter_map
      (fun (name_opt, exp_target) ->
        Option.map (fun name -> (name, exp_target)) name_opt)
      fields
  in
  let line doc = Block.concat [ Block.raw bullet; Block.inline doc ] in
  match projections with
  | [ (name, exp_target) ] ->
      line
        (Doc.seq
           [
             Doc.text "Let ";
             render_exp Prose exp_target;
             Doc.text (F.asprintf " be the %s of " name);
             render_exp Prose exp_source;
             Doc.text ".";
           ])
  | _ ->
      let names, exps_target = List.split projections in
      line
        (Doc.seq
           [
             Doc.text "Let ";
             render_exps Prose exps_target;
             Doc.text " be ";
             Doc.text (render_list (List.map (fun s -> "the " ^ s) names));
             Doc.text " of ";
             render_exp Prose exp_source;
             Doc.text ".";
           ])

(* Check-let instruction rendering (CheckLetSubI / CheckLetMatchI) *)

and render_check_let_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_l : exp) (exp_r : exp)
    (block_inner : block) : Block.t =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let head =
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.seq
             [
               Doc.text "Let!~type~ ";
               render_exp_as_code Prose exp_l;
               Doc.text " be ";
               render_exp Prose exp_r;
               Doc.text ".";
               Doc.text fallthrough;
             ]);
      ]
  in
  if block_inner = [] then head
  else
    Block.concat
      [
        head;
        Block.raw "\n";
        Block.raw
          (block_inner
          |> List.map (render_instr ~level ~backtrack)
          |> String.concat "\n");
      ]

(* Option-get instruction rendering *)

and render_option_get_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_l : exp) (exp_r : exp)
    (block_inner : block) : Block.t =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let head =
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.seq
             [
               Doc.text "Let ";
               render_exp_as_code Prose exp_l;
               Doc.text " be ";
               Doc.text (adoc_link ~link:"option_get" "*!*");
               Doc.text " ";
               render_exp Prose exp_r;
               Doc.text ".";
               Doc.text fallthrough;
             ]);
      ]
  in
  if block_inner = [] then head
  else
    Block.concat
      [
        head;
        Block.raw "\n";
        Block.raw
          (block_inner
          |> List.map (render_instr ~level ~backtrack)
          |> String.concat "\n");
      ]

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
            Doc.to_adoc
              (Doc.link ~target:(string_of_relid id_rel)
                 (Doc.text
                    (render_alter_hint ~caps:true Prose hint
                       (reindent_lines ~level:0)
                       (fun e -> Doc.to_adoc_in_link (render_exp Prose e))
                       exps)))
        | None, None ->
            Doc.to_adoc
              (Doc.link ~target:(string_of_relid id_rel)
                 (render_rel_title_math rel_signature exps))
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

and render_rel_title_math (rel_signature : rel_signature) (exps : exp list) :
    Doc.t =
  let nottyp, inputs = rel_signature in
  let mixop = Mixfix.to_mixop nottyp.it in
  let sexps = List.map (fun e -> Doc.to_adoc_code (render_exp Code e)) exps in
  let num_outputs = Mixop.arity mixop - List.length sexps in
  let holes = List.init num_outputs (fun _ -> "%") in
  let padded = Hints.Input.combine inputs sexps holes in
  Doc.code (Doc.text (Mixop.assemble ~string_of_atom:code_of_atom mixop padded))

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
        (Doc.to_adoc
           (Doc.link ~target:(string_of_relid id_rel)
              (Doc.text (Sl.Print.string_of_relid id_rel))))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true Prose hint_in (reindent_lines ~level:1)
           (fun e -> Doc.to_adoc (render_exp Prose e))
           exps_in_title)
        (adoc_unordered_bullet 0)
        ("Result in "
        ^ render_alter_hint ~caps:false Prose hint_out (reindent_lines ~level:1)
            (fun e -> Doc.to_adoc (render_exp Prose e))
            exps_out)
  | Some hint_in, _, _, _ ->
      F.asprintf "%s:\n\n%s%s."
        (Doc.to_adoc
           (Doc.link ~target:(string_of_relid id_rel)
              (Doc.text (Sl.Print.string_of_relid id_rel))))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true Prose hint_in (reindent_lines ~level:1)
           (fun e -> Doc.to_adoc (render_exp Prose e))
           exps_in_title)
  | _, _, _, Some hint_true ->
      F.asprintf "%s:\n\n%s%s"
        (Doc.to_adoc
           (Doc.link ~target:(string_of_relid id_rel)
              (Doc.text (Sl.Print.string_of_relid id_rel))))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true Prose hint_true (reindent_lines ~level:0)
           (fun e -> Doc.to_adoc (render_exp Prose e))
           exps)
  | _ ->
      Doc.to_adoc
        (Doc.link ~target:(string_of_relid id_rel)
           (Doc.seq
              [
                Doc.text (Sl.Print.string_of_relid id_rel ^ ": ");
                render_rel_title_math rel_signature exps;
              ]))

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
        (Doc.to_adoc
           (Doc.link
              ~target:(string_of_defid ~link:true id_func)
              (Doc.text (string_of_defid id_func))))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true Prose hint (reindent_lines ~level:0)
           (fun p -> Doc.to_adoc (render_param Prose p))
           params)
  | None, None ->
      Doc.to_adoc
        (Doc.link
           ~target:(string_of_defid ~link:true id_func)
           (Doc.text (string_of_defid id_func)))
      ^ Sl.Print.string_of_tparams tparams
      ^ Doc.serialize ~in_code:true ~in_link:true (render_params Code params)

let render_func_header (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : string =
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      Doc.to_adoc
        (Doc.link
           ~target:(string_of_defid ~link:true id_func)
           (Doc.text
              (render_alter_hint ~caps:true Prose hint (reindent_lines ~level:0)
                 (fun p -> Doc.to_adoc (render_param Prose p))
                 params)))
  | None, None ->
      Doc.to_adoc
        (Doc.link
           ~target:(string_of_defid ~link:true id_func)
           (Doc.text
              (string_of_defid id_func
              ^ Sl.Print.string_of_tparams tparams
              ^ Doc.serialize ~in_code:true ~in_link:true
                  (render_params Code params))))

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
    "|===" ^ "\n" ^ "| "
    ^ Doc.to_adoc (render_params Prose params)
    ^ " | " ^ "Result \n\n"
  in
  let table_rows =
    tablerows
    |> List.map (fun tablerow ->
           let exps_sig, exp_res, _ = tablerow in
           let row_output = Doc.to_adoc_code (render_exp Code exp_res) in
           let row_input = Doc.to_adoc_code (render_exps Code exps_sig) in
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
