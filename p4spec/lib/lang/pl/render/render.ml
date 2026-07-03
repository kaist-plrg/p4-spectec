open Domain
open Lib
open Xl
open Ast
open Util.Source
module F = Format
module Backtrack = Backtrack
open Utils

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

(* Oxford-comma join over inline documents (the [Adoc.t] analogue of
   [render_list]). *)

let render_list_doc (items : Adoc.prose list) : Adoc.prose =
  match items with
  | [] -> Adoc.pempty
  | [ item ] -> item
  | [ item_a; item_b ] -> Adoc.pseq [ item_a; Adoc.text " and "; item_b ]
  | _ ->
      let items_rev = List.rev items in
      let items, item_last =
        (items_rev |> List.tl |> List.rev, items_rev |> List.hd)
      in
      Adoc.pseq
        [
          Adoc.pseq
            (List.mapi
               (fun i x -> if i = 0 then x else Adoc.pseq [ Adoc.text ", "; x ])
               items);
          Adoc.text ", and "; item_last;
        ]

(* Doc-producing fold over an alternation hint -- the [Adoc.t] analogue of
   [Hints.Alter.alternate]. Hole items are rendered straight to [Adoc.t] (rather
   than serialized to strings), so when the whole result is wrapped in a
   [Adoc.plink] the nested links collapse structurally and no [to_adoc_in_link] is
   needed. Mirrors [alternate']: [Seq] keeps empties (space-joined), [Brack]
   drops empty pieces, [Fuse] concatenates with no space. [caps] capitalizes the
   first emitted text segment. *)

let alternate_doc ?(caps = false) (hint : Hints.Alter.t)
    (base_text : string -> string) (render : 'a -> Adoc.prose) (items : 'a list) :
    Adoc.prose =
  let atom_doc (atom : atom) : Adoc.prose =
    Adoc.code (Adoc.token ("+" ^ (atom.it |> Atom.string_of_atom) ^ "+"))
  in
  let space_join (docs : Adoc.prose list) : Adoc.prose =
    Adoc.pseq
      (List.mapi
         (fun i d -> if i = 0 then d else Adoc.pseq [ Adoc.text " "; d ])
         docs)
  in
  (* [caps] capitalizes the first character of the whole assembled string. The
     first non-empty piece (of any kind) consumes it; only a leading text
     segment is actually altered -- a leading code span / atom starts with a
     non-letter, so capitalization is a no-op there, matching the old
     [String.capitalize_ascii] on the final string. *)
  let cap_pending = ref caps in
  let consume_cap () = if !cap_pending then cap_pending := false in
  let cap_text (s : string) : string =
    if !cap_pending && s <> "" then (
      cap_pending := false;
      String.capitalize_ascii s)
    else s
  in
  let rec go (hint : Hints.Alter.t) (cursor : int) : int * Adoc.prose option =
    let open Hints.Alter in
    match hint with
    | TextH str ->
        let s = cap_text (base_text str) in
        (cursor, if s = "" then None else Some (Adoc.text s))
    | AtomH atom ->
        consume_cap ();
        (cursor, Some (atom_doc atom))
    | SeqH hints ->
        let cursor, docs =
          List.fold_left
            (fun (cursor, acc) hint ->
              let cursor, d = go hint cursor in
              (cursor, acc @ [ Option.value ~default:Adoc.pempty d ]))
            (cursor, []) hints
        in
        (cursor, Some (space_join docs))
    | BrackH (atom_l, hint, atom_r) ->
        let cursor, d = go hint cursor in
        let pieces =
          List.filter_map Fun.id [ Some (atom_doc atom_l); d; Some (atom_doc atom_r) ]
        in
        (cursor, match pieces with [] -> None | _ -> Some (space_join pieces))
    | HoleH `Next ->
        consume_cap ();
        (cursor + 1, Some (render (List.nth items cursor)))
    | HoleH (`Num idx) ->
        consume_cap ();
        (cursor, Some (render (List.nth items idx)))
    | FuseH (hint_l, hint_r) ->
        let cursor, dl = go hint_l cursor in
        let cursor, dr = go hint_r cursor in
        ( cursor,
          Some
            (Adoc.pseq
               [
                 Option.value ~default:Adoc.pempty dl;
                 Option.value ~default:Adoc.pempty dr;
               ]) )
    | OtherH hintexp ->
        let s = cap_text (El.Print.string_of_exp hintexp) in
        (cursor, Some (Adoc.text s))
  in
  go hint 0 |> snd |> Option.value ~default:Adoc.pempty

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

let render_varid (id_var : id) : Adoc.code =
  if Id.is_underscored id_var then Adoc.token "++_++"
  else
    match String.split_on_char '_' id_var.it with
    | [] -> assert false
    | [ var_type ] -> Adoc.token var_type
    | var_type :: var_subscripts ->
        Adoc.token
          (var_type ^ (var_subscripts |> String.concat "_" |> adoc_subscript))

(* Atoms *)

(* The [+...+] passthrough form of an atom (empty for the invisible Tick). Used
   raw where the string is needed (mixfix assembly, struct-literal markers). *)

let string_of_atom (atom : atom) : string =
  match atom.it with
  | Atom.Tick -> ""
  | _ -> "+" ^ Atom.string_of_atom atom.it ^ "+"

let code_of_atom (atom : atom) : Adoc.code = Adoc.token (string_of_atom atom)

(* Mixfix operators *)

let code_of_mixop (mixop : mixop) : Adoc.code =
  let arity = Mixop.arity mixop in
  let placeholders = List.init arity (fun _ -> "%") in
  Adoc.token
    (Mixop.assemble ~string_of_atom mixop placeholders |> String.trim)

(* Iterators *)

let code_of_iter (iter : iter) : Adoc.code =
  match iter with
  | List -> Adoc.token ("{asterisk}" |> adoc_superscript)
  | Opt -> Adoc.token ("?" |> adoc_superscript)

let code_of_iterexp ((iter, _) : iterexp) : Adoc.code = code_of_iter iter

(* Variables *)

let render_var ((id, _typ, iters) : var) : Adoc.code =
  if Id.is_underscored id then Adoc.token "++_++"
  else
    Adoc.cseq
      [
        render_varid id;
        Adoc.cseq (List.map code_of_iter iters);
      ]

let render_in_itervars (vars : var list) : Adoc.prose =
  let render_in_var var =
    Adoc.pseq
      [
        Adoc.code (render_var var);
        Adoc.text " in ";
        Adoc.code (Adoc.cseq [ render_var var; code_of_iter List ]);
      ]
  in
  render_list_doc (List.map render_in_var vars)

let render_out_itervars (vars : var list) : Adoc.prose =
  vars
  |> List.filter_map (fun var ->
         let id, _, _ = var in
         if Id.is_underscored id then None
         else
           Some
             (Adoc.pseq
                [
                  Adoc.code
                    (Adoc.cseq [ render_var var; code_of_iter List ]);
                  Adoc.text " be the list";
                ]))
  |> render_list_doc

(* Types *)

let code_of_typ (typ : typ) : Adoc.code = Adoc.token (Sl.Print.string_of_typ typ)

let tid_of_typ (typ' : typ') : id option =
  match typ' with Il.VarT (id, _) -> Some id | _ -> None

(* Operators *)

let string_of_unop (unop : unop) : string = Sl.Print.string_of_unop unop

(* Operator words used in prose phrasing (the code form uses the printer). *)

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

(* Doc-producing fold over a mixfix tree -- the [Adoc.code] analogue of
   [Mixop.assemble]. Arguments stay [Adoc.code] so cross-references nest
   structurally. Mirrors [Mixfix.render]: pieces are space-joined and empty
   atoms (e.g. Tick) are dropped. *)

let assemble_doc ~(atom : atom -> string) (mixop : Mixop.t)
    (args : Adoc.code list) : Adoc.code =
  let atom_opt (a : atom) : Adoc.code option =
    match atom a with "" -> None | s -> Some (Adoc.token s)
  in
  let join (pieces : Adoc.code option list) : Adoc.code option =
    match List.filter_map Fun.id pieces with
    | [] -> None
    | docs ->
        Some
          (Adoc.cseq
             (List.mapi
                (fun i d -> if i = 0 then d else Adoc.cseq [ Adoc.token " "; d ])
                docs))
  in
  let rec go (m : Adoc.code Mixfix.t) : Adoc.code option =
    match m with
    | Mixfix.Arg d -> Some d
    | Mixfix.Atom a -> atom_opt a
    | Mixfix.Brack (l, m, r) -> join [ atom_opt l; go m; atom_opt r ]
    | Mixfix.Infix (l, a, r) -> join [ go l; atom_opt a; go r ]
    | Mixfix.Seq ms -> join (List.map go ms)
  in
  go (Mixfix.fill mixop args) |> Option.value ~default:Adoc.cempty

(* Expressions

   Each expression has two renderings: [render_code] is the math / monospace
   form (every token ends up inside a single code span), and [render_prose] is
   the sentence form (prose connectives outside code spans). Value-only shapes
   render in prose simply by lifting their code form with [Adoc.code]. *)

let rec render_code (exp : exp) : Adoc.code =
  match exp.node.it with
  | BoolE b -> Adoc.token (string_of_bool b)
  | NumE n -> Adoc.token (string_of_num n)
  | TextE text -> Adoc.token ("\"" ^ String.escaped text ^ "\"")
  | VarE id_var -> render_varid id_var
  | UnE (unop, _, exp_inner) ->
      Adoc.cseq [ Adoc.token (string_of_unop unop); render_code exp_inner ]
  | BinE (binop, _, exp_l, exp_r) ->
      Adoc.cseq
        [
          render_code exp_l;
          Adoc.token (" " ^ Sl.Print.string_of_binop binop ^ " ");
          render_code exp_r;
        ]
  | CmpE (cmpop, _, exp_l, exp_r) ->
      Adoc.cseq
        [
          render_code exp_l;
          Adoc.token (" " ^ Sl.Print.string_of_cmpop cmpop ^ " ");
          render_code exp_r;
        ]
  | UpCastE (_, exp_inner) | DownCastE (_, exp_inner) -> render_code exp_inner
  | SubE (exp_inner, typ) ->
      Adoc.cseq [ render_code exp_inner; Adoc.token " has type "; code_of_typ typ ]
  | MatchE (exp_inner, pattern) -> (
      let scrut = render_code exp_inner in
      match pattern with
      | Il.CaseP mixop when Mixop.arity mixop = 0 ->
          Adoc.cseq
            [ scrut; Adoc.token " is "; code_of_pattern (Il.CaseP mixop) ]
      | Il.ListP `Nil -> Adoc.cseq [ scrut; Adoc.token " is an empty list" ]
      | Il.ListP `Cons -> Adoc.cseq [ scrut; Adoc.token " is a non-empty list" ]
      | Il.ListP (`Fixed len) ->
          Adoc.cseq [ scrut; Adoc.token (F.asprintf " is a list of length %d" len) ]
      | Il.OptP `None -> Adoc.cseq [ scrut; Adoc.token " is none" ]
      | Il.OptP `Some -> Adoc.cseq [ scrut; Adoc.token " is defined" ]
      | pattern ->
          Adoc.cseq
            [ scrut; Adoc.token " matches pattern "; code_of_pattern pattern ]
      )
  | TupleE exps ->
      Adoc.cseq [ Adoc.token "( "; render_codes ~sep:", " exps; Adoc.token " )" ]
  | CaseE notexp -> code_of_notexp notexp
  | StrE expfields ->
      Adoc.cseq
        [
          Adoc.token "+{+";
          Adoc.cseq
            (List.mapi
               (fun i (atom, exp_f) ->
                 let field =
                   Adoc.cseq
                     [ code_of_atom atom; Adoc.token " "; render_code exp_f ]
                 in
                 if i = 0 then field else Adoc.cseq [ Adoc.token ", "; field ])
               expfields);
          Adoc.token "+}+";
        ]
  | OptE None -> Adoc.token "·"
  | OptE (Some exp_inner) -> render_code exp_inner
  | ListE [] -> Adoc.token "·"
  | ListE [ exp_inner ] -> render_code exp_inner
  | ListE exps ->
      Adoc.cseq
        [ Adoc.token "+[+ "; render_codes ~sep:", " exps; Adoc.token " +]+" ]
  | ConsE (exp_h, exp_t) ->
      Adoc.cseq
        [ render_code exp_h; Adoc.token " {two-colons} "; render_code exp_t ]
  | CatE (exp_l, exp_r) ->
      Adoc.cseq [ render_code exp_l; Adoc.token " {pp} "; render_code exp_r ]
  | MemE (exp_e, exp_s) ->
      Adoc.cseq [ render_code exp_e; Adoc.token " is in "; render_code exp_s ]
  | LenE exp_inner ->
      Adoc.cseq [ Adoc.token "the length of "; render_code exp_inner ]
  | DotE (exp_b, atom) ->
      Adoc.cseq
        [ render_code exp_b; Adoc.token "."; code_of_atom atom ]
  | IdxE (exp_b, exp_i) ->
      Adoc.cseq
        [ render_code exp_b; Adoc.token "["; render_code exp_i; Adoc.token "]" ]
  | SliceE (exp_b, exp_l, exp_h) ->
      Adoc.cseq
        [
          render_code exp_b;
          Adoc.token "[";
          render_code exp_l;
          Adoc.token " : ";
          render_code exp_h;
          Adoc.token "]";
        ]
  | UpdE (exp_b, path, exp_f) ->
      Adoc.cseq
        [
          render_code exp_b;
          Adoc.token "[";
          render_path path;
          Adoc.token " = ";
          render_code exp_f;
          Adoc.token "]";
        ]
  | CallE (id, targs, args) ->
      Adoc.clink ~target:id.it
        (Adoc.cseq
           [
             Adoc.token (string_of_defid id);
             Adoc.token (string_of_targs targs);
             render_args_code args;
           ])
  | IterE (exp_inner, iterexp) -> render_iter_code exp_inner iterexp

and render_codes ?(sep : string = ", ") (exps : exp list) : Adoc.code =
  Adoc.cseq
    (List.mapi
       (fun i exp ->
         if i = 0 then render_code exp
         else Adoc.cseq [ Adoc.token sep; render_code exp ])
       exps)

and render_iter_code (exp_inner : exp) (iterexp : iterexp) : Adoc.code =
  match (exp_inner.node.it, iterexp) with
  | _, (_, []) -> render_code exp_inner
  | (VarE _ | TupleE _), _ ->
      Adoc.cseq [ render_code exp_inner; code_of_iterexp iterexp ]
  | _ ->
      let inner = render_code exp_inner in
      let sexp = Adoc.to_adoc_code inner in
      if String.contains sexp ' ' then
        Adoc.cseq
          [ Adoc.token "( "; inner; Adoc.token " )"; code_of_iterexp iterexp ]
      else Adoc.cseq [ inner; code_of_iterexp iterexp ]

and code_of_notexp (notexp : notexp) : Adoc.code =
  let mixop, exps = Mixfix.split notexp in
  assemble_doc ~atom:string_of_atom mixop (List.map render_code exps)

and code_of_pattern (pattern : pattern) : Adoc.code =
  match pattern with
  | Il.CaseP mixop -> code_of_mixop mixop
  | Il.ListP `Cons -> Adoc.token "_ :: _"
  | Il.ListP (`Fixed len) -> Adoc.token (Format.asprintf "[ _/%d ]" len)
  | Il.ListP `Nil -> Adoc.token "[]"
  | Il.OptP `Some -> Adoc.token "(_)"
  | Il.OptP `None -> Adoc.token "()"

and render_path (path : path) : Adoc.code =
  match path.it with
  | RootP -> Adoc.cempty
  | IdxP (path, exp) ->
      Adoc.cseq [ render_path path; Adoc.token "["; render_code exp; Adoc.token "]" ]
  | SliceP (path, exp_l, exp_h) ->
      Adoc.cseq
        [
          render_path path;
          Adoc.token "[";
          render_code exp_l;
          Adoc.token " : ";
          render_code exp_h;
          Adoc.token "]";
        ]
  | DotP ({ it = RootP; _ }, atom) -> code_of_atom atom
  | DotP (path, atom) ->
      Adoc.cseq [ render_path path; Adoc.token "."; code_of_atom atom ]

and string_of_targs (targs : targ list) : string = Sl.Print.string_of_targs targs

and render_arg_code (arg : arg) : Adoc.code =
  match arg.it with
  | ExpA exp -> render_code exp
  | DefA defid -> Adoc.token (string_of_defid defid)

and render_args_code (args : arg list) : Adoc.code =
  match args with
  | [] -> Adoc.cempty
  | args ->
      Adoc.cseq
        [
          Adoc.token "(";
          Adoc.cseq
            (List.mapi
               (fun i a ->
                 if i = 0 then render_arg_code a
                 else Adoc.cseq [ Adoc.token ", "; render_arg_code a ])
               args);
          Adoc.token ")";
        ]

and render_param_code (param : param) : Adoc.code =
  match param.it with
  | ExpP (_, exp) -> render_code exp
  | DefP (defid, _, _, _) -> Adoc.token (string_of_defid defid)

and render_params_code (params : param list) : Adoc.code =
  match params with
  | [] -> Adoc.cempty
  | params ->
      Adoc.cseq
        [
          Adoc.token "(";
          Adoc.cseq
            (List.mapi
               (fun i param ->
                 if i = 0 then render_param_code param
                 else Adoc.cseq [ Adoc.token ", "; render_param_code param ])
               params);
          Adoc.token ")";
        ]

(* Prose form *)

and render_prose (exp : exp) : Adoc.prose =
  match exp.node.it with
  | BoolE _ | NumE _ | TextE _ | VarE _ | UpCastE _ | DownCastE _ | OptE None
  | ListE _ | ConsE _ | DotE _ | IdxE _ | SliceE _ ->
      Adoc.code (render_code exp)
  | OptE (Some exp_inner) -> render_prose exp_inner
  | StrE expfields ->
      Adoc.pseq
        [
          Adoc.text "+{+";
          Adoc.pseq
            (List.mapi
               (fun i (atom, exp_f) ->
                 let field =
                   Adoc.pseq
                     [ Adoc.text (string_of_atom atom); Adoc.text " "; render_prose exp_f ]
                 in
                 if i = 0 then field else Adoc.pseq [ Adoc.text ", "; field ])
               expfields);
          Adoc.text "+}+";
        ]
  | UnE (unop, _, exp_inner) -> (
      match unop with
      | #Bool.unop -> (
          match render_negated_prose_opt exp_inner with
          | Some p -> p
          | None ->
              Adoc.code
                (Adoc.cseq [ Adoc.token (string_of_unop unop); render_code exp_inner ]))
      | _ ->
          Adoc.code
            (Adoc.cseq [ Adoc.token (string_of_unop unop); render_code exp_inner ]))
  | BinE (`ImplOp, _, exp_l, exp_r) ->
      Adoc.pseq
        [
          Adoc.text "if ";
          render_prose exp_l;
          Adoc.text ", then ";
          render_prose exp_r;
        ]
  | BinE ((#Bool.binop as binop), _, exp_l, exp_r) ->
      Adoc.pseq
        [
          render_prose exp_l;
          Adoc.text (" " ^ string_of_binop binop ^ " ");
          render_prose exp_r;
        ]
  | BinE (#Num.binop, _, _, _) -> Adoc.code (render_code exp)
  | CmpE (cmpop, _, exp_l, exp_r) ->
      Adoc.pseq
        [
          render_prose exp_l;
          Adoc.text (" " ^ string_of_cmpop cmpop ^ " ");
          render_prose exp_r;
        ]
  | SubE (exp_inner, typ) ->
      Adoc.pseq
        [
          Adoc.code (render_code exp_inner);
          Adoc.text " has type ";
          Adoc.code (code_of_typ typ);
        ]
  | MatchE (exp_inner, pattern) -> (
      let scrut = render_prose exp_inner in
      let pat p = Adoc.code (code_of_pattern p) in
      match pattern with
      | Il.CaseP mixop when Mixop.arity mixop = 0 ->
          Adoc.pseq [ scrut; Adoc.text " is "; pat (Il.CaseP mixop) ]
      | Il.ListP `Nil -> Adoc.pseq [ scrut; Adoc.text " is an empty list" ]
      | Il.ListP `Cons -> Adoc.pseq [ scrut; Adoc.text " is a non-empty list" ]
      | Il.ListP (`Fixed len) ->
          Adoc.pseq [ scrut; Adoc.text (F.asprintf " is a list of length %d" len) ]
      | Il.OptP `None -> Adoc.pseq [ scrut; Adoc.text " is none" ]
      | Il.OptP `Some -> Adoc.pseq [ scrut; Adoc.text " is defined" ]
      | pattern -> Adoc.pseq [ scrut; Adoc.text " matches pattern "; pat pattern ]
      )
  | TupleE exps ->
      Adoc.pseq [ Adoc.text "( "; render_proses ~sep:", " exps; Adoc.text " )" ]
  | CaseE notexp -> (
      let hint_opt = exp.hints.Annot.prose in
      let link_opt = tid_of_typ exp.node.note in
      match (hint_opt, link_opt) with
      | Some hints, Some tid ->
          Adoc.plink ~target:tid.it
            (alternate_doc hints (reindent_lines ~level:0) render_prose
               (Mixfix.args notexp))
      | _ -> Adoc.code (code_of_notexp notexp))
  | CatE (exp_l, exp_r) ->
      Adoc.pseq
        [
          render_prose exp_l;
          Adoc.text " concatenated with ";
          render_prose exp_r;
        ]
  | MemE (exp_e, exp_s) ->
      Adoc.pseq [ render_prose exp_e; Adoc.text " is in "; render_prose exp_s ]
  | LenE exp_inner ->
      Adoc.pseq [ Adoc.text "the length of "; render_prose exp_inner ]
  | UpdE (exp_b, path, exp_f) ->
      Adoc.pseq
        [
          Adoc.code (render_code exp_b);
          Adoc.text " with ";
          Adoc.code (render_path path);
          Adoc.text " set to ";
          Adoc.code (render_code exp_f);
        ]
  | CallE (id, _targs, args) -> (
      let hint_in = exp.hints.Annot.prose_in in
      let hint_true = exp.hints.Annot.prose_true in
      match (hint_in, hint_true) with
      | Some hints, _ | _, Some hints ->
          Adoc.plink ~target:id.it
            (alternate_doc hints (reindent_lines ~level:0) render_arg_prose args)
      | None, None -> Adoc.code (render_code exp))
  | IterE (exp_inner, iterexp) -> (
      match iterexp with
      | _, [] -> render_prose exp_inner
      | _ -> Adoc.code (render_code exp))

and render_proses ?(sep : string option) (exps : exp list) : Adoc.prose =
  match sep with
  | Some sep ->
      Adoc.pseq
        (List.mapi
           (fun i exp ->
             if i = 0 then render_prose exp
             else Adoc.pseq [ Adoc.text sep; render_prose exp ])
           exps)
  | None ->
      Adoc.text (render_list (List.map (fun exp -> Adoc.to_adoc (render_prose exp)) exps))

and render_negated_prose_opt (exp : exp) : Adoc.prose option =
  match exp.node.it with
  | MatchE (exp_e, pattern) ->
      Some
        (Adoc.pseq
           [
             render_prose exp_e;
             Adoc.text " does not match pattern ";
             Adoc.code (code_of_pattern pattern);
           ])
  | SubE (exp_e, typ) ->
      Some
        (Adoc.pseq
           [
             Adoc.code (render_code exp_e);
             Adoc.text " does not have type ";
             Adoc.code (code_of_typ typ);
           ])
  | MemE (exp_e, exp_s) ->
      Some
        (Adoc.pseq
           [
             Adoc.code (render_code exp_e);
             Adoc.text " is not in ";
             Adoc.code (render_code exp_s);
           ])
  | CallE (id, _targs, args) -> (
      match exp.hints.Annot.prose_false with
      | Some hints ->
          Some
            (Adoc.plink ~target:id.it
               (alternate_doc hints (reindent_lines ~level:0) render_arg_prose
                  args))
      | None ->
          Some
            (Adoc.code
               (Adoc.cseq [ Adoc.token (string_of_unop `NotOp); render_code exp ])))
  | _ -> None

and render_arg_prose (arg : arg) : Adoc.prose =
  match arg.it with
  | ExpA exp -> render_prose exp
  | DefA defid -> Adoc.code (Adoc.token (string_of_defid defid))

and render_param_prose (param : param) : Adoc.prose =
  match param.it with
  | ExpP (_, exp) -> render_prose exp
  | DefP (defid, _, _, _) -> Adoc.code (Adoc.token (string_of_defid defid))

and render_params_prose (params : param list) : Adoc.prose =
  match params with
  | [] -> Adoc.pempty
  | params ->
      Adoc.pseq
        [
          Adoc.text "(";
          Adoc.pseq
            (List.mapi
               (fun i param ->
                 if i = 0 then render_param_prose param
                 else Adoc.pseq [ Adoc.text ", "; render_param_prose param ])
               params);
          Adoc.text ")";
        ]

(* Case analysis *)

let render_guard (exp_scrut : exp) (guard : guard) : Adoc.prose =
  match guard with
  | BoolG true -> render_prose exp_scrut
  | BoolG false ->
      let node_scrut = exp_scrut.node in
      let neg_inner =
        UnE (`NotOp, `BoolT, exp_scrut) $$ (node_scrut.at, node_scrut.note)
      in
      render_prose (Annot.no_hints neg_inner)
  | CmpG (cmpop, _, exp) ->
      Adoc.pseq
        [
          render_prose exp_scrut;
          Adoc.text (" " ^ string_of_cmpop cmpop ^ " ");
          render_prose exp;
        ]
  | SubG typ ->
      Adoc.pseq
        [
          Adoc.code (render_code exp_scrut);
          Adoc.text " has type ";
          Adoc.code (code_of_typ typ);
        ]
  | MatchG pattern ->
      Adoc.pseq
        [
          render_prose exp_scrut;
          Adoc.text " matches pattern ";
          Adoc.code (code_of_pattern pattern);
        ]
  | MemG exp ->
      Adoc.pseq
        [ render_prose exp_scrut; Adoc.text " is in "; render_prose exp ]
  | CheckLetSubG (_, target) | CheckLetMatchG (_, target) ->
      Adoc.pseq
        [
          Adoc.text "let ";
          Adoc.code (render_code target);
          Adoc.text " be ";
          render_prose exp_scrut;
        ]

(* Instructions *)

let rec render_instr ?(level : int = 0) ?(unordered : bool = false)
    ?(backtrack : Backtrack.ctx option = None) (instr : instr) : Adoc.block =
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
    (instrs : block) : Adoc.block =
  match instrs with
  | [
   ({ node = { it = ReturnI ({ node = { it = BoolE _; _ }; _ } as e); _ }; _ } :
     instr);
  ] ->
      Adoc.inline
        (Adoc.pseq
           [ Adoc.text " return "; Adoc.code (render_code e); Adoc.text "." ])
  | _ ->
      Adoc.concat
        [
          Adoc.raw "\n";
          Adoc.vseq (List.map (render_instr ~level ~backtrack) instrs);
        ]

and render_iterexp_suffix (iterexps : iterexp list) : Adoc.prose =
  match iterexps with
  | [] -> Adoc.pempty
  | _ ->
      let vars = List.concat_map (fun (_, vars) -> vars) iterexps in
      if vars = [] then Adoc.pempty
      else Adoc.pseq [ Adoc.text ", for all "; render_in_itervars vars ]

and render_iterinstr_suffix (iterinstrs : iterinstr list) : Adoc.prose =
  match iterinstrs with
  | [] -> Adoc.pempty
  | _ ->
      let vars =
        List.concat_map (fun (_, vars_in, _vars_out) -> vars_in) iterinstrs
      in
      if vars = [] then Adoc.pempty
      else Adoc.pseq [ Adoc.text ", for each "; render_in_itervars vars ]

(* If instruction rendering *)

and render_children ~(level : int) ~(backtrack : Backtrack.ctx option)
    (block : block) : Adoc.block =
  Adoc.vseq (List.map (render_instr ~level ~backtrack) block)

and render_if_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (cond : exp) (iterexps : iterexp list)
    (block_then : block) : Adoc.block =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let head =
    Adoc.concat
      [
        Adoc.raw bullet;
        Adoc.inline
          (Adoc.pseq
             [
               Adoc.text "Check that ";
               render_prose cond;
               render_iterexp_suffix iterexps;
               Adoc.text ".";
               Adoc.text fallthrough;
             ]);
      ]
  in
  if block_then = [] then head
  else
    Adoc.concat
      [ head; Adoc.raw "\n"; render_children ~level ~backtrack block_then ]

(* Hold instruction rendering *)

and render_hold_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (hints : Annot.hints) (id_rel : id)
    (notexp : notexp) (iterexps : iterexp list) (holdcase : holdcase) : Adoc.block =
  let exps = Mixfix.args notexp in
  let hint_true = hints.Annot.prose_true in
  let hint_false = hints.Annot.prose_false in
  let iter_suffix = Adoc.to_adoc (render_iterexp_suffix iterexps) in
  let render_head ~(hold : bool) : string =
    let hint_opt = if hold then hint_true else hint_false in
    let fallback_verb = if hold then " holds" else " does not hold" in
    match hint_opt with
    | Some hint ->
        Adoc.to_adoc
          (Adoc.plink ~target:(string_of_relid id_rel)
             (alternate_doc hint (reindent_lines ~level:0) render_prose
                exps))
    | None ->
        let math =
          Adoc.to_adoc
            (Adoc.plink ~target:(string_of_relid id_rel) (Adoc.code (code_of_notexp notexp)))
        in
        math ^ fallback_verb
  in
  let if_head ~hold =
    Adoc.concat
      [
        Adoc.raw bullet;
        Adoc.inline
          (Adoc.pseq
             [
               Adoc.text "If ";
               Adoc.text (render_head ~hold);
               Adoc.text iter_suffix;
               Adoc.text ":";
             ]);
      ]
  in
  match holdcase with
  | HoldH (block, _dangle) ->
      Adoc.concat
        [
          if_head ~hold:true;
          render_instrs ~level:(level + 1) ~backtrack block;
        ]
  | NotHoldH (block, _dangle) ->
      Adoc.concat
        [
          if_head ~hold:false;
          render_instrs ~level:(level + 1) ~backtrack block;
        ]
  | BothH (block_hold, block_nothold) ->
      Adoc.concat
        [
          if_head ~hold:true;
          render_instrs ~level:(level + 1) ~backtrack block_hold;
          Adoc.raw "\n";
          Adoc.raw bullet;
          Adoc.inline (Adoc.text "Else:");
          render_instrs ~level:(level + 1) ~backtrack block_nothold;
        ]

(* Case analysis instruction rendering *)

and render_case_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_scrut : exp) (cases : case list)
    (dangle : dangle) : Adoc.block =
  let total = not dangle in
  let n = List.length cases in
  match cases with
  | [ (guard, block_then) ] ->
      let head =
        Adoc.concat
          [
            Adoc.raw bullet;
            Adoc.inline
              (Adoc.pseq
                 [
                   Adoc.text "Check that ";
                   render_guard exp_scrut guard;
                   Adoc.text ".";
                 ]);
          ]
      in
      if block_then = [] then head
      else
        Adoc.concat
          [ head; Adoc.raw "\n"; render_children ~level ~backtrack block_then ]
  | _ ->
      Adoc.vseq
        (cases
        |> List.mapi (fun idx (guard, block_then) ->
               if idx = n - 1 && total then
                 Adoc.concat
                   [
                     Adoc.raw bullet;
                     Adoc.inline (Adoc.text "Else:");
                     render_instrs ~level:(level + 1) ~backtrack block_then;
                   ]
               else
                 let keyword = if idx = 0 then "If" else "Else if" in
                 Adoc.concat
                   [
                     Adoc.raw bullet;
                     Adoc.inline
                       (Adoc.pseq
                          [
                            Adoc.text (keyword ^ " ");
                            render_guard exp_scrut guard;
                            Adoc.text ":";
                          ]);
                     render_instrs ~level:(level + 1) ~backtrack block_then;
                   ]))

(* Group instruction rendering *)

and render_group_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (hints : Annot.hints) (id_rel : id)
    (rel_signature : rel_signature) (exps : exp list) (block : block) : Adoc.block =
  let hint_in = hints.Annot.prose_in in
  let hint_true = hints.Annot.prose_true in
  let title =
    match (hint_in, hint_true) with
    | Some hint, _ | _, Some hint ->
        Adoc.plink ~target:(string_of_relid id_rel)
          (alternate_doc ~caps:true hint (reindent_lines ~level:0)
             render_prose exps)
    | None, None ->
        Adoc.plink ~target:(string_of_relid id_rel)
          (render_rel_title_math rel_signature exps)
  in
  Adoc.concat
    [
      Adoc.raw bullet;
      Adoc.inline (Adoc.pseq [ title; Adoc.text ":" ]);
      render_instrs ~level:(level + 1) ~backtrack block;
    ]

(* Try instruction rendering *)

and render_try_instr ~(level : int) ~(bullet : string) (arms : arm list) :
    Adoc.block =
  let block = Backtrack.BlockLabel.fresh () in
  let level_arm = level + 1 in
  let level_body = level + 2 in
  let total = List.length arms in
  let render_arm idx arm =
    let backtrack = Backtrack.arm_backtrack_ctx ~block ~level_arm ~total idx in
    let anchor = Backtrack.arm_anchor ~block ~level_arm idx in
    Adoc.concat
      [
        Adoc.raw
          (F.asprintf "%s{empty}%s" (adoc_ordered_bullet level_arm) anchor);
        render_instrs ~level:level_body ~backtrack:(Some backtrack) arm;
      ]
  in
  Adoc.concat
    [
      Adoc.raw bullet;
      Adoc.inline
        (Adoc.pseq
           [
             Adoc.text "Try ";
             Adoc.text (Backtrack.render_block_label block);
             Adoc.text ":";
           ]);
      Adoc.raw "\n";
      Adoc.vseq (List.mapi render_arm arms);
    ]

(* Let instruction rendering *)

and render_let_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_l : exp) (exp_r : exp)
    (iterinstrs : iterinstr list) : Adoc.block =
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
    Adoc.concat
      [
        Adoc.raw bullet;
        Adoc.inline
          (Adoc.pseq
             [
               Adoc.text "Let ";
               Adoc.code (render_code exp_l);
               Adoc.text " be ";
               render_prose exp_r;
               render_iterinstr_suffix iterinstrs;
               Adoc.text ".";
               Adoc.text fallthrough;
             ]);
      ]
  else
    let bullet_inner = adoc_unordered_bullet (level + 1) in
    let body =
      Adoc.concat
        [
          Adoc.raw bullet_inner;
          Adoc.inline
            (Adoc.pseq
               [
                 Adoc.text "Let ";
                 Adoc.code (render_code exp_l);
                 Adoc.text " be ";
                 render_prose exp_r;
                 Adoc.text ".";
               ]);
        ]
    in
    Adoc.concat
      [
        Adoc.raw bullet;
        Adoc.inline
          (Adoc.pseq
             [
               Adoc.text "Let ";
               render_out_itervars vars_out_visible;
               Adoc.text " obtained by repeating:";
             ]);
        Adoc.raw "\n+\n--\n";
        body;
        Adoc.raw "\n--\n+\nfor each ";
        Adoc.inline
          (Adoc.pseq
             [ render_in_itervars vars_in_all; Adoc.text "."; Adoc.text fallthrough ]);
      ]

(* Rule instruction rendering *)

and render_rule_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (hints : Annot.hints) (id_rel : id)
    (notexp : notexp) (hint_input : Hints.Input.t) (iterinstrs : iterinstr list)
    : Adoc.block =
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
          (* Not wrapped in a visible link, but inner links are suppressed:
             render to a string with links off. *)
          Adoc.to_adoc_in_link
            (alternate_doc hint_out unindent_lines render_prose exps_out)
        in
        let prose_in =
          Adoc.to_adoc
            (Adoc.plink ~target:(string_of_relid id_rel)
               (alternate_doc hint_in unindent_lines render_prose exps_in))
        in
        if adoc_fits_in_width_short prose_in then
          F.asprintf "Let %s be the result of %s" prose_out prose_in
        else
          F.asprintf "Let %s be\n%sthe result of %s" prose_out
            (adoc_unordered_bullet level)
            prose_in
    | _ ->
        F.asprintf "Let %s"
          (Adoc.to_adoc
             (Adoc.plink ~target:(string_of_relid id_rel)
                (Adoc.code (code_of_notexp notexp))))
  in
  if vars_out_visible = [] then
    Adoc.concat
      [
        Adoc.raw bullet;
        Adoc.raw rule_body;
        Adoc.inline
          (Adoc.pseq
             [ render_iterinstr_suffix iterinstrs; Adoc.text "."; Adoc.text fallthrough ]);
      ]
  else
    let bullet_inner = adoc_unordered_bullet (level + 1) in
    Adoc.concat
      [
        Adoc.raw bullet;
        Adoc.inline
          (Adoc.pseq
             [
               Adoc.text "Let ";
               render_out_itervars vars_out_visible;
               Adoc.text " obtained by repeating:";
             ]);
        Adoc.raw "\n+\n--\n";
        Adoc.raw bullet_inner;
        Adoc.raw rule_body;
        Adoc.raw ".";
        Adoc.raw "\n--\n+\nfor each ";
        Adoc.inline
          (Adoc.pseq
             [ render_in_itervars vars_in_all; Adoc.text "."; Adoc.text fallthrough ]);
      ]

(* Result instruction rendering *)

and render_result_instr ~(bullet : string) (hints : Annot.hints)
    (rel_signature : rel_signature) (exps : exp list) : Adoc.block =
  let nottyp, hint_input = rel_signature in
  let typs = Mixfix.args nottyp.it in
  let is_conditional = Hints.Input.is_conditional hint_input typs in
  let line doc = Adoc.concat [ Adoc.raw bullet; Adoc.inline doc ] in
  if is_conditional then line (Adoc.text "Then, the relation holds.")
  else
    match (hints.Annot.prose_out, exps) with
    | Some hint, _ ->
        line
          (Adoc.pseq
             [
               Adoc.text "Result in ";
               alternate_doc hint (reindent_lines ~level:0) render_prose
                 exps;
               Adoc.text ".";
             ])
    | None, [] -> line (Adoc.text "The relation holds.")
    | None, _ ->
        line
          (Adoc.pseq
             [ Adoc.text "Result in "; render_proses exps; Adoc.text "." ])

(* Return instruction rendering *)

and render_return_instr ~(bullet : string) (exp : exp) : Adoc.block =
  Adoc.concat
    [
      Adoc.raw bullet;
      Adoc.inline
        (Adoc.pseq [ Adoc.text "Return "; render_prose exp; Adoc.text "." ]);
    ]

(* Debug instruction rendering *)

and render_debug_instr ~(bullet : string) (exp : exp) : Adoc.block =
  Adoc.concat
    [
      Adoc.raw bullet;
      Adoc.inline
        (Adoc.pseq [ Adoc.text "(debug: "; render_prose exp; Adoc.text ")" ]);
    ]

(* Destruct instruction rendering *)

and render_destruct_instr ~(bullet : string)
    (fields : (string option * exp) list) (exp_source : exp) : Adoc.block =
  let projections =
    List.filter_map
      (fun (name_opt, exp_target) ->
        Option.map (fun name -> (name, exp_target)) name_opt)
      fields
  in
  let line doc = Adoc.concat [ Adoc.raw bullet; Adoc.inline doc ] in
  match projections with
  | [ (name, exp_target) ] ->
      line
        (Adoc.pseq
           [
             Adoc.text "Let ";
             render_prose exp_target;
             Adoc.text (F.asprintf " be the %s of " name);
             render_prose exp_source;
             Adoc.text ".";
           ])
  | _ ->
      let names, exps_target = List.split projections in
      line
        (Adoc.pseq
           [
             Adoc.text "Let ";
             render_proses exps_target;
             Adoc.text " be ";
             Adoc.text (render_list (List.map (fun s -> "the " ^ s) names));
             Adoc.text " of ";
             render_prose exp_source;
             Adoc.text ".";
           ])

(* Check-let instruction rendering (CheckLetSubI / CheckLetMatchI) *)

and render_check_let_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_l : exp) (exp_r : exp)
    (block_inner : block) : Adoc.block =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let head =
    Adoc.concat
      [
        Adoc.raw bullet;
        Adoc.inline
          (Adoc.pseq
             [
               Adoc.text "Let!~type~ ";
               Adoc.code (render_code exp_l);
               Adoc.text " be ";
               render_prose exp_r;
               Adoc.text ".";
               Adoc.text fallthrough;
             ]);
      ]
  in
  if block_inner = [] then head
  else
    Adoc.concat
      [ head; Adoc.raw "\n"; render_children ~level ~backtrack block_inner ]

(* Option-get instruction rendering *)

and render_option_get_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (exp_l : exp) (exp_r : exp)
    (block_inner : block) : Adoc.block =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let head =
    Adoc.concat
      [
        Adoc.raw bullet;
        Adoc.inline
          (Adoc.pseq
             [
               Adoc.text "Let ";
               Adoc.code (render_code exp_l);
               Adoc.text " be ";
               Adoc.text (adoc_link ~link:"option_get" "*!*");
               Adoc.text " ";
               render_prose exp_r;
               Adoc.text ".";
               Adoc.text fallthrough;
             ]);
      ]
  in
  if block_inner = [] then head
  else
    Adoc.concat
      [ head; Adoc.raw "\n"; render_children ~level ~backtrack block_inner ]

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
            Adoc.to_adoc
              (Adoc.plink ~target:(string_of_relid id_rel)
                 (alternate_doc ~caps:true hint (reindent_lines ~level:0)
                    render_prose exps))
        | None, None ->
            Adoc.to_adoc
              (Adoc.plink ~target:(string_of_relid id_rel)
                 (render_rel_title_math rel_signature exps))
      in
      title ^ ":\n" ^ Adoc.serialize (render_instrs block)
  | _ -> assert false

and render_elseblock (elseblock_opt : elseblock option) : string =
  match elseblock_opt with
  | None | Some [] -> ""
  | Some block ->
      "\n\n" ^ adoc_ordered_bullet 0 ^ "Otherwise:"
      ^ Adoc.serialize (render_instrs ~level:1 block)

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
    Adoc.prose =
  let nottyp, inputs = rel_signature in
  let mixop = Mixfix.to_mixop nottyp.it in
  let dexps = List.map render_code exps in
  let num_outputs = Mixop.arity mixop - List.length dexps in
  let holes = List.init num_outputs (fun _ -> Adoc.token "%") in
  let padded = Hints.Input.combine inputs dexps holes in
  Adoc.code (assemble_doc ~atom:string_of_atom mixop padded)

and render_rel_title_adoc (hints : Annot.hints) (id_rel : id)
    (rel_signature : rel_signature) (exps : exp list) : string =
  let exps_in_title =
    match hints.Annot.prose_input_exps with
    | Some exps_in_sl -> List.map lift_synthesized_exp exps_in_sl
    | None -> exps
  in
  let title =
    Adoc.plink ~target:(string_of_relid id_rel)
      (Adoc.text (Sl.Print.string_of_relid id_rel))
  in
  let title_header =
    Adoc.concat
      [ Adoc.inline (Adoc.pseq [ title; Adoc.text ":" ]); Adoc.raw "\n\n" ]
  in
  match
    (hints.prose_in, hints.prose_out, hints.prose_output_exps, hints.prose_true)
  with
  | Some _, Some _, None, _ -> assert false
  | Some hint_in, Some hint_out, Some exps_out_sl, _ ->
      let exps_out = List.map lift_synthesized_exp exps_out_sl in
      Adoc.serialize
        (Adoc.concat
           [
             title_header;
             Adoc.raw (adoc_unordered_bullet 0);
             Adoc.inline
               (alternate_doc ~caps:true hint_in (reindent_lines ~level:1)
                  render_prose exps_in_title);
             Adoc.raw ":\n";
             Adoc.raw (adoc_unordered_bullet 0);
             Adoc.inline (Adoc.text "Result in ");
             Adoc.inline
               (alternate_doc ~caps:false hint_out (reindent_lines ~level:1)
                  render_prose exps_out);
             Adoc.raw ".";
           ])
  | Some hint_in, _, _, _ ->
      Adoc.serialize
        (Adoc.concat
           [
             title_header;
             Adoc.raw (adoc_unordered_bullet 0);
             Adoc.inline
               (alternate_doc ~caps:true hint_in (reindent_lines ~level:1)
                  render_prose exps_in_title);
             Adoc.raw ".";
           ])
  | _, _, _, Some hint_true ->
      Adoc.serialize
        (Adoc.concat
           [
             title_header;
             Adoc.raw (adoc_unordered_bullet 0);
             Adoc.inline
               (alternate_doc ~caps:true hint_true (reindent_lines ~level:0)
                  render_prose exps);
           ])
  | _ ->
      Adoc.serialize
        (Adoc.inline
           (Adoc.plink ~target:(string_of_relid id_rel)
              (Adoc.pseq
                 [
                   Adoc.text (Sl.Print.string_of_relid id_rel ^ ": ");
                   render_rel_title_math rel_signature exps;
                 ])))

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
  let title =
    Adoc.plink
      ~target:(string_of_defid ~link:true id_func)
      (Adoc.text (string_of_defid id_func))
  in
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      Adoc.serialize
        (Adoc.concat
           [
             Adoc.inline (Adoc.pseq [ title; Adoc.text ":" ]);
             Adoc.raw "\n\n";
             Adoc.raw (adoc_unordered_bullet 0);
             Adoc.inline
               (alternate_doc ~caps:true hint (reindent_lines ~level:0)
                  render_param_prose params);
           ])
  | None, None ->
      Adoc.serialize
        (Adoc.concat
           [
             Adoc.inline title;
             Adoc.raw (Sl.Print.string_of_tparams tparams);
             Adoc.raw
               (Adoc.to_adoc_code
                  (render_params_code params));
           ])

let render_func_header (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : string =
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      Adoc.to_adoc
        (Adoc.plink
           ~target:(string_of_defid ~link:true id_func)
           (Adoc.text
              (Adoc.to_adoc
                 (alternate_doc ~caps:true hint (reindent_lines ~level:0)
                    render_param_prose params))))
  | None, None ->
      Adoc.to_adoc
        (Adoc.plink
           ~target:(string_of_defid ~link:true id_func)
           (Adoc.text
              (string_of_defid id_func
              ^ Sl.Print.string_of_tparams tparams
              ^ Adoc.to_adoc_code
                  (render_params_code params))))

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
    Adoc.concat
      [
        Adoc.raw "|===\n| ";
        Adoc.inline (render_params_prose params);
        Adoc.raw " | Result \n\n";
      ]
  in
  let table_rows =
    Adoc.vseq
      (tablerows
      |> List.map (fun tablerow ->
             let exps_sig, exp_res, _ = tablerow in
             let row_output = Adoc.to_adoc_code (render_code exp_res) in
             let row_input = Adoc.to_adoc_code (render_codes exps_sig) in
             Adoc.raw ("| " ^ row_input ^ " | " ^ row_output)))
  in
  Adoc.serialize
    (Adoc.concat
       [
         Adoc.raw (render_func_header hints id_func [] params);
         Adoc.raw ":\n";
         Adoc.raw table_meta;
         table_header;
         table_rows;
         Adoc.raw "\n\n|===";
       ])

let render_defined_func_def (hints : Annot.hints) (func : definedfunc) : string
    =
  let id_func, tparams, params, _typ, block, elseblock_opt = func in
  render_func_header hints id_func tparams params
  ^ "\n\n"
  ^ strip_leading_newline (Adoc.serialize (render_instrs block))
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
