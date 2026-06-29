open Domain
open Lib
open Xl
open Ast
open Util.Source
module F = Format
module Backtrack = Backtrack
open Adoc

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

let render_list_doc (items : Doc.prose list) : Doc.prose =
  match items with
  | [] -> Doc.pempty
  | [ item ] -> item
  | [ item_a; item_b ] -> Doc.pseq [ item_a; Doc.text " and "; item_b ]
  | _ ->
      let items_rev = List.rev items in
      let items, item_last =
        (items_rev |> List.tl |> List.rev, items_rev |> List.hd)
      in
      Doc.pseq
        [
          Doc.pseq
            (List.mapi
               (fun i x -> if i = 0 then x else Doc.pseq [ Doc.text ", "; x ])
               items);
          Doc.text ", and "; item_last;
        ]

(* Doc-producing fold over an alternation hint -- the [Doc.t] analogue of
   [Hints.Alter.alternate]. Hole items are rendered straight to [Doc.t] (rather
   than serialized to strings), so when the whole result is wrapped in a
   [Doc.plink] the nested links collapse structurally and no [to_adoc_in_link] is
   needed. Mirrors [alternate']: [Seq] keeps empties (space-joined), [Brack]
   drops empty pieces, [Fuse] concatenates with no space. [caps] capitalizes the
   first emitted text segment. *)

let alternate_doc ?(caps = false) (hint : Hints.Alter.t)
    (base_text : string -> string) (render : 'a -> Doc.prose) (items : 'a list) :
    Doc.prose =
  let atom_doc (atom : atom) : Doc.prose =
    Doc.code (Doc.token ("+" ^ (atom.it |> Atom.string_of_atom) ^ "+"))
  in
  let space_join (docs : Doc.prose list) : Doc.prose =
    Doc.pseq
      (List.mapi
         (fun i d -> if i = 0 then d else Doc.pseq [ Doc.text " "; d ])
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
  let rec go (hint : Hints.Alter.t) (cursor : int) : int * Doc.prose option =
    let open Hints.Alter in
    match hint with
    | TextH str ->
        let s = cap_text (base_text str) in
        (cursor, if s = "" then None else Some (Doc.text s))
    | AtomH atom ->
        consume_cap ();
        (cursor, Some (atom_doc atom))
    | SeqH hints ->
        let cursor, docs =
          List.fold_left
            (fun (cursor, acc) hint ->
              let cursor, d = go hint cursor in
              (cursor, acc @ [ Option.value ~default:Doc.pempty d ]))
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
            (Doc.pseq
               [
                 Option.value ~default:Doc.pempty dl;
                 Option.value ~default:Doc.pempty dr;
               ]) )
    | OtherH hintexp ->
        let s = cap_text (El.Print.string_of_exp hintexp) in
        (cursor, Some (Doc.text s))
  in
  go hint 0 |> snd |> Option.value ~default:Doc.pempty

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

let render_varid (id_var : id) : Doc.code =
  if Id.is_underscored id_var then Doc.token "++_++"
  else
    match String.split_on_char '_' id_var.it with
    | [] -> assert false
    | [ var_type ] -> Doc.token var_type
    | var_type :: var_subscripts ->
        Doc.token
          (var_type ^ (var_subscripts |> String.concat "_" |> adoc_subscript))

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

let render_var ((id, _typ, iters) : var) : Doc.code =
  if Id.is_underscored id then Doc.token "++_++"
  else
    Doc.cseq
      [
        render_varid id;
        Doc.token (String.concat "" (List.map code_of_iter iters));
      ]

let render_in_itervars (vars : var list) : Doc.prose =
  let render_in_var var =
    Doc.pseq
      [
        Doc.code (render_var var);
        Doc.text " in ";
        Doc.code (Doc.cseq [ render_var var; Doc.token (code_of_iter List) ]);
      ]
  in
  render_list_doc (List.map render_in_var vars)

let render_out_itervars (vars : var list) : Doc.prose =
  vars
  |> List.filter_map (fun var ->
         let id, _, _ = var in
         if Id.is_underscored id then None
         else
           Some
             (Doc.pseq
                [
                  Doc.code
                    (Doc.cseq [ render_var var; Doc.token (code_of_iter List) ]);
                  Doc.text " be the list";
                ]))
  |> render_list_doc

(* Types *)

let code_of_typ (typ : typ) : Doc.code = Doc.token (Sl.Print.string_of_typ typ)

let tid_of_typ (typ' : typ') : id option =
  match typ' with Il.VarT (id, _) -> Some id | _ -> None

(* Operators *)

let render_unop = Sl.Print.string_of_unop

(* Operator words used in prose phrasing (the code form uses the printer). *)

let binop_word (binop : binop) : string =
  match binop with
  | `AndOp -> "and"
  | `OrOp -> "or"
  | `ImplOp -> "implies"
  | `EquivOp -> "is equivalent to"
  | _ -> Sl.Print.string_of_binop binop

let cmpop_word (cmpop : cmpop) : string =
  match cmpop with
  | `EqOp -> "is equal to"
  | `NeOp -> "is not equal to"
  | `LtOp -> "is less than"
  | `GtOp -> "is greater than"
  | `LeOp -> "is less than or equal to"
  | `GeOp -> "is greater than or equal to"

(* Doc-producing fold over a mixfix tree -- the [Doc.code] analogue of
   [Mixop.assemble]. Arguments stay [Doc.code] so cross-references nest
   structurally. Mirrors [Mixfix.render]: pieces are space-joined and empty
   atoms (e.g. Tick) are dropped. *)

let assemble_doc ~(atom : atom -> string) (mixop : Mixop.t)
    (args : Doc.code list) : Doc.code =
  let atom_opt (a : atom) : Doc.code option =
    match atom a with "" -> None | s -> Some (Doc.token s)
  in
  let join (pieces : Doc.code option list) : Doc.code option =
    match List.filter_map Fun.id pieces with
    | [] -> None
    | docs ->
        Some
          (Doc.cseq
             (List.mapi
                (fun i d -> if i = 0 then d else Doc.cseq [ Doc.token " "; d ])
                docs))
  in
  let rec go (m : Doc.code Mixfix.t) : Doc.code option =
    match m with
    | Mixfix.Arg d -> Some d
    | Mixfix.Atom a -> atom_opt a
    | Mixfix.Brack (l, m, r) -> join [ atom_opt l; go m; atom_opt r ]
    | Mixfix.Infix (l, a, r) -> join [ go l; atom_opt a; go r ]
    | Mixfix.Seq ms -> join (List.map go ms)
  in
  go (Mixfix.fill mixop args) |> Option.value ~default:Doc.cempty

(* Expressions

   Each expression has two renderings: [render_code] is the math / monospace
   form (every token ends up inside a single code span), and [render_prose] is
   the sentence form (prose connectives outside code spans). Value-only shapes
   render in prose simply by lifting their code form with [Doc.code]. *)

let rec render_code (exp : exp) : Doc.code =
  match exp.node.it with
  | BoolE b -> Doc.token (string_of_bool b)
  | NumE n -> Doc.token (string_of_num n)
  | TextE text -> Doc.token ("\"" ^ String.escaped text ^ "\"")
  | VarE id_var -> render_varid id_var
  | UnE (unop, _, exp_inner) ->
      Doc.cseq [ Doc.token (render_unop unop); render_code exp_inner ]
  | BinE (binop, _, exp_l, exp_r) ->
      Doc.cseq
        [
          render_code exp_l;
          Doc.token (" " ^ Sl.Print.string_of_binop binop ^ " ");
          render_code exp_r;
        ]
  | CmpE (cmpop, _, exp_l, exp_r) ->
      Doc.cseq
        [
          render_code exp_l;
          Doc.token (" " ^ Sl.Print.string_of_cmpop cmpop ^ " ");
          render_code exp_r;
        ]
  | UpCastE (_, exp_inner) | DownCastE (_, exp_inner) -> render_code exp_inner
  | SubE (exp_inner, typ) ->
      Doc.cseq [ render_code exp_inner; Doc.token " has type "; code_of_typ typ ]
  | MatchE (exp_inner, pattern) -> (
      let scrut = render_code exp_inner in
      match pattern with
      | Il.CaseP mixop when Mixop.arity mixop = 0 ->
          Doc.cseq
            [ scrut; Doc.token " is "; Doc.token (code_of_pattern (Il.CaseP mixop)) ]
      | Il.ListP `Nil -> Doc.cseq [ scrut; Doc.token " is an empty list" ]
      | Il.ListP `Cons -> Doc.cseq [ scrut; Doc.token " is a non-empty list" ]
      | Il.ListP (`Fixed len) ->
          Doc.cseq [ scrut; Doc.token (F.asprintf " is a list of length %d" len) ]
      | Il.OptP `None -> Doc.cseq [ scrut; Doc.token " is none" ]
      | Il.OptP `Some -> Doc.cseq [ scrut; Doc.token " is defined" ]
      | pattern ->
          Doc.cseq
            [ scrut; Doc.token " matches pattern "; Doc.token (code_of_pattern pattern) ]
      )
  | TupleE exps ->
      Doc.cseq [ Doc.token "( "; render_codes ~sep:", " exps; Doc.token " )" ]
  | CaseE notexp -> code_of_notexp notexp
  | StrE expfields ->
      Doc.cseq
        [
          Doc.token "+{+";
          Doc.cseq
            (List.mapi
               (fun i (atom, exp_f) ->
                 let field =
                   Doc.cseq
                     [ Doc.token (code_of_atom atom); Doc.token " "; render_code exp_f ]
                 in
                 if i = 0 then field else Doc.cseq [ Doc.token ", "; field ])
               expfields);
          Doc.token "+}+";
        ]
  | OptE None -> Doc.token "·"
  | OptE (Some exp_inner) -> render_code exp_inner
  | ListE [] -> Doc.token "·"
  | ListE [ exp_inner ] -> render_code exp_inner
  | ListE exps ->
      Doc.cseq
        [ Doc.token "+[+ "; render_codes ~sep:", " exps; Doc.token " +]+" ]
  | ConsE (exp_h, exp_t) ->
      Doc.cseq
        [ render_code exp_h; Doc.token " {two-colons} "; render_code exp_t ]
  | CatE (exp_l, exp_r) ->
      Doc.cseq [ render_code exp_l; Doc.token " {pp} "; render_code exp_r ]
  | MemE (exp_e, exp_s) ->
      Doc.cseq [ render_code exp_e; Doc.token " is in "; render_code exp_s ]
  | LenE exp_inner ->
      Doc.cseq [ Doc.token "the length of "; render_code exp_inner ]
  | DotE (exp_b, atom) ->
      Doc.cseq
        [ render_code exp_b; Doc.token "."; Doc.token (code_of_atom atom) ]
  | IdxE (exp_b, exp_i) ->
      Doc.cseq
        [ render_code exp_b; Doc.token "["; render_code exp_i; Doc.token "]" ]
  | SliceE (exp_b, exp_l, exp_h) ->
      Doc.cseq
        [
          render_code exp_b;
          Doc.token "[";
          render_code exp_l;
          Doc.token " : ";
          render_code exp_h;
          Doc.token "]";
        ]
  | UpdE (exp_b, path, exp_f) ->
      Doc.cseq
        [
          render_code exp_b;
          Doc.token "[";
          render_path path;
          Doc.token " = ";
          render_code exp_f;
          Doc.token "]";
        ]
  | CallE (id, targs, args) ->
      Doc.clink ~target:id.it
        (Doc.cseq
           [
             Doc.token (string_of_defid id);
             Doc.token (string_of_targs targs);
             render_args_code args;
           ])
  | IterE (exp_inner, iterexp) -> render_iter_code exp_inner iterexp

and render_codes ?(sep : string = ", ") (exps : exp list) : Doc.code =
  Doc.cseq
    (List.mapi
       (fun i exp ->
         if i = 0 then render_code exp
         else Doc.cseq [ Doc.token sep; render_code exp ])
       exps)

and render_iter_code (exp_inner : exp) (iterexp : iterexp) : Doc.code =
  match (exp_inner.node.it, iterexp) with
  | _, (_, []) -> render_code exp_inner
  | (VarE _ | TupleE _), _ ->
      Doc.cseq [ render_code exp_inner; Doc.token (code_of_iterexp iterexp) ]
  | _ ->
      let inner = render_code exp_inner in
      let sexp = Doc.to_adoc_code inner in
      if String.contains sexp ' ' then
        Doc.cseq
          [ Doc.token "( "; inner; Doc.token (" )" ^ code_of_iterexp iterexp) ]
      else Doc.cseq [ inner; Doc.token (code_of_iterexp iterexp) ]

and code_of_notexp (notexp : notexp) : Doc.code =
  let mixop, exps = Mixfix.split notexp in
  assemble_doc ~atom:code_of_atom mixop (List.map render_code exps)

and code_of_pattern (pattern : pattern) : string =
  match pattern with
  | Il.CaseP mixop -> code_of_mixop mixop
  | Il.ListP `Cons -> "_ :: _"
  | Il.ListP (`Fixed len) -> Format.asprintf "[ _/%d ]" len
  | Il.ListP `Nil -> "[]"
  | Il.OptP `Some -> "(_)"
  | Il.OptP `None -> "()"

and render_path (path : path) : Doc.code =
  match path.it with
  | RootP -> Doc.cempty
  | IdxP (path, exp) ->
      Doc.cseq [ render_path path; Doc.token "["; render_code exp; Doc.token "]" ]
  | SliceP (path, exp_l, exp_h) ->
      Doc.cseq
        [
          render_path path;
          Doc.token "[";
          render_code exp_l;
          Doc.token " : ";
          render_code exp_h;
          Doc.token "]";
        ]
  | DotP ({ it = RootP; _ }, atom) -> Doc.token (code_of_atom atom)
  | DotP (path, atom) ->
      Doc.cseq [ render_path path; Doc.token "."; Doc.token (code_of_atom atom) ]

and string_of_targs (targs : targ list) = Sl.Print.string_of_targs targs

and render_arg_code (arg : arg) : Doc.code =
  match arg.it with
  | ExpA exp -> render_code exp
  | DefA defid -> Doc.token (string_of_defid defid)

and render_args_code (args : arg list) : Doc.code =
  match args with
  | [] -> Doc.cempty
  | args ->
      Doc.cseq
        [
          Doc.token "(";
          Doc.cseq
            (List.mapi
               (fun i a ->
                 if i = 0 then render_arg_code a
                 else Doc.cseq [ Doc.token ", "; render_arg_code a ])
               args);
          Doc.token ")";
        ]

and render_param_code (param : param) : Doc.code =
  match param.it with
  | ExpP (_, exp) -> render_code exp
  | DefP (defid, _, _, _) -> Doc.token (string_of_defid defid)

and render_params_code (params : param list) : Doc.code =
  match params with
  | [] -> Doc.cempty
  | params ->
      Doc.cseq
        [
          Doc.token "(";
          Doc.cseq
            (List.mapi
               (fun i param ->
                 if i = 0 then render_param_code param
                 else Doc.cseq [ Doc.token ", "; render_param_code param ])
               params);
          Doc.token ")";
        ]

(* Prose form *)

and render_prose (exp : exp) : Doc.prose =
  match exp.node.it with
  | BoolE _ | NumE _ | TextE _ | VarE _ | UpCastE _ | DownCastE _ | OptE None
  | ListE _ | ConsE _ | DotE _ | IdxE _ | SliceE _ ->
      Doc.code (render_code exp)
  | OptE (Some exp_inner) -> render_prose exp_inner
  | StrE expfields ->
      Doc.pseq
        [
          Doc.text "+{+";
          Doc.pseq
            (List.mapi
               (fun i (atom, exp_f) ->
                 let field =
                   Doc.pseq
                     [ Doc.text (code_of_atom atom); Doc.text " "; render_prose exp_f ]
                 in
                 if i = 0 then field else Doc.pseq [ Doc.text ", "; field ])
               expfields);
          Doc.text "+}+";
        ]
  | UnE (unop, _, exp_inner) -> (
      match unop with
      | #Bool.unop -> (
          match render_negated_prose_opt exp_inner with
          | Some p -> p
          | None ->
              Doc.code
                (Doc.cseq [ Doc.token (render_unop unop); render_code exp_inner ]))
      | _ ->
          Doc.code
            (Doc.cseq [ Doc.token (render_unop unop); render_code exp_inner ]))
  | BinE (`ImplOp, _, exp_l, exp_r) ->
      Doc.pseq
        [
          Doc.text "if ";
          render_prose exp_l;
          Doc.text ", then ";
          render_prose exp_r;
        ]
  | BinE ((#Bool.binop as binop), _, exp_l, exp_r) ->
      Doc.pseq
        [
          render_prose exp_l;
          Doc.text (" " ^ binop_word binop ^ " ");
          render_prose exp_r;
        ]
  | BinE (#Num.binop, _, _, _) -> Doc.code (render_code exp)
  | CmpE (cmpop, _, exp_l, exp_r) ->
      Doc.pseq
        [
          render_prose exp_l;
          Doc.text (" " ^ cmpop_word cmpop ^ " ");
          render_prose exp_r;
        ]
  | SubE (exp_inner, typ) ->
      Doc.pseq
        [
          Doc.code (render_code exp_inner);
          Doc.text " has type ";
          Doc.code (code_of_typ typ);
        ]
  | MatchE (exp_inner, pattern) -> (
      let scrut = render_prose exp_inner in
      let pat p = Doc.code (Doc.token (code_of_pattern p)) in
      match pattern with
      | Il.CaseP mixop when Mixop.arity mixop = 0 ->
          Doc.pseq [ scrut; Doc.text " is "; pat (Il.CaseP mixop) ]
      | Il.ListP `Nil -> Doc.pseq [ scrut; Doc.text " is an empty list" ]
      | Il.ListP `Cons -> Doc.pseq [ scrut; Doc.text " is a non-empty list" ]
      | Il.ListP (`Fixed len) ->
          Doc.pseq [ scrut; Doc.text (F.asprintf " is a list of length %d" len) ]
      | Il.OptP `None -> Doc.pseq [ scrut; Doc.text " is none" ]
      | Il.OptP `Some -> Doc.pseq [ scrut; Doc.text " is defined" ]
      | pattern -> Doc.pseq [ scrut; Doc.text " matches pattern "; pat pattern ]
      )
  | TupleE exps ->
      Doc.pseq [ Doc.text "( "; render_proses ~sep:", " exps; Doc.text " )" ]
  | CaseE notexp -> (
      let hint_opt = exp.hints.Annot.prose in
      let link_opt = tid_of_typ exp.node.note in
      match (hint_opt, link_opt) with
      | Some hints, Some tid ->
          Doc.plink ~target:tid.it
            (alternate_doc hints (reindent_lines ~level:0) render_prose
               (Mixfix.args notexp))
      | _ -> Doc.code (code_of_notexp notexp))
  | CatE (exp_l, exp_r) ->
      Doc.pseq
        [
          render_prose exp_l;
          Doc.text " concatenated with ";
          render_prose exp_r;
        ]
  | MemE (exp_e, exp_s) ->
      Doc.pseq [ render_prose exp_e; Doc.text " is in "; render_prose exp_s ]
  | LenE exp_inner ->
      Doc.pseq [ Doc.text "the length of "; render_prose exp_inner ]
  | UpdE (exp_b, path, exp_f) ->
      Doc.pseq
        [
          Doc.code (render_code exp_b);
          Doc.text " with ";
          Doc.code (render_path path);
          Doc.text " set to ";
          Doc.code (render_code exp_f);
        ]
  | CallE (id, _targs, args) -> (
      let hint_in = exp.hints.Annot.prose_in in
      let hint_true = exp.hints.Annot.prose_true in
      match (hint_in, hint_true) with
      | Some hints, _ | _, Some hints ->
          Doc.plink ~target:id.it
            (alternate_doc hints (reindent_lines ~level:0) render_arg_prose args)
      | None, None -> Doc.code (render_code exp))
  | IterE (exp_inner, iterexp) -> (
      match iterexp with
      | _, [] -> render_prose exp_inner
      | _ -> Doc.code (render_code exp))

and render_proses ?(sep : string option) (exps : exp list) : Doc.prose =
  match sep with
  | Some sep ->
      Doc.pseq
        (List.mapi
           (fun i exp ->
             if i = 0 then render_prose exp
             else Doc.pseq [ Doc.text sep; render_prose exp ])
           exps)
  | None ->
      Doc.text (render_list (List.map (fun exp -> Doc.to_adoc (render_prose exp)) exps))

and render_negated_prose_opt (exp : exp) : Doc.prose option =
  match exp.node.it with
  | MatchE (exp_e, pattern) ->
      Some
        (Doc.pseq
           [
             render_prose exp_e;
             Doc.text " does not match pattern ";
             Doc.code (Doc.token (code_of_pattern pattern));
           ])
  | SubE (exp_e, typ) ->
      Some
        (Doc.pseq
           [
             Doc.code (render_code exp_e);
             Doc.text " does not have type ";
             Doc.code (code_of_typ typ);
           ])
  | MemE (exp_e, exp_s) ->
      Some
        (Doc.pseq
           [
             Doc.code (render_code exp_e);
             Doc.text " is not in ";
             Doc.code (render_code exp_s);
           ])
  | CallE (id, _targs, args) -> (
      match exp.hints.Annot.prose_false with
      | Some hints ->
          Some
            (Doc.plink ~target:id.it
               (alternate_doc hints (reindent_lines ~level:0) render_arg_prose
                  args))
      | None ->
          Some
            (Doc.code
               (Doc.cseq [ Doc.token (render_unop `NotOp); render_code exp ])))
  | _ -> None

and render_arg_prose (arg : arg) : Doc.prose =
  match arg.it with
  | ExpA exp -> render_prose exp
  | DefA defid -> Doc.code (Doc.token (string_of_defid defid))

and render_param_prose (param : param) : Doc.prose =
  match param.it with
  | ExpP (_, exp) -> render_prose exp
  | DefP (defid, _, _, _) -> Doc.code (Doc.token (string_of_defid defid))

and render_params_prose (params : param list) : Doc.prose =
  match params with
  | [] -> Doc.pempty
  | params ->
      Doc.pseq
        [
          Doc.text "(";
          Doc.pseq
            (List.mapi
               (fun i param ->
                 if i = 0 then render_param_prose param
                 else Doc.pseq [ Doc.text ", "; render_param_prose param ])
               params);
          Doc.text ")";
        ]

(* Case analysis *)

let render_guard (exp_scrut : exp) (guard : guard) : Doc.prose =
  match guard with
  | BoolG true -> render_prose exp_scrut
  | BoolG false ->
      let node_scrut = exp_scrut.node in
      let neg_inner =
        UnE (`NotOp, `BoolT, exp_scrut) $$ (node_scrut.at, node_scrut.note)
      in
      render_prose (Annot.no_hints neg_inner)
  | CmpG (cmpop, _, exp) ->
      Doc.pseq
        [
          render_prose exp_scrut;
          Doc.text (" " ^ cmpop_word cmpop ^ " ");
          render_prose exp;
        ]
  | SubG typ ->
      Doc.pseq
        [
          Doc.code (render_code exp_scrut);
          Doc.text " has type ";
          Doc.code (code_of_typ typ);
        ]
  | MatchG pattern ->
      Doc.pseq
        [
          render_prose exp_scrut;
          Doc.text " matches pattern ";
          Doc.code (Doc.token (code_of_pattern pattern));
        ]
  | MemG exp ->
      Doc.pseq
        [ render_prose exp_scrut; Doc.text " is in "; render_prose exp ]
  | CheckLetSubG (_, target) | CheckLetMatchG (_, target) ->
      Doc.pseq
        [
          Doc.text "let ";
          Doc.code (render_code target);
          Doc.text " be ";
          render_prose exp_scrut;
        ]

(* Instructions *)

let rec render_instr ?(level : int = 0) ?(unordered : bool = false)
    ?(backtrack : Backtrack.ctx option = None) (instr : instr) : Block.t =
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
    (instrs : block) : Block.t =
  match instrs with
  | [
   ({ node = { it = ReturnI ({ node = { it = BoolE _; _ }; _ } as e); _ }; _ } :
     instr);
  ] ->
      Block.inline
        (Doc.pseq
           [ Doc.text " return "; Doc.code (render_code e); Doc.text "." ])
  | _ ->
      Block.concat
        [
          Block.raw "\n";
          Block.vseq (List.map (render_instr ~level ~backtrack) instrs);
        ]

and render_iterexp_suffix (iterexps : iterexp list) : Doc.prose =
  match iterexps with
  | [] -> Doc.pempty
  | _ ->
      let vars = List.concat_map (fun (_, vars) -> vars) iterexps in
      if vars = [] then Doc.pempty
      else Doc.pseq [ Doc.text ", for all "; render_in_itervars vars ]

and render_iterinstr_suffix (iterinstrs : iterinstr list) : Doc.prose =
  match iterinstrs with
  | [] -> Doc.pempty
  | _ ->
      let vars =
        List.concat_map (fun (_, vars_in, _vars_out) -> vars_in) iterinstrs
      in
      if vars = [] then Doc.pempty
      else Doc.pseq [ Doc.text ", for each "; render_in_itervars vars ]

(* If instruction rendering *)

and render_children ~(level : int) ~(backtrack : Backtrack.ctx option)
    (block : block) : Block.t =
  Block.vseq (List.map (render_instr ~level ~backtrack) block)

and render_if_instr ~(level : int) ~(bullet : string)
    ~(backtrack : Backtrack.ctx option) (cond : exp) (iterexps : iterexp list)
    (block_then : block) : Block.t =
  let fallthrough = Backtrack.render_fallthrough_link backtrack in
  let head =
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.pseq
             [
               Doc.text "Check that ";
               render_prose cond;
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
          (Doc.plink ~target:(string_of_relid id_rel)
             (alternate_doc hint (reindent_lines ~level:0) render_prose
                exps))
    | None ->
        let math =
          Doc.to_adoc
            (Doc.plink ~target:(string_of_relid id_rel) (Doc.code (code_of_notexp notexp)))
        in
        math ^ fallback_verb
  in
  let if_head ~hold =
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.pseq
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
          render_instrs ~level:(level + 1) ~backtrack block;
        ]
  | NotHoldH (block, _dangle) ->
      Block.concat
        [
          if_head ~hold:false;
          render_instrs ~level:(level + 1) ~backtrack block;
        ]
  | BothH (block_hold, block_nothold) ->
      Block.concat
        [
          if_head ~hold:true;
          render_instrs ~level:(level + 1) ~backtrack block_hold;
          Block.raw "\n";
          Block.raw bullet;
          Block.inline (Doc.text "Else:");
          render_instrs ~level:(level + 1) ~backtrack block_nothold;
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
              (Doc.pseq
                 [
                   Doc.text "Check that ";
                   render_guard exp_scrut guard;
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
                     render_instrs ~level:(level + 1) ~backtrack block_then;
                   ]
               else
                 let keyword = if idx = 0 then "If" else "Else if" in
                 Block.concat
                   [
                     Block.raw bullet;
                     Block.inline
                       (Doc.pseq
                          [
                            Doc.text (keyword ^ " ");
                            render_guard exp_scrut guard;
                            Doc.text ":";
                          ]);
                     render_instrs ~level:(level + 1) ~backtrack block_then;
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
        Doc.plink ~target:(string_of_relid id_rel)
          (alternate_doc ~caps:true hint (reindent_lines ~level:0)
             render_prose exps)
    | None, None ->
        Doc.plink ~target:(string_of_relid id_rel)
          (render_rel_title_math rel_signature exps)
  in
  Block.concat
    [
      Block.raw bullet;
      Block.inline (Doc.pseq [ title; Doc.text ":" ]);
      render_instrs ~level:(level + 1) ~backtrack block;
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
    Block.concat
      [
        Block.raw
          (F.asprintf "%s{empty}%s" (adoc_ordered_bullet level_arm) anchor);
        render_instrs ~level:level_body ~backtrack:(Some backtrack) arm;
      ]
  in
  Block.concat
    [
      Block.raw bullet;
      Block.inline
        (Doc.pseq
           [
             Doc.text "Try ";
             Doc.text (Backtrack.render_block_label block);
             Doc.text ":";
           ]);
      Block.raw "\n";
      Block.vseq (List.mapi render_arm arms);
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
          (Doc.pseq
             [
               Doc.text "Let ";
               Doc.code (render_code exp_l);
               Doc.text " be ";
               render_prose exp_r;
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
            (Doc.pseq
               [
                 Doc.text "Let ";
                 Doc.code (render_code exp_l);
                 Doc.text " be ";
                 render_prose exp_r;
                 Doc.text ".";
               ]);
        ]
    in
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.pseq
             [
               Doc.text "Let ";
               render_out_itervars vars_out_visible;
               Doc.text " obtained by repeating:";
             ]);
        Block.raw "\n+\n--\n";
        body;
        Block.raw "\n--\n+\nfor each ";
        Block.inline
          (Doc.pseq
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
          (* Not wrapped in a visible link, but inner links are suppressed:
             render to a string with links off. *)
          Doc.to_adoc_in_link
            (alternate_doc hint_out unindent_lines render_prose exps_out)
        in
        let prose_in =
          Doc.to_adoc
            (Doc.plink ~target:(string_of_relid id_rel)
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
          (Doc.to_adoc
             (Doc.plink ~target:(string_of_relid id_rel)
                (Doc.code (code_of_notexp notexp))))
  in
  if vars_out_visible = [] then
    Block.concat
      [
        Block.raw bullet;
        Block.raw rule_body;
        Block.inline
          (Doc.pseq
             [ render_iterinstr_suffix iterinstrs; Doc.text "."; Doc.text fallthrough ]);
      ]
  else
    let bullet_inner = adoc_unordered_bullet (level + 1) in
    Block.concat
      [
        Block.raw bullet;
        Block.inline
          (Doc.pseq
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
          (Doc.pseq
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
          (Doc.pseq
             [
               Doc.text "Result in ";
               alternate_doc hint (reindent_lines ~level:0) render_prose
                 exps;
               Doc.text ".";
             ])
    | None, [] -> line (Doc.text "The relation holds.")
    | None, _ ->
        line
          (Doc.pseq
             [ Doc.text "Result in "; render_proses exps; Doc.text "." ])

(* Return instruction rendering *)

and render_return_instr ~(bullet : string) (exp : exp) : Block.t =
  Block.concat
    [
      Block.raw bullet;
      Block.inline
        (Doc.pseq [ Doc.text "Return "; render_prose exp; Doc.text "." ]);
    ]

(* Debug instruction rendering *)

and render_debug_instr ~(bullet : string) (exp : exp) : Block.t =
  Block.concat
    [
      Block.raw bullet;
      Block.inline
        (Doc.pseq [ Doc.text "(debug: "; render_prose exp; Doc.text ")" ]);
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
        (Doc.pseq
           [
             Doc.text "Let ";
             render_prose exp_target;
             Doc.text (F.asprintf " be the %s of " name);
             render_prose exp_source;
             Doc.text ".";
           ])
  | _ ->
      let names, exps_target = List.split projections in
      line
        (Doc.pseq
           [
             Doc.text "Let ";
             render_proses exps_target;
             Doc.text " be ";
             Doc.text (render_list (List.map (fun s -> "the " ^ s) names));
             Doc.text " of ";
             render_prose exp_source;
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
          (Doc.pseq
             [
               Doc.text "Let!~type~ ";
               Doc.code (render_code exp_l);
               Doc.text " be ";
               render_prose exp_r;
               Doc.text ".";
               Doc.text fallthrough;
             ]);
      ]
  in
  if block_inner = [] then head
  else
    Block.concat
      [ head; Block.raw "\n"; render_children ~level ~backtrack block_inner ]

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
          (Doc.pseq
             [
               Doc.text "Let ";
               Doc.code (render_code exp_l);
               Doc.text " be ";
               Doc.text (adoc_link ~link:"option_get" "*!*");
               Doc.text " ";
               render_prose exp_r;
               Doc.text ".";
               Doc.text fallthrough;
             ]);
      ]
  in
  if block_inner = [] then head
  else
    Block.concat
      [ head; Block.raw "\n"; render_children ~level ~backtrack block_inner ]

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
              (Doc.plink ~target:(string_of_relid id_rel)
                 (alternate_doc ~caps:true hint (reindent_lines ~level:0)
                    render_prose exps))
        | None, None ->
            Doc.to_adoc
              (Doc.plink ~target:(string_of_relid id_rel)
                 (render_rel_title_math rel_signature exps))
      in
      title ^ ":\n" ^ Block.serialize (render_instrs block)
  | _ -> assert false

and render_elseblock (elseblock_opt : elseblock option) : string =
  match elseblock_opt with
  | None | Some [] -> ""
  | Some block ->
      "\n\n" ^ adoc_ordered_bullet 0 ^ "Otherwise:"
      ^ Block.serialize (render_instrs ~level:1 block)

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
    Doc.prose =
  let nottyp, inputs = rel_signature in
  let mixop = Mixfix.to_mixop nottyp.it in
  let dexps = List.map render_code exps in
  let num_outputs = Mixop.arity mixop - List.length dexps in
  let holes = List.init num_outputs (fun _ -> Doc.token "%") in
  let padded = Hints.Input.combine inputs dexps holes in
  Doc.code (assemble_doc ~atom:code_of_atom mixop padded)

and render_rel_title_adoc (hints : Annot.hints) (id_rel : id)
    (rel_signature : rel_signature) (exps : exp list) : string =
  let exps_in_title =
    match hints.Annot.prose_input_exps with
    | Some exps_in_sl -> List.map lift_synthesized_exp exps_in_sl
    | None -> exps
  in
  let title =
    Doc.plink ~target:(string_of_relid id_rel)
      (Doc.text (Sl.Print.string_of_relid id_rel))
  in
  let title_header =
    Block.concat
      [ Block.inline (Doc.pseq [ title; Doc.text ":" ]); Block.raw "\n\n" ]
  in
  match
    (hints.prose_in, hints.prose_out, hints.prose_output_exps, hints.prose_true)
  with
  | Some _, Some _, None, _ -> assert false
  | Some hint_in, Some hint_out, Some exps_out_sl, _ ->
      let exps_out = List.map lift_synthesized_exp exps_out_sl in
      Block.serialize
        (Block.concat
           [
             title_header;
             Block.raw (adoc_unordered_bullet 0);
             Block.inline
               (alternate_doc ~caps:true hint_in (reindent_lines ~level:1)
                  render_prose exps_in_title);
             Block.raw ":\n";
             Block.raw (adoc_unordered_bullet 0);
             Block.inline (Doc.text "Result in ");
             Block.inline
               (alternate_doc ~caps:false hint_out (reindent_lines ~level:1)
                  render_prose exps_out);
             Block.raw ".";
           ])
  | Some hint_in, _, _, _ ->
      Block.serialize
        (Block.concat
           [
             title_header;
             Block.raw (adoc_unordered_bullet 0);
             Block.inline
               (alternate_doc ~caps:true hint_in (reindent_lines ~level:1)
                  render_prose exps_in_title);
             Block.raw ".";
           ])
  | _, _, _, Some hint_true ->
      Block.serialize
        (Block.concat
           [
             title_header;
             Block.raw (adoc_unordered_bullet 0);
             Block.inline
               (alternate_doc ~caps:true hint_true (reindent_lines ~level:0)
                  render_prose exps);
           ])
  | _ ->
      Block.serialize
        (Block.inline
           (Doc.plink ~target:(string_of_relid id_rel)
              (Doc.pseq
                 [
                   Doc.text (Sl.Print.string_of_relid id_rel ^ ": ");
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
    Doc.plink
      ~target:(string_of_defid ~link:true id_func)
      (Doc.text (string_of_defid id_func))
  in
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      Block.serialize
        (Block.concat
           [
             Block.inline (Doc.pseq [ title; Doc.text ":" ]);
             Block.raw "\n\n";
             Block.raw (adoc_unordered_bullet 0);
             Block.inline
               (alternate_doc ~caps:true hint (reindent_lines ~level:0)
                  render_param_prose params);
           ])
  | None, None ->
      Block.serialize
        (Block.concat
           [
             Block.inline title;
             Block.raw (Sl.Print.string_of_tparams tparams);
             Block.raw
               (Doc.to_adoc_code
                  (render_params_code params));
           ])

let render_func_header (hints : Annot.hints) (id_func : id)
    (tparams : tparam list) (params : param list) : string =
  match (hints.prose_in, hints.prose_true) with
  | Some hint, _ | _, Some hint ->
      Doc.to_adoc
        (Doc.plink
           ~target:(string_of_defid ~link:true id_func)
           (Doc.text
              (Doc.to_adoc
                 (alternate_doc ~caps:true hint (reindent_lines ~level:0)
                    render_param_prose params))))
  | None, None ->
      Doc.to_adoc
        (Doc.plink
           ~target:(string_of_defid ~link:true id_func)
           (Doc.text
              (string_of_defid id_func
              ^ Sl.Print.string_of_tparams tparams
              ^ Doc.to_adoc_code
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
    Block.concat
      [
        Block.raw "|===\n| ";
        Block.inline (render_params_prose params);
        Block.raw " | Result \n\n";
      ]
  in
  let table_rows =
    Block.vseq
      (tablerows
      |> List.map (fun tablerow ->
             let exps_sig, exp_res, _ = tablerow in
             let row_output = Doc.to_adoc_code (render_code exp_res) in
             let row_input = Doc.to_adoc_code (render_codes exps_sig) in
             Block.raw ("| " ^ row_input ^ " | " ^ row_output)))
  in
  Block.serialize
    (Block.concat
       [
         Block.raw (render_func_header hints id_func [] params);
         Block.raw ":\n";
         Block.raw table_meta;
         table_header;
         table_rows;
         Block.raw "\n\n|===";
       ])

let render_defined_func_def (hints : Annot.hints) (func : definedfunc) : string
    =
  let id_func, tparams, params, _typ, block, elseblock_opt = func in
  render_func_header hints id_func tparams params
  ^ "\n\n"
  ^ strip_leading_newline (Block.serialize (render_instrs block))
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
