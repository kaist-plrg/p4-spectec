open Ast
open Util.Print

(* Names *)

let print_id (id : id) = id
let print_ctor (ctor : ctor) = ctor
let print_field (field : field) = field

(* Type parameters *)

let print_tparam (tparam : tparam) = "'" ^ tparam

let print_tparams (tparams : tparam list) =
  tparams |> List.map print_tparam |> String.concat " "

(* Types *)

let rec print_typ typ =
  match typ with
  | UnitT -> "unit"
  | BoolT -> "bool"
  | StringT -> "string"
  | BigintT -> "Bigint.t"
  | NameT name -> print_id name
  | VarT var -> "'" ^ var
  | AppT (name, [ typ ]) -> print_typ typ ^ " " ^ print_id name
  | AppT (name, typs) ->
      "(" ^ String.concat ", " (List.map print_typ typs) ^ ") " ^ print_id name
  | TupleT [] -> "unit"
  | TupleT [ typ ] -> print_typ typ
  | TupleT typs -> "(" ^ String.concat " * " (List.map print_typ typs) ^ ")"
  | OpenRowT typcases ->
      "[> "
      ^ String.concat " | "
          (List.map
             (fun (ctor, typs_arg) ->
               match typs_arg with
               | [] -> "`" ^ print_ctor ctor
               | [ typ ] -> "`" ^ print_ctor ctor ^ " of " ^ print_typ typ
               | typs ->
                   "`" ^ print_ctor ctor ^ " of ("
                   ^ String.concat " * " (List.map print_typ typs)
                   ^ ")")
             typcases)
      ^ "]"

(* Type definitions *)

let print_pv_ctor (ctor, typs_arg) =
  match typs_arg with
  | [] -> "`" ^ print_ctor ctor
  | [ typ ] -> "`" ^ print_ctor ctor ^ " of " ^ print_typ typ
  | typs ->
      "`" ^ print_ctor ctor ^ " of ("
      ^ String.concat " * " (List.map print_typ typs)
      ^ ")"

let print_deftyp deftyp =
  match deftyp with
  | AliasTD typ -> print_typ typ
  | RecordTD fields ->
      "{ "
      ^ String.concat "; "
          (List.map
             (fun (field, typ) -> print_field field ^ " : " ^ print_typ typ)
             fields)
      ^ " }"
  | VariantTD [] -> "[ `Empty ]"
  | VariantTD ctors ->
      "[\n"
      ^ String.concat "\n| "
          (List.map (fun pv_ctor -> "  " ^ print_pv_ctor pv_ctor) ctors)
      ^ "\n]"

let print_typdef_content (tparams, name, deftyp) =
  let str_tparams_prefix =
    match tparams with
    | [] -> ""
    | [ tparam ] -> print_tparam tparam ^ " "
    | tparams -> "(" ^ String.concat ", " (List.map print_tparam tparams) ^ ") "
  in
  str_tparams_prefix ^ print_id name ^ " =\n" ^ print_deftyp deftyp

let print_typdef typdef = "type " ^ print_typdef_content typdef

(* Patterns *)

let rec print_pat pat =
  match pat with
  | WildP -> "_"
  | VarP id -> print_id id
  | LitP lit -> lit
  | TupleP pats -> "(" ^ String.concat ", " (List.map print_pat pats) ^ ")"
  | ListP pats -> "[" ^ String.concat "; " (List.map print_pat pats) ^ "]"
  | ConsP (pat_hd, pat_tl) -> print_pat pat_hd ^ " :: " ^ print_pat pat_tl
  | OptP None -> "None"
  | OptP (Some pat) -> "Some (" ^ print_pat pat ^ ")"
  | OpenP id -> "#" ^ print_id id
  | VariantP (`Poly (ctor, [])) -> "`" ^ print_ctor ctor
  | VariantP (`Poly (ctor, pats)) ->
      "`" ^ print_ctor ctor ^ " ("
      ^ String.concat ", " (List.map print_pat pats)
      ^ ")"
  | VariantP (`Mono (ctor, [])) -> print_ctor ctor
  | VariantP (`Mono (ctor, [ pat ])) -> print_ctor ctor ^ " " ^ print_pat pat
  | VariantP (`Mono (ctor, pats)) ->
      print_ctor ctor ^ " ("
      ^ String.concat ", " (List.map print_pat pats)
      ^ ")"
  | AsP (pat, id) -> "(" ^ print_pat pat ^ " as " ^ print_id id ^ ")"
  | OrP [] -> "()"
  | OrP pats -> "(" ^ String.concat " | " (List.map print_pat pats) ^ ")"

(* Operators *)

let print_unop (op : unop) = op
let print_binop (op : binop) = op

(* Expressions *)

let is_compound_expr = function
  | MatchE _ | IfE _ | TryE _ | LetE _ | FunE _ | SeqE _ | AnnotE _ | CoerceE _
  | AppE (_, _ :: _) ->
      true
  | _ -> false

let rec print_expr ~level expr =
  let ind = indent level in
  let ind_inner = indent (level + 1) in
  match expr with
  | UnitE -> "()"
  | BoolE bool -> string_of_bool bool
  | BigintE str -> "(Bigint.of_string \"" ^ str ^ "\")"
  | StrE str -> "\"" ^ String.escaped str ^ "\""
  | LitE lit -> lit
  | VarE id -> print_id id
  | BinopE (op, expr_l, expr_r) ->
      let wrap expr =
        if is_compound_expr expr then "(" ^ print_expr ~level expr ^ ")"
        else print_expr ~level expr
      in
      "(" ^ wrap expr_l ^ " " ^ print_binop op ^ " " ^ wrap expr_r ^ ")"
  | UnopE (op, expr) ->
      let str =
        if is_compound_expr expr then "(" ^ print_expr ~level expr ^ ")"
        else print_expr ~level expr
      in
      "(" ^ print_unop op ^ " " ^ str ^ ")"
  | TupleE [] -> "()"
  | TupleE [ expr ] -> print_expr ~level expr
  | TupleE exprs ->
      let print_tuple_elem expr =
        if is_compound_expr expr then "(" ^ print_expr ~level expr ^ ")"
        else print_expr ~level expr
      in
      "(" ^ String.concat ", " (List.map print_tuple_elem exprs) ^ ")"
  | ListE exprs ->
      "[" ^ String.concat "; " (List.map (print_expr ~level) exprs) ^ "]"
  | ConsE (expr_hd, expr_tl) ->
      "(" ^ print_expr ~level expr_hd ^ " :: " ^ print_expr ~level expr_tl ^ ")"
  | OptE None -> "None"
  | OptE (Some expr) -> "Some (" ^ print_expr ~level expr ^ ")"
  | VariantE (ctor, []) -> "`" ^ print_ctor ctor
  | VariantE (ctor, exprs) ->
      "`" ^ print_ctor ctor ^ " ("
      ^ String.concat ", " (List.map (print_expr ~level) exprs)
      ^ ")"
  | RecordE [] -> "{}"
  | RecordE fields ->
      "{ "
      ^ String.concat "; "
          (List.map
             (fun (field, expr) ->
               print_field field ^ " = " ^ print_expr ~level expr)
             fields)
      ^ " }"
  | RecordUpdateE (expr_base, []) -> print_expr ~level expr_base
  | RecordUpdateE (expr_base, fields) ->
      "{ " ^ "("
      ^ print_expr ~level expr_base
      ^ ")" ^ " with "
      ^ String.concat "; "
          (List.map
             (fun (field, expr) ->
               print_field field ^ " = " ^ print_expr ~level expr)
             fields)
      ^ " }"
  | FieldE (expr_rec, field) ->
      print_expr ~level expr_rec ^ "." ^ print_field field
  | AppE (expr_fn, []) -> print_expr ~level expr_fn ^ " ()"
  | AppE (expr_fn, exprs_arg) ->
      let print_arg expr =
        if is_compound_expr expr then
          "(\n"
          ^ Util.Print.indent (level + 1)
          ^ print_expr ~level:(level + 1) expr
          ^ "\n" ^ Util.Print.indent level ^ ")"
        else "(" ^ print_expr ~level expr ^ ")"
      in
      print_expr ~level expr_fn ^ " "
      ^ String.concat " " (List.map print_arg exprs_arg)
  | IfE (expr_cond, expr_then, expr_else_opt) ->
      let str_else =
        match expr_else_opt with
        | None -> ""
        | Some expr_else -> "\n" ^ ind ^ "else " ^ print_expr ~level expr_else
      in
      "if "
      ^ print_expr ~level expr_cond
      ^ "\n" ^ ind ^ "then "
      ^ print_expr ~level:(level + 1) expr_then
      ^ str_else
  | MatchE (expr_scrut, cases) ->
      let print_case (pat, expr) =
        let body_str = print_expr ~level:(level + 2) expr in
        let body_wrapped =
          match expr with
          | MatchE _ | TryE _ ->
              "\n"
              ^ indent (level + 2)
              ^ "begin\n"
              ^ indent (level + 3)
              ^ print_expr ~level:(level + 3) expr
              ^ "\n"
              ^ indent (level + 2)
              ^ "end"
          | _ -> "\n" ^ ind_inner ^ "  " ^ body_str
        in
        ind_inner ^ "| " ^ print_pat pat ^ " ->" ^ body_wrapped
      in
      "match "
      ^ print_expr ~level expr_scrut
      ^ " with\n"
      ^ String.concat "\n" (List.map print_case cases)
  | LetE (pat_bind, expr_rhs, expr_cont) ->
      "let " ^ print_pat pat_bind ^ " = " ^ print_expr ~level expr_rhs ^ " in\n"
      ^ ind
      ^ print_expr ~level expr_cont
  | TryE (expr_body, handlers) ->
      let print_handler (pat, expr) =
        ind_inner ^ "| " ^ print_pat pat ^ " ->\n" ^ ind_inner ^ "  "
        ^ print_expr ~level:(level + 2) expr
      in
      "try\n" ^ ind_inner
      ^ print_expr ~level:(level + 1) expr_body
      ^ "\n" ^ ind ^ "with\n"
      ^ String.concat "\n" (List.map print_handler handlers)
  | FunE (pats_param, expr_body) ->
      let print_fun_arg pat =
        match pat with
        | VariantP (`Poly (_, _ :: _))
        | VariantP (`Mono (_, _ :: _))
        | OptP (Some _) ->
            "(" ^ print_pat pat ^ ")"
        | _ -> print_pat pat
      in
      "fun "
      ^ String.concat " " (List.map print_fun_arg pats_param)
      ^ " ->\n" ^ ind_inner
      ^ print_expr ~level:(level + 1) expr_body
  | SeqE [] -> "()"
  | SeqE exprs ->
      "(\n" ^ ind_inner
      ^ String.concat (";\n" ^ ind_inner)
          (List.map (print_expr ~level:(level + 1)) exprs)
      ^ "\n" ^ ind ^ ")"
  | AnnotE (expr, typ) ->
      "(" ^ print_expr ~level expr ^ " : " ^ print_typ typ ^ ")"
  | CoerceE (expr, typ) ->
      "(" ^ print_expr ~level expr ^ " :> " ^ print_typ typ ^ ")"

(* Parameters *)

let print_param (id, typ_opt) =
  match typ_opt with
  | None -> print_id id
  | Some typ -> "(" ^ print_id id ^ " : " ^ print_typ typ ^ ")"

(* Function definitions *)

let print_funcdef (id, tparams, params, typ_ret_opt, expr_body) =
  match tparams with
  | [] ->
      let str_params =
        match params with
        | [] -> "()"
        | _ -> String.concat " " (List.map print_param params)
      in
      let str_ret =
        match typ_ret_opt with None -> "" | Some typ -> " : " ^ print_typ typ
      in
      print_id id ^ " " ^ str_params ^ str_ret ^ " =\n  "
      ^ print_expr ~level:1 expr_body
  | _ ->
      (* Explicit forall: OCaml can't infer instantiation at a different
         type than the member's own params inside a [let rec]. *)
      let quantifier = String.concat " " (List.map (fun t -> "'" ^ t) tparams) in
      let param_typ_strs =
        List.map
          (fun (id, typ_opt) ->
            match typ_opt with
            | Some typ -> print_typ typ
            | None ->
                failwith
                  (Printf.sprintf
                     "print_funcdef: generic function %s: untyped param %s \
                      (higher-order params not supported)"
                     id id))
          params
      in
      let ret_typ_str =
        match typ_ret_opt with
        | Some typ -> print_typ typ
        | None -> failwith (Printf.sprintf "print_funcdef: generic function %s: missing return type" id)
      in
      let arrow_typ_str =
        match param_typ_strs with
        | [] -> "unit -> " ^ ret_typ_str
        | _ -> String.concat " -> " (param_typ_strs @ [ ret_typ_str ])
      in
      let str_params =
        match params with
        | [] -> "()"
        | _ -> String.concat " " (List.map (fun (id, _) -> print_id id) params)
      in
      print_id id ^ " : " ^ quantifier ^ ". " ^ arrow_typ_str ^ " =\n  fun "
      ^ str_params ^ " ->\n  " ^ print_expr ~level:1 expr_body

(* Top-level items *)

let print_toplevel toplevel =
  match toplevel with
  | Raw str -> str
  | TypeRec [] -> ""
  | TypeRec (typdef_h :: typdefs_t) ->
      "type "
      ^ print_typdef_content typdef_h
      ^ "\n\n"
      ^ String.concat "\n\n"
          (List.map
             (fun typdef -> "and " ^ print_typdef_content typdef)
             typdefs_t)
  | Let (id, expr) -> "let " ^ print_id id ^ " = " ^ print_expr ~level:0 expr
  | LetRec [] -> ""
  | LetRec (funcdef_h :: funcdefs_t) ->
      "let rec " ^ print_funcdef funcdef_h ^ "\n\n"
      ^ String.concat "\n\n"
          (List.map (fun funcdef -> "and " ^ print_funcdef funcdef) funcdefs_t)

(* Files *)

let print_file (items : file) =
  let buf = Buffer.create (1024 * 64) in
  List.iter
    (fun toplevel ->
      let str = print_toplevel toplevel in
      if str <> "" then (
        Buffer.add_string buf str;
        Buffer.add_char buf '\n'))
    items;
  Buffer.contents buf
