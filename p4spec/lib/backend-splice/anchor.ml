open Lang
open Error
open Util.Source
module Set = Set.Make (String)

(* Anchor targets *)

type presentation = Prose | Latex

(* Function and relation declarations *)

type prose = { func : Set.t; rel : Set.t }
type latex = { func : Set.t; rel : Set.t }
type targets = { prose : prose; latex : latex }

let empty_prose : prose = { func = Set.empty; rel = Set.empty }
let empty_latex : latex = { func = Set.empty; rel = Set.empty }
let empty : targets = { prose = empty_prose; latex = empty_latex }

(* EL declarations *)

type decls = { funcs : Set.t; rels : Set.t }

let collect_decls (spec_el : El.spec) : decls =
  List.fold_left
    (fun decls (def : El.def) ->
      match def.it with
      | ExternRelD (id, _, _) | RelD (id, _, _) ->
          { decls with rels = Set.add id.it decls.rels }
      | ExternDecD (id, _, _, _, _)
      | BuiltinDecD (id, _, _, _, _)
      | FuncDecD (id, _, _, _, _) ->
          { decls with funcs = Set.add id.it decls.funcs }
      | _ -> decls)
    { funcs = Set.empty; rels = Set.empty }
    spec_el

(* Skeleton targets *)

let add_ids (splice : string) (ids_declared : Set.t) (ids : string list)
    (set : Set.t) : Set.t =
  List.fold_left
    (fun set id ->
      if not (Set.mem id ids_declared) then set
      else if Set.mem id set then (
        warn no_region (Format.asprintf "duplicate %s target: %s" splice id);
        set)
      else Set.add id set)
    set ids

let add_funcs_prose (decls : Set.t) (targets : targets) (ids : string list) :
    targets =
  let prose =
    {
      targets.prose with
      func = add_ids "func-title-prose" decls ids targets.prose.func;
    }
  in
  { targets with prose }

let add_rels_prose (decls : Set.t) (targets : targets) (ids : string list) :
    targets =
  let prose =
    {
      targets.prose with
      rel = add_ids "relation-title-prose" decls ids targets.prose.rel;
    }
  in
  { targets with prose }

let add_funcs_latex (decls : Set.t) (targets : targets) (ids : string list) :
    targets =
  let latex =
    {
      targets.latex with
      func = add_ids "func-title-latex" decls ids targets.latex.func;
    }
  in
  { targets with latex }

let add_rels_latex (decls : Set.t) (targets : targets) (ids : string list) :
    targets =
  let latex =
    {
      targets.latex with
      rel = add_ids "relation-title-latex" decls ids targets.latex.rel;
    }
  in
  { targets with latex }

let rec collect_targets (decls : decls) (targets : targets) (source : Source.t)
    : targets =
  if Source.eos source then targets
  else
    let first (options : (unit -> 'a option) list) : 'a option =
      let rec first = function
        | [] -> None
        | option :: options -> (
            match option () with
            | Some _ as value -> value
            | None -> first options)
      in
      first options
    in
    let add (splice : string) (update : string list -> targets) : targets option
        =
      if Parser.parse_splice_start source splice then
        Some (update (Parser.parse_ids source))
      else None
    in
    let targets_opt =
      first
        [
          (fun () ->
            add "func-title-prose" (add_funcs_prose decls.funcs targets));
          (fun () ->
            add "func-title-latex" (add_funcs_latex decls.funcs targets));
          (fun () ->
            add "relation-title-prose" (add_rels_prose decls.rels targets));
          (fun () ->
            add "relation-title-latex" (add_rels_latex decls.rels targets));
        ]
    in
    match targets_opt with
    | Some targets -> collect_targets decls targets source
    | None ->
        Source.adv source;
        collect_targets decls targets source

(* Renderer lookups *)

let presentation_name (presentation : presentation) : string =
  match presentation with Prose -> "prose" | Latex -> "latex"

let anchor_name_func (presentation : presentation) (name : string) : string =
  "function_" ^ presentation_name presentation ^ "_" ^ name

let anchor_name_rel (presentation : presentation) (name : string) : string =
  "relation_" ^ presentation_name presentation ^ "_" ^ name

let anchors_of (presentation : presentation) (targets : targets) : Ctx.anchors =
  let funcs, rels =
    match presentation with
    | Prose -> (targets.prose.func, targets.prose.rel)
    | Latex -> (targets.latex.func, targets.latex.rel)
  in
  let func (name : string) : string option =
    if Set.mem name funcs then Some (anchor_name_func presentation name)
    else None
  in
  let rel (name : string) : string option =
    if Set.mem name rels then Some (anchor_name_rel presentation name) else None
  in
  { func; rel }

(* Entry point *)

let collect (spec_el : El.spec) (sources : (string * string) list) : Ctx.t =
  let decls = collect_decls spec_el in
  let targets =
    List.fold_left
      (fun targets (file, content) ->
        let source = Source.{ file; s = content; i = 0 } in
        collect_targets decls targets source)
      empty sources
  in
  Ctx.make ~anchors_prose:(anchors_of Prose targets)
    ~anchors_latex:(anchors_of Latex targets)
