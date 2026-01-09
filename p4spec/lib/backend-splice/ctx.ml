open Lang
open Error
open Util.Source

(* Context *)

module SyntaxMap = Map.Make (Kinds.SyntaxId)
module RelTitleMap = Map.Make (Kinds.RelTitleId)
module RuleGroupMap = Map.Make (Kinds.RuleGroupId)
module FuncTitleMap = Map.Make (Kinds.FuncTitleId)
module FuncMap = Map.Make (Kinds.FuncId)
module TableMap = Map.Make (Kinds.TableId)

type t = {
  filename : string;
  (* Syntax *)
  mutable syntax : Kinds.Syntax.source SyntaxMap.t;
  (* Relation title *)
  mutable rel_title_source : Kinds.RelTitle.source RelTitleMap.t;
  mutable rel_title_prose : Kinds.RelTitle.prose RelTitleMap.t;
  (* Rule group *)
  mutable rulegroup_source : Kinds.RuleGroup.source RuleGroupMap.t;
  mutable rulegroup_prose : Kinds.RuleGroup.prose RuleGroupMap.t;
  (* Function title *)
  mutable func_title_source : Kinds.FuncTitle.source FuncTitleMap.t;
  mutable func_title_prose : Kinds.FuncTitle.prose FuncTitleMap.t;
  (* Function *)
  mutable func_source : Kinds.Func.source FuncMap.t;
  mutable func_prose : Kinds.Func.prose FuncMap.t;
  (* Table *)
  mutable table_source : Kinds.Table.source TableMap.t;
  mutable table_prose : Kinds.Table.prose TableMap.t;
}

(* Initialization *)

(* Initialization from EL *)

let init_el_def (ctx : t) (def_el : El.def) : unit =
  match def_el.it with
  (* Syntax *)
  | ExternSynD (id_syntax, hints) ->
      let syntax = Kinds.Syntax.ExternS hints in
      ctx.syntax <- SyntaxMap.add id_syntax.it syntax ctx.syntax
  | TypD (id_syntax, tparams, deftyp, hints) ->
      let syntax = Kinds.Syntax.DefinedS (tparams, deftyp, hints) in
      ctx.syntax <- SyntaxMap.add id_syntax.it syntax ctx.syntax
  (* Relation title *)
  | ExternRelD (id_rel, nottyp, hints) ->
      let rel_title = Kinds.RelTitle.ExternS (nottyp, hints) in
      ctx.rel_title_source <-
        RelTitleMap.add id_rel.it rel_title ctx.rel_title_source
  | RelD (id_rel, nottyp, hints) ->
      let rel_title = Kinds.RelTitle.DefinedS (nottyp, hints) in
      ctx.rel_title_source <-
        RelTitleMap.add id_rel.it rel_title ctx.rel_title_source
  (* Rule group *)
  | RuleGroupD (id_rel, id_rulegroup, rules) ->
      let rulegroup = rules in
      ctx.rulegroup_source <-
        RuleGroupMap.add
          (id_rel.it, id_rulegroup.it)
          rulegroup ctx.rulegroup_source
  (* Function title *)
  | ExternDecD (id_func, tparams, params, plaintyp, hints) ->
      let func_title =
        Kinds.FuncTitle.ExternS (tparams, params, plaintyp, hints)
      in
      ctx.func_title_source <-
        FuncTitleMap.add id_func.it func_title ctx.func_title_source
  | BuiltinDecD (id_func, tparams, params, plaintyp, hints) ->
      let func_title =
        Kinds.FuncTitle.BuiltinS (tparams, params, plaintyp, hints)
      in
      ctx.func_title_source <-
        FuncTitleMap.add id_func.it func_title ctx.func_title_source
  | FuncDecD (id_func, tparams, params, plaintyp, hints) ->
      let func_title =
        Kinds.FuncTitle.DefinedS (tparams, params, plaintyp, hints)
      in
      ctx.func_title_source <-
        FuncTitleMap.add id_func.it func_title ctx.func_title_source
  (* Function *)
  | FuncDefD (id_func, tparams, args, exp, prems) ->
      let func = (tparams, args, exp, prems) in
      let funcs =
        FuncMap.find_opt id_func.it ctx.func_source |> Option.value ~default:[]
      in
      let funcs = funcs @ [ func ] in
      ctx.func_source <- FuncMap.add id_func.it funcs ctx.func_source
  (* Table *)
  | TableDefD (id_table, tablerows) ->
      let table = tablerows in
      ctx.table_source <- TableMap.add id_table.it table ctx.table_source
  | _ -> ()

let init_el (ctx : t) (spec_el : El.spec) : unit =
  List.iter (init_el_def ctx) spec_el

(* Initialization from PL *)

let init_pl_def (ctx : t) (def_sl : Pl.def) : unit =
  let id_of_rel_title = function
    | Pl.ProseRelTitle (`Hold (id_rel, _, _))
    | Pl.ProseRelTitle (`Yield (id_rel, _, _, _, _))
    | Pl.MathRelTitle (id_rel, _, _) ->
        id_rel
  in
  let id_of_rulegroup_title = function
    | Pl.ProseRuleTitle (`Hold (id_rulegroup, _, _))
    | Pl.ProseRuleTitle (`Yield (id_rulegroup, _, _))
    | Pl.MathRuleTitle (id_rulegroup, _, _) ->
        id_rulegroup
  in
  let id_of_func_title = function
    | Pl.ProseFuncTitle (`Check (id_def, _, _))
    | Pl.ProseFuncTitle (`Yield (id_def, _, _))
    | Pl.MathFuncTitle (id_def, _, _) ->
        id_def
  in
  match def_sl.it with
  (* Relation title *)
  | ExternRelD rel_title ->
      let id_rel = id_of_rel_title rel_title in
      let rel_title = Kinds.RelTitle.ExternP rel_title in
      ctx.rel_title_prose <-
        RelTitleMap.add id_rel.it rel_title ctx.rel_title_prose
  (* Relation title *)
  (* Rule group *)
  | RelD (rel_title, rulegroups) ->
      let id_rel = id_of_rel_title rel_title in
      let rel_title = Kinds.RelTitle.DefinedP rel_title in
      ctx.rel_title_prose <-
        RelTitleMap.add id_rel.it rel_title ctx.rel_title_prose;
      List.iter
        (fun rulegroup ->
          let rulegroup_title, _ = rulegroup in
          let id_rulegroup = id_of_rulegroup_title rulegroup_title in
          ctx.rulegroup_prose <-
            RuleGroupMap.add
              (id_rel.it, id_rulegroup.it)
              rulegroup ctx.rulegroup_prose)
        rulegroups
  (* Function title *)
  (* Function *)
  | ExternDecD func_title ->
      let id_def = id_of_func_title func_title in
      let func_title = Kinds.FuncTitle.ExternP func_title in
      ctx.func_title_prose <-
        FuncTitleMap.add id_def.it func_title ctx.func_title_prose
  | BuiltinDecD func_title ->
      let id_def = id_of_func_title func_title in
      let func_title = Kinds.FuncTitle.BuiltinP func_title in
      ctx.func_title_prose <-
        FuncTitleMap.add id_def.it func_title ctx.func_title_prose
  | FuncDecD func ->
      let func_title, _ = func in
      let id_func = id_of_func_title func_title in
      let func_title = Kinds.FuncTitle.DefinedP func_title in
      ctx.func_title_prose <-
        FuncTitleMap.add id_func.it func_title ctx.func_title_prose;
      ctx.func_prose <- FuncMap.add id_func.it func ctx.func_prose
  (* Table *)
  | TableDecD tablefunc ->
      let func_title, _ = tablefunc in
      let id_table = id_of_func_title func_title in
      ctx.table_prose <- TableMap.add id_table.it tablefunc ctx.table_prose

let init_pl (ctx : t) (spec_pl : Pl.spec) : unit =
  List.iter (init_pl_def ctx) spec_pl

let init (spec_el : El.spec) (spec_pl : Pl.spec) (filename : string) : t =
  let ctx =
    {
      filename;
      syntax = SyntaxMap.empty;
      rel_title_source = RelTitleMap.empty;
      rel_title_prose = RelTitleMap.empty;
      rulegroup_source = RuleGroupMap.empty;
      rulegroup_prose = RuleGroupMap.empty;
      func_title_source = FuncTitleMap.empty;
      func_title_prose = FuncTitleMap.empty;
      func_source = FuncMap.empty;
      func_prose = FuncMap.empty;
      table_source = TableMap.empty;
      table_prose = TableMap.empty;
    }
  in
  init_el ctx spec_el;
  init_pl ctx spec_pl;
  ctx

(* Finders *)

let find_syntax (ctx : t) (id : Kinds.SyntaxId.t) : Kinds.Syntax.source =
  match SyntaxMap.find_opt id ctx.syntax with
  | Some syntax -> syntax
  | None ->
      error no_region ("syntax " ^ id ^ " was not found in " ^ ctx.filename)

let find_rel_title_source (ctx : t) (id : Kinds.RelTitleId.t) :
    Kinds.RelTitle.source =
  match RelTitleMap.find_opt id ctx.rel_title_source with
  | Some rel_title -> rel_title
  | None ->
      error no_region
        ("relation title " ^ id ^ " was not found in " ^ ctx.filename)

let find_rel_title_prose (ctx : t) (id : Kinds.RelTitleId.t) :
    Kinds.RelTitle.prose =
  match RelTitleMap.find_opt id ctx.rel_title_prose with
  | Some rel_title -> rel_title
  | None ->
      error no_region
        ("relation title " ^ id ^ " was not found in " ^ ctx.filename)

let find_rulegroup_source (ctx : t) (id : Kinds.RuleGroupId.t) :
    Kinds.RuleGroup.source =
  match RuleGroupMap.find_opt id ctx.rulegroup_source with
  | Some rulegroup -> rulegroup
  | None ->
      error no_region
        ("rule group " ^ fst id ^ "/" ^ snd id ^ " was not found in "
       ^ ctx.filename)

let find_rulegroup_prose (ctx : t) (id : Kinds.RuleGroupId.t) :
    Kinds.RuleGroup.prose =
  match RuleGroupMap.find_opt id ctx.rulegroup_prose with
  | Some rulegroup -> rulegroup
  | None ->
      error no_region
        ("rule group " ^ fst id ^ "/" ^ snd id ^ " was not found in "
       ^ ctx.filename)

let find_func_title_source (ctx : t) (id : Kinds.FuncTitleId.t) :
    Kinds.FuncTitle.source =
  match FuncTitleMap.find_opt id ctx.func_title_source with
  | Some func_title -> func_title
  | None ->
      error no_region
        ("function title " ^ id ^ " was not found in " ^ ctx.filename)

let find_func_title_prose (ctx : t) (id : Kinds.FuncTitleId.t) :
    Kinds.FuncTitle.prose =
  match FuncTitleMap.find_opt id ctx.func_title_prose with
  | Some func_title -> func_title
  | None ->
      error no_region
        ("function title " ^ id ^ " was not found in " ^ ctx.filename)

let find_func_source (ctx : t) (id : Kinds.FuncId.t) : Kinds.Func.source =
  match FuncMap.find_opt id ctx.func_source with
  | Some funcs -> funcs
  | None ->
      error no_region ("function " ^ id ^ " was not found in " ^ ctx.filename)

let find_func_prose (ctx : t) (id : Kinds.FuncId.t) : Kinds.Func.prose =
  match FuncMap.find_opt id ctx.func_prose with
  | Some func -> func
  | None ->
      error no_region ("function " ^ id ^ " was not found in " ^ ctx.filename)

let find_table_source (ctx : t) (id : Kinds.TableId.t) : Kinds.Table.source =
  match TableMap.find_opt id ctx.table_source with
  | Some table -> table
  | None -> error no_region ("table " ^ id ^ " was not found in " ^ ctx.filename)

let find_table_prose (ctx : t) (id : Kinds.TableId.t) : Kinds.Table.prose =
  match TableMap.find_opt id ctx.table_prose with
  | Some table -> table
  | None -> error no_region ("table " ^ id ^ " was not found in " ^ ctx.filename)
