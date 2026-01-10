open Lang
open Error
open Util.Source

(* Generic map with "used" tracking *)

module Make (K : sig
  type t

  val compare : t -> t -> int
end) (V : sig
  type t
end) =
struct
  module M = Map.Make (K)

  type entry = { mutable used : bool; data : V.t }
  type t = entry M.t

  let empty : t = M.empty

  let add (key : K.t) (data : V.t) (ctx : t) : t =
    M.add key { used = false; data } ctx

  let find_opt (key : K.t) (ctx : t) : V.t option =
    match M.find_opt key ctx with Some entry -> Some entry.data | None -> None

  let use_opt (key : K.t) (ctx : t) : V.t option =
    match M.find_opt key ctx with
    | Some entry ->
        entry.used <- true;
        Some entry.data
    | None -> None

  let unused (ctx : t) : K.t list =
    M.fold
      (fun key entry keys_unused ->
        if entry.used then keys_unused else key :: keys_unused)
      ctx []
    |> List.rev
end

module Syntax = Make (Kinds.SyntaxId) (Kinds.Syntax)
module RelTitleSource = Make (Kinds.RelTitleId) (Kinds.RelTitleSource)
module RelTitleProse = Make (Kinds.RelTitleId) (Kinds.RelTitleProse)
module RuleGroupSource = Make (Kinds.RuleGroupId) (Kinds.RuleGroupSource)
module RuleGroupProse = Make (Kinds.RuleGroupId) (Kinds.RuleGroupProse)
module FuncTitleSource = Make (Kinds.FuncTitleId) (Kinds.FuncTitleSource)
module FuncTitleProse = Make (Kinds.FuncTitleId) (Kinds.FuncTitleProse)
module FuncSource = Make (Kinds.FuncId) (Kinds.FuncSource)
module FuncProse = Make (Kinds.FuncId) (Kinds.FuncProse)
module TableSource = Make (Kinds.TableId) (Kinds.TableSource)
module TableProse = Make (Kinds.TableId) (Kinds.TableProse)

(* Context *)

type t = {
  mutable filename : string;
  (* Syntax *)
  mutable syntax : Syntax.t;
  (* Relation title *)
  mutable rel_title_source : RelTitleSource.t;
  mutable rel_title_prose : RelTitleProse.t;
  (* Rule group *)
  mutable rulegroup_source : RuleGroupSource.t;
  mutable rulegroup_prose : RuleGroupProse.t;
  (* Function title *)
  mutable func_title_source : FuncTitleSource.t;
  mutable func_title_prose : FuncTitleProse.t;
  (* Function *)
  mutable func_source : FuncSource.t;
  mutable func_prose : FuncProse.t;
  (* Table *)
  mutable table_source : TableSource.t;
  mutable table_prose : TableProse.t;
}

(* Initialization *)

(* Initialization from EL *)

let init_el_def (ctx : t) (def_el : El.def) : unit =
  match def_el.it with
  (* Syntax *)
  | ExternSynD (id_syntax, hints) ->
      let syntax = Kinds.Syntax.ExternS hints in
      ctx.syntax <- Syntax.add id_syntax.it syntax ctx.syntax
  | TypD (id_syntax, tparams, deftyp, hints) ->
      let syntax = Kinds.Syntax.DefinedS (tparams, deftyp, hints) in
      ctx.syntax <- Syntax.add id_syntax.it syntax ctx.syntax
  (* Relation title *)
  | ExternRelD (id_rel, nottyp, hints) ->
      let rel_title = Kinds.RelTitleSource.ExternS (nottyp, hints) in
      ctx.rel_title_source <-
        RelTitleSource.add id_rel.it rel_title ctx.rel_title_source
  | RelD (id_rel, nottyp, hints) ->
      let rel_title = Kinds.RelTitleSource.DefinedS (nottyp, hints) in
      ctx.rel_title_source <-
        RelTitleSource.add id_rel.it rel_title ctx.rel_title_source
  (* Rule group *)
  | RuleGroupD (id_rel, id_rulegroup, rules) ->
      let rulegroup = rules in
      ctx.rulegroup_source <-
        RuleGroupSource.add
          (id_rel.it, id_rulegroup.it)
          rulegroup ctx.rulegroup_source
  (* Function title *)
  | ExternDecD (id_func, tparams, params, plaintyp, hints) ->
      let func_title =
        Kinds.FuncTitleSource.ExternS (tparams, params, plaintyp, hints)
      in
      ctx.func_title_source <-
        FuncTitleSource.add id_func.it func_title ctx.func_title_source
  | BuiltinDecD (id_func, tparams, params, plaintyp, hints) ->
      let func_title =
        Kinds.FuncTitleSource.BuiltinS (tparams, params, plaintyp, hints)
      in
      ctx.func_title_source <-
        FuncTitleSource.add id_func.it func_title ctx.func_title_source
  | FuncDecD (id_func, tparams, params, plaintyp, hints) ->
      let func_title =
        Kinds.FuncTitleSource.DefinedS (tparams, params, plaintyp, hints)
      in
      ctx.func_title_source <-
        FuncTitleSource.add id_func.it func_title ctx.func_title_source
  (* Function *)
  | FuncDefD (id_func, tparams, args, exp, prems) ->
      let func = (tparams, args, exp, prems) in
      let funcs =
        FuncSource.find_opt id_func.it ctx.func_source
        |> Option.value ~default:[]
      in
      let funcs = funcs @ [ func ] in
      ctx.func_source <- FuncSource.add id_func.it funcs ctx.func_source
  (* Table *)
  | TableDefD (id_table, tablerows) ->
      let table = tablerows in
      ctx.table_source <- TableSource.add id_table.it table ctx.table_source
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
      let rel_title = Kinds.RelTitleProse.ExternP rel_title in
      ctx.rel_title_prose <-
        RelTitleProse.add id_rel.it rel_title ctx.rel_title_prose
  (* Relation title *)
  (* Rule group *)
  | RelD (rel_title, rulegroups) ->
      let id_rel = id_of_rel_title rel_title in
      let rel_title = Kinds.RelTitleProse.DefinedP rel_title in
      ctx.rel_title_prose <-
        RelTitleProse.add id_rel.it rel_title ctx.rel_title_prose;
      List.iter
        (fun rulegroup ->
          let rulegroup_title, _ = rulegroup in
          let id_rulegroup = id_of_rulegroup_title rulegroup_title in
          ctx.rulegroup_prose <-
            RuleGroupProse.add
              (id_rel.it, id_rulegroup.it)
              rulegroup ctx.rulegroup_prose)
        rulegroups
  (* Function title *)
  (* Function *)
  | ExternDecD func_title ->
      let id_def = id_of_func_title func_title in
      let func_title = Kinds.FuncTitleProse.ExternP func_title in
      ctx.func_title_prose <-
        FuncTitleProse.add id_def.it func_title ctx.func_title_prose
  | BuiltinDecD func_title ->
      let id_def = id_of_func_title func_title in
      let func_title = Kinds.FuncTitleProse.BuiltinP func_title in
      ctx.func_title_prose <-
        FuncTitleProse.add id_def.it func_title ctx.func_title_prose
  | FuncDecD func ->
      let func_title, _ = func in
      let id_func = id_of_func_title func_title in
      let func_title = Kinds.FuncTitleProse.DefinedP func_title in
      ctx.func_title_prose <-
        FuncTitleProse.add id_func.it func_title ctx.func_title_prose;
      ctx.func_prose <- FuncProse.add id_func.it func ctx.func_prose
  (* Table *)
  | TableDecD tablefunc ->
      let func_title, _ = tablefunc in
      let id_table = id_of_func_title func_title in
      ctx.table_prose <- TableProse.add id_table.it tablefunc ctx.table_prose

let init_pl (ctx : t) (spec_pl : Pl.spec) : unit =
  List.iter (init_pl_def ctx) spec_pl

let init (spec_el : El.spec) (spec_pl : Pl.spec) : t =
  let ctx =
    {
      filename = "";
      syntax = Syntax.empty;
      rel_title_source = RelTitleSource.empty;
      rel_title_prose = RelTitleProse.empty;
      rulegroup_source = RuleGroupSource.empty;
      rulegroup_prose = RuleGroupProse.empty;
      func_title_source = FuncTitleSource.empty;
      func_title_prose = FuncTitleProse.empty;
      func_source = FuncSource.empty;
      func_prose = FuncProse.empty;
      table_source = TableSource.empty;
      table_prose = TableProse.empty;
    }
  in
  init_el ctx spec_el;
  init_pl ctx spec_pl;
  ctx

let set_filename (ctx : t) (filename : string) : unit = ctx.filename <- filename

(* Users *)

let use_syntax (ctx : t) (id : Kinds.SyntaxId.t) : Kinds.Syntax.t =
  match Syntax.use_opt id ctx.syntax with
  | Some syntax -> syntax
  | None ->
      error no_region ("syntax " ^ id ^ " was not found in " ^ ctx.filename)

let use_rel_title_source (ctx : t) (id : Kinds.RelTitleId.t) :
    Kinds.RelTitleSource.t =
  match RelTitleSource.use_opt id ctx.rel_title_source with
  | Some rel_title -> rel_title
  | None ->
      error no_region
        ("relation title " ^ id ^ " was not found in " ^ ctx.filename)

let use_rel_title_prose (ctx : t) (id : Kinds.RelTitleId.t) :
    Kinds.RelTitleProse.t =
  match RelTitleProse.use_opt id ctx.rel_title_prose with
  | Some rel_title -> rel_title
  | None ->
      error no_region
        ("relation title " ^ id ^ " was not found in " ^ ctx.filename)

let use_rulegroup_source (ctx : t) (id : Kinds.RuleGroupId.t) :
    Kinds.RuleGroupSource.t =
  match RuleGroupSource.use_opt id ctx.rulegroup_source with
  | Some rulegroup -> rulegroup
  | None ->
      error no_region
        ("rule group " ^ fst id ^ "/" ^ snd id ^ " was not found in "
       ^ ctx.filename)

let use_rulegroup_prose (ctx : t) (id : Kinds.RuleGroupId.t) :
    Kinds.RuleGroupProse.t =
  match RuleGroupProse.use_opt id ctx.rulegroup_prose with
  | Some rulegroup -> rulegroup
  | None ->
      error no_region
        ("rule group " ^ fst id ^ "/" ^ snd id ^ " was not found in "
       ^ ctx.filename)

let use_func_title_source (ctx : t) (id : Kinds.FuncTitleId.t) :
    Kinds.FuncTitleSource.t =
  match FuncTitleSource.use_opt id ctx.func_title_source with
  | Some func_title -> func_title
  | None ->
      error no_region
        ("function title " ^ id ^ " was not found in " ^ ctx.filename)

let use_func_title_prose (ctx : t) (id : Kinds.FuncTitleId.t) :
    Kinds.FuncTitleProse.t =
  match FuncTitleProse.use_opt id ctx.func_title_prose with
  | Some func_title -> func_title
  | None ->
      error no_region
        ("function title " ^ id ^ " was not found in " ^ ctx.filename)

let use_func_source (ctx : t) (id : Kinds.FuncId.t) : Kinds.FuncSource.t =
  match FuncSource.use_opt id ctx.func_source with
  | Some funcs -> funcs
  | None ->
      error no_region ("function " ^ id ^ " was not found in " ^ ctx.filename)

let use_func_prose (ctx : t) (id : Kinds.FuncId.t) : Kinds.FuncProse.t =
  match FuncProse.use_opt id ctx.func_prose with
  | Some func -> func
  | None ->
      error no_region ("function " ^ id ^ " was not found in " ^ ctx.filename)

let use_table_source (ctx : t) (id : Kinds.TableId.t) : Kinds.TableSource.t =
  match TableSource.use_opt id ctx.table_source with
  | Some table -> table
  | None -> error no_region ("table " ^ id ^ " was not found in " ^ ctx.filename)

let use_table_prose (ctx : t) (id : Kinds.TableId.t) : Kinds.TableProse.t =
  match TableProse.use_opt id ctx.table_prose with
  | Some table -> table
  | None -> error no_region ("table " ^ id ^ " was not found in " ^ ctx.filename)

(* Users *)

let unused_syntax (ctx : t) : unit =
  let unused = Syntax.unused ctx.syntax in
  Format.eprintf "warning: unused syntax splices %d/%d (%.2f%%)\n"
    (List.length unused)
    (Syntax.M.cardinal ctx.syntax)
    (if Syntax.M.cardinal ctx.syntax = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (Syntax.M.cardinal ctx.syntax)
       *. 100.0);
  unused |> List.iter (fun id -> Format.eprintf "\t- syntax %s unused\n" id)

let unused_rel_title_source (ctx : t) : unit =
  let unused = RelTitleSource.unused ctx.rel_title_source in
  Format.eprintf "warning: unused relation title sources %d/%d (%.2f%%)\n"
    (List.length unused)
    (RelTitleSource.M.cardinal ctx.rel_title_source)
    (if RelTitleSource.M.cardinal ctx.rel_title_source = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (RelTitleSource.M.cardinal ctx.rel_title_source)
       *. 100.0);
  unused
  |> List.iter (fun id ->
         Format.eprintf "\t- relation title source %s unused\n" id)

let unused_rel_title_prose (ctx : t) : unit =
  let unused = RelTitleProse.unused ctx.rel_title_prose in
  Format.eprintf "warning: unused relation title prose %d/%d (%.2f%%)\n"
    (List.length unused)
    (RelTitleProse.M.cardinal ctx.rel_title_prose)
    (if RelTitleProse.M.cardinal ctx.rel_title_prose = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (RelTitleProse.M.cardinal ctx.rel_title_prose)
       *. 100.0);
  unused
  |> List.iter (fun id ->
         Format.eprintf "\t- relation title prose %s unused\n" id)

let unused_rulegroup_source (ctx : t) : unit =
  let unused = RuleGroupSource.unused ctx.rulegroup_source in
  Format.eprintf "warning: unused rule group sources %d/%d (%.2f%%)\n"
    (List.length unused)
    (RuleGroupSource.M.cardinal ctx.rulegroup_source)
    (if RuleGroupSource.M.cardinal ctx.rulegroup_source = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (RuleGroupSource.M.cardinal ctx.rulegroup_source)
       *. 100.0);
  unused
  |> List.iter (fun (id_rel, id_rulegroup) ->
         Format.eprintf "\t- rule group source %s/%s unused\n" id_rel
           id_rulegroup)

let unused_rulegroup_prose (ctx : t) : unit =
  let unused = RuleGroupProse.unused ctx.rulegroup_prose in
  Format.eprintf "warning: unused rule group prose %d/%d (%.2f%%)\n"
    (List.length unused)
    (RuleGroupProse.M.cardinal ctx.rulegroup_prose)
    (if RuleGroupProse.M.cardinal ctx.rulegroup_prose = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (RuleGroupProse.M.cardinal ctx.rulegroup_prose)
       *. 100.0);
  unused
  |> List.iter (fun (id_rel, id_rulegroup) ->
         Format.eprintf "\t- rule group prose %s/%s unused\n" id_rel
           id_rulegroup)

let unused_func_title_source (ctx : t) : unit =
  let unused = FuncTitleSource.unused ctx.func_title_source in
  Format.eprintf "warning: unused function title sources %d/%d (%.2f%%)\n"
    (List.length unused)
    (FuncTitleSource.M.cardinal ctx.func_title_source)
    (if FuncTitleSource.M.cardinal ctx.func_title_source = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (FuncTitleSource.M.cardinal ctx.func_title_source)
       *. 100.0);
  unused
  |> List.iter (fun id ->
         Format.eprintf "\t- function title source %s unused\n" id)

let unused_func_title_prose (ctx : t) : unit =
  let unused = FuncTitleProse.unused ctx.func_title_prose in
  Format.eprintf "warning: unused function title prose %d/%d (%.2f%%)\n"
    (List.length unused)
    (FuncTitleProse.M.cardinal ctx.func_title_prose)
    (if FuncTitleProse.M.cardinal ctx.func_title_prose = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (FuncTitleProse.M.cardinal ctx.func_title_prose)
       *. 100.0);
  unused
  |> List.iter (fun id ->
         Format.eprintf "\t- function title prose %s unused\n" id)

let unused_func_source (ctx : t) : unit =
  let unused = FuncSource.unused ctx.func_source in
  Format.eprintf "warning: unused function sources %d/%d (%.2f%%)\n"
    (List.length unused)
    (FuncSource.M.cardinal ctx.func_source)
    (if FuncSource.M.cardinal ctx.func_source = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (FuncSource.M.cardinal ctx.func_source)
       *. 100.0);
  unused
  |> List.iter (fun id -> Format.eprintf "\t- function source %s unused\n" id)

let unused_func_prose (ctx : t) : unit =
  let unused = FuncProse.unused ctx.func_prose in
  Format.eprintf "warning: unused function prose %d/%d (%.2f%%)\n"
    (List.length unused)
    (FuncProse.M.cardinal ctx.func_prose)
    (if FuncProse.M.cardinal ctx.func_prose = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (FuncProse.M.cardinal ctx.func_prose)
       *. 100.0);
  unused
  |> List.iter (fun id -> Format.eprintf "\t- function prose %s unused\n" id)

let unused_table_source (ctx : t) : unit =
  let unused = TableSource.unused ctx.table_source in
  Format.eprintf "warning: unused table sources %d/%d (%.2f%%)\n"
    (List.length unused)
    (TableSource.M.cardinal ctx.table_source)
    (if TableSource.M.cardinal ctx.table_source = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (TableSource.M.cardinal ctx.table_source)
       *. 100.0);
  unused
  |> List.iter (fun id -> Format.eprintf "\t- table source %s unused\n" id)

let unused_table_prose (ctx : t) : unit =
  let unused = TableProse.unused ctx.table_prose in
  Format.eprintf "warning: unused table prose %d/%d (%.2f%%)\n"
    (List.length unused)
    (TableProse.M.cardinal ctx.table_prose)
    (if TableProse.M.cardinal ctx.table_prose = 0 then 0.0
     else
       float_of_int (List.length unused)
       /. float_of_int (TableProse.M.cardinal ctx.table_prose)
       *. 100.0);
  unused
  |> List.iter (fun id -> Format.eprintf "\t- table prose %s unused\n" id)

let unused (ctx : t) : unit =
  unused_syntax ctx;
  unused_rel_title_source ctx;
  unused_rel_title_prose ctx;
  unused_rulegroup_source ctx;
  unused_rulegroup_prose ctx;
  unused_func_title_source ctx;
  unused_func_title_prose ctx;
  unused_func_source ctx;
  unused_func_prose ctx;
  unused_table_source ctx;
  unused_table_prose ctx
