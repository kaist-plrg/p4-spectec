open Lang
open Splice
open Util.Source

(* Syntax splicer *)

module Syntax : SPLICE = struct
  type key = Kinds.SyntaxId.t
  type value = Kinds.Syntax.source

  let name = "syntax"
  let prefix = "[source,bison]\n----\n"
  let suffix = "\n----"
  let parse_keys (source : Source.t) : key list = Parser.parse_syntax_ids source

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_syntax ctx) keys

  let render (keys : key list) (values : value list) : string =
    List.map2
      (fun (id_typ : key) (syntax : value) ->
        let def =
          match syntax with
          | Kinds.Syntax.ExternS hints ->
              El.ExternSynD (id_typ $ no_region, hints) $ no_region
          | Kinds.Syntax.DefinedS (tparams, deftyp, hints) ->
              El.TypD (id_typ $ no_region, tparams, deftyp, hints) $ no_region
        in
        El.Print.string_of_def def)
      keys values
    |> String.concat "\n\n"
end

(* Relation title splicer *)

module RelTitleSource : SPLICE = struct
  type key = Kinds.RelTitleId.t
  type value = Kinds.RelTitle.source

  let name = "relation-title-source"

  let prefix =
    ".Click to view the specification source\n[%collapsible]\n====\n----\n"

  let suffix = "\n----\n====\n\n[.empty]\n--\n\n\n--\n\n"

  let parse_keys (source : Source.t) : key list =
    Parser.parse_rel_title_ids source

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_rel_title_source ctx) keys

  let render (keys : key list) (values : value list) : string =
    List.map2
      (fun (id_rel : key) (rel_title : value) ->
        let def =
          match rel_title with
          | Kinds.RelTitle.ExternS (nottyp, hints) ->
              El.ExternRelD (id_rel $ no_region, nottyp, hints) $ no_region
          | Kinds.RelTitle.DefinedS (nottyp, hints) ->
              El.RelD (id_rel $ no_region, nottyp, hints) $ no_region
        in
        El.Print.string_of_def def)
      keys values
    |> String.concat "\n\n"
end

module RelTitleProse : SPLICE = struct
  type key = Kinds.RelTitleId.t
  type value = Kinds.RelTitle.prose

  let name = "relation-title-prose"
  let prefix = "****\n"
  let suffix = "\n****"

  let parse_keys (source : Source.t) : key list =
    Parser.parse_rel_title_ids source

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_rel_title_prose ctx) keys

  let render (keys : key list) (value : value list) : string =
    List.map2
      (fun (_id_rel : key) (rel_title_prose : value) ->
        let rel_title =
          match rel_title_prose with
          | Kinds.RelTitle.ExternP rel_title | Kinds.RelTitle.DefinedP rel_title
            ->
              rel_title
        in
        Pl.Render.render_rel_title rel_title)
      keys value
    |> String.concat "\n\n"
end

(* Rule group splicer *)

module RuleGroupSource : SPLICE = struct
  type key = Kinds.RuleGroupId.t
  type value = Kinds.RuleGroup.source

  let name = "rulegroup-source"

  let prefix =
    ".Click to view the specification source\n[%collapsible]\n====\n----\n"

  let suffix = "\n----\n====\n\n[.empty]\n--\n\n\n--\n\n"

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_rulegroup_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_rulegroup_source ctx) keys

  let render (keys : key list) (values : value list) : string =
    List.map2
      (fun ((id_rel, id_rulegroup) : key) (rules : value) ->
        let def =
          El.RuleGroupD (id_rel $ no_region, id_rulegroup $ no_region, rules)
          $ no_region
        in
        El.Print.string_of_def def)
      keys values
    |> String.concat "\n\n"
end

module RuleGroupProse : SPLICE = struct
  type key = Kinds.RuleGroupId.t
  type value = Kinds.RuleGroup.prose

  let name = "rulegroup-prose"
  let prefix = "****\n"
  let suffix = "\n****"

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_rulegroup_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_rulegroup_prose ctx) keys

  let render (keys : key list) (values : value list) : string =
    List.map2
      (fun ((id_rel, _) : key) (rulegroup : value) ->
        Pl.Render.render_rulegroup (id_rel $ no_region) rulegroup)
      keys values
    |> String.concat "\n\n"
end

(* Function title splicer *)

module FuncTitleSource : SPLICE = struct
  type key = Kinds.FuncTitleId.t
  type value = Kinds.FuncTitle.source

  let name = "func-title-source"

  let prefix =
    ".Click to view the specification source\n[%collapsible]\n====\n----\n"

  let suffix = "\n----\n====\n\n[.empty]\n--\n\n\n--\n\n"

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_func_title_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_func_title_source ctx) keys

  let render (keys : key list) (values : value list) : string =
    List.map2
      (fun (id_def : key) (func_title : value) ->
        let def =
          match func_title with
          | Kinds.FuncTitle.ExternS (tparams, params, plaintyp, hints) ->
              El.ExternDecD
                (id_def $ no_region, tparams, params, plaintyp, hints)
              $ no_region
          | Kinds.FuncTitle.BuiltinS (tparams, params, plaintyp, hints) ->
              El.BuiltinDecD
                (id_def $ no_region, tparams, params, plaintyp, hints)
              $ no_region
          | Kinds.FuncTitle.DefinedS (tparams, params, plaintyp, hints) ->
              El.FuncDecD (id_def $ no_region, tparams, params, plaintyp, hints)
              $ no_region
        in
        El.Print.string_of_def def)
      keys values
    |> String.concat "\n\n"
end

module FuncTitleProse : SPLICE = struct
  type key = Kinds.FuncTitleId.t
  type value = Kinds.FuncTitle.prose

  let name = "func-title-prose"
  let prefix = "****\n"
  let suffix = "\n****"

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_func_title_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_func_title_prose ctx) keys

  let render (keys : key list) (values : value list) : string =
    List.map2
      (fun (_id_def : key) (func_title : value) ->
        let func_title =
          match func_title with
          | Kinds.FuncTitle.ExternP func_title
          | Kinds.FuncTitle.BuiltinP func_title
          | Kinds.FuncTitle.DefinedP func_title ->
              func_title
        in
        Pl.Render.render_func_title func_title)
      keys values
    |> String.concat "\n\n"
end

(* Function splicer *)

module FuncSource : SPLICE = struct
  type key = Kinds.FuncId.t
  type value = Kinds.Func.source

  let name = "func-source"
  let prefix = "****\n"
  let suffix = "\n****"

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_func_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_func_source ctx) keys

  let render (keys : key list) (values : value list) : string =
    List.map2
      (fun (id_def : key) (funcs : value) ->
        let defs =
          List.map
            (fun (tparams, args, exp, prems) ->
              El.FuncDefD (id_def $ no_region, tparams, args, exp, prems)
              $ no_region)
            funcs
        in
        defs |> List.map El.Print.string_of_def |> String.concat "\n\n")
      keys values
    |> String.concat "\n\n"
end

module FuncProse : SPLICE = struct
  type key = Kinds.FuncId.t
  type value = Kinds.Func.prose

  let name = "func-prose"
  let prefix = "****\n"
  let suffix = "\n****"

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_func_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_func_prose ctx) keys

  let render (keys : key list) (values : value list) : string =
    List.map2
      (fun (_id_def : key) (func : value) ->
        Pl.Render.render_defined_func_def func)
      keys values
    |> String.concat "\n\n"
end

(* Table splicer *)

module TableSource : SPLICE = struct
  type key = Kinds.TableId.t
  type value = Kinds.Table.source

  let name = "table-source"

  let prefix =
    ".Click to view the specification source\n[%collapsible]\n====\n----\n"

  let suffix = "\n----\n====\n\n[.empty]\n--\n\n\n--\n\n"

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_table_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_table_source ctx) keys

  let render (keys : key list) (values : value list) : string =
    List.map2
      (fun (id_def : key) (tablerows : value) ->
        let def = El.TableDefD (id_def $ no_region, tablerows) $ no_region in
        El.Print.string_of_def def)
      keys values
    |> String.concat "\n\n"
end

module TableProse : SPLICE = struct
  type key = Kinds.TableId.t
  type value = Kinds.Table.prose

  let name = "table-prose"
  let prefix = ""
  let suffix = "\n"

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_table_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_table_prose ctx) keys

  let render (keys : key list) (values : value list) : string =
    List.map2
      (fun (_id_def : key) (tablefunc : value) ->
        Pl.Render.render_table_func_def tablefunc)
      keys values
    |> String.concat "\n\n"
end
