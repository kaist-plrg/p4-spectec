open Util.Source
module RenderCtx = Prose.Ctx

(* Signature for splicing modules *)

module type Splice = sig
  type key
  type value

  val name : string
  val prefix : string option
  val suffix : string option
  val parse_keys : Source.t -> key list
  val find_values : Ctx.t -> key list -> value list
  val render : Ctx.t -> key list -> value list -> string
end

(* Syntax splicer *)

module Syntax : Splice = struct
  type key = Kinds.SyntaxId.t
  type value = Kinds.syntax

  let name = "syntax"
  let prefix = Some "[source,bison]\n----\n"
  let suffix = Some "\n----"
  let parse_keys (source : Source.t) : key list = Parser.parse_syntax_ids source

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_syntax ctx) keys

  let render (_ctx : Ctx.t) (keys : key list) (values : value list) : string =
    List.map2
      (fun (id_typ : key) ((tparams, deftyp, hints) : value) ->
        El.Render.render_type_def (id_typ $ no_region) tparams deftyp hints)
      keys values
    |> String.concat "\n\n"
end

(* Relation splicer *)

module Relation : Splice = struct
  type key = Kinds.RelationId.t
  type value = Kinds.relation

  let name = "relation"
  let prefix = Some "\n----\n"
  let suffix = Some "\n----"

  let parse_keys (source : Source.t) : key list =
    Parser.parse_relation_ids source

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_relation ctx) keys

  let render (_ctx : Ctx.t) (keys : key list) (value : value list) : string =
    List.map2
      (fun (id_rel : key) ((nottyp, hints) : value) ->
        El.Render.render_relation_def (id_rel $ no_region) nottyp hints)
      keys value
    |> String.concat "\n\n"
end

(* Rule group splicer *)

module RuleGroup : Splice = struct
  type key = Kinds.RuleGroupId.t
  type value = Kinds.rulegroup

  let name = "rulegroup"
  let prefix = Some "\n----\n"
  let suffix = Some "\n----"

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_rulegroup_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_rulegroup ctx) keys

  let render (_ctx : Ctx.t) (keys : key list) (values : value list) : string =
    List.map2
      (fun ((id_rel, id_rulegroup) : key) (rules : value) ->
        El.Render.render_rulegroup_def (id_rel $ no_region)
          (id_rulegroup $ no_region) rules)
      keys values
    |> String.concat "\n\n"
end

(* Rule prose splicer *)

module RuleProse : Splice = struct
  type key = Kinds.RuleProseId.t
  type value = Kinds.ruleprose

  let name = "ruleprose"
  let prefix = None
  let suffix = None

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_ruleprose_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_ruleprose ctx) keys

  let render (ctx : Ctx.t) (keys : key list) (values : value list) : string =
    List.map2
      (fun ((id_rel, _) : key) ((mixop, inputs, exps, instrs) : value) ->
        let rulegroup_pl =
          Prose.Prosify.prosify_rulegroup ctx.prose_ctx (id_rel $ no_region)
            mixop inputs exps instrs
        in
        Prose.Pl.Print.prose_of_rulegroup rulegroup_pl)
      keys values
    |> String.concat "\n\n"
end

(* Function prose splicer *)

module FuncProse : Splice = struct
  type key = Kinds.FuncProseId.t
  type value = Kinds.funcprose

  let name = "funcprose"
  let prefix = None
  let suffix = Some "\n"

  let parse_keys (source : Source.t) : key list =
    [ Parser.parse_funcprose_id source ]

  let find_values (ctx : Ctx.t) (keys : key list) : value list =
    List.map (Ctx.find_funcprose ctx) keys

  let render (ctx : Ctx.t) (keys : key list) (values : value list) : string =
    List.map2
      (fun (id_def : key) ((tparams, args, typ, instrs) : value) ->
        let func_pl =
          Prose.Prosify.prosify_func ctx.prose_ctx (id_def $ no_region) tparams
            args typ instrs
        in
        Prose.Pl.Print.prose_of_func func_pl)
      keys values
    |> String.concat "\n\n"
end
