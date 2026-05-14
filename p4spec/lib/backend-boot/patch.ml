open Domain
open Lang
module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature
open Error
open Util.Source

(* Convert a meta-value to an expression that evaluates to the same value *)

let rec value_as_exp (value : Value.t) : Il.exp =
  let at_value = value.at in
  let typ_value = value.note.typ in
  let exp =
    match value.it with
    | BoolV b -> Il.BoolE b
    | NumV n -> Il.NumE n
    | TextV t -> Il.TextE t
    | StructV valuefields ->
        let expfields =
          List.map
            (fun (atom_field, value_field) ->
              let exp_field = value_as_exp value_field in
              (atom_field, exp_field))
            valuefields
        in
        Il.StrE expfields
    | CaseV valuecase ->
        let notexp = Mixfix.map value_as_exp valuecase in
        Il.CaseE notexp
    | TupleV values ->
        let exps = List.map value_as_exp values in
        Il.TupleE exps
    | OptV value_opt ->
        let exp_opt = Option.map value_as_exp value_opt in
        Il.OptE exp_opt
    | ListV values ->
        let exps = List.map value_as_exp values in
        Il.ListE exps
    | _ ->
        error at_value
          "cannot convert a function or extern value to an expression"
  in
  exp $$ (at_value, typ_value)

(* Parsing *)

let parse_spec (layer_spec : Config.layer) (interface_spec : Config.interface) :
    Value.t =
  let parse_spec =
    match interface_spec with
    | P4_interface -> assert false
    | IL_interface -> Interface.SpecTec_IL.parse_program []
    | SL_interface -> Interface.SpecTec_SL.parse_program []
  in
  match parse_spec [ layer_spec.specdir ] with
  | Run.Pass value_spec -> value_spec
  | Run.Fail (`Syntax (at, msg)) -> error at msg

let parse_target (target : Config.target) (level_target : Config.level) :
    Value.t =
  let parse_target =
    match level_target.interface with
    | P4_interface -> Interface.P4.parse_program
    | IL_interface | SL_interface -> assert false
  in
  match parse_target target.includes [ target.path ] with
  | Run.Pass value_target -> value_target
  | Run.Fail (`Syntax (at, msg)) -> error at msg

(* Patch *)

(* Create a synthetic "main" meta-function calling [rel] on [value] *)
(* def main() : text = "pass" -- let x = [value] -- [rel]: x *)

let apply_il (level_meta : Config.level) (value_meta : Value.t)
    (value_spec : Value.t) : Value.t =
  (* Find the relation in the spec *)
  let rel_meta = level_meta.layer.rel in
  let defs = Interface.SpecTec_IL.unboot_scriptIL value_spec in
  let mixop_rel, inputs_rel =
    List.find_map
      (fun (def : Il.def) ->
        match def.it with
        | RelD (id, nottyp, inputs, _, _, _) when id.it = rel_meta ->
            let mixop, _ = Mixfix.split nottyp.it in
            Some (mixop, inputs)
        | _ -> None)
      defs
    |> function
    | Some (mixop_rel, inputs_rel) -> (mixop_rel, inputs_rel)
    | None ->
        error no_region
          (Format.asprintf "relation %s not found in the spec" rel_meta)
  in
  (* Validate the relation, that it has exactly one input *)
  check
    (List.length inputs_rel = 1)
    no_region
    (Format.asprintf "relation %s must have exactly one input" rel_meta);
  (* Create the main function definition *)
  let def =
    let id = "main" $ no_region in
    let tparams = [] in
    let params = [] in
    let typ_ret = Il.TextT $ no_region in
    let clauses =
      let clause =
        let args = [] in
        let exp_output = Il.TextE "pass" $$ (no_region, Il.TextT) in
        let prems =
          let exp_bind =
            Il.VarE ("x" $ no_region)
            $$ (no_region, Il.VarT ("x" $ no_region, []))
          in
          let exp_value = value_as_exp value_meta in
          let prem_bind = Il.LetPr (exp_bind, exp_value) $ no_region in
          let exps_output =
            List.init
              (Mixop.arity mixop_rel - List.length inputs_rel)
              (fun idx ->
                Il.VarE ("y_" ^ string_of_int idx $ no_region)
                $$ ( no_region,
                     Il.VarT ("y_" ^ string_of_int idx $ no_region, []) ))
          in
          let prem_call =
            let exps =
              Hints.Input.combine inputs_rel [ exp_bind ] exps_output
            in
            let notexp = Mixfix.fill mixop_rel exps in
            Il.RulePr (rel_meta $ no_region, notexp, inputs_rel) $ no_region
          in
          [ prem_bind; prem_call ]
        in
        (args, exp_output, prems) $ no_region
      in
      [ clause ]
    in
    let elseclause_opt = None in
    let hints = [] in
    Il.FuncDecD (id, tparams, params, typ_ret, clauses, elseclause_opt, hints)
    $ no_region
  in
  (* Il.Print.string_of_def def |> print_endline; *)
  let value_spec = defs @ [ def ] |> Interface.SpecTec_IL.boot_specIL in
  value_spec

let apply_sl (level_meta : Config.level) (value_meta : Value.t)
    (value_spec : Value.t) : Value.t =
  (* Find the relation in the spec *)
  let rel_meta = level_meta.layer.rel in
  let defs = Interface.SpecTec_SL.unboot_scriptSL value_spec in
  let mixop_rel, inputs_rel =
    List.find_map
      (fun (def : Sl.def) ->
        match def.it with
        | RelD (id, (nottyp, inputs), _, _, _, _) when id.it = rel_meta ->
            let mixop, _ = Mixfix.split nottyp.it in
            Some (mixop, inputs)
        | _ -> None)
      defs
    |> function
    | Some (mixop_rel, inputs_rel) -> (mixop_rel, inputs_rel)
    | None ->
        error no_region
          (Format.asprintf "relation %s not found in the spec" rel_meta)
  in
  (* Validate the relation, that it has exactly one input *)
  check
    (List.length inputs_rel = 1)
    no_region
    (Format.asprintf "relation %s must have exactly one input" rel_meta);
  (* Create the main function definition *)
  let def =
    let id = "main" $ no_region in
    let tparams = [] in
    let params = [] in
    let typ_ret = Il.TextT $ no_region in
    let block =
      let exp_bind =
        Il.VarE ("x" $ no_region) $$ (no_region, Il.VarT ("x" $ no_region, []))
      in
      let exp_value = value_as_exp value_meta in
      let instr_return =
        let exp_output = Il.TextE "pass" $$ (no_region, Il.TextT) in
        Sl.(ReturnI exp_output $$ (no_region, { iid = 0 }))
      in
      let instr_call_sl =
        let exps_output_sl =
          List.init
            (Mixop.arity mixop_rel - List.length inputs_rel)
            (fun idx ->
              Il.VarE ("y_" ^ string_of_int idx $ no_region)
              $$ (no_region, Il.VarT ("y_" ^ string_of_int idx $ no_region, [])))
        in
        let exps = Hints.Input.combine inputs_rel [ exp_bind ] exps_output_sl in
        let notexp = Mixfix.fill mixop_rel exps in
        Sl.(
          RuleI (rel_meta $ no_region, notexp, inputs_rel, [], [ instr_return ])
          $$ (no_region, { iid = 0 }))
      in
      let instr_bind =
        Sl.(
          LetI (exp_bind, exp_value, [], [ instr_call_sl ])
          $$ (no_region, { iid = 0 }))
      in
      [ instr_bind ]
    in
    let elseblock_opt = None in
    let hints = [] in
    Sl.FuncDecD (id, tparams, params, typ_ret, block, elseblock_opt, hints)
    $ no_region
  in
  (* Sl.Print.string_of_def def_sl |> print_endline; *)
  let value_spec = defs @ [ def ] |> Interface.SpecTec_SL.boot_specSL in
  value_spec

let apply (level_meta : Config.level) (value_meta : Value.t)
    (level_spec : Config.level) (value_spec : Value.t) : Value.t =
  match level_spec.interface with
  | P4_interface -> assert false
  | IL_interface -> apply_il level_meta value_meta value_spec
  | SL_interface -> apply_sl level_meta value_meta value_spec

let apply_target (target : Config.target) (level_target : Config.level)
    (level_spec : Config.level) : Value.t =
  (* Parse the target spec as a meta-value *)
  let value_spec = parse_spec level_target.layer level_spec.interface in
  (* Parse the target as a meta-value *)
  let value_target = parse_target target level_target in
  (* Create a synthetic "main" meta-function *)
  apply level_target value_target level_spec value_spec

let apply_interm (level_meta : Config.level) (value_meta : Value.t)
    (level_spec : Config.level) : Value.t =
  (* Parse the spec as a meta-value *)
  let value_spec = parse_spec level_meta.layer level_spec.interface in
  (* Create a synthetic "main" meta-function *)
  apply level_meta value_meta level_spec value_spec

let apply_tower (tower : Config.tower) : Value.t =
  (* Reverse the levels, from target to booter *)
  let levels =
    (tower.level_boot :: tower.levels_interm) @ [ tower.level_target ]
    |> List.rev
  in
  (* Pair of levels, from target to booter *)
  let level_pairs =
    levels
    |> List.fold_left
         (fun (level_above, level_pairs) level ->
           match level_above with
           | None -> (Some level, level_pairs)
           | Some level_above ->
               (Some level, level_pairs @ [ (level_above, level) ]))
         (None, [])
    |> snd
  in
  (* Patch the target *)
  let level_pair_target, level_pairs =
    (List.hd level_pairs, List.tl level_pairs)
  in
  let value_script =
    let level_target, level_spec = level_pair_target in
    apply_target tower.target level_target level_spec
  in
  (* Patch the intermediate levels *)
  List.fold_left
    (fun value_meta level_pair ->
      let level_meta, level_spec = level_pair in
      apply_interm level_meta value_meta level_spec)
    value_script level_pairs
