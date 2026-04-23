open Domain
open Lang
open Il
module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature
open Error
open Util.Source

let rec value_as_exp (value : value) : exp =
  let at_value = value.at in
  let typ_value = value.note.typ in
  let exp =
    match value.it with
    | BoolV b -> BoolE b
    | NumV n -> NumE n
    | TextV t -> TextE t
    | StructV valuefields ->
        let expfields =
          List.map
            (fun (atom_field, value_field) ->
              let exp_field = value_as_exp value_field in
              (atom_field, exp_field))
            valuefields
        in
        StrE expfields
    | CaseV (mixop, values_fields) ->
        let exps_fields = List.map value_as_exp values_fields in
        CaseE (mixop, exps_fields)
    | TupleV values ->
        let exps = List.map value_as_exp values in
        TupleE exps
    | OptV value_opt ->
        let exp_opt = Option.map value_as_exp value_opt in
        OptE exp_opt
    | ListV values ->
        let exps = List.map value_as_exp values in
        ListE exps
    | _ ->
        error at_value
          "cannot convert a function or extern value to an expression"
  in
  exp $$ (at_value, typ_value)

let apply (filenames_spectec_p4 : string list) (rel_p4 : string)
    (includes_p4 : string list) (filename_p4 : string) : Value.t =
  (* Parse the P4 spec as a meta-value *)
  let value_spectec_p4 =
    match Interface.SpecTec.parse_program [] filenames_spectec_p4 with
    | Run.Pass value_spectec -> value_spectec
    | Run.Fail (`Syntax (at, msg)) -> error at msg
  in
  (* Parse the P4 program as a meta-value, and wrap it again as an input to the boot spec *)
  let value_p4 =
    match Interface.P4.parse_program includes_p4 [ filename_p4 ] with
    | Run.Pass value_p4 -> value_p4
    | Run.Fail (`Syntax (at, msg)) -> error at msg
  in
  (* Create a synthetic "main" meta-function calling [rel_p4] on [value_p4] *)
  (* def main() : text = "pass" -- let x = [value_p4] -- [rel_p4]: x *)
  let defs_il = Interface.SpecTec.unboot_spec value_spectec_p4 in
  let mixop_rel, inputs_rel =
    List.find_map
      (fun def_il ->
        match def_il.it with
        | RelD (id, nottyp, inputs, _, _, _) when id.it = rel_p4 ->
            let mixop, _ = nottyp.it in
            Some (mixop, inputs)
        | _ -> None)
      defs_il
    |> function
    | Some (mixop_rel, inputs_rel) -> (mixop_rel, inputs_rel)
    | None ->
        error no_region
          (Format.asprintf "relation %s not found in the spec" rel_p4)
  in
  check
    (List.length inputs_rel = 1)
    no_region
    (Format.asprintf "relation %s must have exactly one input" rel_p4);
  let def_il =
    let id_il = "main" $ no_region in
    let tparams_il = [] in
    let params_il = [] in
    let typ_ret_il = TextT $ no_region in
    let clauses_il =
      let clause_il =
        let args_il = [] in
        let exp_output_il = TextE "pass" $$ (no_region, TextT) in
        let prems_il =
          let exp_bind =
            VarE ("x" $ no_region) $$ (no_region, VarT ("x" $ no_region, []))
          in
          let exp_value = value_as_exp value_p4 in
          let prem_bind_il = LetPr (exp_bind, exp_value) $ no_region in
          let exps_output =
            List.init
              (Mixop.arity mixop_rel - List.length inputs_rel)
              (fun idx ->
                VarE ("y_" ^ string_of_int idx $ no_region)
                $$ (no_region, VarT ("y_" ^ string_of_int idx $ no_region, [])))
          in
          let prem_call_il =
            let exps =
              Hints.Input.combine inputs_rel [ exp_bind ] exps_output
            in
            let notexp_il = (mixop_rel, exps) in
            RulePr (rel_p4 $ no_region, notexp_il, inputs_rel) $ no_region
          in
          [ prem_bind_il; prem_call_il ]
        in
        (args_il, exp_output_il, prems_il) $ no_region
      in
      [ clause_il ]
    in
    let elseclause_opt_il = None in
    let hints_il = [] in
    FuncDecD
      ( id_il,
        tparams_il,
        params_il,
        typ_ret_il,
        clauses_il,
        elseclause_opt_il,
        hints_il )
    $ no_region
  in
  Il.Print.string_of_def def_il |> print_endline;
  let value_spectec_p4 = defs_il @ [ def_il ] |> Interface.SpecTec.boot_spec in
  value_spectec_p4
