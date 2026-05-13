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
    | CaseV (mixop, values_fields) ->
        let exps_fields = List.map value_as_exp values_fields in
        Il.CaseE (mixop, exps_fields)
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

(* Patch *)

(* Create a synthetic "main" meta-function calling [rel] on [value] *)
(* def main() : text = "pass" -- let x = [value] -- [rel]: x *)

let apply_il (value_spec : Value.t) (rel : string) (value_program : Value.t) :
    Value.t =
  let defs = Interface.SpecTec_IL.unboot_scriptIL value_spec in
  let mixop_rel, inputs_rel =
    List.find_map
      (fun (def : Il.def) ->
        match def.it with
        | RelD (id, nottyp, inputs, _, _, _) when id.it = rel ->
            let mixop, _ = nottyp.it in
            Some (mixop, inputs)
        | _ -> None)
      defs
    |> function
    | Some (mixop_rel, inputs_rel) -> (mixop_rel, inputs_rel)
    | None ->
        error no_region
          (Format.asprintf "relation %s not found in the spec" rel)
  in
  check
    (List.length inputs_rel = 1)
    no_region
    (Format.asprintf "relation %s must have exactly one input" rel);
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
          let exp_value = value_as_exp value_program in
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
            let notexp = (mixop_rel, exps) in
            Il.RulePr (rel $ no_region, notexp, inputs_rel) $ no_region
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

let apply_sl (value_spec : Value.t) (rel : string) (value_program : Value.t) :
    Value.t =
  let defs = Interface.SpecTec_SL.unboot_scriptSL value_spec in
  let mixop_rel, inputs_rel =
    List.find_map
      (fun (def : Sl.def) ->
        match def.it with
        | RelD (id, (nottyp, inputs), _, _, _, _) when id.it = rel ->
            let mixop, _ = nottyp.it in
            Some (mixop, inputs)
        | _ -> None)
      defs
    |> function
    | Some (mixop_rel, inputs_rel) -> (mixop_rel, inputs_rel)
    | None ->
        error no_region
          (Format.asprintf "relation %s not found in the spec" rel)
  in
  check
    (List.length inputs_rel = 1)
    no_region
    (Format.asprintf "relation %s must have exactly one input" rel);
  let def =
    let id = "main" $ no_region in
    let tparams = [] in
    let params = [] in
    let typ_ret = Il.TextT $ no_region in
    let block =
      let exp_bind =
        Il.VarE ("x" $ no_region) $$ (no_region, Il.VarT ("x" $ no_region, []))
      in
      let exp_value = value_as_exp value_program in
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
        let notexp = (mixop_rel, exps) in
        Sl.(
          RuleI (rel $ no_region, notexp, inputs_rel, [], [ instr_return ])
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

let apply ~(mode : Run.mode) (value_spec : Value.t) (rel : string)
    (value_program : Value.t) : Value.t =
  match mode with
  | IL_mode -> apply_il value_spec rel value_program
  | SL_mode -> apply_sl value_spec rel value_program
  | Empty_mode -> assert false

(* Parsing a spec as a meta-value *)

let parse_spec ~(mode : Run.mode) (filenames_spec : string list) : Value.t =
  let parse_program =
    match mode with
    | IL_mode -> Interface.SpecTec_IL.parse_program
    | SL_mode -> Interface.SpecTec_SL.parse_program
    | Empty_mode -> assert false
  in
  match parse_program [] filenames_spec with
  | Run.Pass value_spec -> value_spec
  | Run.Fail (`Syntax (at, msg)) -> error at msg

(* Parsing a P4 program as a meta-value *)

let parse_p4_program (includes_p4 : string list) (filename_p4 : string) :
    Value.t =
  match Interface.P4.parse_program includes_p4 [ filename_p4 ] with
  | Run.Pass value_p4 -> value_p4
  | Run.Fail (`Syntax (at, msg)) -> error at msg

(* Patch - P4 |> P4 spec *)

let apply_p4_on_p4_spec ~(mode : Run.mode) (filenames_p4_spec : string list)
    (rel_p4 : string) (includes_p4 : string list) (filename_p4 : string) :
    Value.t =
  (* Parse the P4 spec as a meta-value *)
  let value_p4_spec = parse_spec ~mode filenames_p4_spec in
  (* Parse the P4 program as a meta-value, and wrap it again as an input to the boot spec *)
  let value_p4 = parse_p4_program includes_p4 filename_p4 in
  (* Create a synthetic "main" meta-function calling [rel_p4] on [value_p4] *)
  apply ~mode value_p4_spec rel_p4 value_p4

(* Patch on N - P4 |> P4 spec |> [ SpecTec spec * N ] |> SpecTec spec |> OCaml *)

let apply_n_p4 ~(depth : int) ~(mode : Run.mode) (filenames_spec : string list)
    (rel : string) (filenames_spec_p4 : string list) (rel_p4 : string)
    (includes_p4 : string list) (filename_p4 : string) : Value.t =
  (* Bundle the P4 spec and its input P4 program as a single meta-value *)
  let value_script =
    apply_p4_on_p4_spec ~mode filenames_spec_p4 rel_p4 includes_p4 filename_p4
  in
  List.fold_left
    (fun value_script _ ->
      (* Parse the SpecTec spec as a meta-value *)
      let value_spec = parse_spec ~mode filenames_spec in
      (* Create a synthetic "main" meta-function calling [rel] on [value_script] *)
      apply ~mode value_spec rel value_script)
    value_script (List.init depth Fun.id)

(* Patch - SpecTec |> SpecTec spec *)

let apply_spectec_on_spectec_spec ~(mode : Run.mode)
    (filenames_spec_pgm : string list) (rel_pgm : string)
    (filename_pgm : string) : Value.t =
  (* Parse the SpecTec spec as a meta-value *)
  let value_pgm_spec = parse_spec ~mode filenames_spec_pgm in
  (* Parse the SpecTec program as a meta-value, and wrap it again as an input to the boot spec *)
  let value_pgm = parse_spec ~mode [ filename_pgm ] in
  (* Create a synthetic "main" meta-function calling [rel_pgm] on [value_pgm] *)
  apply ~mode value_pgm_spec rel_pgm value_pgm

(* Patch on N - SpecTec pgm |> [ SpecTec spec * N ] |> SpecTec spec |> OCaml *)

let apply_n_spectec ~(depth : int) ~(mode : Run.mode)
    (filenames_spec : string list) (rel : string)
    (filenames_spec_pgm : string list) (rel_pgm : string)
    (filename_pgm : string) : Value.t =
  (* Bundle the SpecTec spec and its input SpecTec program as a single meta-value *)
  let value_script =
    apply_spectec_on_spectec_spec ~mode filenames_spec_pgm rel_pgm filename_pgm
  in
  List.fold_left
    (fun value_script _ ->
      (* Parse the SpecTec spec as a meta-value *)
      let value_spec = parse_spec ~mode filenames_spec in
      (* Create a synthetic "main" meta-function calling [rel] on [value] *)
      apply ~mode value_spec rel value_script)
    value_script (List.init depth Fun.id)
