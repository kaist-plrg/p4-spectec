open Ast
open Util.Source
module F = Format

type shorthand = instr list -> (instr list * instr list) option

let force_let instrs =
  match instrs with
  | { it = CheckI (ExpCond { it = MatchE (exp, _); _ }); _ }
    :: { it = LetI (exp_l, exp_r); _ }
    :: instrs_rest
    when Eq.eq_exp exp exp_r ->
      Some ([ CheckLetI (exp_l, exp) $ exp_r.at ], instrs_rest)
  | { it = CheckI (ExpCond { it = SubE (exp, typ); _ }); at; _ }
    :: { it = LetI (exp_l, { it = DownCastE (typ_r, exp_r); _ }); _ }
    :: instrs_rest
    when Eq.eq_exp exp exp_r && Eq.eq_typ typ typ_r ->
      Some ([ CheckLetI (exp_l, exp) $ exp_r.at ], instrs_rest)
  | _ -> None

let rec apply_shorthands (shorthands : shorthand list) (instrs : instr list) :
    instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match List.find_map (fun shorthand -> shorthand instrs) shorthands with
      | Some (short_instrs, instrs_rest) ->
          short_instrs @ apply_shorthands shorthands instrs_rest
      | None -> instr_h :: apply_shorthands shorthands instrs_t)

let apply_all_shorthands (instrs : instr list) : instr list =
  let shorthands : shorthand list = [ force_let ] in
  apply_shorthands shorthands instrs
