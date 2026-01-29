open Lang
open Pl
open Util.Source
module F = Format

(* Shorthands *)

type shorthand = instr list -> (instr list * instr list) option

(* Shortens the following sequence of instructions:

   - Check exp_r matches pat
   - Let exp_l = exp_r

   Or,

   - Check exp_r <: typ
   - Let exp_l = exp_r as typ

   Into:

   - Check let exp_l = exp_r *)

let force_let (instrs : instr list) : (instr list * instr list) option =
  match instrs with
  | { it = CheckI (ExpCond { it = MatchE (exp, _); _ }); _ }
    :: { it = LetI (exp_l, exp_r); _ }
    :: instrs_rest
    when Eq.eq_exp exp exp_r ->
      Some ([ CheckLetI (exp_l, exp) $ exp_r.at ], instrs_rest)
  | { it = CheckI (ExpCond { it = SubE (exp, typ); _ }); _ }
    :: { it = LetI (exp_l, { it = DownCastE (typ_r, exp_r); _ }); _ }
    :: instrs_rest
    when Eq.eq_exp exp exp_r && Eq.eq_typ typ typ_r ->
      Some ([ CheckLetI (exp_l, exp) $ exp_r.at ], instrs_rest)
  | _ -> None

(* Shortens the following sequence of instructions:

   - Let exp_opt = exp_r
   - Check let (Some exp_l) = exp_opt

   Into:

   - Let exp_l = ! exp_r *)

let option_get (instrs : instr list) : (instr list * instr list) option =
  match instrs with
  | { it = LetI (exp_opt, exp_call); at; _ }
    :: { it = CheckLetI ({ it = OptE (Some exp_l); _ }, exp_r); _ }
    :: instrs_rest
    when Eq.eq_exp exp_opt exp_r ->
      Some ([ OptionGetI (exp_l, exp_call) $ at ], instrs_rest)
  | _ -> None

(* Shortens the following sequence of instructions:

   - Let exp = exp_r
   - Check exp matches Some
   - Let (Some exp_l) = exp

   Into:

   - Let exp_l = ! exp_r *)

let check_option_get (instrs : instr list) : (instr list * instr list) option =
  match instrs with
  | { it = LetI (exp_l_original, exp_r_original); at; _ }
    :: {
         it = CheckI (ExpCond { it = MatchE (exp_match, Il.OptP `Some); _ });
         _;
       }
    :: { it = LetI ({ it = OptE (Some exp_l_get); _ }, exp_r_get); _ }
    :: instrs_rest
    when Eq.eq_exp exp_l_original exp_match && Eq.eq_exp exp_match exp_r_get ->
      Some ([ OptionGetI (exp_l_get, exp_r_original) $ at ], instrs_rest)
  | _ -> None

(* Shorthand application *)

let rec apply_shorthand (shorthand : shorthand) (instrs : instr list) :
    instr list =
  match instrs with
  | [] -> []
  | { it = BranchI (branch, cond, instrs_branch); at; note } :: instrs_t -> (
      let instrs_branch = apply_shorthand shorthand instrs_branch in
      let instr_h = BranchI (branch, cond, instrs_branch) $$ (at, note) in
      let instrs = instr_h :: instrs_t in
      match shorthand instrs with
      | Some (shortened_instrs, instrs_rest) ->
          shortened_instrs @ apply_shorthand shorthand instrs_rest
      | None -> instr_h :: apply_shorthand shorthand instrs_t)
  | { it = OtherwiseI instrs_otherwise; at; note } :: instrs_t -> (
      let instrs_otherwise = apply_shorthand shorthand instrs_otherwise in
      let instr_h = OtherwiseI instrs_otherwise $$ (at, note) in
      let instrs = instr_h :: instrs_t in
      match shorthand instrs with
      | Some (shortened_instrs, instrs_rest) ->
          shortened_instrs @ apply_shorthand shorthand instrs_rest
      | None -> instr_h :: apply_shorthand shorthand instrs_t)
  | instr_h :: instrs_t -> (
      match shorthand instrs with
      | Some (shortened_instrs, instrs_rest) ->
          shortened_instrs @ apply_shorthand shorthand instrs_rest
      | None -> instr_h :: apply_shorthand shorthand instrs_t)

let apply_check_option_get (instrs : instr list) : instr list =
  instrs |> apply_shorthand check_option_get
