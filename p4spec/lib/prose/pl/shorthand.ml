open Ast
open Lang
open Util.Source
module F = Format

type shorthand = instr list -> (instr list * instr list) option

(* Shorthands: applied in order *)

(* Shortens the following sequence of instructions:

   - Check exp_r matches pat;
   - Let exp_l = exp_r ;

   Or,

   - Check exp_r <: typ;
   - Let exp_l = exp_r as typ;

   Into a CheckLetI:
   => Check let exp_l = exp_r *)

let force_let instrs =
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

   - Let exp_opt = exp_r ;
   - Check let (Some exp_l) = exp_opt;

   Into an OptionGetI:
   => Let exp_l = ! exp_r *)

let option_get instrs =
  match instrs with
  | { it = LetI (exp_opt, exp_call); at; _ }
    :: { it = CheckLetI ({ it = OptE (Some exp_l); _ }, exp_r); _ }
    :: instrs_rest
    when Eq.eq_exp exp_opt exp_r ->
      Some ([ OptionGetI (exp_l, exp_call) $ at ], instrs_rest)
  | _ -> None

(* Shortens the following sequence of instructions:

   - Let exp = exp_r ;
   - Check exp matches Some;
   - Let (Some exp_l) = exp ;

   Into an OptionGetI:
   => Let exp_l = ! exp_r *)

let check_option_get instrs =
  match instrs with
  | { it = LetI (exp_opt, exp_call); at; _ }
    :: { it = CheckI (ExpCond { it = MatchE (exp_opt', Il.OptP `Some); _ }); _ }
    :: { it = LetI ({ it = OptE (Some exp_l); _ }, exp_r); _ }
    :: instrs_rest
    when Eq.eq_exp exp_opt exp_opt' && Eq.eq_exp exp_opt' exp_r ->
      Some ([ OptionGetI (exp_l, exp_call) $ at ], instrs_rest)
  | _ -> None

let rec apply_shorthand (shorthand : shorthand) (instrs : instr list) :
    instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match shorthand instrs with
      | Some (shortened_instrs, instrs_rest) ->
          shortened_instrs @ apply_shorthand shorthand instrs_rest
      | None -> instr_h :: apply_shorthand shorthand instrs_t)

let apply_all_shorthands (instrs : instr list) : instr list =
  instrs |> apply_shorthand check_option_get
