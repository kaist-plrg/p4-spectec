open Error
open Util.Source

(* Helpers *)

let wrap_value (value : Il.Ast.value') (typ : Il.Ast.typ') : Il.Ast.value =
  Il.Ast.(value $$$ { vid = -1; typ })

(* Initialization *)

let init (spec : Sl.Ast.spec) (includes_p4 : string list) (filename_p4 : string)
    : Sl.Ast.value * Sl.Ast.value =
  match
    Interp_sl.Run.run_program spec "V1Model_init" includes_p4 filename_p4 []
  with
  | Pass ([ value_ctx; value_sto ], _, _, _) -> (value_ctx, value_sto)
  | Pass (_, at, _, _) ->
      error_no_region "V1Model_init should return a context and a store"
  | Fail (at, msg, _) | IllFormed (at, msg, _) -> error at msg

let parse (spec : Sl.Ast.spec) (filename_p4 : string) (value_ctx : Sl.Ast.value)
    (value_sto : Sl.Ast.value) : Sl.Ast.value * Sl.Ast.value * Sl.Ast.value =
  match
    Interp_sl.Run.run spec "V1Model_parser" filename_p4 [ value_ctx; value_sto ]
  with
  | Pass ([ value_result; value_ctx; value_sto ], _, _, _) ->
      (value_result, value_ctx, value_sto)
  | Pass (_, at, _, _) ->
      error_no_region
        "V1Model_parser should return a result, context, and a store"
  | Fail (at, msg, _) | IllFormed (at, msg, _) -> error at msg

let run (spec : Sl.Ast.spec) (includes_p4 : string list) (filename_p4 : string)
    =
  let value_ctx, value_sto = init spec includes_p4 filename_p4 in
  print_endline "Initial Context:";
  Il.Print.string_of_value value_ctx |> print_endline;
  print_endline "Initial Store:";
  Il.Print.string_of_value value_sto |> print_endline;
  let _value_result, _value_ctx, _value_sto =
    parse spec filename_p4 value_ctx value_sto
  in
  ()
