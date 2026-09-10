open Util.Source

(* [code] identifies an algorithmic-conversion diagnostic across runs. *)

type code =
  | Free_variable_in_expression
  | Table_row_binding_shape
  | Repeated_table_binding
  | Relation_call_in_otherwise
  | Function_call_in_otherwise
  | Condition_in_otherwise
  | Bind_both_sides_of_equality
  | Non_invertible_binding
  | Parallel_binding_dimension_mismatch
  | Pattern_non_variant_type
  | Pattern_overlap
  | Pattern_incomplete
  | Iteration_missing_loop_variable

let string_of_code = function
  | Free_variable_in_expression -> "free-variable-in-expression"
  | Table_row_binding_shape -> "table-row-binding-shape"
  | Repeated_table_binding -> "repeated-table-binding"
  | Relation_call_in_otherwise -> "relation-call-in-otherwise"
  | Function_call_in_otherwise -> "function-call-in-otherwise"
  | Condition_in_otherwise -> "condition-in-otherwise"
  | Bind_both_sides_of_equality -> "bind-both-sides-of-equality"
  | Non_invertible_binding -> "non-invertible-binding"
  | Parallel_binding_dimension_mismatch -> "parallel-binding-dimension-mismatch"
  | Pattern_non_variant_type -> "pattern-non-variant-type"
  | Pattern_overlap -> "pattern-overlap"
  | Pattern_incomplete -> "pattern-incomplete"
  | Iteration_missing_loop_variable -> "iteration-missing-loop-variable"

let render_code (c : code) : string = "algo/" ^ string_of_code c

exception AlgoError of Diagnostic.t

let error ?code ?detail ?(related = []) (at : region) (msg : string) =
  let related =
    List.map (fun (region, message) -> { Diagnostic.region; message }) related
  in
  raise
    (AlgoError
       (Diagnostic.error
          ?code:(Option.map render_code code)
          ?detail ~related ~source:"algo" at msg))

(* Checks *)

let check ?code ?detail ?(related = []) (b : bool) (at : region) (msg : string)
    : unit =
  if not b then error ?code ?detail ~related at msg
