open Util.Source

(* [code] identifies an elaboration diagnostic across runs. *)

type code =
  | Vart_targ_arity_mismatch
  | Call_targ_arity_mismatch
  | Call_arg_arity_mismatch
  | Funarg_signature_mismatch
  | Funarg_name_mismatch
  | Functyp_tparam_arity_mismatch
  | Functyp_param_arity_mismatch
  | Funarg_expected_exp_got_fun
  | Funarg_expected_fun_got_exp
  | Funparam_tparam_not_distinct
  | Builtin_dec_tparam_not_distinct
  | Extern_dec_tparam_not_distinct
  | Dec_tparam_not_distinct
  | Syn_tparam_not_distinct
  | Typ_tparam_mismatch
  | Clause_tparam_mismatch
  | Clause_arg_arity_mismatch
  | Extern_syn_invalid_id
  | Syn_invalid_id
  | Typ_invalid_id
  | Typ_invalid_tparam
  | Extend_non_variant_struct
  | Extend_non_variant_primitive
  | Extend_non_variant_tparam
  | Extend_non_variant_extern
  | Extend_incomplete
  | Variant_mixop_collision
  | Typ_fully_redefined
  | Typ_define_extern
  | Hole_outside_hint
  | Fuse_outside_hint
  | Unparen_outside_hint
  | Latex_outside_hint
  | Var_prem_invalid_metavar
  | Var_prem_type_redefined
  | Var_def_invalid_metavar
  | Var_def_type_redefined
  | Negated_premise_has_outputs
  | Iter_var_premise
  | Iter_otherwise_premise
  | Prem_multiple_otherwise
  | Rule_relation_mismatch
  | Rule_multiple_otherwise
  | Rule_otherwise_with_other_rule
  | Relation_input_hint_empty
  | Relation_input_hint_duplicate_index
  | Relation_input_hint_out_of_bounds
  | Relation_input_hint_non_hole
  | Relation_no_input_hint
  | Table_function_parameter
  | Table_non_bool_return
  | Typ_missing_definition
  | Relation_missing_rules
  | Table_missing_rows
  | Dec_missing_clauses
  | Type_undefined
  | Rule_relation_undefined
  | Relation_undefined
  | Extern_relation_rules
  | Function_declaration_required
  | Function_undefined
  | Table_function_undefined
  | Table_function_required
  | Meta_variable_redefined
  | Type_redeclared
  | Extern_relation_redefined
  | Relation_redefined
  | Rule_group_redefined
  | Relation_otherwise_redefined
  | Function_otherwise_redefined
  | Builtin_function_redefined
  | Extern_function_redefined
  | Function_redefined
  | Table_function_redefined
  | Table_rows_redefined
  | Iteration_dimension_mismatch
  | Empty_iteration_expression
  | Empty_iteration_premise

let string_of_code = function
  | Vart_targ_arity_mismatch -> "vart-targ-arity-mismatch"
  | Call_targ_arity_mismatch -> "call-targ-arity-mismatch"
  | Call_arg_arity_mismatch -> "call-arg-arity-mismatch"
  | Funarg_signature_mismatch -> "funarg-signature-mismatch"
  | Funarg_name_mismatch -> "funarg-name-mismatch"
  | Functyp_tparam_arity_mismatch -> "functyp-tparam-arity-mismatch"
  | Functyp_param_arity_mismatch -> "functyp-param-arity-mismatch"
  | Funarg_expected_exp_got_fun -> "funarg-expected-exp-got-fun"
  | Funarg_expected_fun_got_exp -> "funarg-expected-fun-got-exp"
  | Funparam_tparam_not_distinct -> "funparam-tparam-not-distinct"
  | Builtin_dec_tparam_not_distinct -> "builtin-dec-tparam-not-distinct"
  | Extern_dec_tparam_not_distinct -> "extern-dec-tparam-not-distinct"
  | Dec_tparam_not_distinct -> "dec-tparam-not-distinct"
  | Syn_tparam_not_distinct -> "syn-tparam-not-distinct"
  | Typ_tparam_mismatch -> "typ-tparam-mismatch"
  | Clause_tparam_mismatch -> "clause-tparam-mismatch"
  | Clause_arg_arity_mismatch -> "clause-arg-arity-mismatch"
  | Extern_syn_invalid_id -> "extern-syn-invalid-id"
  | Syn_invalid_id -> "syn-invalid-id"
  | Typ_invalid_id -> "typ-invalid-id"
  | Typ_invalid_tparam -> "typ-invalid-tparam"
  | Extend_non_variant_struct -> "extend-non-variant-struct"
  | Extend_non_variant_primitive -> "extend-non-variant-primitive"
  | Extend_non_variant_tparam -> "extend-non-variant-tparam"
  | Extend_non_variant_extern -> "extend-non-variant-extern"
  | Extend_incomplete -> "extend-incomplete"
  | Variant_mixop_collision -> "variant-mixop-collision"
  | Typ_fully_redefined -> "typ-fully-redefined"
  | Typ_define_extern -> "typ-define-extern"
  | Hole_outside_hint -> "hole-outside-hint"
  | Fuse_outside_hint -> "fuse-outside-hint"
  | Unparen_outside_hint -> "unparen-outside-hint"
  | Latex_outside_hint -> "latex-outside-hint"
  | Var_prem_invalid_metavar -> "var-prem-invalid-metavar"
  | Var_prem_type_redefined -> "var-prem-type-redefined"
  | Var_def_invalid_metavar -> "var-def-invalid-metavar"
  | Var_def_type_redefined -> "var-def-type-redefined"
  | Negated_premise_has_outputs -> "negated-premise-has-outputs"
  | Iter_var_premise -> "iter-var-premise"
  | Iter_otherwise_premise -> "iter-otherwise-premise"
  | Prem_multiple_otherwise -> "prem-multiple-otherwise"
  | Rule_relation_mismatch -> "rule-relation-mismatch"
  | Rule_multiple_otherwise -> "rule-multiple-otherwise"
  | Rule_otherwise_with_other_rule -> "rule-otherwise-with-other-rule"
  | Relation_input_hint_empty -> "relation-input-hint-empty"
  | Relation_input_hint_duplicate_index -> "relation-input-hint-duplicate-index"
  | Relation_input_hint_out_of_bounds -> "relation-input-hint-out-of-bounds"
  | Relation_input_hint_non_hole -> "relation-input-hint-non-hole"
  | Relation_no_input_hint -> "relation-no-input-hint"
  | Table_function_parameter -> "table-function-parameter"
  | Table_non_bool_return -> "table-non-bool-return"
  | Typ_missing_definition -> "typ-missing-definition"
  | Relation_missing_rules -> "relation-missing-rules"
  | Table_missing_rows -> "table-missing-rows"
  | Dec_missing_clauses -> "dec-missing-clauses"
  | Type_undefined -> "type-undefined"
  | Rule_relation_undefined -> "rule-relation-undefined"
  | Relation_undefined -> "relation-undefined"
  | Extern_relation_rules -> "extern-relation-rules"
  | Function_declaration_required -> "function-declaration-required"
  | Function_undefined -> "function-undefined"
  | Table_function_undefined -> "table-function-undefined"
  | Table_function_required -> "table-function-required"
  | Meta_variable_redefined -> "meta-variable-redefined"
  | Type_redeclared -> "type-redeclared"
  | Extern_relation_redefined -> "extern-relation-redefined"
  | Relation_redefined -> "relation-redefined"
  | Rule_group_redefined -> "rule-group-redefined"
  | Relation_otherwise_redefined -> "relation-otherwise-redefined"
  | Function_otherwise_redefined -> "function-otherwise-redefined"
  | Builtin_function_redefined -> "builtin-function-redefined"
  | Extern_function_redefined -> "extern-function-redefined"
  | Function_redefined -> "function-redefined"
  | Table_function_redefined -> "table-function-redefined"
  | Table_rows_redefined -> "table-rows-redefined"
  | Iteration_dimension_mismatch -> "iteration-dimension-mismatch"
  | Empty_iteration_expression -> "empty-iteration-expression"
  | Empty_iteration_premise -> "empty-iteration-premise"

let render_code (c : code) : string = "elab/" ^ string_of_code c

exception ElabError of Diagnostic.t

let error ?code ?detail ?(related = []) (at : region) (msg : string) =
  let related =
    List.map (fun (region, message) -> { Diagnostic.region; message }) related
  in
  raise
    (ElabError
       (Diagnostic.error
          ?code:(Option.map render_code code)
          ?detail ~related ~source:"elab" at msg))

let error_of_failtraces (failtraces : Util.Attempt.failtrace list) =
  raise
    (ElabError
       (Diagnostic.of_failtraces ~source:"elab" ~fallback:"elaboration failed"
          failtraces))

let warn ?code ?detail ?(related = []) (at : region) (msg : string) =
  let related =
    List.map (fun (region, message) -> { Diagnostic.region; message }) related
  in
  Diagnostic.warn
    ?code:(Option.map render_code code)
    ?detail ~related ~source:"elab" at msg

(* Checks *)

let check ?code ?detail ?(related = []) (b : bool) (at : region) (msg : string)
    : unit =
  if not b then error ?code ?detail ~related at msg
