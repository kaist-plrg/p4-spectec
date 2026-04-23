open Lang
module Value = Runtime.Value
open Runtime.Dynamic_Runner.Signature
open Error
open Util.Source

module Make
    (Interface : INTERFACE)
    (Extern : EXTERN)
    (MakeInterp_IL : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_IL)
    (MakeInterp_SL : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_SL) : RUNNER = struct
  module Interp_IL = MakeInterp_IL (Interface) (Extern) ()
  module Interp_SL = MakeInterp_SL (Interface) (Extern) ()

  (* Initialization *)

  let spec : spec ref = ref Empty
  let mode : mode ref = ref Empty_mode
  let init_mode (mode_ : mode) : unit = mode := mode_

  let call_pgm (relname : string) (includes_p4 : string list)
      (filename_p4 : string) : Value.t * Value.t =
    match !mode with
    | IL_mode -> (
        let rel_result_il =
          Interp_IL.eval_program relname includes_p4 filename_p4
        in
        match rel_result_il with
        | Pass [ value_ctx; value_arch ] -> (value_ctx, value_arch)
        | Pass _ -> error no_region "unexpected number of return values"
        | Fail (`Syntax (at, msg) | `Runtime (at, msg)) -> error at msg)
    | SL_mode -> (
        let rel_result_sl =
          Interp_SL.eval_program relname includes_p4 filename_p4
        in
        match rel_result_sl with
        | Pass [ value_ctx; value_arch ] -> (value_ctx, value_arch)
        | Pass _ -> error no_region "unexpected number of return values"
        | Fail (`Syntax (at, msg) | `Runtime (at, msg)) -> error at msg)
    | Empty_mode -> assert false

  let init_call_pgm () = Spec.Pgm.register call_pgm

  let call_rel (relname : string) (values_input : Value.t list) : Value.t list =
    match !mode with
    | IL_mode -> (
        let rel_result_il = Interp_IL.eval_rel relname values_input in
        match rel_result_il with
        | Pass values_output -> values_output
        | Fail (at, msg) -> error at msg)
    | SL_mode -> (
        let rel_result_sl = Interp_SL.eval_rel relname values_input in
        match rel_result_sl with
        | Pass values_output -> values_output
        | Fail (at, msg) -> error at msg)
    | Empty_mode -> assert false

  let init_call_rel () = Spec.Rel.register call_rel

  let call_func (funcname : string) (typs_input : Sl.typ list)
      (values_input : Value.t list) : Value.t =
    match !mode with
    | IL_mode -> (
        let func_result_il =
          Interp_IL.eval_func funcname typs_input values_input
        in
        match func_result_il with
        | Pass value_output -> value_output
        | Fail (at, msg) -> error at msg)
    | SL_mode -> (
        let func_result_sl =
          Interp_SL.eval_func funcname typs_input values_input
        in
        match func_result_sl with
        | Pass value_output -> value_output
        | Fail (at, msg) -> error at msg)
    | Empty_mode -> assert false

  let init_call_func () = Spec.Func.register call_func

  let init ?(cache = true) ?(det = false) (spec_ : spec) : unit =
    Interface.init spec_;
    (match spec_ with
    | IL spec_il ->
        spec := IL spec_il;
        init_mode IL_mode;
        Interp_IL.init ~cache ~det spec_il
    | SL spec_sl ->
        spec := SL spec_sl;
        init_mode SL_mode;
        Interp_SL.init ~cache ~det spec_sl
    | Empty -> assert false);
    init_call_pgm ();
    init_call_rel ();
    init_call_func ()

  (* Relation runner *)

  let run_program (relname : string) (includes : string list)
      (filename : string) : program_result =
    match !spec with
    | IL _ -> Interp_IL.eval_program relname includes filename
    | SL _ -> Interp_SL.eval_program relname includes filename
    | Empty -> assert false

  let run_program_internal (relname : string) (value_program : Value.t) :
      rel_result =
    match !spec with
    | IL _ -> Interp_IL.eval_rel relname [ value_program ]
    | SL _ -> Interp_SL.eval_rel relname [ value_program ]
    | Empty -> assert false

  (* Meta-function runner *)

  let run_func (funcname : string) (typs : Typ.t list) (values : Value.t list) :
      func_result =
    match !spec with
    | IL _ -> Interp_IL.eval_func funcname typs values
    | SL _ -> Interp_SL.eval_func funcname typs values
    | Empty -> assert false

  (* Parsing *)

  let parse_file (includes : string list) (filenames : string list) :
      parse_result =
    Interface.parse_program includes filenames

  let parse_string (filename : string) (str : string) : parse_result =
    Interface.parse_string filename str

  (* Unparsing *)

  let unparse_program (value_program : Value.t) : string =
    Interface.unparse_program value_program
end
