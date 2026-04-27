module Value = Runtime.Value
open Runtime.Dynamic_Runner.Signature

module Make
    (Interface : INTERFACE)
    (MakeExtern : functor
      (Interp_IL : INTERP_IL)
      (Interp_SL : INTERP_SL)
      -> EXTERN)
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
  (* Recursive instantiations *)

  module rec Extern : EXTERN = struct
    include MakeExtern (Interp_IL) (Interp_SL)
  end

  and Interp_IL : INTERP_IL = struct
    include MakeInterp_IL (Interface) (Extern) ()
  end

  and Interp_SL : INTERP_SL = struct
    include MakeInterp_SL (Interface) (Extern) ()
  end

  (* Initialization *)

  let spec : spec ref = ref Empty
  let mode : mode ref = ref Empty_mode
  let init_mode (mode_ : mode) : unit = mode := mode_

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
    Extern.init_mode !mode

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

  let run_rel (relname : string) (values : Value.t list) : rel_result =
    match !spec with
    | IL _ -> Interp_IL.eval_rel relname values
    | SL _ -> Interp_SL.eval_rel relname values
    | Empty -> assert false

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
