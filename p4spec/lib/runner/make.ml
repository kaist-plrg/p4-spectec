module Value = Runtime.Value
open Runtime.Dynamic_Runner.Signature

module Make_rec
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
  module Interface = Interface

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

  (* Shared state *)

  let mode : mode ref = ref Empty_mode
  let init_mode (mode_ : mode) : unit = mode := mode_

  (* Interpreter *)

  module Interp = struct
    let eval_program (relname : string) (includes : string list)
        (filename : string) : program_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_program relname includes filename
      | SL_mode -> Interp_SL.eval_program relname includes filename
      | Empty_mode -> assert false

    let eval_rel (relname : string) (values : Value.t list) : rel_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_rel relname values
      | SL_mode -> Interp_SL.eval_rel relname values
      | Empty_mode -> assert false

    let eval_func (funcname : string) (typs : Typ.t list)
        (values : Value.t list) : func_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_func funcname typs values
      | SL_mode -> Interp_SL.eval_func funcname typs values
      | Empty_mode -> assert false

    (* Clear the cache *)

    let clear () : unit =
      Interp_IL.clear ();
      Interp_SL.clear ();
      Extern.clear ()
  end

  (* Initialization *)

  let init ?(cache = true) ?(det = false) ?(guard = false) (spec_ : spec) : unit
      =
    Interface.init spec_;
    (match spec_ with
    | IL spec_il ->
        init_mode IL_mode;
        Interp_IL.init ~cache ~det ~guard spec_il
    | SL spec_sl ->
        init_mode SL_mode;
        Interp_SL.init ~cache ~det ~guard spec_sl
    | Empty -> assert false);
    Extern.init_mode !mode

  (* Clear the cache *)

  let clear () : unit =
    Extern.clear ();
    Interp.clear ()
end

(* Variant for externs that do not depend on the interpreters.
   Because there is no Extern↔Interp circular dependency, no module rec is needed. *)

module Make_nonrec
    (Interface : INTERFACE)
    (MakeExtern : functor () -> EXTERN)
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
  module Interface = Interface

  (* Sequential instantiations: Extern is independent of the interpreters *)

  module Extern : EXTERN = MakeExtern ()
  module Interp_IL : INTERP_IL = MakeInterp_IL (Interface) (Extern) ()
  module Interp_SL : INTERP_SL = MakeInterp_SL (Interface) (Extern) ()

  (* Shared state *)

  let mode : mode ref = ref Empty_mode
  let init_mode (mode_ : mode) : unit = mode := mode_

  (* Interpreter *)

  module Interp = struct
    let eval_program (relname : string) (includes : string list)
        (filename : string) : program_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_program relname includes filename
      | SL_mode -> Interp_SL.eval_program relname includes filename
      | Empty_mode -> assert false

    let eval_rel (relname : string) (values : Value.t list) : rel_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_rel relname values
      | SL_mode -> Interp_SL.eval_rel relname values
      | Empty_mode -> assert false

    let eval_func (funcname : string) (typs : Typ.t list)
        (values : Value.t list) : func_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_func funcname typs values
      | SL_mode -> Interp_SL.eval_func funcname typs values
      | Empty_mode -> assert false

    (* Clear the cache *)

    let clear () : unit =
      Interp_IL.clear ();
      Interp_SL.clear ();
      Extern.clear ()
  end

  (* Initialization *)

  let init ?(cache = true) ?(det = false) ?(guard = false) (spec_ : spec) : unit
      =
    Interface.init spec_;
    (match spec_ with
    | IL spec_il ->
        init_mode IL_mode;
        Interp_IL.init ~cache ~det ~guard spec_il
    | SL spec_sl ->
        init_mode SL_mode;
        Interp_SL.init ~cache ~det ~guard spec_sl
    | Empty -> assert false);
    Extern.init_mode !mode

  (* Clear the cache *)

  let clear () : unit =
    Extern.clear ();
    Interp.clear ()
end
