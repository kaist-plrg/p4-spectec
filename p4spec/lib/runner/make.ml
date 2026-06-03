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
      -> INTERP_SL)
    (MakeInterp_ML : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_ML) : RUNNER = struct
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

  and Interp_ML : INTERP_ML = struct
    include MakeInterp_ML (Interface) (Extern) ()
  end

  (* Shared state *)

  let mode : mode ref = ref Empty_mode
  let init_mode (mode_ : mode) : unit = mode := mode_

  (* Interpreter *)

  module Interp = struct
    let eval_program (relname : string) (includes : string list) (path : string)
        : program_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_program relname includes path
      | SL_mode -> Interp_SL.eval_program relname includes path
      | ML_mode -> Interp_ML.eval_program relname includes path
      | Empty_mode -> assert false

    let eval_rel (relname : string) (values : Value.t list) : rel_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_rel relname values
      | SL_mode -> Interp_SL.eval_rel relname values
      | ML_mode -> Interp_ML.eval_rel relname values
      | Empty_mode -> assert false

    let eval_func (funcname : string) (typs : Typ.t list)
        (values : Value.t list) : func_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_func funcname typs values
      | SL_mode -> Interp_SL.eval_func funcname typs values
      | ML_mode -> Interp_ML.eval_func funcname typs values
      | Empty_mode -> assert false

    (* Cache management *)

    module Cache = struct
      let cache_on () =
        Interp_IL.Cache.cache_on ();
        Interp_SL.Cache.cache_on ();
        Interp_ML.Cache.cache_on ()

      let cache_off () =
        Interp_IL.Cache.cache_off ();
        Interp_SL.Cache.cache_off ();
        Interp_ML.Cache.cache_off ()
    end

    (* Clear the cache *)

    let clear () : unit =
      Interp_IL.clear ();
      Interp_SL.clear ();
      Interp_ML.clear ();
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
    | ML ->
        init_mode ML_mode;
        Interp_ML.init ~cache ~det ~guard ()
    | Empty -> assert false);
    Extern.init_mode !mode

  (* Cache management *)

  let clear () : unit =
    Extern.clear ();
    Interp.clear ()

  module Cache = struct
    let cache_on () =
      Interp_IL.Cache.cache_on ();
      Interp_SL.Cache.cache_on ();
      Interp_ML.Cache.cache_on ()

    let cache_off () =
      Interp_IL.Cache.cache_off ();
      Interp_SL.Cache.cache_off ();
      Interp_ML.Cache.cache_off ()
  end
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
      -> INTERP_SL)
    (MakeInterp_ML : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_ML) : RUNNER = struct
  module Interface = Interface

  (* Sequential instantiations: Extern is independent of the interpreters *)

  module Extern : EXTERN = MakeExtern ()
  module Interp_IL : INTERP_IL = MakeInterp_IL (Interface) (Extern) ()
  module Interp_SL : INTERP_SL = MakeInterp_SL (Interface) (Extern) ()
  module Interp_ML : INTERP_ML = MakeInterp_ML (Interface) (Extern) ()

  (* Shared state *)

  let mode : mode ref = ref Empty_mode
  let init_mode (mode_ : mode) : unit = mode := mode_

  (* Interpreter *)

  module Interp = struct
    let eval_program (relname : string) (includes : string list) (path : string)
        : program_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_program relname includes path
      | SL_mode -> Interp_SL.eval_program relname includes path
      | ML_mode -> Interp_ML.eval_program relname includes path
      | Empty_mode -> assert false

    let eval_rel (relname : string) (values : Value.t list) : rel_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_rel relname values
      | SL_mode -> Interp_SL.eval_rel relname values
      | ML_mode -> Interp_ML.eval_rel relname values
      | Empty_mode -> assert false

    let eval_func (funcname : string) (typs : Typ.t list)
        (values : Value.t list) : func_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_func funcname typs values
      | SL_mode -> Interp_SL.eval_func funcname typs values
      | ML_mode -> Interp_ML.eval_func funcname typs values
      | Empty_mode -> assert false

    (* Cache management *)

    module Cache = struct
      let cache_on () =
        Interp_IL.Cache.cache_on ();
        Interp_SL.Cache.cache_on ();
        Interp_ML.Cache.cache_on ()

      let cache_off () =
        Interp_IL.Cache.cache_off ();
        Interp_SL.Cache.cache_off ();
        Interp_ML.Cache.cache_off ()
    end

    (* Clear the cache *)

    let clear () : unit =
      Interp_IL.clear ();
      Interp_SL.clear ();
      Interp_ML.clear ();
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
    | ML ->
        init_mode ML_mode;
        Interp_ML.init ~cache ~det ~guard ()
    | Empty -> assert false);
    Extern.init_mode !mode

  (* Cache management *)

  let clear () : unit =
    Extern.clear ();
    Interp.clear ()

  module Cache = struct
    let cache_on () =
      Interp_IL.Cache.cache_on ();
      Interp_SL.Cache.cache_on ();
      Interp_ML.Cache.cache_on ()

    let cache_off () =
      Interp_IL.Cache.cache_off ();
      Interp_SL.Cache.cache_off ();
      Interp_ML.Cache.cache_off ()
  end
end
