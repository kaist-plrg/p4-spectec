module Value = Runtime.Value
open Runtime.Dynamic_Runner.Signature

module Make_rec
    (Interface : INTERFACE)
    (MakeExtern : functor
      (Interp_AL : INTERP_AL)
      (Interp_SL : INTERP_SL)
      (Interp_PL : INTERP_PL)
      -> EXTERN)
    (MakeInterp_AL : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_AL)
    (MakeInterp_SL : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_SL)
    (MakeInterp_PL : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_PL) : RUNNER = struct
  module Interface = Interface

  (* Recursive instantiations *)

  module rec Extern : EXTERN = struct
    include MakeExtern (Interp_AL) (Interp_SL) (Interp_PL)
  end

  and Interp_AL : INTERP_AL = struct
    include MakeInterp_AL (Interface) (Extern) ()
  end

  and Interp_SL : INTERP_SL = struct
    include MakeInterp_SL (Interface) (Extern) ()
  end

  and Interp_PL : INTERP_PL = struct
    include MakeInterp_PL (Interface) (Extern) ()
  end

  (* Shared state *)

  let mode : mode ref = ref Empty_mode
  let init_mode (mode_ : mode) : unit = mode := mode_

  (* Interpreter *)

  module Interp = struct
    let eval_program (relname : string) (includes : string list) (path : string)
        : program_result =
      match !mode with
      | AL_mode -> Interp_AL.eval_program relname includes path
      | SL_mode -> Interp_SL.eval_program relname includes path
      | PL_mode -> Interp_PL.eval_program relname includes path
      | Empty_mode -> assert false

    let eval_rel (relname : string) (values : Value.t list) : rel_result =
      match !mode with
      | AL_mode -> Interp_AL.eval_rel relname values
      | SL_mode -> Interp_SL.eval_rel relname values
      | PL_mode -> Interp_PL.eval_rel relname values
      | Empty_mode -> assert false

    let eval_func (funcname : string) (typs : Typ.t list)
        (values : Value.t list) : func_result =
      match !mode with
      | AL_mode -> Interp_AL.eval_func funcname typs values
      | SL_mode -> Interp_SL.eval_func funcname typs values
      | PL_mode -> Interp_PL.eval_func funcname typs values
      | Empty_mode -> assert false

    (* Cache management *)

    module Cache = struct
      let cache_on () =
        Interp_AL.Cache.cache_on ();
        Interp_SL.Cache.cache_on ();
        Interp_PL.Cache.cache_on ()

      let cache_off () =
        Interp_AL.Cache.cache_off ();
        Interp_SL.Cache.cache_off ();
        Interp_PL.Cache.cache_off ()
    end

    (* Clear the cache *)

    let clear () : unit =
      Interp_AL.clear ();
      Interp_SL.clear ();
      Interp_PL.clear ();
      Extern.clear ()
  end

  (* Initialization *)

  let init ?(cache = true) ?(det = false) ?(guard = false) (spec_ : spec) : unit
      =
    Interface.init spec_;
    (match spec_ with
    | AL spec_al ->
        init_mode AL_mode;
        Interp_AL.init ~cache ~det ~guard spec_al
    | SL spec_sl ->
        init_mode SL_mode;
        Interp_SL.init ~cache ~det ~guard spec_sl
    | PL spec_pl ->
        init_mode PL_mode;
        Interp_PL.init ~cache ~det ~guard spec_pl
    | Empty -> assert false);
    Extern.init_mode !mode

  (* Cache management *)

  let clear () : unit =
    Extern.clear ();
    Interp.clear ()

  module Cache = struct
    let cache_on () =
      Interp_AL.Cache.cache_on ();
      Interp_SL.Cache.cache_on ();
      Interp_PL.Cache.cache_on ()

    let cache_off () =
      Interp_AL.Cache.cache_off ();
      Interp_SL.Cache.cache_off ();
      Interp_PL.Cache.cache_off ()
  end
end

(* Variant for externs that do not depend on the interpreters.
   Because there is no Extern↔Interp circular dependency, no module rec is needed. *)

module Make_nonrec
    (Interface : INTERFACE)
    (MakeExtern : functor () -> EXTERN)
    (MakeInterp_AL : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_AL)
    (MakeInterp_SL : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_SL)
    (MakeInterp_PL : functor
      (Interface : INTERFACE)
      (Extern : EXTERN)
      ()
      -> INTERP_PL) : RUNNER = struct
  module Interface = Interface

  (* Sequential instantiations: Extern is independent of the interpreters *)

  module Extern : EXTERN = MakeExtern ()
  module Interp_AL : INTERP_AL = MakeInterp_AL (Interface) (Extern) ()
  module Interp_SL : INTERP_SL = MakeInterp_SL (Interface) (Extern) ()
  module Interp_PL : INTERP_PL = MakeInterp_PL (Interface) (Extern) ()

  (* Shared state *)

  let mode : mode ref = ref Empty_mode
  let init_mode (mode_ : mode) : unit = mode := mode_

  (* Interpreter *)

  module Interp = struct
    let eval_program (relname : string) (includes : string list) (path : string)
        : program_result =
      match !mode with
      | AL_mode -> Interp_AL.eval_program relname includes path
      | SL_mode -> Interp_SL.eval_program relname includes path
      | PL_mode -> Interp_PL.eval_program relname includes path
      | Empty_mode -> assert false

    let eval_rel (relname : string) (values : Value.t list) : rel_result =
      match !mode with
      | AL_mode -> Interp_AL.eval_rel relname values
      | SL_mode -> Interp_SL.eval_rel relname values
      | PL_mode -> Interp_PL.eval_rel relname values
      | Empty_mode -> assert false

    let eval_func (funcname : string) (typs : Typ.t list)
        (values : Value.t list) : func_result =
      match !mode with
      | AL_mode -> Interp_AL.eval_func funcname typs values
      | SL_mode -> Interp_SL.eval_func funcname typs values
      | PL_mode -> Interp_PL.eval_func funcname typs values
      | Empty_mode -> assert false

    (* Cache management *)

    module Cache = struct
      let cache_on () =
        Interp_AL.Cache.cache_on ();
        Interp_SL.Cache.cache_on ();
        Interp_PL.Cache.cache_on ()

      let cache_off () =
        Interp_AL.Cache.cache_off ();
        Interp_SL.Cache.cache_off ();
        Interp_PL.Cache.cache_off ()
    end

    (* Clear the cache *)

    let clear () : unit =
      Interp_AL.clear ();
      Interp_SL.clear ();
      Interp_PL.clear ();
      Extern.clear ()
  end

  (* Initialization *)

  let init ?(cache = true) ?(det = false) ?(guard = false) (spec_ : spec) : unit
      =
    Interface.init spec_;
    (match spec_ with
    | AL spec_al ->
        init_mode AL_mode;
        Interp_AL.init ~cache ~det ~guard spec_al
    | SL spec_sl ->
        init_mode SL_mode;
        Interp_SL.init ~cache ~det ~guard spec_sl
    | PL spec_pl ->
        init_mode PL_mode;
        Interp_PL.init ~cache ~det ~guard spec_pl
    | Empty -> assert false);
    Extern.init_mode !mode

  (* Cache management *)

  let clear () : unit =
    Extern.clear ();
    Interp.clear ()

  module Cache = struct
    let cache_on () =
      Interp_AL.Cache.cache_on ();
      Interp_SL.Cache.cache_on ();
      Interp_PL.Cache.cache_on ()

    let cache_off () =
      Interp_AL.Cache.cache_off ();
      Interp_SL.Cache.cache_off ();
      Interp_PL.Cache.cache_off ()
  end
end
