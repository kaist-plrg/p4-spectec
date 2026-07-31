(* Re-export the spec sub-modules so callers can write Spec.Func.*, Spec.Pack.*,
   etc. without knowing about the spec_impl/ directory. *)

module Func = Spec_impl.Func
module Rel = Spec_impl.Rel
module Pgm = Spec_impl.Pgm
module Pack = Spec_impl.Pack
module Unpack = Spec_impl.Unpack

(* Bundle of per-SIM spec trampolines. Each SIM gets its own instance so that
   concurrent runners do not share mutable call refs. *)

module type S = sig
  module Func : Func.S
  module Rel : Rel.S
  module Pgm : Pgm.S
end

(* Every application creates fresh mutable refs for the three
   trampoline families, independent of every other application. *)

module Make () : S = struct
  module Func = Func.Make ()
  module Rel = Rel.Make ()
  module Pgm = Pgm.Make ()
end
