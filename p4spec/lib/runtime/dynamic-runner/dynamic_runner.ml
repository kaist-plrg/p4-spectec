include Dynamic
module Signature = Signature

(* An empty extern module, used to represent
   the absence of an extern module *)

module Cache_Empty : Signature.CACHE = struct
  let cache_on () = ()
  let cache_off () = ()
end

module Extern_Empty : Signature.EXTERN = struct
  module Cache = Cache_Empty

  let eval_extern_rel _ _ = failwith "unimplemented: eval_extern_rel"
  let eval_extern_func _ _ _ = failwith "unimplemented: eval_extern_func"
  let checkpoint () = 0
  let seff _ _ = false
  let clear () = ()
  let init_mode _ = ()

  let init ~cache ~det ~guard _ =
    ignore cache;
    ignore det;
    ignore guard
end
