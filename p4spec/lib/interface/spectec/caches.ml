open Lang
module VCache = Runtime.Dynamic.Caches.ValueCache
module MCache = Domain.Caches.MixopCache

(* Enable flags *)

let cache_enabled = ref true

(* Boot caches *)

let boot_mixop_cache : Il.value MCache.t = MCache.create ~size:4096
let boot_value_cache : Il.value VCache.t = VCache.create ~size:4096

let boot_value_pingpong_cache : Il.value VCache.t =
  VCache.create ~size:(256 * 1024)

(* Unboot caches *)

let unboot_mixop_cache : Il.mixop VCache.t = VCache.create ~size:4096
let unboot_typ_cache : Il.typ VCache.t = VCache.create ~size:4096
let unboot_value_cache : Il.value VCache.t = VCache.create ~size:4096

let unboot_value_pingpong_cache : Il.value VCache.t =
  VCache.create ~size:(256 * 1024)

(* Toggles *)

let cache_on () = cache_enabled := true

let cache_off () =
  cache_enabled := false;
  MCache.reset boot_mixop_cache;
  VCache.reset boot_value_cache;
  VCache.reset boot_value_pingpong_cache;
  VCache.reset unboot_mixop_cache;
  VCache.reset unboot_typ_cache;
  VCache.reset unboot_value_cache;
  VCache.reset unboot_value_pingpong_cache
