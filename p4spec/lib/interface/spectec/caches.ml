module Il = Lang.Il
module VCache = Runtime.Dynamic.Caches.ValueCache
module MCache = Domain.Caches.MixopCache

(* Cache instance *)

type cache = {
  mutable enabled : bool;
  boot_mixop : Il.value MCache.t;
  boot_value : Il.value VCache.t;
  boot_value_pingpong : Il.value VCache.t;
  unboot_mixop : Il.mixop VCache.t;
  unboot_typ : Il.typ VCache.t;
  unboot_value : Il.value VCache.t;
  unboot_value_pingpong : Il.value VCache.t;
}

(* Boot caches *)

let find_boot_mixop_cache : (Il.mixop -> Il.value option) ref =
  ref (fun _ -> None)

let add_boot_mixop_cache : (Il.mixop -> Il.value -> unit) ref =
  ref (fun _ _ -> ())

let find_boot_value_cache : (Il.value -> Il.value option) ref =
  ref (fun _ -> None)

let add_boot_value_cache : (Il.value -> Il.value -> unit) ref =
  ref (fun _ _ -> ())

let find_boot_value_pingpong_cache : (Il.value -> Il.value option) ref =
  ref (fun _ -> None)

let add_boot_value_pingpong_cache : (Il.value -> Il.value -> unit) ref =
  ref (fun _ _ -> ())

(* Unboot caches *)

let find_unboot_mixop_cache : (Il.value -> Il.mixop option) ref =
  ref (fun _ -> None)

let add_unboot_mixop_cache : (Il.value -> Il.mixop -> unit) ref =
  ref (fun _ _ -> ())

let find_unboot_typ_cache : (Il.value -> Il.typ option) ref =
  ref (fun _ -> None)

let add_unboot_typ_cache : (Il.value -> Il.typ -> unit) ref =
  ref (fun _ _ -> ())

let find_unboot_value_cache : (Il.value -> Il.value option) ref =
  ref (fun _ -> None)

let add_unboot_value_cache : (Il.value -> Il.value -> unit) ref =
  ref (fun _ _ -> ())

let find_unboot_value_pingpong_cache : (Il.value -> Il.value option) ref =
  ref (fun _ -> None)

let add_unboot_value_pingpong_cache : (Il.value -> Il.value -> unit) ref =
  ref (fun _ _ -> ())

(* Setter and unsetter *)

let unset_cache () : unit =
  (* Boot caches *)
  (find_boot_mixop_cache := fun _ -> None);
  (add_boot_mixop_cache := fun _ _ -> ());
  (find_boot_value_cache := fun _ -> None);
  (add_boot_value_cache := fun _ _ -> ());
  (find_boot_value_pingpong_cache := fun _ -> None);
  (add_boot_value_pingpong_cache := fun _ _ -> ());
  (* Unboot caches *)
  (find_unboot_mixop_cache := fun _ -> None);
  (add_unboot_mixop_cache := fun _ _ -> ());
  (find_unboot_typ_cache := fun _ -> None);
  (add_unboot_typ_cache := fun _ _ -> ());
  (find_unboot_value_cache := fun _ -> None);
  (add_unboot_value_cache := fun _ _ -> ());
  (find_unboot_value_pingpong_cache := fun _ -> None);
  add_unboot_value_pingpong_cache := fun _ _ -> ()

let set_cache (cache : cache) : unit =
  (* A no-op if the cache is marked as disabled *)
  (if not cache.enabled then unset_cache ()
   else
     (* Boot caches *)
     find_boot_mixop_cache := fun mixop -> MCache.find cache.boot_mixop mixop);
  (add_boot_mixop_cache :=
     fun mixop value -> MCache.add cache.boot_mixop mixop value);
  (find_boot_value_cache := fun value -> VCache.find cache.boot_value value);
  (add_boot_value_cache :=
     fun value result -> VCache.add cache.boot_value value result);
  (find_boot_value_pingpong_cache :=
     fun value -> VCache.find cache.boot_value_pingpong value);
  (add_boot_value_pingpong_cache :=
     fun value result -> VCache.add cache.boot_value_pingpong value result);
  (* Unboot caches *)
  (find_unboot_mixop_cache :=
     fun value_mixop -> VCache.find cache.unboot_mixop value_mixop);
  (add_unboot_mixop_cache :=
     fun value_mixop mixop -> VCache.add cache.unboot_mixop value_mixop mixop);
  (find_unboot_typ_cache :=
     fun value_typ -> VCache.find cache.unboot_typ value_typ);
  (add_unboot_typ_cache :=
     fun value_typ typ -> VCache.add cache.unboot_typ value_typ typ);
  (find_unboot_value_cache :=
     fun value_value -> VCache.find cache.unboot_value value_value);
  (add_unboot_value_cache :=
     fun value_value value -> VCache.add cache.unboot_value value_value value);
  (find_unboot_value_pingpong_cache :=
     fun value_value -> VCache.find cache.unboot_value_pingpong value_value);
  add_unboot_value_pingpong_cache :=
    fun value_value value ->
      VCache.add cache.unboot_value_pingpong value_value value
