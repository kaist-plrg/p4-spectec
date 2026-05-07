open Entry

(* Clock-eviction cache

   A circular array of slots sits beside the hashtable.
   * Each slot carries the key currently occupying it and
   * a reference bit that is set on every hit and
   * cleared by the clock hand on its first pass.

   The hand evicts the first slot whose reference bit is already clear,
   giving every recently-accessed entry at least one full revolution of grace. *)

module Make (Entry : ENTRY) = struct
  module Table = Hashtbl.Make (Entry)

  type slot = { mutable key : Entry.t; mutable ref : bool }

  type 'a t = {
    table : ('a * int) Table.t;
    clock : slot array;
    occ : bool array; (* true iff slot is occupied *)
    capacity : int;
    mutable count : int; (* number of occupied slots *)
    mutable hand : int; (* eviction hand position *)
    mutable fill : int; (* next slot for sequential initial fill *)
  }

  let create ~(size : int) =
    let capacity = max 1 size in
    {
      table = Table.create capacity;
      clock =
        Array.init capacity (fun _ -> { key = Entry.default; ref = false });
      occ = Array.make capacity false;
      capacity;
      count = 0;
      hand = 0;
      fill = 0;
    }

  let size (cache : 'a t) : int = cache.count

  let clear (cache : 'a t) : unit =
    Table.clear cache.table;
    Array.fill cache.occ 0 cache.capacity false;
    cache.count <- 0;
    cache.hand <- 0;
    cache.fill <- 0

  let reset (cache : 'a t) : unit =
    Table.reset cache.table;
    Array.fill cache.occ 0 cache.capacity false;
    cache.count <- 0;
    cache.hand <- 0;
    cache.fill <- 0

  let find (cache : 'a t) (key : Entry.t) : 'a option =
    match Table.find_opt cache.table key with
    | None -> None
    | Some (value, idx) ->
        cache.clock.(idx).ref <- true;
        Some value

  (* Advance the hand until a slot can be evicted; return its index. *)

  let evict (cache : 'a t) : int =
    let capacity = cache.capacity in
    let rec sweep () =
      let idx = cache.hand in
      cache.hand <- (idx + 1) mod capacity;
      if not cache.occ.(idx) then sweep ()
      else if cache.clock.(idx).ref then (
        cache.clock.(idx).ref <- false;
        sweep ())
      else (
        Table.remove cache.table cache.clock.(idx).key;
        cache.occ.(idx) <- false;
        cache.count <- cache.count - 1;
        idx)
    in
    sweep ()

  let add (cache : 'a t) (key : Entry.t) (value : 'a) : unit =
    match Table.find_opt cache.table key with
    | Some (_, idx) ->
        Table.replace cache.table key (value, idx);
        cache.clock.(idx).ref <- true
    | None ->
        let idx =
          if cache.count < cache.capacity then (
            let idx = cache.fill in
            cache.fill <- (cache.fill + 1) mod cache.capacity;
            idx)
          else evict cache
        in
        cache.clock.(idx).key <- key;
        cache.clock.(idx).ref <- true;
        cache.occ.(idx) <- true;
        Table.replace cache.table key (value, idx);
        cache.count <- cache.count + 1
end
