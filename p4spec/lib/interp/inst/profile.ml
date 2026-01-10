open Domain.Lib
module Value = Runtime.Dynamic_Il.Value
open Util

module Stat = struct
  type stat = {
    mutable count : int;
    mutable time_inclusive : float;
    mutable time_exclusive : float;
  }

  type t = (string, stat) Hashtbl.t

  let create () : t = Hashtbl.create 64
  let clear (t : t) : unit = Hashtbl.clear t

  let get (t : t) (id : string) : stat =
    match Hashtbl.find_opt t id with
    | Some stat -> stat
    | None ->
        let stat = { count = 0; time_inclusive = 0.0; time_exclusive = 0.0 } in
        Hashtbl.add t id stat;
        stat

  let collect (t : t) : (string * stat) list =
    Hashtbl.fold (fun id stat stats -> (id, stat) :: stats) t []
    |> List.sort (fun (_, stat_a) (_, stat_b) ->
           Float.compare stat_b.time_inclusive stat_a.time_inclusive)
end

type info = { id : Id.t; time_start : float; mutable time_children : float }
type frame = Rel of info | Func of info

let make () =
  (* Stack of frames *)
  let time_start = ref 0.0 in
  let stack : frame Stack.t = Stack.create () in
  (* Statistics *)
  let stats_rel = Stat.create () in
  let stats_func = Stat.create () in
  (* Profiling handler *)
  let module H : Handler.HANDLER = struct
    include Handler.Default

    let init_spec (_spec : Handler.spec) : unit =
      time_start := Time.now ();
      Stack.clear stack;
      Stat.clear stats_rel;
      Stat.clear stats_func

    let log (stats : (string * Stat.stat) list) : unit =
      Format.printf "  %-40s %8s %12s %12s %12s\n" "Name" "Calls" "Inclusive"
        "Exclusive" "Avg";
      Format.printf "  %s\n" (String.make 90 '-');
      List.iter
        (fun (id, stat) ->
          let avg =
            if stat.Stat.count > 0 then
              stat.Stat.time_inclusive /. float_of_int stat.Stat.count
            else 0.0
          in
          Format.printf "  %-40s %8d %11.4fs %11.4fs %11.6fs\n" id stat.count
            stat.time_inclusive stat.time_exclusive avg)
        stats;
      Format.printf "\n"

    let finish () : unit =
      let time_elapsed = Time.now () -. !time_start in
      let stats_rel = Stat.collect stats_rel in
      let stats_func = Stat.collect stats_func in
      Format.printf "\n=== Profiling Results ===\n\n";
      Format.printf "Total time elapsed: %11.4fs\n\n" time_elapsed;
      if stats_rel <> [] then (
        Format.printf "Relations (sorted by inclusive time):\n";
        log stats_rel);
      if stats_func <> [] then (
        Format.printf "Functions (sorted by inclusive time):\n";
        log stats_func)

    let is_recursive (frame : frame) : bool =
      let is_recursive = ref false in
      Stack.iter
        (fun frame_parent ->
          match (frame, frame_parent) with
          | Rel info, Rel info_parent ->
              if Id.eq info.id info_parent.id then is_recursive := true
          | Func info, Func info_parent ->
              if Id.eq info.id info_parent.id then is_recursive := true
          | _ -> ())
        stack;
      !is_recursive

    let on_exit (id : Id.t) : unit =
      if not (Stack.is_empty stack) then (
        let frame = Stack.pop stack in
        let info = match frame with Rel info | Func info -> info in
        let time_elapsed = Time.now () -. info.time_start in
        let time_exclusive = time_elapsed -. info.time_children in
        let stat = Stat.get stats_rel id.it in
        stat.count <- stat.count + 1;
        if not (is_recursive frame) then
          stat.time_inclusive <- stat.time_inclusive +. time_elapsed;
        stat.time_exclusive <- stat.time_exclusive +. time_exclusive;
        if not (Stack.is_empty stack) then
          let frame_parent = Stack.top stack in
          let info_parent =
            match frame_parent with Rel info | Func info -> info
          in
          info_parent.time_children <- info_parent.time_children +. time_elapsed)

    let on_rel_enter (rid : RId.t) (_values_input : Value.t list) : unit =
      let frame =
        Rel { id = rid; time_start = Time.now (); time_children = 0.0 }
      in
      Stack.push frame stack

    let on_rel_exit (rid : RId.t) : unit = on_exit rid

    let on_func_enter (fid : FId.t) (_values_input : Value.t list) : unit =
      let frame =
        Func { id = fid; time_start = Time.now (); time_children = 0.0 }
      in
      Stack.push frame stack

    let on_func_exit (fid : FId.t) : unit = on_exit fid
  end in
  (* Return the handler *)
  (module H : Handler.HANDLER)
