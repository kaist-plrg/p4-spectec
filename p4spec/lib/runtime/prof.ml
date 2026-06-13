(* Env-gated exclusive-time profiler for compiled funcs/relations.

   The OCaml backend, when generated with SPEC_PROF_GEN=1, wraps every
   dispatcher (`f__<name>` / `r__<name>`) in [Prof.wrap "<name>" (fun _ -> ..)].
   At runtime, set SPEC_PROF=1 to record per-name inclusive/exclusive wall time;
   a table sorted by exclusive time is dumped at exit (to stderr, or to the file
   named by SPEC_PROF_OUT). When SPEC_PROF is unset, [wrap] just calls the
   thunk. *)

module Time = Util.Time

let enabled =
  match Sys.getenv_opt "SPEC_PROF" with Some ("1" | "true") -> true | _ -> false

type stat = {
  mutable count : int;
  mutable incl : float; (* inclusive, recursion not double-counted *)
  mutable excl : float; (* exclusive: own time minus children *)
  mutable depth : int; (* active invocations, for recursion detection *)
}

type frame = { stat : stat; start : float; mutable children : float }

let tbl : (string, stat) Hashtbl.t = Hashtbl.create 4096
let stack : frame list ref = ref []

let get (name : string) : stat =
  match Hashtbl.find_opt tbl name with
  | Some s -> s
  | None ->
      let s = { count = 0; incl = 0.; excl = 0.; depth = 0 } in
      Hashtbl.add tbl name s;
      s

let wrap (name : string) (f : unit -> 'a) : 'a =
  if not enabled then f ()
  else begin
    let s = get name in
    let recursive = s.depth > 0 in
    s.depth <- s.depth + 1;
    let fr = { stat = s; start = Time.now (); children = 0. } in
    stack := fr :: !stack;
    let finish () =
      let elapsed = Time.now () -. fr.start in
      (match !stack with _ :: rest -> stack := rest | [] -> ());
      s.depth <- s.depth - 1;
      s.count <- s.count + 1;
      if not recursive then s.incl <- s.incl +. elapsed;
      s.excl <- s.excl +. (elapsed -. fr.children);
      match !stack with
      | parent :: _ -> parent.children <- parent.children +. elapsed
      | [] -> ()
    in
    match f () with
    | r ->
        finish ();
        r
    | exception e ->
        finish ();
        raise e
  end

let dump () : unit =
  if not enabled then ()
  else begin
    let rows =
      Hashtbl.fold (fun name s acc -> (name, s) :: acc) tbl []
      |> List.sort (fun (_, a) (_, b) -> Float.compare b.excl a.excl)
    in
    let oc =
      match Sys.getenv_opt "SPEC_PROF_OUT" with
      | Some path when path <> "" -> open_out path
      | _ -> stderr
    in
    Printf.fprintf oc "%-56s %8s %12s %12s %12s\n" "name" "calls" "excl_ms"
      "incl_ms" "us/call";
    List.iter
      (fun (name, s) ->
        let per =
          if s.count > 0 then s.excl *. 1e6 /. float_of_int s.count else 0.
        in
        Printf.fprintf oc "%-56s %8d %12.3f %12.3f %12.3f\n" name s.count
          (s.excl *. 1e3) (s.incl *. 1e3) per)
      rows;
    flush oc;
    if oc != stderr then close_out oc
  end

let () = if enabled then at_exit dump
