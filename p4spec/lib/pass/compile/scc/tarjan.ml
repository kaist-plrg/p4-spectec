(* Returns SCCs in topological order: if A calls B, B's SCC appears before A's. *)
let tarjan (n : int) (adj : int list array) : int list list =
  let index = Array.make n (-1) in
  let lowlink = Array.make n 0 in
  let on_stack = Array.make n false in
  let counter = ref 0 in
  let stack = ref [] in
  let sccs = ref [] in
  let rec visit v =
    index.(v) <- !counter;
    lowlink.(v) <- !counter;
    incr counter;
    stack := v :: !stack;
    on_stack.(v) <- true;
    List.iter
      (fun w ->
        if index.(w) = -1 then (
          visit w;
          lowlink.(v) <- min lowlink.(v) lowlink.(w))
        else if on_stack.(w) then lowlink.(v) <- min lowlink.(v) index.(w))
      adj.(v);
    if lowlink.(v) = index.(v) then (
      let scc = ref [] in
      let go = ref true in
      while !go do
        match !stack with
        | [] -> assert false
        | w :: rest ->
            stack := rest;
            on_stack.(w) <- false;
            scc := w :: !scc;
            if w = v then go := false
      done;
      sccs := !scc :: !sccs)
  in
  for v = 0 to n - 1 do
    if index.(v) = -1 then visit v
  done;
  List.rev !sccs
