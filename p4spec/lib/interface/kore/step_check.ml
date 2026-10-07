(* Checking a run step by step against K (krun -check-steps DIR). Each next
   configuration must be one of those the search binary of the kompiled
   definition DIR finds one step on, and the interpreter must take no step
   from the last configuration of a run that ends. A nondeterministic program
   is checked along the run the spec takes, without exploring the others
   (decisions D11 in k-in-p4). *)

module Value = Runtime.Value

type t = {
  dir : string;
  info : Load.info;
  current : string;  (* the configuration given to K *)
  next : string;  (* what K writes *)
  mutable last : Value.t option;
  mutable steps : int;
  mutable branching : int;  (* steps where K allows more than one *)
  mutable failure : string option;
}

let make dir info =
  {
    dir;
    info;
    current = Filename.temp_file "kstep" ".kore";
    next = Filename.temp_file "knext" ".kore";
    last = None;
    steps = 0;
    branching = 0;
    failure = None;
  }

(* The configurations of a disjunction, as the search binary writes them *)
let rec alternatives (p : Ast.pattern) : Ast.pattern list =
  match p with
  | Ast.App ("\\or", _, ps) -> List.concat_map alternatives ps
  | Ast.App ("\\bottom", _, _) -> []
  | Ast.App ("\\equals", _, [ _; p ]) -> [ p ]
  | p -> [ p ]

(* Run DIR/binary on a configuration; the exit code *)
let run_k c binary term depth extra =
  Out_channel.with_open_bin c.current (fun oc ->
      output_string oc (Ast.string_of_pattern (Unparse.pattern_of_term c.info term)));
  if Sys.file_exists c.next then Sys.remove c.next;
  Sys.command
    (Filename.quote_command ~stdin:"/dev/null" ~stderr:"/dev/null"
       (Filename.concat c.dir binary)
       ([ c.current; string_of_int depth; c.next ] @ extra))

(* A new configuration of the run: one step after the last one *)
let step c term =
  (match (c.failure, c.last) with
  | None, Some last ->
      if run_k c "search" last 1 [] <> 0 then
        c.failure <- Some (Printf.sprintf "search failed after %d steps" c.steps)
      else
        let nexts =
          alternatives (Parse.pattern_of_file c.next)
          |> List.map (fun p -> Unparse.normalize c.info p |> Ast.string_of_pattern)
        in
        if List.mem (Unparse.string_of_term c.info term) nexts then (
          c.steps <- c.steps + 1;
          if List.length (List.sort_uniq String.compare nexts) > 1 then
            c.branching <- c.branching + 1)
        else
          c.failure <-
            Some
              (Printf.sprintf "step %d is none of the %d steps K allows" (c.steps + 1)
                 (List.length nexts))
  | _ -> ());
  c.last <- Some term

(* After the run: K takes no step from the last configuration *)
let finish c ~ended =
  (match (c.failure, c.last, ended) with
  | None, Some last, true ->
      if run_k c "interpreter" last 1 [ "--statistics" ] <> 0 then
        c.failure <- Some "the interpreter failed on the last configuration"
      else if
        In_channel.with_open_bin c.next In_channel.input_line <> Some "0"
      then
        c.failure <-
          Some
            (Printf.sprintf "K takes a step from the configuration after %d steps"
               c.steps)
  | _ -> ());
  List.iter (fun f -> if Sys.file_exists f then Sys.remove f) [ c.current; c.next ];
  match c.failure with
  | None ->
      Format.printf "steps check: each of %d steps is one K allows (K allows more \
                     than one at %d)%s\n"
        c.steps c.branching
        (if ended then ", and K takes no step from the last" else "")
  | Some msg -> Format.printf "steps check FAILED: %s\n" msg

(* A handler that sees the configuration of each step: the last argument of
   each call of $run (spec-k/6-entry.watsup) *)
let handler c : (module Inst.Handler.HANDLER) =
  (module struct
    include Inst.Handler.Default

    let on_func_enter (fid : Domain.Lib.FId.t) (values : Value.t list) =
      if String.equal (Domain.Lib.FId.to_string fid) "run" then
        match List.rev values with term :: _ -> step c term | [] -> ()
  end)
