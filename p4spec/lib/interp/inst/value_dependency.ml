module Value = Runtime.Dynamic_Il.Value
module Dep = Runtime.Testgen_neg.Dep

let make_deriving () =
  (* Value dependency graph and a reader for it *)
  let vdg = ref (Dep.Graph.empty ()) in
  let read () = !vdg in
  (* Instruction coverage measurement handler *)
  let module H : Handler.HANDLER = struct
    include Handler.Default

    let init_spec (_spec : Handler.spec) : unit = Hook.cache_off ()
    let finish () : unit = Hook.cache_on ()

    let on_program (value_program : Value.t) : unit =
      vdg := Dep.Graph.assemble_graph value_program

    let on_value (value : Value.t) : unit =
      Dep.Graph.add_node ~taint:false !vdg value

    let on_value_dependency (value : Value.t) (value_dep : Value.t)
        (label : Dep.Edges.label) : unit =
      Dep.Graph.add_edge !vdg value value_dep label
  end in
  (* Return the handler and the reader *)
  ((module H : Handler.HANDLER), read)

let make_non_deriving () =
  (* Value dependency graph and a reader for it *)
  let vdg = ref (Dep.Graph.empty ()) in
  let read () = !vdg in
  (* Instruction coverage measurement handler *)
  let module H : Handler.HANDLER = struct
    include Handler.Default

    let on_program (value_program : Value.t) : unit =
      vdg := Dep.Graph.assemble_graph value_program
  end in
  (* Return the handler and the reader *)
  ((module H : Handler.HANDLER), read)

let make ~(derive : bool) =
  if derive then make_deriving () else make_non_deriving ()
