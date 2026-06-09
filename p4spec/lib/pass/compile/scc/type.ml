open Lang
open Sl

(* ── Reference collection ── *)

(* Collect type names referenced in a typ, excluding tparams. *)
let rec refs_typ (tparams : string list) (typ : Sl.typ) : string list =
  match typ.it with
  | Il.BoolT | Il.NumT _ | Il.TextT | Il.FuncT _ -> []
  | Il.VarT (id, targs) ->
      let here = if List.mem id.it tparams then [] else [ id.it ] in
      here @ List.concat_map (refs_typ tparams) targs
  | Il.TupleT typs -> List.concat_map (refs_typ tparams) typs
  | Il.IterT (t, _) -> refs_typ tparams t

let refs_deftyp (tparams : string list) (deftyp : Sl.deftyp) : string list =
  match deftyp.it with
  | Il.PlainT t -> refs_typ tparams t
  | Il.StructT typfields ->
      List.concat_map (fun (_, t) -> refs_typ tparams t) typfields
  | Il.VariantT typcases ->
      List.concat_map
        (fun ((nottyp : nottyp), _, _) ->
          List.concat_map (refs_typ tparams) (Domain.Mixfix.args nottyp.it))
        typcases

(* ── Node classification ── *)

let is_typedef (def : def) : bool =
  match def.it with TypD _ | ExternTypD _ -> true | _ -> false

let typedef_id (def : def) : string =
  match def.it with
  | TypD (id, _, _, _) -> id.it
  | ExternTypD (id, _) -> id.it
  | _ -> assert false

let typedef_refs (def : def) : string list =
  match def.it with
  | TypD (_, tparams, deftyp, _) ->
      let tp = List.map (fun (p : tparam) -> p.it) tparams in
      refs_deftyp tp deftyp
  | ExternTypD _ -> []
  | _ -> assert false

(* ── Entry point ── *)

(* Compute SCCs on type definitions and return groups in topological order
   (dependencies first). Each group becomes one Ml.TypeRec in codegen. *)
let compute (spec : spec) : def list list =
  let defs = List.filter is_typedef spec in
  let n = List.length defs in
  if n = 0 then []
  else
    let defs_arr = Array.of_list defs in
    let name_idx : (string, int) Hashtbl.t = Hashtbl.create (n * 2) in
    Array.iteri
      (fun i def -> Hashtbl.replace name_idx (typedef_id def) i)
      defs_arr;
    let adj = Array.make n [] in
    Array.iteri
      (fun i def ->
        let refs = typedef_refs def in
        let edges : (int, unit) Hashtbl.t = Hashtbl.create 4 in
        List.iter
          (fun name ->
            match Hashtbl.find_opt name_idx name with
            | Some j when j <> i -> Hashtbl.replace edges j ()
            | _ -> ())
          refs;
        adj.(i) <- Hashtbl.fold (fun j () acc -> j :: acc) edges [])
      defs_arr;
    let sccs = Tarjan.tarjan n adj in
    List.map (fun scc -> List.map (fun i -> defs_arr.(i)) scc) sccs
