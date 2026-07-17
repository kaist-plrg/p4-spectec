open Lang
module Typ = Runtime.Type
module Typdef = Typ.Typdef
module Naming = Gen.Interface.Naming
open Util.Source

(* Direct dependencies of marshal/unmarshal for a given type *)

let typ_deps (ctx : Ctx.t) (typ : Sl.typ) : Sl.typ list =
  match typ.it with
  | Il.BoolT | Il.NumT _ | Il.TextT | Il.FuncT _ -> []
  | Il.TupleT typs -> typs
  | Il.IterT (typ, _) -> [ typ ]
  | Il.VarT (id, targs) -> (
      let theta = Naming.build_theta ctx id targs in
      match Ctx.find_typdef ctx id with
      | Typdef.Param | Typdef.Defining _ | Typdef.Extern -> []
      | Typdef.Defined (_, deftyp) -> (
          match deftyp.it with
          | Il.PlainT typ ->
              let typ = Typ.Subst.subst_typ theta typ in
              [ typ ]
          | Il.StructT typfields ->
              typfields |> List.map snd |> Typ.Subst.subst_typs theta
          | Il.VariantT typcases ->
              List.concat_map
                (fun (nottyp, _, _) ->
                  let typs = Domain.Mixfix.args nottyp.it in
                  Typ.Subst.subst_typs theta typs)
                typcases))

(* Entry point *)

(* Compute SCCs on the marshal/unmarshal call graph and return groups *)

let compute (ctx : Ctx.t) (typs : Sl.typ list) : Sl.typ list list =
  let n = List.length typs in
  if n = 0 then []
  else
    let typs_arr = Array.of_list typs in
    let name_idx : (string, int) Hashtbl.t = Hashtbl.create (n * 2) in
    Array.iteri
      (fun i typ -> Hashtbl.replace name_idx (Naming.name typ) i)
      typs_arr;
    let adj = Array.make n [] in
    Array.iteri
      (fun i typ ->
        let deps = typ_deps ctx typ in
        let edges : (int, unit) Hashtbl.t = Hashtbl.create 4 in
        List.iter
          (fun dep ->
            let dep_name = Naming.name dep in
            match Hashtbl.find_opt name_idx dep_name with
            | Some j when j <> i -> Hashtbl.replace edges j ()
            | _ -> ())
          deps;
        adj.(i) <- Hashtbl.fold (fun j () acc -> j :: acc) edges [])
      typs_arr;
    let sccs = Tarjan.tarjan n adj in
    List.map (fun scc -> List.map (fun i -> typs_arr.(i)) scc) sccs
