open Domain.Lib
open Lang
open Il
open Runtime.Static
open Error
open Envs

(* Binding occurrences of identifiers, singular or multiple (parallel) *)

module Occ = struct
  type t = Single of Typdim.t | Multi of Typdim.t

  let strip = function Single typ -> typ | Multi typ -> typ
  let to_string t = t |> strip |> Typdim.to_string

  let add_iter (iter : iter) = function
    | Single typ -> Single (Typdim.add_iter iter typ)
    | Multi typ -> Multi (Typdim.add_iter iter typ)
end

(* Environment for identifier bindings *)

module BEnv = struct
  include MakeIdEnv (Occ)

  let singleton id typ = add id (Occ.Single (typ, [])) empty
  let flatten (benv : t) : VEnv.t = map Occ.strip benv

  let describe_binds (benv : t) : string =
    let ids =
      dom benv |> IdSet.elements
      |> List.map (fun (id : Id.t) -> Format.asprintf "`%s`" id.it)
    in
    match ids with
    | [] -> "no variables"
    | [ id ] -> "variable " ^ id
    | _ -> "variables " ^ String.concat ", " ids

  let union (benv_a : t) (benv_b : t) : t =
    let ids = IdSet.union (dom benv_a) (dom benv_b) in
    IdSet.fold
      (fun id benv ->
        let bind_a = find_opt id benv_a in
        let bind_b = find_opt id benv_b in
        match (bind_a, bind_b) with
        | Some bind_a, Some bind_b ->
            let typdim_a = Occ.strip bind_a in
            let typdim_b = Occ.strip bind_b in
            (if not (Typdim.equiv typdim_a typdim_b) then
               let typ_a, _ = typdim_a in
               let typ_b, _ = typdim_b in
               error ~code:Parallel_binding_dimension_mismatch typ_b.at
                 (Format.asprintf
                    "parallel bindings for `%s` have incompatible dimensions: \
                     `%s` and `%s`"
                    id.it (Occ.to_string bind_a) (Occ.to_string bind_b))
                 ~related:[ (typ_a.at, "first bound here") ]
                 ~detail:
                   "A variable can have only one type and iteration dimension \
                    within a binder pattern. These two positions give the \
                    variable different dimensions.");
            add id (Occ.Multi typdim_a) benv
        | Some bind, None | None, Some bind -> add id bind benv
        | None, None ->
            (* [id] comes from the union of both environment domains. *)
            assert false)
      ids empty
end
