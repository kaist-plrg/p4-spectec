open Xl
open El.Ast
open Runtime_static
open Util.Source

let rec sub_plaintyp (tdenv : Envs.TDEnv.t) (plaintyp_a : plaintyp)
    (plaintyp_b : plaintyp) : bool =
  Equiv.equiv_plaintyp tdenv plaintyp_a plaintyp_b
  || sub_plaintyp' tdenv plaintyp_a plaintyp_b

and sub_plaintyp' (tdenv : Envs.TDEnv.t) (plaintyp_a : plaintyp)
    (plaintyp_b : plaintyp) : bool =
  let plaintyp_a = Plaintyp.expand_plaintyp tdenv plaintyp_a in
  let plaintyp_b = Plaintyp.expand_plaintyp tdenv plaintyp_b in
  match (plaintyp_a.it, plaintyp_b.it) with
  | NumT numtyp_a, NumT numtyp_b -> Num.sub numtyp_a numtyp_b
  | VarT _, VarT _ -> (
      let kind_a = Plaintyp.kind_plaintyp tdenv plaintyp_a in
      let kind_b = Plaintyp.kind_plaintyp tdenv plaintyp_b in
      match (kind_a, kind_b) with
      | Variant typcases_a, Variant typcases_b ->
          let nottyps_a = List.map fst typcases_a |> List.map fst in
          let nottyps_b = List.map fst typcases_b |> List.map fst in
          List.for_all
            (fun nottyp_a ->
              List.exists (Equiv.equiv_nottyp tdenv nottyp_a) nottyps_b)
            nottyps_a
      | _ -> false)
  | ParenT plaintyp_a, _ -> sub_plaintyp tdenv plaintyp_a plaintyp_b
  | _, ParenT plaintyp_b -> sub_plaintyp tdenv plaintyp_a plaintyp_b
  | TupleT plaintyps_a, TupleT plaintyps_b ->
      List.length plaintyps_a = List.length plaintyps_b
      && List.for_all2 (sub_plaintyp tdenv) plaintyps_a plaintyps_b
  | IterT (plaintyp_a, iter_a), IterT (plaintyp_b, iter_b) when iter_a = iter_b
    ->
      sub_plaintyp tdenv plaintyp_a plaintyp_b
  | IterT (plaintyp_a, Opt), IterT (plaintyp_b, List) ->
      sub_plaintyp tdenv plaintyp_a plaintyp_b
  | _, IterT (plaintyp_b, Opt) -> sub_plaintyp tdenv plaintyp_a plaintyp_b
  | _, IterT (plaintyp_b, List) -> sub_plaintyp tdenv plaintyp_a plaintyp_b
  | _ -> false
