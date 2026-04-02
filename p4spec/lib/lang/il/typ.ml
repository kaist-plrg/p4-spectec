open Ast
open Util.Source

let rec typ_of_param (param : param) : typ =
  match param.it with
  | ExpP typ -> typ
  | DefP (_, tparams, params, typ) ->
      let typs_param = typs_of_params params in
      FuncT (tparams, typs_param, typ) $ param.at

and typs_of_params (params : param list) : typ list =
  List.map typ_of_param params
