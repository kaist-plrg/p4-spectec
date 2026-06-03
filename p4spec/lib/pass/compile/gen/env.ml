open Lang

(* ===== Variable map ===== *)

module VarKey = struct
  type t = string * Il.iter list

  let compare (id_a, iters_a) (id_b, iters_b) =
    let c = String.compare id_a id_b in
    if c <> 0 then c else compare iters_a iters_b
end

module VarMap = Map.Make (VarKey)

(* ===== Environment =====

   var_map maps (id_str * iter_context) → OCaml identifier.
   Key (id, [])         : scalar binding (inside an iter lambda, the current element)
   Key (id, [Il.List])  : outer list binding (before entering a List iter lambda)
   Key (id, [Il.Opt])   : outer option binding (before entering an Opt iter lambda)

   var_default is the fallback when a key is not in var_map; defaults to Ml.Names.var.
   make_func_body_env wraps var_default with sanitize_ocaml_id to handle Fresh ids.

   ctor_table maps (type_id ^ "\x00" ^ mixop_str) → ctor_name.
   ctor_rev_table maps (type_id ^ "\x00" ^ ctor_name) → mixop.
*)

type env = { var_map : string VarMap.t }

let make_env () : env = { var_map = VarMap.empty }

(* Lookup the OCaml name for (id_str, iters); fail if not found (indicates missing binding site). *)
let lookup_var (env : env) (id_str : string) (iters : Il.iter list) : string =
  match VarMap.find_opt (id_str, iters) env.var_map with
  | Some name -> name
  | None ->
      failwith
        (Printf.sprintf "lookup_var: unbound variable (%s, [%s])" id_str
           (String.concat ";" (List.map Il.Print.string_of_iter iters)))

(* Bind (id_str, iters) → ocaml_name in the env. *)
let with_var (env : env) (id_str : string) (iters : Il.iter list)
    (ocaml_name : string) : env =
  { var_map = VarMap.add (id_str, iters) ocaml_name env.var_map }
