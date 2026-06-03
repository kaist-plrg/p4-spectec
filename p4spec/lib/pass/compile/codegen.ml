open Lang
open Util.Source

let compile_spec (path_out : string) (spec : Sl.spec) =
  let spec_sl, dispatch_table = Mono.monomorphize spec in
  (* Add non-poly builtins/externs to dispatch_table *)
  List.iter
    (fun def ->
      match def.it with
      | Sl.BuiltinDecD (id, [], _, _, _)
        when not (Hashtbl.mem dispatch_table id.it) ->
          Hashtbl.replace dispatch_table id.it
            {
              Mono.original_name = id.it;
              concrete_targs = [];
              kind = Mono.Builtin;
            }
      | Sl.ExternDecD (id, [], _, _, _)
        when not (Hashtbl.mem dispatch_table id.it) ->
          Hashtbl.replace dispatch_table id.it
            {
              Mono.original_name = id.it;
              concrete_targs = [];
              kind = Mono.Extern;
            }
      | _ -> ())
    spec_sl;
  let _env = Gen.Env.make_env () in
  let type_defs = Gen.Type.compile_defs spec_sl in
  let ml_file : Ml.Ast.file = [ Ml.Ast.TypeRec type_defs ] in
  let out_str = Ml.Print.print_file ml_file in
  let oc = open_out path_out in
  output_string oc out_str;
  close_out oc
