module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature

let ( let* ) = Result.bind

let parse_files (mode : Run.mode) (paths_spec : string list) :
    (Value.t, Pass.error) result =
  match mode with
  | AL_mode ->
      let* spec_al = Pass.algo paths_spec in
      Ok (Ali.Boot.boot_spec spec_al)
  | SL_mode ->
      let* spec_sl = Pass.structure ~final:true paths_spec in
      Ok (Sli.Boot.boot_spec spec_sl)
  | PL_mode -> assert false
  | Empty_mode -> assert false

let parse_string (mode : Run.mode) (_path : string) (str : string) :
    (Value.t, Pass.error) result =
  match mode with
  | AL_mode ->
      let* spec_el = Pass.parse_string str in
      let* spec_il = Pass.elab_spec spec_el in
      let* spec_al = Pass.algo_spec spec_il in
      Ok (Ali.Boot.boot_spec spec_al)
  | SL_mode ->
      let* spec_el = Pass.parse_string str in
      let* spec_il = Pass.elab_spec spec_el in
      let* spec_al = Pass.algo_spec spec_il in
      let* spec_sl = Pass.struct_spec ~final:true spec_al in
      Ok (Sli.Boot.boot_spec spec_sl)
  | PL_mode -> assert false
  | Empty_mode -> assert false
