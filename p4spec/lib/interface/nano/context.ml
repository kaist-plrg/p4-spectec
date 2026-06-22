module SMap = Map.Make (String)

type t = bool SMap.t list

let context : t ref = ref [ SMap.empty ]

let reset () = context := [ SMap.empty ]

let declare (id : string) (is_type : bool) : unit =
  match !context with
  | [] -> failwith "ill-formed context"
  | m :: l -> context := SMap.add id is_type m :: l

let declare_type id = declare id true
let declare_var id = declare id false

let find_opt (id : string) (ctx : t) : bool option =
  let rec loop = function
    | [] -> None
    | m :: rest -> (
        match SMap.find_opt id m with None -> loop rest | Some k -> Some k)
  in
  loop ctx

let is_typename (id : string) : bool =
  match find_opt id !context with Some true -> true | _ -> false

let push_scope () = context := SMap.empty :: !context

let pop_scope () =
  match !context with
  | [] -> failwith "ill-formed context"
  | [ _ ] -> failwith "pop would produce ill-formed context"
  | _ :: l -> context := l
