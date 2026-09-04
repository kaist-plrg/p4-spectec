open Lang
open Il
module Value = Runtime.Value
open Util.Source

let error = Error.error

let id_of_name (value : value) : string =
  Value.Get.mtch value
    [
      ("_ID text", fun values -> values |> Value.Get.nth 0 |> Value.Get.text);
      ("APPLY", fun _ -> "apply");
      ("KEY", fun _ -> "key");
      ("ACTIONS", fun _ -> "actions");
      ("STATE", fun _ -> "state");
      ("_TID text", fun values -> values |> Value.Get.nth 0 |> Value.Get.text);
      ("LIST", fun _ -> "list");
    ]
    (fun _ -> error no_region "@id_of_name: unexpected value")
