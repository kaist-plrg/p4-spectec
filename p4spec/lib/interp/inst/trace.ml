open Domain.Lib
open Lang
module Value = Runtime.Value

(* Verbosity level *)

type level = Simple | Full

(* String utilities *)

let normalize_whitespace (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let in_space = ref false in
  String.iter
    (fun c ->
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then (
        if not !in_space then Buffer.add_char buf ' ';
        in_space := true)
      else (
        Buffer.add_char buf c;
        in_space := false))
    s;
  Buffer.contents buf

let summarize_value ?(max_len = 100) (value : Value.t) : string =
  let summarize ?(max_len = 100) (s : string) : string =
    if String.length s <= max_len then s
    else String.sub s 0 (max_len - 3) ^ "..."
  in
  value |> Value.to_string |> summarize ~max_len

let format_values (values : Value.t list) : string =
  match values with
  | [] -> ""
  | _ ->
      let svalues = List.map summarize_value values in
      Format.sprintf "  [in: %s]" (String.concat ", " svalues)

let make ?(level = Simple) ?(fmt = Format.std_formatter) () =
  let depth = ref 0 in
  let indent () =
    Format.sprintf "[%2d] %s" !depth (String.make (!depth * 2) ' ')
  in
  let module H : Handler.HANDLER = struct
    include Handler.Default

    let init_spec _ = depth := 0

    (* Common events *)

    let on_rel_enter (rid : RId.t) (values : Value.t list) : unit =
      Format.fprintf fmt "%s-> %s\n%!" (indent ()) rid.it;
      if level = Full && values <> [] then
        Format.fprintf fmt "%s%s\n%!" (indent ())
          (format_values values |> normalize_whitespace);
      incr depth

    let on_rel_exit (rid : RId.t) : unit =
      decr depth;
      Format.fprintf fmt "%s<- %s\n%!" (indent ()) rid.it

    let on_func_enter (fid : FId.t) (values : Value.t list) : unit =
      Format.fprintf fmt "%s-> $%s\n%!" (indent ()) fid.it;
      if level = Full && values <> [] then
        Format.fprintf fmt "%s%s\n%!" (indent ())
          (format_values values |> normalize_whitespace);
      incr depth

    let on_func_exit (fid : FId.t) : unit =
      decr depth;
      Format.fprintf fmt "%s<- $%s\n%!" (indent ()) fid.it

    (* IL events *)

    let on_prem (prem : Il.prem) : unit =
      if level = Full then
        Format.fprintf fmt "%s  | -- %s\n%!" (indent ())
          (Il.Print.string_of_prem prem |> normalize_whitespace)

    (* SL events *)

    let on_instr (instr : Sl.instr) : unit =
      if level = Full then
        Format.fprintf fmt "%s  | %s\n%!" (indent ())
          (Sl.Print.string_of_instr ~short:true instr |> normalize_whitespace)
  end in
  (module H : Handler.HANDLER)
