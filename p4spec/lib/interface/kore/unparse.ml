(* Writing values of the K spec back to KORE, and normalizing KORE terms for
   comparison with krun's output *)

open Ast
open Load
module Value = Runtime.Value
module Num = Lang.Xl.Num

let get (v : Value.t) (mixop : string) : Value.t list option = Value.Get.(v |>>? mixop)

let rec sort_of_value (v : Value.t) : sort =
  match get v "SORT sortid sort*" with
  | Some [ name; sorts ] -> Sort (Value.Get.text name, List.map sort_of_value (Value.Get.list sorts))
  | _ -> (
      match get v "SORTVAR sortid" with
      | Some [ name ] -> SortVar (Value.Get.text name)
      | _ -> error "not a sort: %s" (Value.to_string v))

(* The unique sort hooked to a scalar hook (decisions D9 in k-in-p4) *)
let sort_of_hook (info : info) (hook : string) : sort =
  match Hashtbl.fold (fun s h acc -> if h = hook then s :: acc else acc) info.sort_hooks [] with
  | [ s ] -> Sort (s, [])
  | [] -> error "no sort is hooked to %s" hook
  | ss -> error "several sorts are hooked to %s: %s" hook (String.concat ", " ss)

(* Constructor symbols of a collection sort, from its concat/element/unit attributes *)
let collection_symbol (info : info) (s : sort) (which : string) : string =
  let name = match s with Sort (n, _) -> n | SortVar n -> n in
  match Hashtbl.find_opt info.sort_attrs name with
  | Some attrs -> (
      match find_attr which attrs with
      | Some (App (_, _, [ App (sym, _, _) ])) -> sym
      | _ -> error "sort %s has no %s attribute" name which)
  | None -> error "undeclared sort %s" name

let dv s text = App ("\\dv", [ s ], [ Str text ])

let pairs_of_map (v : Value.t) : (Value.t * Value.t) list =
  match get v "`{ k `}" with
  | Some [ pairs ] ->
      Value.Get.list pairs
      |> List.map (fun pair ->
             match get pair "k ':' v" with Some [ k; v ] -> (k, v) | _ -> error "not a map entry")
  | _ -> error "not a map: %s" (Value.to_string v)

let elements_of_set (v : Value.t) : Value.t list =
  match get v "`{ k `}" with Some [ elems ] -> Value.Get.list elems | _ -> error "not a set"

let build_collection (info : info) (s : sort) (elems : pattern list) : pattern =
  match elems with
  | [] -> App (collection_symbol info s "unit", [], [])
  | [ e ] -> e
  | e :: rest ->
      let concat = collection_symbol info s "concat" in
      List.fold_left (fun acc x -> App (concat, [], [ acc; x ])) e rest

let rec pattern_of_term (info : info) (v : Value.t) : pattern =
  let ( >>? ) mixop k = match get v mixop with Some args -> Some (k args) | None -> None in
  let first = List.find_map (fun f -> f ()) in
  match
    first
      [
        (fun () ->
          "APP symid sort* term*" >>? function
          | [ f; sorts; args ] ->
              App
                ( Value.Get.text f,
                  List.map sort_of_value (Value.Get.list sorts),
                  List.map (pattern_of_term info) (Value.Get.list args) )
          | _ -> assert false);
        (fun () ->
          "DV sort text" >>? function
          | [ s; t ] -> dv (sort_of_value s) (Value.Get.text t)
          | _ -> assert false);
        (fun () ->
          "INT int" >>? function
          | [ i ] -> (
              match Value.Get.num i with
              | `Int i | `Nat i -> dv (sort_of_hook info "INT.Int") (Bigint.to_string i))
          | _ -> assert false);
        (fun () ->
          "BOOL bool" >>? function
          | [ b ] -> dv (sort_of_hook info "BOOL.Bool") (string_of_bool (Value.Get.bool b))
          | _ -> assert false);
        (fun () ->
          "STRING text" >>? function
          | [ t ] -> dv (sort_of_hook info "STRING.String") (Value.Get.text t)
          | _ -> assert false);
        (fun () ->
          "FLOAT text" >>? function
          | [ t ] -> dv (sort_of_hook info "FLOAT.Float") (Value.Get.text t)
          | _ -> assert false);
        (fun () ->
          "BYTES nat*" >>? function
          | [ l ] ->
              let byte b = match Value.Get.num b with `Int i | `Nat i -> Char.chr (Bigint.to_int_exn i) in
              dv (sort_of_hook info "BYTES.Bytes") (String.of_seq (List.to_seq (List.map byte (Value.Get.list l))))
          | _ -> assert false);
        (fun () ->
          "MINT nat nat" >>? function
          | [ w; n ] ->
              let num x = match Value.Get.num x with `Int i | `Nat i -> i in
              let w = num w and n = num n in
              (* MInt{N} is the MInt sort applied to the sort with nat attribute N *)
              let width_sort =
                Hashtbl.fold
                  (fun s attrs acc ->
                    if string_attr "nat" attrs = Some (Bigint.to_string w) then Some s else acc)
                  info.sort_attrs None
              in
              let width_sort =
                match width_sort with Some s -> s | None -> error "no sort stands for the width %s" (Bigint.to_string w)
              in
              let mint = match sort_of_hook info "MINT.MInt" with Sort (s, _) | SortVar s -> s in
              dv (Sort (mint, [ Sort (width_sort, []) ])) (Bigint.to_string n ^ "p" ^ Bigint.to_string w)
          | _ -> assert false);
        (fun () ->
          "MAP sort m" >>? function
          | [ s; m ] ->
              let s = sort_of_value s in
              let element = collection_symbol info s "element" in
              pairs_of_map m
              |> List.map (fun (k, v) -> App (element, [], [ pattern_of_term info k; pattern_of_term info v ]))
              |> build_collection info s
          | _ -> assert false);
        (fun () ->
          "SET sort s" >>? function
          | [ s; set ] ->
              let s = sort_of_value s in
              let element = collection_symbol info s "element" in
              elements_of_set set
              |> List.map (fun e -> App (element, [], [ pattern_of_term info e ]))
              |> build_collection info s
          | _ -> assert false);
        (fun () ->
          "RANGEMAP sort rangeitem*" >>? function
          | [ s; items ] ->
              (* each range [a, b) |-> v as RangeMap:Range(a, b) r|-> v, in order, as
                 print_range_map writes them *)
              let s = sort_of_value s in
              let element = collection_symbol info s "element" in
              let key i =
                let i = match Value.Get.num i with `Int i | `Nat i -> i in
                App ("inj", [ sort_of_hook info "INT.Int"; Sort ("SortKItem", []) ], [ dv (sort_of_hook info "INT.Int") (Bigint.to_string i) ])
              in
              Value.Get.list items
              |> List.map (fun item ->
                     match get item "RANGE int int term" with
                     | Some [ a; b; v ] ->
                         App (element, [], [ App ("LblRangeMap'Coln'Range", [], [ key a; key b ]); pattern_of_term info v ])
                     | _ -> error "not a range: %s" (Value.to_string item))
              |> build_collection info s
          | _ -> assert false);
        (fun () ->
          "LIST sort t" >>? function
          | [ s; l ] ->
              let s = sort_of_value s in
              let element = collection_symbol info s "element" in
              Value.Get.list l
              |> List.map (fun e -> App (element, [], [ pattern_of_term info e ]))
              |> build_collection info s
          | _ -> assert false);
      ]
  with
  | Some p -> p
  | None -> error "not a term: %s" (Value.to_string v)

(* Normal form for comparison: associativity sugar expanded, and for Map and
   Set sorts, nested concatenations flattened, units dropped, and elements
   sorted. Lists keep their order. *)

let normalize (info : info) (p : pattern) : pattern =
  let unordered =
    Hashtbl.fold
      (fun s h acc ->
        if h = "MAP.Map" || h = "SET.Set" then
          match collection_symbol info (Sort (s, [])) "concat", collection_symbol info (Sort (s, [])) "unit" with
          | concat, unit -> (concat, (unit, Sort (s, []))) :: acc
          | exception Error _ -> acc
        else acc)
      info.sort_hooks []
  in
  let rec norm (p : pattern) : pattern =
    match p with
    | App (f, sorts, args) -> (
        let args = List.map norm args in
        match List.assoc_opt f unordered with
        | Some (unit, s) ->
            let rec flatten q =
              match q with
              | App (g, _, xs) when g = f -> List.concat_map flatten xs
              | App (g, _, []) when g = unit -> []
              | q -> [ q ]
            in
            let elems = List.concat_map flatten args in
            let elems = List.sort (fun a b -> compare (string_of_pattern a) (string_of_pattern b)) elems in
            ignore sorts;
            build_collection info s elems
        | None -> App (f, sorts, args))
    | p -> p
  in
  (* Bound variables get canonical names by binder depth, since the LLVM
     backend names them afresh when rewriting ends (substitution.md) *)
  let is_binder f =
    match Hashtbl.find_opt info.symbols f with Some { is_binder; _ } -> is_binder | None -> false
  in
  let rec alpha (env : (string * string) list) (depth : int) (p : pattern) : pattern =
    match p with
    | App ("\\dv", [ s ], [ Str x ]) when sort_hook info s = Some "KVAR.KVar" -> (
        match List.assoc_opt x env with Some y -> App ("\\dv", [ s ], [ Str y ]) | None -> p)
    | App (f, sorts, App ("\\dv", [ s ], [ Str x ]) :: rest) when is_binder f ->
        let y = Printf.sprintf "#bound%d" depth in
        App (f, sorts, App ("\\dv", [ s ], [ Str y ]) :: List.map (alpha ((x, y) :: env) (depth + 1)) rest)
    | App (f, sorts, args) -> App (f, sorts, List.map (alpha env depth) args)
    | p -> p
  in
  norm (alpha [] 0 (desugar_assoc p))

let string_of_term (info : info) (v : Value.t) : string =
  string_of_pattern (normalize info (pattern_of_term info v))
