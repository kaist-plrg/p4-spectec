(* Writing values of the K spec back to KORE, and normalizing KORE terms for
   comparison with krun's output *)

open Ast
open Load
module Value = Runtime.Value
module Num = Lang.Xl.Num

let get (v : Value.t) (mixop : string) : Value.t list option =
  Value.Get.(v |>>? mixop)

let rec sort_of_value (v : Value.t) : sort =
  match get v "SORT sortid sort*" with
  | Some [ name; sorts ] ->
      Sort (Value.Get.text name, List.map sort_of_value (Value.Get.list sorts))
  | _ -> (
      match get v "SORTVAR sortid" with
      | Some [ name ] -> SortVar (Value.Get.text name)
      | _ -> error "not a sort: %s" (Value.to_string v))

(* The unique sort hooked to a scalar hook *)
let sort_of_hook (info : info) (hook : string) : sort =
  match
    Hashtbl.fold
      (fun s h acc -> if h = hook then s :: acc else acc)
      info.sort_hooks []
  with
  | [ s ] -> Sort (s, [])
  | [] -> error "no sort is hooked to %s" hook
  | ss ->
      error "several sorts are hooked to %s: %s" hook (String.concat ", " ss)

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

(* Each entry of a map: the entry, its key, and its value *)
let pairs_of_map (v : Value.t) : (Value.t * Value.t * Value.t) list =
  match get v "`{ k `}" with
  | Some [ pairs ] ->
      Value.Get.list pairs
      |> List.map (fun pair ->
             match get pair "k ':' v" with
             | Some [ k; v ] -> (pair, k, v)
             | _ -> error "not a map entry")
  | _ -> error "not a map: %s" (Value.to_string v)

let elements_of_set (v : Value.t) : Value.t list =
  match get v "`{ k `}" with
  | Some [ elems ] -> Value.Get.list elems
  | _ -> error "not a set"

let build_collection (info : info) (s : sort) (elems : pattern list) : pattern =
  match elems with
  | [] -> App (collection_symbol info s "unit", [], [])
  | [ e ] -> e
  | e :: rest ->
      let concat = collection_symbol info s "concat" in
      List.fold_left (fun acc x -> App (concat, [], [ acc; x ])) e rest

(* With memo, each element of a Map or Set is written through memo, which may
   give its text written before (see normal_text) *)
let rec pattern_of_term ?memo (info : info) (v : Value.t) : pattern =
  let memoized (e : Value.t) (build : unit -> pattern) : pattern =
    match memo with Some m -> m e build | None -> build ()
  in
  let ( >>? ) mixop k =
    match get v mixop with Some args -> Some (k args) | None -> None
  in
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
                  List.map (pattern_of_term ?memo info) (Value.Get.list args) )
          | _ -> assert false);
        (fun () ->
          "DV sort text" >>? function
          | [ s; t ] -> dv (sort_of_value s) (Value.Get.text t)
          | _ -> assert false);
        (fun () ->
          "INT int" >>? function
          | [ i ] -> (
              match Value.Get.num i with
              | `Int i | `Nat i ->
                  dv (sort_of_hook info "INT.Int") (Bigint.to_string i))
          | _ -> assert false);
        (fun () ->
          "BOOL bool" >>? function
          | [ b ] ->
              dv
                (sort_of_hook info "BOOL.Bool")
                (string_of_bool (Value.Get.bool b))
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
              let byte b =
                match Value.Get.num b with
                | `Int i | `Nat i -> Char.chr (Bigint.to_int_exn i)
              in
              dv
                (sort_of_hook info "BYTES.Bytes")
                (String.of_seq (List.to_seq (List.map byte (Value.Get.list l))))
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
                    if string_attr "nat" attrs = Some (Bigint.to_string w) then
                      Some s
                    else acc)
                  info.sort_attrs None
              in
              let width_sort =
                match width_sort with
                | Some s -> s
                | None ->
                    error "no sort stands for the width %s" (Bigint.to_string w)
              in
              let mint =
                match sort_of_hook info "MINT.MInt" with
                | Sort (s, _) | SortVar s -> s
              in
              dv
                (Sort (mint, [ Sort (width_sort, []) ]))
                (Bigint.to_string n ^ "p" ^ Bigint.to_string w)
          | _ -> assert false);
        (fun () ->
          "MAP sort m" >>? function
          | [ s; m ] ->
              let s = sort_of_value s in
              let element = collection_symbol info s "element" in
              pairs_of_map m
              |> List.map (fun ((pair, k, v) : Value.t * Value.t * Value.t) ->
                     memoized pair (fun () ->
                         App
                           ( element,
                             [],
                             [
                               pattern_of_term ?memo info k;
                               pattern_of_term ?memo info v;
                             ] )))
              |> build_collection info s
          | _ -> assert false);
        (fun () ->
          "SET sort s" >>? function
          | [ s; set ] ->
              let s = sort_of_value s in
              let element = collection_symbol info s "element" in
              elements_of_set set
              |> List.map (fun e ->
                     memoized e (fun () ->
                         App (element, [], [ pattern_of_term ?memo info e ])))
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
                App
                  ( "inj",
                    [ sort_of_hook info "INT.Int"; Sort ("SortKItem", []) ],
                    [ dv (sort_of_hook info "INT.Int") (Bigint.to_string i) ] )
              in
              Value.Get.list items
              |> List.map (fun item ->
                     match get item "RANGE int int term" with
                     | Some [ a; b; v ] ->
                         App
                           ( element,
                             [],
                             [
                               App
                                 ("LblRangeMap'Coln'Range", [], [ key a; key b ]);
                               pattern_of_term ?memo info v;
                             ] )
                     | _ -> error "not a range: %s" (Value.to_string item))
              |> build_collection info s
          | _ -> assert false);
        (fun () ->
          "LIST sort t" >>? function
          | [ s; l ] ->
              let s = sort_of_value s in
              let element = collection_symbol info s "element" in
              Value.Get.list l
              |> List.map (fun e ->
                     App (element, [], [ pattern_of_term ?memo info e ]))
              |> build_collection info s
          | _ -> assert false);
      ]
  with
  | Some p -> p
  | None -> error "not a term: %s" (Value.to_string v)

(* Normal form for comparison, as text. It covers how krun writes a
   configuration, not what K computes, which is the spec's: associativity
   sugar expanded; for Map and Set sorts, nested concatenations flattened,
   units dropped, and elements sorted by their text (lists keep their order);
   variables named as the LLVM backend's printer names them; and bound
   variables then named canonically by binder depth, since the printer names
   them afresh (substitution.md).

   Each element is written once, when it is sorted, and its text is reused
   above it; an element can also come as the text it had before (rendered). *)

type normal =
  | Node of string * sort list * normal list
  | Text of string (* written already *)
  | Leaf of pattern (* a variable or a string *)

let rec add_normal b = function
  | Text s -> Buffer.add_string b s
  | Leaf p -> add_pattern b p
  | Node (name, sorts, args) ->
      Buffer.add_string b name;
      Buffer.add_char b '{';
      Buffer.add_string b (string_of_sorts sorts);
      Buffer.add_string b "}(";
      List.iteri
        (fun i n ->
          if i > 0 then Buffer.add_string b ", ";
          add_normal b n)
        args;
      Buffer.add_char b ')'

let text_of_normal n =
  let b = Buffer.create 256 in
  add_normal b n;
  Buffer.contents b

(* An element given as its normal text *)
let rendered (text : string) : pattern = App ("\\rendered", [], [ Str text ])

let has_binders (info : info) : bool =
  Hashtbl.fold
    (fun _ (sym : symbol) acc -> acc || sym.is_binder)
    info.symbols false

(* The normalizer of a definition *)
let normal_text (info : info) : pattern -> string =
  (* concat symbol of each Map and Set sort, with its unit *)
  let unordered =
    Hashtbl.fold
      (fun s h acc ->
        if h = "MAP.Map" || h = "SET.Set" then
          match
            ( collection_symbol info (Sort (s, [])) "concat",
              collection_symbol info (Sort (s, [])) "unit" )
          with
          | concat, unit -> (concat, unit) :: acc
          | exception Error _ -> acc
        else acc)
      info.sort_hooks []
  in
  (* the normal form, and for a Map or Set its concat and sorted elements *)
  let rec norm (p : pattern) : normal * (string * string list) option =
    match p with
    | App ("\\rendered", [], [ Str s ]) -> (Text s, None)
    | App (f, sorts, args) -> (
        match List.assoc_opt f unordered with
        | Some unit ->
            let elems =
              List.concat_map
                (fun a ->
                  match a with
                  | App (g, _, []) when g = unit -> []
                  | _ -> (
                      match norm a with
                      | _, Some (g, es) when g = f -> es
                      | n, _ -> [ text_of_normal n ]))
                args
              |> List.sort String.compare
            in
            let n =
              match elems with
              | [] -> Node (unit, [], [])
              | e :: rest ->
                  List.fold_left
                    (fun acc x -> Node (f, [], [ acc; Text x ]))
                    (Text e) rest
            in
            (n, Some (f, elems))
        | None -> (Node (f, sorts, List.map (fun a -> fst (norm a)) args), None)
        )
    | p -> (Leaf p, None)
  in
  let is_binder f =
    match Hashtbl.find_opt info.symbols f with
    | Some { is_binder; _ } -> is_binder
    | None -> false
  in
  let rec alpha (env : (string * string) list) (depth : int) (p : pattern) :
      pattern =
    match p with
    | App ("\\dv", [ s ], [ Str x ]) when sort_hook info s = Some "KVAR.KVar"
      -> (
        match List.assoc_opt x env with
        | Some y -> App ("\\dv", [ s ], [ Str y ])
        | None -> p)
    | App (f, sorts, App ("\\dv", [ s ], [ Str x ]) :: rest) when is_binder f ->
        let y = Printf.sprintf "#bound%d" depth in
        App
          ( f,
            sorts,
            App ("\\dv", [ s ], [ Str y ])
            :: List.map (alpha ((x, y) :: env) (depth + 1)) rest )
    | App (f, sorts, args) -> App (f, sorts, List.map (alpha env depth) args)
    | p -> p
  in
  let is_kvar s = sort_hook info s = Some "KVAR.KVar" in
  (* Variable names as the LLVM backend's printer gives them
     (runtime/util/ConfigurationPrinter.cpp): each binder's variable is a
     variable of its own, free variables are one per name, and each gets its
     name when first printed, with a number from one counter added if another
     variable has that name already. So a free variable can be renamed: krun
     prints (\y.y) y as (\y.y) y0. The spec's names are the LLVM ones with
     the primes its substitution adds (3.2-hook-substitution.watsup) *)
  let printer_names (p : pattern) : pattern =
    let used = Hashtbl.create 16 and free = Hashtbl.create 16 in
    let counter = ref 0 in
    let name x =
      let base =
        let n = ref (String.length x) in
        while !n > 0 && x.[!n - 1] = '\'' do
          decr n
        done;
        String.sub x 0 !n
      in
      let rec go suffix =
        if Hashtbl.mem used (base ^ suffix) then (
          let suffix = string_of_int !counter in
          incr counter;
          go suffix)
        else base ^ suffix
      in
      let y = go "" in
      Hashtbl.replace used y ();
      y
    in
    let rec walk env p =
      match p with
      | App ("\\dv", [ s ], [ Str x ]) when is_kvar s -> (
          match List.assoc_opt x env with
          | Some y -> App ("\\dv", [ s ], [ Str y ])
          | None ->
              let y =
                match Hashtbl.find_opt free x with
                | Some y -> y
                | None ->
                    let y = name x in
                    Hashtbl.replace free x y;
                    y
              in
              App ("\\dv", [ s ], [ Str y ]))
      | App (f, sorts, App ("\\dv", [ s ], [ Str x ]) :: rest) when is_binder f
        ->
          let y = name x in
          App
            ( f,
              sorts,
              App ("\\dv", [ s ], [ Str y ])
              :: List.map (walk ((x, y) :: env)) rest )
      | App (f, sorts, args) -> App (f, sorts, List.map (walk env) args)
      | p -> p
    in
    walk [] p
  in
  let binders = has_binders info in
  fun p ->
    let p = desugar_assoc p in
    let p = if binders then alpha [] 0 (printer_names p) else p in
    text_of_normal (fst (norm p))

(* The normal texts of the elements of Maps and Sets in terms of the spec,
   kept by value id from one step to the next. Only for definitions without
   binders, where an element's normal text does not depend on where it is. *)
type memo = {
  normal : pattern -> string;
  mutable current : (int, string) Hashtbl.t; (* used by this step *)
  mutable previous : (int, string) Hashtbl.t; (* used by the step before *)
}

let make_memo (info : info) : memo option =
  if has_binders info then None
  else
    Some
      {
        normal = normal_text info;
        current = Hashtbl.create 1024;
        previous = Hashtbl.create 1024;
      }

let next_step (m : memo) =
  m.previous <- m.current;
  m.current <- Hashtbl.create (Hashtbl.length m.previous)

(* The normal text of a term of the spec, reusing what memo kept *)
let string_of_term ?memo (info : info) : Value.t -> string =
  match memo with
  | None ->
      let normal = normal_text info in
      fun v -> normal (pattern_of_term info v)
  | Some m ->
      let element (e : Value.t) build =
        let id = e.Util.Source.note.Lang.Il.vid in
        match Hashtbl.find_opt m.current id with
        | Some s -> rendered s
        | None ->
            let s =
              match Hashtbl.find_opt m.previous id with
              | Some s -> s
              | None -> m.normal (build ())
            in
            Hashtbl.replace m.current id s;
            rendered s
      in
      fun v -> m.normal (pattern_of_term ~memo:element info v)
