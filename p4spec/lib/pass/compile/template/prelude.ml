(* Prelude for generated OCaml *)

let make_opt_splitN (n : int) : string =
  let xs = List.init n (fun i -> "x" ^ string_of_int i) in
  let tuple elems =
    match elems with [ x ] -> x | _ -> "(" ^ String.concat ", " elems ^ ")"
  in
  let somes = xs |> List.map (fun x -> "Some " ^ x) |> tuple in
  let nones = List.init n (fun _ -> "None") |> tuple in
  Format.asprintf "let split%d = function\n| Some %s -> %s\n| None -> %s" n
    (tuple xs) somes nones

let make_opt_combineN (n : int) : string =
  assert (n >= 2);
  let os = List.init n (fun i -> "o" ^ string_of_int i) in
  let xs = List.init n (fun i -> "x" ^ string_of_int i) in
  let tuple elems =
    match elems with [ x ] -> x | _ -> "(" ^ String.concat ", " elems ^ ")"
  in
  if n = 2 then
    Format.asprintf
      "let combine2 o0 o1 = match o0, o1 with\n\
       | Some x0, Some x1 -> Some (x0, x1)\n\
       | None, None -> None\n\
       | _ -> failwith \"mismatch in optionality of iterated variables\""
  else
    let prev_xs = List.filteri (fun i _ -> i < n - 1) xs in
    let prev_os = List.filteri (fun i _ -> i < n - 1) os in
    let last_o = "o" ^ string_of_int (n - 1) in
    let last_x = "x" ^ string_of_int (n - 1) in
    let prev_combine =
      "combine" ^ string_of_int (n - 1) ^ " " ^ String.concat " " prev_os
    in
    Format.asprintf
      "let combine%d %s = match %s, %s with\n\
       | Some %s, Some %s -> Some %s\n\
       | None, None -> None\n\
       | _ -> failwith \"mismatch in optionality of iterated variables\"" n
      (String.concat " " os) prev_combine last_o (tuple prev_xs) last_x
      (tuple xs)

(* Fused [splitM (Option.map f (combineN o0 .. o(N-1)))] into a single match.
   [fold_N_M f o0 .. o(N-1)]: all inputs [Some] -> apply [f] (returning an
   M-tuple) and re-wrap each output in [Some]; all [None] -> M [None]s; mixed
   optionality fails. Mirrors [make_list_foldN_M] without the recursion. *)
let make_opt_foldN_M ((n, m) : int * int) : string =
  let concat = String.concat " " in
  let tuple elems =
    match elems with [ x ] -> x | _ -> "(" ^ String.concat ", " elems ^ ")"
  in
  let os = List.init n (fun i -> "o" ^ string_of_int i) in
  let xs = List.init n (fun i -> "x" ^ string_of_int i) in
  let bs = List.init m (fun i -> "b" ^ string_of_int i) in
  let scrut = tuple os in
  let some_pat = tuple (List.map (fun x -> "Some " ^ x) xs) in
  let none_pat = tuple (List.init n (fun _ -> "None")) in
  let bind_bs = Format.asprintf "let %s = f %s in" (tuple bs) (concat xs) in
  let some_result = tuple (List.map (fun b -> "Some " ^ b) bs) in
  let none_result = tuple (List.init m (fun _ -> "None")) in
  Format.asprintf
    "let fold_%d_%d f %s =\n\
    \  match %s with\n\
    \  | %s -> %s %s\n\
    \  | %s -> %s\n\
    \  | _ -> failwith \"mismatch in optionality of iterated variables\"" n m
    (concat os) scrut some_pat bind_bs some_result none_pat none_result

(* Fused [match combineN o0 .. o(N-1) with None -> true | Some (..) -> f ..].
   [for_all_N f o0 .. o(N-1)]: all [Some] -> apply [f]; all [None] -> true
   (vacuous); mixed optionality fails. *)
let make_opt_for_allN (n : int) : string =
  let concat = String.concat " " in
  let tuple elems =
    match elems with [ x ] -> x | _ -> "(" ^ String.concat ", " elems ^ ")"
  in
  let os = List.init n (fun i -> "o" ^ string_of_int i) in
  let xs = List.init n (fun i -> "x" ^ string_of_int i) in
  let scrut = tuple os in
  let some_pat = tuple (List.map (fun x -> "Some " ^ x) xs) in
  let none_pat = tuple (List.init n (fun _ -> "None")) in
  Format.asprintf
    "let for_all_%d f %s =\n\
    \  match %s with\n\
    \  | %s -> f %s\n\
    \  | %s -> true\n\
    \  | _ -> failwith \"mismatch in optionality of iterated variables\"" n
    (concat os) scrut some_pat (concat xs) none_pat

let opt_prelude (ctx : Ctx.t) =
  let header = "module Option = struct\ninclude Option\n" in
  let opt_splitNs =
    ctx.preamble.opts.splits |> List.map make_opt_splitN |> String.concat "\n"
  in
  let opt_combineNs =
    match ctx.preamble.opts.combines with
    | [] -> ""
    | arities ->
        let max_n = List.fold_left max 0 arities in
        if max_n < 2 then ""
        else
          List.init (max_n - 1) (fun i -> make_opt_combineN (i + 2))
          |> String.concat "\n"
  in
  let opt_foldNs =
    ctx.preamble.opts.folds |> List.map make_opt_foldN_M |> String.concat "\n"
  in
  let opt_forallNs =
    ctx.preamble.opts.foralls |> List.map make_opt_for_allN
    |> String.concat "\n"
  in
  let footer = "\nend" in
  header ^ opt_splitNs ^ "\n" ^ opt_combineNs ^ "\n" ^ opt_foldNs ^ "\n"
  ^ opt_forallNs ^ footer

let make_list_splitN (n : int) : string =
  let xs = List.init n (fun i -> "x" ^ string_of_int i) in
  let accs = List.init n (fun i -> "acc" ^ string_of_int i) in
  let tuple elems =
    match elems with [ x ] -> x | _ -> "(" ^ String.concat ", " elems ^ ")"
  in
  if n = 1 then "let split1 xs = xs"
  else
    let elem_pat = tuple xs in
    let acc_pat = tuple accs in
    let cons = List.map2 (fun x acc -> x ^ " :: " ^ acc) xs accs |> tuple in
    let init = List.init n (fun _ -> "[]") |> tuple in
    Format.asprintf
      "let split%d xs =\n  List.fold_right (fun %s %s -> %s) xs %s" n elem_pat
      acc_pat cons init

let make_list_combineN (n : int) : string =
  assert (n >= 2);
  let ls = List.init n (fun i -> "l" ^ string_of_int i) in
  let xs = List.init n (fun i -> "x" ^ string_of_int i) in
  let tuple elems =
    match elems with [ x ] -> x | _ -> "(" ^ String.concat ", " elems ^ ")"
  in
  if n = 2 then "let combine2 l0 l1 = List.combine l0 l1"
  else
    let prev_xs = List.filteri (fun i _ -> i < n - 1) xs in
    let prev_ls = List.filteri (fun i _ -> i < n - 1) ls in
    let last_l = "l" ^ string_of_int (n - 1) in
    let last_x = "x" ^ string_of_int (n - 1) in
    let prev_combine =
      "combine" ^ string_of_int (n - 1) ^ " " ^ String.concat " " prev_ls
    in
    Format.asprintf "let combine%d %s =\n  List.map2 (fun %s %s -> %s) (%s) %s"
      n (String.concat " " ls) (tuple prev_xs) last_x (tuple xs) prev_combine
      last_l

(* Fused [combineN |> List.map f |> splitM] into a single tail-recursive pass.
   [fold_left_N_M f l0 .. l(N-1)] walks the N input lists in lockstep, applies
   [f] (returning an M-tuple) per element, and returns an M-tuple of lists.
   Outputs are built by consing and [List.rev]'d once at the end. *)
let make_list_foldN_M ((n, m) : int * int) : string =
  let concat = String.concat " " in
  let tuple elems =
    match elems with [ x ] -> x | _ -> "(" ^ String.concat ", " elems ^ ")"
  in
  let ls = List.init n (fun i -> "l" ^ string_of_int i) in
  let ts = List.init n (fun i -> "t" ^ string_of_int i) in
  let xs = List.init n (fun i -> "x" ^ string_of_int i) in
  let accs = List.init m (fun i -> "a" ^ string_of_int i) in
  let bs = List.init m (fun i -> "b" ^ string_of_int i) in
  let scrut = tuple ls in
  let nil_pat = tuple (List.init n (fun _ -> "[]")) in
  let cons_pat = tuple (List.map2 (fun x t -> x ^ " :: " ^ t) xs ts) in
  let rev_result = tuple (List.map (fun a -> "List.rev " ^ a) accs) in
  let bind_bs = Format.asprintf "let %s = f %s in" (tuple bs) (concat xs) in
  let go_next =
    Format.asprintf "go %s %s"
      (concat (List.map2 (fun b a -> "(" ^ b ^ " :: " ^ a ^ ")") bs accs))
      (concat ts)
  in
  let init_call =
    Format.asprintf "go %s %s"
      (concat (List.init m (fun _ -> "[]")))
      (concat ls)
  in
  Format.asprintf
    "let fold_left_%d_%d f %s =\n\
    \  let rec go %s %s =\n\
    \    match %s with\n\
    \    | %s -> %s\n\
    \    | %s -> %s %s\n\
    \    | _ -> invalid_arg \"fold_left_%d_%d\"\n\
    \  in %s" n m (concat ls) (concat accs) (concat ls) scrut nil_pat rev_result
    cons_pat bind_bs go_next n m init_call

(* Fused [List.for_all f (combineN l0 .. l(N-1))] into a single lockstep walk.
   [for_all_N f l0 .. l(N-1)] short-circuits on the first [false]. *)
let make_list_for_allN (n : int) : string =
  let concat = String.concat " " in
  let tuple elems =
    match elems with [ x ] -> x | _ -> "(" ^ String.concat ", " elems ^ ")"
  in
  let ls = List.init n (fun i -> "l" ^ string_of_int i) in
  let ts = List.init n (fun i -> "t" ^ string_of_int i) in
  let xs = List.init n (fun i -> "x" ^ string_of_int i) in
  let scrut = tuple ls in
  let nil_pat = tuple (List.init n (fun _ -> "[]")) in
  let cons_pat = tuple (List.map2 (fun x t -> x ^ " :: " ^ t) xs ts) in
  Format.asprintf
    "let for_all_%d f %s =\n\
    \  let rec go %s =\n\
    \    match %s with\n\
    \    | %s -> true\n\
    \    | %s -> f %s && go %s\n\
    \    | _ -> invalid_arg \"for_all_%d\"\n\
    \  in go %s" n (concat ls) (concat ls) scrut nil_pat cons_pat (concat xs)
    (concat ts) n (concat ls)

let list_prelude (ctx : Ctx.t) =
  let header = "module List = struct\ninclude List\n" in
  let list_splitNs =
    ctx.preamble.lists.splits |> List.map make_list_splitN |> String.concat "\n"
  in
  let list_combineNs =
    match ctx.preamble.lists.combines with
    | [] -> ""
    | arities ->
        let max_n = List.fold_left max 0 arities in
        if max_n < 2 then ""
        else
          List.init (max_n - 1) (fun i -> make_list_combineN (i + 2))
          |> String.concat "\n"
  in
  let list_foldNs =
    ctx.preamble.lists.folds |> List.map make_list_foldN_M |> String.concat "\n"
  in
  let list_forallNs =
    ctx.preamble.lists.foralls
    |> List.map make_list_for_allN
    |> String.concat "\n"
  in
  let footer = "\nend" in
  header ^ list_splitNs ^ "\n" ^ list_combineNs ^ "\n" ^ list_foldNs ^ "\n"
  ^ list_forallNs ^ footer

let prelude (ctx : Ctx.t) =
  let common =
    {|(* Generated by p4spectec ocaml — do not edit manually *)

[@@@warning "-8-11-26-27-30-32-33-39"]

open Domain
open Lang
module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Run = Runtime.Dynamic_Runner.Signature
open Util.Source

exception Unmatch of string

let make_typ_var_ (s : string) (targs_ : Typ.t list) : Typ.t =
  Typ.Make.var {it = s; at = no_region; note = ()} targs_

let make_atom_ (s : string) : Atom.t phrase =
  {it = Atom.Atom s; at = no_region; note = ()}

let make_case_
    (mixop : Mixop.t)
    (payload : Value.t list)
    (typ : Typ.t) : Value.t =
  Value.Make.(mixop <|! payload <<|! typ)

let get_field_
    (fields : (Atom.t phrase * 'a) list)
    (s : string) : 'a =
  snd (List.find (fun ({it; _}, _) -> it = Atom.Atom s) fields)

|}
  in
  let opt_prelude = opt_prelude ctx in
  let list_prelude = list_prelude ctx in
  common ^ opt_prelude ^ "\n" ^ list_prelude
