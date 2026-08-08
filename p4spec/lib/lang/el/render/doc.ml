(* Documents *)

type t =
  | Empty
  | Text of string
  | Break of string
  | Line
  | Cat of t * t
  | Nest of int * t
  | Group of t

type mode = Flat | Broken
type command = int * mode * t

(* Constructors *)

let empty = Empty
let text s = if s = "" then Empty else Text s
let break s = Break s
let line = Line
let nest indent doc = if indent = 0 then doc else Nest (indent, doc)
let group doc = Group doc

(* Combinators *)

let ( ^^ ) doc_l doc_r =
  match (doc_l, doc_r) with
  | Empty, doc | doc, Empty -> doc
  | _ -> Cat (doc_l, doc_r)

let concat docs = List.fold_left ( ^^ ) empty docs

let rec join sep = function
  | [] -> empty
  | [ doc ] -> doc
  | doc_h :: docs_t -> doc_h ^^ sep ^^ join sep docs_t

let flow = function
  | [] -> empty
  | doc_h :: docs_t ->
      List.fold_left
        (fun doc_l doc_r -> doc_l ^^ group (break " " ^^ doc_r))
        doc_h docs_t

(* Layout *)

let rec fits width_remaining commands =
  if width_remaining < 0 then false
  else
    match commands with
    | [] -> true
    | (indent, mode, doc) :: commands_t -> (
        match doc with
        | Empty -> fits width_remaining commands_t
        | Text s -> fits (width_remaining - String.length s) commands_t
        | Break s -> (
            match mode with
            | Flat -> fits (width_remaining - String.length s) commands_t
            | Broken -> true)
        | Line -> true
        | Cat (doc_l, doc_r) ->
            fits width_remaining
              ((indent, mode, doc_l) :: (indent, mode, doc_r) :: commands_t)
        | Nest (offset, doc) ->
            fits width_remaining ((indent + offset, mode, doc) :: commands_t)
        | Group doc -> (
            match mode with
            | Flat -> fits width_remaining ((indent, Flat, doc) :: commands_t)
            | Broken ->
                fits width_remaining ((indent, Broken, doc) :: commands_t)))

let render ~width doc =
  if width <= 0 then invalid_arg "Doc.render: width must be positive";
  let buf = Buffer.create 256 in
  let append_newline indent =
    Buffer.add_char buf '\n';
    Buffer.add_string buf (String.make indent ' ')
  in
  let rec render_commands column = function
    | [] -> ()
    | (indent, mode, doc) :: commands_t -> (
        match doc with
        | Empty -> render_commands column commands_t
        | Text s ->
            Buffer.add_string buf s;
            render_commands (column + String.length s) commands_t
        | Break s -> (
            match mode with
            | Flat ->
                Buffer.add_string buf s;
                render_commands (column + String.length s) commands_t
            | Broken ->
                append_newline indent;
                render_commands indent commands_t)
        | Line ->
            append_newline indent;
            render_commands indent commands_t
        | Cat (doc_l, doc_r) ->
            render_commands column
              ((indent, mode, doc_l) :: (indent, mode, doc_r) :: commands_t)
        | Nest (offset, doc) ->
            render_commands column ((indent + offset, mode, doc) :: commands_t)
        | Group doc ->
            let mode =
              match mode with
              | Flat -> Flat
              | Broken ->
                  if fits (width - column) ((indent, Flat, doc) :: commands_t)
                  then Flat
                  else Broken
            in
            render_commands column ((indent, mode, doc) :: commands_t))
  in
  render_commands 0 [ (0, Broken, doc) ];
  Buffer.contents buf
