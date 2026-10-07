(* A recursive-descent parser for KORE text *)

open Ast

exception Error of string

type token =
  | Id of string (* identifier, \connective, or @setvar *)
  | String of string
  | LBrace
  | RBrace
  | LParen
  | RParen
  | LBrack
  | RBrack
  | Comma
  | Colon
  | ColonEq
  | EOF

type lexer = { src : string; mutable pos : int; mutable line : int }

let error (lx : lexer) msg =
  raise (Error (Printf.sprintf "line %d: %s" lx.line msg))

let is_id_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_id_char c =
  is_id_start c || (c >= '0' && c <= '9') || c = '\'' || c = '-'

let rec skip (lx : lexer) =
  let n = String.length lx.src in
  if lx.pos < n then
    match lx.src.[lx.pos] with
    | '\n' ->
        lx.line <- lx.line + 1;
        lx.pos <- lx.pos + 1;
        skip lx
    | ' ' | '\t' | '\r' ->
        lx.pos <- lx.pos + 1;
        skip lx
    | '/' when lx.pos + 1 < n && lx.src.[lx.pos + 1] = '/' ->
        while lx.pos < n && lx.src.[lx.pos] <> '\n' do
          lx.pos <- lx.pos + 1
        done;
        skip lx
    | '/' when lx.pos + 1 < n && lx.src.[lx.pos + 1] = '*' ->
        lx.pos <- lx.pos + 2;
        while
          lx.pos + 1 < n
          && not (lx.src.[lx.pos] = '*' && lx.src.[lx.pos + 1] = '/')
        do
          if lx.src.[lx.pos] = '\n' then lx.line <- lx.line + 1;
          lx.pos <- lx.pos + 1
        done;
        lx.pos <- lx.pos + 2;
        skip lx
    | _ -> ()

(* KORE string literal escapes: quote, backslash, n, t, r, f, xHH, uHHHH, UHHHHHHHH *)
let add_utf8 b code =
  let add c = Buffer.add_char b (Char.chr c) in
  if code < 0x80 then add code
  else if code < 0x800 then (
    add (0xC0 lor (code lsr 6));
    add (0x80 lor (code land 0x3F)))
  else if code < 0x10000 then (
    add (0xE0 lor (code lsr 12));
    add (0x80 lor ((code lsr 6) land 0x3F));
    add (0x80 lor (code land 0x3F)))
  else (
    add (0xF0 lor (code lsr 18));
    add (0x80 lor ((code lsr 12) land 0x3F));
    add (0x80 lor ((code lsr 6) land 0x3F));
    add (0x80 lor (code land 0x3F)))

let lex_string (lx : lexer) : string =
  let b = Buffer.create 16 in
  let n = String.length lx.src in
  lx.pos <- lx.pos + 1;
  let hex k =
    if lx.pos + k > n then error lx "bad escape";
    let v = int_of_string ("0x" ^ String.sub lx.src lx.pos k) in
    lx.pos <- lx.pos + k;
    v
  in
  let rec go () =
    if lx.pos >= n then error lx "unterminated string";
    match lx.src.[lx.pos] with
    | '"' -> lx.pos <- lx.pos + 1
    | '\\' ->
        lx.pos <- lx.pos + 1;
        let c = lx.src.[lx.pos] in
        lx.pos <- lx.pos + 1;
        (match c with
        | '"' -> Buffer.add_char b '"'
        | '\\' -> Buffer.add_char b '\\'
        | 'n' -> Buffer.add_char b '\n'
        | 't' -> Buffer.add_char b '\t'
        | 'r' -> Buffer.add_char b '\r'
        | 'f' -> Buffer.add_char b '\012'
        | 'x' -> Buffer.add_char b (Char.chr (hex 2))
        | 'u' -> add_utf8 b (hex 4)
        | 'U' -> add_utf8 b (hex 8)
        | c -> error lx (Printf.sprintf "unknown escape \\%c" c));
        go ()
    | c ->
        Buffer.add_char b c;
        lx.pos <- lx.pos + 1;
        go ()
  in
  go ();
  Buffer.contents b

let next (lx : lexer) : token =
  skip lx;
  let n = String.length lx.src in
  if lx.pos >= n then EOF
  else
    let c = lx.src.[lx.pos] in
    let single t =
      lx.pos <- lx.pos + 1;
      t
    in
    match c with
    | '{' -> single LBrace
    | '}' -> single RBrace
    | '(' -> single LParen
    | ')' -> single RParen
    | '[' -> single LBrack
    | ']' -> single RBrack
    | ',' -> single Comma
    | ':' when lx.pos + 1 < n && lx.src.[lx.pos + 1] = '=' ->
        lx.pos <- lx.pos + 2;
        ColonEq
    | ':' -> single Colon
    | '"' -> String (lex_string lx)
    | '\\' | '@' ->
        let start = lx.pos in
        lx.pos <- lx.pos + 1;
        while lx.pos < n && is_id_char lx.src.[lx.pos] do
          lx.pos <- lx.pos + 1
        done;
        Id (String.sub lx.src start (lx.pos - start))
    | c when is_id_start c ->
        let start = lx.pos in
        while lx.pos < n && is_id_char lx.src.[lx.pos] do
          lx.pos <- lx.pos + 1
        done;
        Id (String.sub lx.src start (lx.pos - start))
    | c -> error lx (Printf.sprintf "unexpected character %C" c)

(* Parser with one token of lookahead *)

type parser = { lx : lexer; mutable tok : token }

let advance p = p.tok <- next p.lx

let expect p t what =
  if p.tok = t then advance p else error p.lx ("expected " ^ what)

let ident p =
  match p.tok with
  | Id s ->
      advance p;
      s
  | _ -> error p.lx "expected an identifier"

let keyword p k =
  match p.tok with
  | Id s when s = k -> advance p
  | _ -> error p.lx ("expected " ^ k)

(* comma-separated list between open and close *)
let seq p opn cls item =
  expect p opn "an opening bracket";
  if p.tok = cls then (
    advance p;
    [])
  else
    let rec go acc =
      let x = item p in
      if p.tok = Comma then (
        advance p;
        go (x :: acc))
      else (
        expect p cls "a closing bracket";
        List.rev (x :: acc))
    in
    go []

let rec sort p : sort =
  let name = ident p in
  if p.tok = LBrace then Sort (name, seq p LBrace RBrace sort) else SortVar name

let rec pattern p : pattern =
  match p.tok with
  | String s ->
      advance p;
      Str s
  | Id name ->
      advance p;
      if p.tok = Colon && name.[0] <> '\\' then (
        advance p;
        let s = sort p in
        if name.[0] = '@' then SetVar (name, s) else Var (name, s))
      else
        let sorts = seq p LBrace RBrace sort in
        let args = seq p LParen RParen pattern in
        App (name, sorts, args)
  | _ -> error p.lx "expected a pattern"

let attrs p = seq p LBrack RBrack pattern
let sort_params p = seq p LBrace RBrace ident

let sentence p : sentence =
  match ident p with
  | "import" ->
      let name = ident p in
      ignore (attrs p);
      Import name
  | ("sort" | "hooked-sort") as kw ->
      let name = ident p in
      let params = sort_params p in
      SortDecl { name; params; hooked = kw = "hooked-sort"; attrs = attrs p }
  | ("symbol" | "hooked-symbol") as kw ->
      let name = ident p in
      let params = sort_params p in
      let args = seq p LParen RParen sort in
      expect p Colon "':'";
      let result = sort p in
      SymbolDecl
        {
          name;
          params;
          args;
          result;
          hooked = kw = "hooked-symbol";
          attrs = attrs p;
        }
  | "alias" ->
      let name = ident p in
      ignore (sort_params p);
      ignore (seq p LParen RParen sort);
      expect p Colon "':'";
      ignore (sort p);
      keyword p "where";
      ignore (pattern p);
      expect p ColonEq "':='";
      ignore (pattern p);
      AliasDecl { name; attrs = attrs p }
  | "axiom" ->
      let params = sort_params p in
      let pattern = pattern p in
      Axiom { params; pattern; attrs = attrs p }
  | "claim" ->
      let params = sort_params p in
      let pattern = pattern p in
      Claim { params; pattern; attrs = attrs p }
  | kw -> error p.lx ("unknown sentence " ^ kw)

let module_ p : module_ =
  keyword p "module";
  let name = ident p in
  let rec go acc =
    match p.tok with
    | Id "endmodule" ->
        advance p;
        List.rev acc
    | _ -> go (sentence p :: acc)
  in
  let sentences = go [] in
  { name; sentences; attrs = attrs p }

let make (src : string) : parser =
  let lx = { src; pos = 0; line = 1 } in
  { lx; tok = next lx }

let definition_of_string (src : string) : definition =
  let p = make src in
  let attrs = attrs p in
  let rec go acc =
    if p.tok = EOF then List.rev acc else go (module_ p :: acc)
  in
  { attrs; modules = go [] }

let pattern_of_string (src : string) : pattern =
  let p = make src in
  let pat = pattern p in
  if p.tok <> EOF then error p.lx "trailing input after pattern";
  pat

let read_file (path : string) : string =
  In_channel.with_open_bin path In_channel.input_all

let definition_of_file path = definition_of_string (read_file path)
let pattern_of_file path = pattern_of_string (read_file path)
