(*
 * Parser debugging utilities using Menhir's inspection API:
 *
 *   Debugs parser stack state and token consumption
 *)

module MI = MenhirLib.General
module Parser = Parser.Make (struct let lex_env = Env.init "" end)
module I = Parser.Incremental
module Engine = Parser.MenhirInterpreter
module P = Printf
open Lang

let get_debug_level () = Debug_config.get_parser_debug_level ()

let token_name token =
  try
    match token with
    | Tokens.ABSTRACT _ -> "abstract"
    | Tokens.ACTION _ -> "action"
    | Tokens.ACTIONS _ -> "actions"
    | Tokens.APPLY _ -> "apply"
    | Tokens.BOOL _ -> "bool"
    | Tokens.BIT _ -> "bit"
    | Tokens.BREAK _ -> "break"
    | Tokens.CONST _ -> "const"
    | Tokens.CONTINUE _ -> "continue"
    | Tokens.CONTROL _ -> "control"
    | Tokens.DEFAULT _ -> "default"
    | Tokens.ELSE _ -> "else"
    | Tokens.ENTRIES _ -> "entries"
    | Tokens.ENUM _ -> "enum"
    | Tokens.ERROR _ -> "error"
    | Tokens.EXIT _ -> "exit"
    | Tokens.EXTERN _ -> "extern"
    | Tokens.HEADER _ -> "header"
    | Tokens.HEADER_UNION _ -> "header_union"
    | Tokens.IF _ -> "if"
    | Tokens.IN _ -> "in"
    | Tokens.INOUT _ -> "inout"
    | Tokens.INT _ -> "int"
    | Tokens.KEY _ -> "key"
    | Tokens.LIST _ -> "list"
    | Tokens.SELECT _ -> "select"
    | Tokens.MATCH_KIND _ -> "match_kind"
    | Tokens.OUT _ -> "out"
    | Tokens.PACKAGE _ -> "package"
    | Tokens.PARSER _ -> "parser"
    | Tokens.PRIORITY _ -> "priority"
    | Tokens.RETURN _ -> "return"
    | Tokens.STATE _ -> "state"
    | Tokens.STRING _ -> "string"
    | Tokens.STRUCT _ -> "struct"
    | Tokens.SWITCH _ -> "switch"
    | Tokens.TABLE _ -> "table"
    | Tokens.THIS _ -> "this"
    | Tokens.TRANSITION _ -> "transition"
    | Tokens.TUPLE _ -> "tuple"
    | Tokens.TYPEDEF _ -> "typedef"
    | Tokens.TYPE _ -> "type"
    | Tokens.VALUE_SET _ -> "value_set"
    | Tokens.VARBIT _ -> "varbit"
    | Tokens.VOID _ -> "void"
    | Tokens.TRUE _ -> "true"
    | Tokens.FALSE _ -> "false"
    | Tokens.FOR _ -> "for"
    | Tokens.END _ -> "end"
    | Tokens.TYPENAME -> "typename"
    | Tokens.IDENTIFIER -> "identifier"
    | Tokens.NAME s -> P.sprintf "name %s" (Il.Print.string_of_value s)
    | Tokens.STRING_LITERAL _ -> "string_literal"
    | Tokens.NUMBER _ -> "number"
    | Tokens.LE _ -> "<="
    | Tokens.GE _ -> ">="
    | Tokens.SHL _ -> ">>"
    | Tokens.AND _ -> "&"
    | Tokens.OR _ -> "|"
    | Tokens.NE _ -> "!="
    | Tokens.EQ _ -> "=="
    | Tokens.PLUS _ -> "+"
    | Tokens.MINUS _ -> "-"
    | Tokens.PLUS_SAT _ -> "PLUS_SAT"
    | Tokens.MINUS_SAT _ -> "MINUS_SAT"
    | Tokens.MUL _ -> "*"
    | Tokens.INVALID _ -> "INVALID"
    | Tokens.DIV _ -> "DIV"
    | Tokens.MOD _ -> "MOD"
    | Tokens.BIT_OR _ -> "BIT_OR"
    | Tokens.BIT_AND _ -> "BIT_AND"
    | Tokens.BIT_XOR _ -> "BIT_XOR"
    | Tokens.COMPLEMENT _ -> "COMPLEMENT"
    | Tokens.L_BRACKET _ -> "L_BRACKET"
    | Tokens.R_BRACKET _ -> "R_BRACKET"
    | Tokens.L_BRACE _ -> "L_BRACE"
    | Tokens.R_BRACE _ -> "R_BRACE"
    | Tokens.L_ANGLE _ -> "L_ANGLE"
    | Tokens.L_ANGLE_ARGS _ -> "L_ANGLE_ARGS"
    | Tokens.R_ANGLE _ -> "R_ANGLE"
    | Tokens.R_ANGLE_SHIFT _ -> "R_ANGLE_SHIFT"
    | Tokens.L_PAREN _ -> "L_PAREN"
    | Tokens.R_PAREN _ -> "R_PAREN"
    | Tokens.ASSIGN _ -> "ASSIGN"
    | Tokens.COLON _ -> "COLON"
    | Tokens.COMMA _ -> "COMMA"
    | Tokens.QUESTION _ -> "QUESTION"
    | Tokens.DOT _ -> "DOT"
    | Tokens.NOT _ -> "NOT"
    | Tokens.SEMICOLON _ -> "SEMICOLON"
    | Tokens.AT _ -> "AT"
    | Tokens.PLUSPLUS _ -> "PLUSPLUS"
    | Tokens.DONTCARE _ -> "DONTCARE"
    | Tokens.MASK _ -> "MASK"
    | Tokens.DOTS _ -> "DOTS"
    | Tokens.RANGE _ -> "RANGE"
    | Tokens.PRAGMA _ -> "PRAGMA"
    | Tokens.PRAGMA_END _ -> "PRAGMA_END"
    | Tokens.UNEXPECTED_TOKEN _ -> "UNEXPECTED_TOKEN"
    | _ -> "unknown"
  with _ -> "UNKNOWN_TOKEN"

(* Recursively collect stack states using top and pop *)
let rec collect_stack env acc =
  match Parser.MenhirInterpreter.top env with
  | None -> List.rev acc
  | Some (Parser.MenhirInterpreter.Element (state, _, _, _)) -> (
      let state_num = Parser.MenhirInterpreter.number state in
      match Parser.MenhirInterpreter.pop env with
      | None -> List.rev (state_num :: acc)
      | Some env' -> collect_stack env' (state_num :: acc))

let print_state env =
  let current_state = Parser.MenhirInterpreter.current_state_number env in
  let states = collect_stack env [] in
  let debug_level = get_debug_level () in

  if Debug_config.debug_enabled debug_level Basic then
    Printf.printf "@State %d:\n" current_state;
  match states with
  | [] ->
      if Debug_config.debug_enabled debug_level Verbose then
        Printf.printf "+Stack empty\n"
  | _ ->
      if Debug_config.debug_enabled debug_level Verbose then
        Printf.printf "+Stack: [%s]\n"
          (String.concat "; " (List.map string_of_int states))

let debug_parse lexer lexbuf =
  let supplier = Engine.lexer_lexbuf_to_supplier lexer lexbuf in
  let checkpoint = I.p4program lexbuf.lex_curr_p in
  let debug_level = get_debug_level () in
  let rec loop checkpoint =
    (match checkpoint with
    | Engine.InputNeeded env -> print_state env
    | Engine.Shifting (env, _, _) -> print_state env
    | Engine.AboutToReduce (env, _) ->
        print_state env;
        if Debug_config.debug_enabled debug_level Verbose then
          Printf.printf "--- About to reduce\n"
    | Engine.HandlingError env ->
        print_state env;
        if Debug_config.debug_enabled debug_level Basic then
          Printf.printf "Parser: Handling error\n"
    | _ -> ());
    match checkpoint with
    | Engine.InputNeeded _env ->
        let token, _, _ = supplier () in
        if Debug_config.debug_enabled debug_level Verbose then
          Printf.printf "\n|-> Consuming token: %s\n\n" (token_name token);
        loop
          (Engine.offer checkpoint (token, Lexing.dummy_pos, Lexing.dummy_pos))
    | Engine.Shifting _ | Engine.AboutToReduce _ ->
        loop (Engine.resume checkpoint)
    | Engine.HandlingError _env ->
        if Debug_config.debug_enabled debug_level Basic then
          Printf.printf "Parser: Syntax error occurred\n";
        raise Parser.Error
    | Engine.Accepted v ->
        if Debug_config.debug_enabled debug_level Basic then
          Printf.printf "Parser: Parsing completed successfully\n";
        v
    | Engine.Rejected -> failwith "Parser: Rejected"
  in
  if Debug_config.debug_enabled debug_level Basic then
    Printf.printf "Parser: Starting parse with debug level %s\n"
      (match debug_level with
      | Quiet -> "quiet"
      | Basic -> "basic"
      | Verbose -> "verbose"
      | Full -> "full");
  loop checkpoint
