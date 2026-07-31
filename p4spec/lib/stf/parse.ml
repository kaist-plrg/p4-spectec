(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)

open Core
open Util.Error

let error = error_stf

let lex (filename : string) =
  try
    let file = In_channel.read_all filename in
    Lexing.from_string file
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error

let parse (lexbuf : Lexing.lexbuf) =
  try Parser.stmts Lexer.token lexbuf with
  | Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Format.asprintf "Parse error at %s:%d:%d\n" pos.pos_fname pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol)
      |> error

let parse_file (filename : string) =
  let tokens = lex filename in
  parse tokens
