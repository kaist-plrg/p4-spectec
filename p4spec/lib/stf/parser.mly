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

%{
open Ast
%}

%token END
%token ADD ALL BYTES CHECK_COUNTER EXPECT NO_PACKET PACKET PACKETS EXACT REMOVE SETDEFAULT WAIT PACKET_WILDCARD
%token MIRRORING_ADD MIRRORING_ADD_MC MIRRORING_GET
%token MC_GROUP_CREATE MC_NODE_CREATE MC_NODE_ASSOCIATE
%token REGISTER_READ REGISTER_WRITE REGISTER_RESET
%token<string> ID
%token COLON COMMA DATA_TERN DOT
%token<string> INT_CONST_DEC TERN_CONST_HEX INT_CONST_HEX INT_CONST_BIN DATA_DEC DATA_HEX
%token LPAREN RPAREN SLASH EQUAL EQ LT LE GT GE NE LBRACKET RBRACKET

%start <Ast.stmt list> stmts

%%

stmts:
  stmt* END
  { $1 } ;

stmt:
  | ADD qualified_name priority match_list action
    { Add($2, Some($3), List.rev $4, $5, None) }
  | ADD qualified_name match_list action
    { Add($2, None, List.rev $3, $4, None) }
  | ADD qualified_name priority match_list action EQUAL ID
    { Add($2, Some($3), List.rev $4, $5, Some($7)) }
  | ADD qualified_name match_list action EQUAL ID
    { Add($2, None, List.rev $3, $4, Some($6)) }
  | CHECK_COUNTER ID LPAREN id_or_index RPAREN
    { CheckCounter($2, $4, (None, Eq, "0")) }
  | CHECK_COUNTER ID LPAREN id_or_index RPAREN count_type logical_cond number
    { CheckCounter($2, $4, (Some($6), $7, $8)) }
  | EXPECT port expect_data EXACT
    { Expect($2, $3, true) }
  | EXPECT port expect_data
    { Expect($2, $3, false) }
  | EXPECT port EXACT
    { Expect($2, None, true) }
  | EXPECT port
    { Expect($2, None, false) }
  | MIRRORING_ADD session port_mirror
    { MirroringAdd($2, $3) }
  | MIRRORING_ADD_MC session mgid
    { MirroringAddMc($2, $3) }
  | MIRRORING_GET session
    { MirroringGet($2) }
  | NO_PACKET
    { NoPacket }
  | PACKET port packet_data
    { Packet($2, $3) }
  | SETDEFAULT qualified_name action
    { SetDefault($2, $3) }
  | MC_GROUP_CREATE mgid
    { McGroupCreate($2) }
  | MC_NODE_CREATE rid number_list
    { McNodeCreate($2, $3) }
  | MC_NODE_ASSOCIATE mgid handle
    { McNodeAssociate($2, $3) }
  | REGISTER_READ qualified_name number
    { RegisterRead($2, $3) }
  | REGISTER_WRITE qualified_name number number
    { RegisterWrite($2, $3, $4) }
  | REGISTER_RESET qualified_name
    { RegisterReset($2) }
  | REMOVE ALL
    { RemoveAll }
  | WAIT
    { Wait }

number:
  | INT_CONST_DEC
    { $1 }
  | INT_CONST_BIN
    { $1 }
  | INT_CONST_HEX
    { $1 }
  | TERN_CONST_HEX
    { $1 }

number_or_lpm:
  | number SLASH number
    { Ast.Slash($1, $3) }
  | number
    { Ast.Num($1) }

number_list:
  | number
    { [$1] }
  | number number_list
    { $1 :: $2 }

match_list:
  | matcht
    { [$1] }
  | match_list matcht
    { $2 :: $1 }

matcht:
  | qualified_name COLON number_or_lpm
    { ($1, $3) }

qualified_name:
  | ID
    { $1 }
  | ID LBRACKET INT_CONST_DEC RBRACKET
    { $1 ^ "[" ^ $3 ^ "]" }
  | qualified_name DOT ID
    { $1 ^ "." ^ $3 }

id_or_index:
  | ID
    { Id($1) }
  | number
    { Index($1) }

count_type:
  | BYTES
    { Bytes }
  | PACKETS
    { Packets }

logical_cond:
  | EQ
    { Eq }
  | NE
    { Ne }
  | LE
    { Le }
  | LT
    { Lt }
  | GE
    { Ge }
  | GT
    { Gt }

action:
  | qualified_name LPAREN args RPAREN
    { $1, $3 }
  | qualified_name LPAREN RPAREN
    { $1, [] }

args:
  | arg
    { [$1] }
  | arg COMMA args
    { $1 :: $3 }

arg:
  | ADD COLON number
    { "add", $3 }
  | ID COLON number
    { $1, $3 }

session:
  | INT_CONST_DEC
    { $1 }

port:
  | DATA_DEC
    { $1 }

port_mirror:
  | INT_CONST_DEC
    { $1 }

mgid:
  | INT_CONST_DEC
    { $1 }

rid:
  | INT_CONST_DEC
    { $1 }

handle:
  | INT_CONST_DEC
    { $1 }

priority:
  | INT_CONST_DEC
    { int_of_string $1 }

expect_data:
  | expect_datum
    { $1 }
  | expect_data expect_datum
    { match $1, $2 with
      | Some(x), Some(y) -> Some(x ^ y)
      | None, Some(x) | Some(x), None -> Some(x)
      | None, None -> None }

packet_data:
  | packet_datum
    { $1 }
  | packet_data packet_datum
    { $1 ^ $2 }

expect_datum:
  | packet_datum
    { Some($1) }
  | PACKET_WILDCARD
    { Some("*") }
  | DATA_TERN
    { None }

packet_datum:
  | DATA_DEC
    { $1 }
  | DATA_HEX
    { $1 }
