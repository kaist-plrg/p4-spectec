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

type name = string
type id = string
type number = string
type port = string
type handle = string
type packet = string
type expect = string
type exact = bool
type session = number
type arg = id * number
type action = name * arg list
type mtchkind = Num of number | Slash of number * number
type mtch = name * mtchkind
type id_or_index = Id of string | Index of number
type cond = Eq | Ne | Le | Lt | Ge | Gt
type ctr = Bytes | Packets

type stmt =
  | Wait
  | RemoveAll
  | Expect of port * expect option * exact
  | Packet of port * packet
  | NoPacket
  | Add of name * int option * mtch list * action * id option
  | SetDefault of name * action
  | CheckCounter of id * id_or_index * (ctr option * cond * number)
  | MirroringAdd of session * port
  | MirroringAddMc of session * id
  | MirroringGet of session
  | McGroupCreate of id
  | McNodeCreate of id * port list
  | McNodeAssociate of id * handle
  | RegisterRead of name * number
  | RegisterWrite of name * number * number
  | RegisterReset of name
