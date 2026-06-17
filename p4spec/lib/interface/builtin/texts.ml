open Lang
open Xl
open Il
module Typ = Runtime.Type.Typ

module Make (V : Valrep.VAL) = struct
  open Util.Source

  (* dec $text_to_int(text) : int *)

  let text_to_int (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    Extract.zero at targs;
    let text = Extract.one at values_input |> V.Get.text in
    let i = Bigint.of_string text in
    let value = V.Make.int i in
    add value;
    value

  (* dec $int_to_text(int) : text *)

  let int_to_text (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    Extract.zero at targs;
    let num = Extract.one at values_input |> V.Get.num in
    let value = V.Make.text (Num.string_of_num num) in
    add value;
    value

  (* dec $split_text(text, text) : text* *)

  let split_text (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    Extract.zero at targs;
    let value_text, value_separator = Extract.two at values_input in
    let text = V.Get.text value_text in
    let separator = V.Get.text value_separator in
    assert (String.length separator = 1);
    let parts = String.split_on_char (String.get separator 0) text in
    let values = List.map V.Make.text parts in
    let typ_list = Typ.Make.list Typ.Make.bool in
    let value = V.Make.list typ_list values in
    add value;
    value

  (* dec $strip_prefix(text, text) : text *)

  let strip_prefix (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    Extract.zero at targs;
    let value_text, value_prefix = Extract.two at values_input in
    let text = V.Get.text value_text in
    let prefix = V.Get.text value_prefix in
    assert (String.starts_with ~prefix text);
    let text =
      String.sub text (String.length prefix)
        (String.length text - String.length prefix)
    in
    let value = V.Make.text text in
    add value;
    value

  (* dec $strip_suffix(text, text) : text *)

  let strip_suffix (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    Extract.zero at targs;
    let value_text, value_suffix = Extract.two at values_input in
    let text = V.Get.text value_text in
    let suffix = V.Get.text value_suffix in
    assert (String.ends_with ~suffix text);
    let text = String.sub text 0 (String.length text - String.length suffix) in
    let value = V.Make.text text in
    add value;
    value

  (* dec $strip_all_whitespace(text) : text *)

  let strip_all_whitespace (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    Extract.zero at targs;
    let value = Extract.one at values_input in
    let text =
      value |> V.Get.text |> String.split_on_char ' ' |> String.concat ""
    in
    let value = V.Make.text text in
    add value;
    value
end
