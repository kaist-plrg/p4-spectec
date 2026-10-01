open Domain.Lib

(* Hints *)

type 'a hint = { id : HId.t; value : 'a }

type hints = {
  prose : Hints.Alter.t hint option;
  prose_in : Hints.Alter.t hint option;
  prose_out : Hints.Alter.t hint option;
  prose_true : Hints.Alter.t hint option;
  prose_false : Hints.Alter.t hint option;
  prose_fields : Hints.Fields.t hint option;
  prose_input_exps : Sl.exp list option;
  prose_output_exps : Sl.exp list option;
}

let empty : hints =
  {
    prose = None;
    prose_in = None;
    prose_out = None;
    prose_true = None;
    prose_false = None;
    prose_fields = None;
    prose_input_exps = None;
    prose_output_exps = None;
  }

type 'a t = { node : 'a; hints : hints }

(* Wrap a node with no prose hints. *)

let no_hints (node : 'a) : 'a t = { node; hints = empty }
