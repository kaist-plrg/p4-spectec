type t = {
  prose_in : Hintenv.t;
  prose_out : Hintenv.t;
  prose_true : Hintenv.t;
  prose_false : Hintenv.t;
}

let empty =
  {
    prose_in = Hintenv.empty;
    prose_out = Hintenv.empty;
    prose_true = Hintenv.empty;
    prose_false = Hintenv.empty;
  }
