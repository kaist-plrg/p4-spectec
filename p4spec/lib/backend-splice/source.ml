(* Source file position tracker *)

type t = { file : string; s : string; mutable i : int }

let eos (src : t) = src.i = String.length src.s
let get (src : t) = src.s.[src.i]
let str (src : t) (j : int) = String.sub src.s j (src.i - j)
let advn (src : t) (i : int) = src.i <- src.i + i
let adv (src : t) = advn src 1
let left (src : t) = String.length src.s - src.i

let col (src : t) =
  let j = ref src.i in
  while !j > 0 && src.s.[!j - 1] <> '\n' do
    decr j
  done;
  src.i - !j

let pos (src : t) =
  let line = ref 1 in
  let col = ref 1 in
  for j = 0 to src.i - 1 do
    if src.s.[j] = '\n' then (
      incr line;
      col := 1)
    else incr col
  done;
  Util.Source.{ file = src.file; line = !line; column = !col }

let region src =
  let pos = pos src in
  Util.Source.{ left = pos; right = pos }
