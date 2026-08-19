(* Pure OCaml logic we want to reach from K. *)

let ml_add a b = a + b

let ml_fib n =
  let rec go a b n = if n <= 0 then a else go b (a + b) (n - 1) in
  go 0 1 n

let ml_upper s = String.uppercase_ascii s

let ml_describe s =
  Printf.sprintf "[ocaml] len=%d rev=%s"
    (String.length s)
    (String.init (String.length s) (fun i -> s.[String.length s - 1 - i]))

(* Expose them to the C side by name. *)
let () =
  Callback.register "ml_add" ml_add;
  Callback.register "ml_fib" ml_fib;
  Callback.register "ml_upper" ml_upper;
  Callback.register "ml_describe" ml_describe
