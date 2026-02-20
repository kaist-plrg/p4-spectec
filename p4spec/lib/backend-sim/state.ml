module Value = Runtime.Sim.Value
module IO = Runtime.Sim.Io

(* The state consists of:
    - current evaluation context
    - current arch
    - list of transmissions to perform *)

type s = Value.t * Value.t * IO.tx list

(* State monad with failure *)

type 'a state = s -> 'a option * s

(* Get, Put, Modify: state manipulation functions *)

let get : s state = fun s -> (Some s, s)
let put (x : s) : unit state = fun _ -> (Some (), x)
let modify (f : s -> s) : unit state = fun s -> (Some (), f s)

(* Return: wrap a value into the monad *)

let return (a : 'a) : 'a state = fun s -> (Some a, s)

(* Bind: sequence two computations in the monad *)

let bind (m : 'a state) (f : 'a -> 'b state) s =
  match m s with Some a, s -> f a s | None, s -> (None, s)

let ( >> ) (ma : 'a state) (mb : 'b state) = bind ma (fun _ -> mb)
let ( let* ) = bind

(* Map: apply a function to the result of a computation in the monad *)

let map (m : 'a state) (f : 'a -> 'b) = bind m (fun a -> return (f a))
let ( let+ ) = map

(* Sequence: run each state action from left to right and return accumulated result *)

let sequence (ms : 'a state list) : 'a list state =
  let rec seq acc = function
    | [] -> return (List.rev acc)
    | m :: ms ->
        let* x = m in
        seq (x :: acc) ms
  in
  seq [] ms

(* Combinators *)

(* If computation succeeds, run `some`. Otherwise, run `none` *)

let on_result (m : 'a state) ~(some : 'a -> 'b state) ~(none : unit -> 'b state)
    : 'b state =
 fun s ->
  let result, s' = m s in
  match result with Some x -> some x s' | None -> none () s'

(* Apply: apply a function to the current context and arch, updating them *)

let apply (f : Value.t -> Value.t -> Value.t * Value.t * 'a) : 'a state =
  let* value_ctx, value_arch, txs = get in
  let value_ctx, value_arch, result = f value_ctx value_arch in
  let+ () = put (value_ctx, value_arch, txs) in
  result

(* Guard: fail the computation if the condition is false *)

let guard (cond : bool) : unit state =
  if cond then return () else fun s -> (None, s)

let empty : 'a state = fun s -> (None, s)
let run (m : 'a state) (s : s) : 'a option * s = m s
