(* State monad with failure *)

type ('s, 'a) t = 's -> 'a option * 's

let return (a : 'a) : ('s, 'a) t = fun s -> (Some a, s)

let bind (m : ('s, 'a) t) (f : 'a -> ('s, 'b) t) s =
  match m s with Some a, s -> f a s | None, s -> (None, s)

let map (m : ('s, 'a) t) (f : 'a -> 'b) = bind m (fun a -> return (f a))
let get : ('s, 's) t = fun s -> (Some s, s)
let put (x : 's) : ('s, unit) t = fun _ -> (Some (), x)
let modify (f : 's -> 's) : ('s, unit) t = fun s -> (Some (), f s)

let guard (cond : bool) : ('s, unit) t =
  if cond then return () else fun s -> (None, s)

let empty : ('s, 'a) t = fun s -> (None, s)
let run (m : ('s, 'a) t) (s : 's) : 'a option * 's = m s
let ( let* ) = bind
let ( let+ ) = map
let ( >> ) (ma : ('s, 'a) t) (mb : ('s, 'b) t) = bind ma (fun _ -> mb)

let ( <| ) (m : ('s, 'a -> 'b) t) (x : 'a) : ('s, 'b) t =
  let* f = m in
  return (f x)
