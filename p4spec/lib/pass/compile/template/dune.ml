(* Generated [compiled/dune]
   [-opaque] keeps the [.cmx] jobs parallel despite the linear cmi chain *)

let dune (name : string) : string =
  Printf.sprintf
    "(library\n\
    \ (name %s)\n\
    \ (public_name p4spectec.%s)\n\
    \ (libraries util domain frontend lang pass runtime)\n\
    \ (ocamlopt_flags (:standard -opaque)))\n"
    (Split.name_lib name) (Split.name_lib name)
