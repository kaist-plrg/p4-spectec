open Source

exception ParseError of region * string
exception UnparseError of string
exception ElabError of region * string
exception StructError of region * string
exception ProseError of region * string
exception BuiltinError of region * string
exception InterpError of region * string
exception ArchError of region * string
exception StfError of string
exception SpliceError of region * string

let debug_errors = false

let string_of_error at msg =
  if at = no_region then msg else string_of_region at ^ ": " ^ msg

let warn (at : region) (category : string) (msg : string) =
  Printf.eprintf "%s\n%!" (string_of_error at (category ^ " warning: " ^ msg))

(* Parser errors *)

let error_parse (at : region) (msg : string) = raise (ParseError (at, msg))
let error_parse_no_region (msg : string) = raise (ParseError (no_region, msg))

(* Unparser errors *)

let error_unparse (msg : string) = raise (UnparseError msg)

(* Elaboration errors *)

let error_elab (at : region) (msg : string) = raise (ElabError (at, msg))
let warn_elab (at : region) (msg : string) = warn at "elab" msg

(* Structuring errors *)

let error_struct (at : region) (msg : string) = raise (StructError (at, msg))
let warn_struct (at : region) (msg : string) = warn at "struct" msg

(* Prosification errors *)

let error_prose (at : region) (msg : string) = raise (ProseError (at, msg))
let warn_prose (at : region) (msg : string) = warn at "prose" msg

(* Builtin errors *)

let error_builtin (at : region) (msg : string) = raise (BuiltinError (at, msg))
let warn_builtin (at : region) (msg : string) = warn at "builtin" msg

(* Interpreter errors *)

let error_interp (at : region) (msg : string) = raise (InterpError (at, msg))
let warn_interp (at : region) (msg : string) = warn at "interp" msg
let error_arch (at : region) (msg : string) = raise (ArchError (at, msg))
let warn_arch (at : region) (msg : string) = warn at "arch" msg
let error_stf (msg : string) = raise (StfError msg)

(* Splicer errors *)

let error_splice (at : region) (msg : string) = raise (SpliceError (at, msg))
let warn_splice (at : region) (msg : string) = warn at "splice" msg
