(* Sorted directory traversal skips include directories. *)
(* [include_file] is included in directory order even if absent on disk. *)
(* [include_file] must use the supplied directory's path spelling. *)
val collect : ?include_file:string -> string -> string list

(* The nearest ancestor containing a *.spec file defines the root. *)
val root_of_file : string -> string option
