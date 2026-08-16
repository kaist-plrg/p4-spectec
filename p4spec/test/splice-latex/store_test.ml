open Lang
open Backend_splice

module Key = struct
  type t = string

  let to_string key = key
  let to_anchor key = key
  let compare = String.compare
  let parse = Parser.parse_ids
end

module Data = struct
  type t = string
end

module Init = struct
  type key = Key.t
  type value = Data.t

  let init (_spec_el : El.spec) (_spec_pl : Pl.spec) =
    [ ("alpha", "A"); ("beta", "B"); ("gamma", "C") ]
end

module Primary_value = struct
  type t = Data.t

  let render (_context : Ctx.t) values = "primary:" ^ String.concat "," values
end

module Alternate_value = struct
  type t = Data.t

  let render (_context : Ctx.t) values = "alternate:" ^ String.concat "," values
end

module Primary_config = struct
  let name = "primary"
  let prefix = "<primary>"
  let suffix = "</primary>"
  let header = false
end

module Alternate_config = struct
  let name = "alternate"
  let prefix = "<alternate>"
  let suffix = "</alternate>"
  let header = false
end

module Primary = Splicer.Make (Key) (Primary_value) (Init) (Primary_config)

module Alternate =
  Splicer.Make (Key) (Alternate_value) (Init) (Alternate_config)

let splice (module S : Splicer.SPLICER) content =
  let source = Source.{ file = "fixture.adoc"; s = content; i = 0 } in
  print_endline (S.splice source)

let () =
  Primary.init [] [];
  Alternate.init [] [];
  splice (module Primary) " alpha}";
  splice (module Alternate) " beta}";
  Primary.warn_unused ();
  Alternate.warn_unused ()
