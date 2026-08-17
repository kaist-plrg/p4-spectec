module Anchor = Anchor
module Ctx = Ctx
module Driver = Driver
module Error = Error
module Parser = Parser
module Source = Source
module Splicer = Splicer
module Splicers = Splicers

type error = Error.error

let to_region_msg (error : error) : Util.Source.region * string =
  Error.to_region_msg error

let splice_files (spec_el : Lang.El.spec) (spec_pl : Lang.Pl.spec)
    (path_pairs : (string * string) list) : (unit, error) result =
  try
    Driver.splice_files spec_el spec_pl path_pairs;
    Ok ()
  with
  | Error.SpliceError error -> Error error
  | Backend_latex.El.LatexError error ->
      let at, msg = Backend_latex.to_region_msg error in
      let error : error = { Error.at; msg } in
      Error error
