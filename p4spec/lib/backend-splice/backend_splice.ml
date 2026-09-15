module Anchor = Anchor
module Ctx = Ctx
module Driver = Driver
module Error = Error
module Parser = Parser
module Source = Source
module Splicer = Splicer
module Splicers = Splicers

type error = Diagnostic.t

let splice_files (spec_el : Lang.El.spec) (spec_pl : Lang.Pl.spec)
    (path_pairs : (string * string) list) : (unit, error) result =
  try
    Driver.splice_files spec_el spec_pl path_pairs;
    Ok ()
  with Error.SpliceError error -> Error error
