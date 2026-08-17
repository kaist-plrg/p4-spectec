open Domain
open Lang
open El
open Util.Source

module Make (At : sig
  val at : region
end) =
struct
  let phrase it = it $ At.at
  let id name = phrase name
  let atom value = phrase value
  let plaintyp value = phrase value
  let nottyp value = phrase value
  let deftyp value = phrase value
  let exp value = phrase value
  let path value = phrase value
  let arg value = phrase value
  let param value = phrase value
  let prem value = phrase value
  let rule value = phrase value
  let row value = phrase value
  let tablerow value = phrase value
  let def value = phrase value
  let var name = exp (VarE (id name))
  let named_type name = plaintyp (VarT (id name, []))
end
