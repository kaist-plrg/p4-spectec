type error = El_latex.Error.error
type 'a result = ('a, error) Stdlib.result

let to_region_msg (error : error) : Util.Source.region * string =
  El_latex.Error.to_region_msg error

module El = struct
  type anchors = El_latex.Render.anchors

  let anchors = El_latex.Render.anchors

  let render_def ?(anchors : anchors option) (def : Lang.El.def) : string result
      =
    try Ok (El_latex.Render.render_def ?anchors def)
    with El_latex.Error.LatexError error -> Error error

  let render_defs ?(anchors : anchors option) (defs : Lang.El.def list) :
      string result =
    try Ok (El_latex.Render.render_defs ?anchors defs)
    with El_latex.Error.LatexError error -> Error error
end
