type error = El_latex.Error.error

let to_region_msg (error : error) : Util.Source.region * string =
  El_latex.Error.to_region_msg error

module El = struct
  include El_latex.Render

  exception LatexError = El_latex.Error.LatexError
end
