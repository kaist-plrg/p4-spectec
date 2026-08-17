module El = struct
  include El_latex.Render

  exception LatexError = El_latex.Error.LatexError
end
