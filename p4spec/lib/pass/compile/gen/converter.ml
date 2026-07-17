let apply (tag : string) (expr_resolve_ml : Ml.expr) (expr_arg_ml : Ml.expr) :
    Ml.expr =
  let id_converter_ml = "converter__" ^ tag in
  Ml.LetE
    (Ml.VarP id_converter_ml, expr_resolve_ml, Ml.AppE (Ml.VarE id_converter_ml, [ expr_arg_ml ]))
