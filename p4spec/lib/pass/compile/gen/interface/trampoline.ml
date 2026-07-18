(* Trampoline helpers *)

(* Bind the current per-instance trampoline once at a function's entry, as the
   outermost [Chain.t] link — generated code reads [interface]/[extern] off [trampoline__] *)

let chain : Chain.t =
  Chain.make_let (Ml.VarP "trampoline__")
    (Ml.UnopE ("!", Ml.VarE "trampoline_cur__"))

(* Field accessors on [trampoline__] *)

let call_builtin : Ml.expr =
  Ml.FieldE (Ml.FieldE (Ml.VarE "trampoline__", "interface"), "call_builtin")

let eval_extern_func : Ml.expr =
  Ml.FieldE (Ml.FieldE (Ml.VarE "trampoline__", "extern"), "eval_extern_func")

let eval_extern_rel : Ml.expr =
  Ml.FieldE (Ml.FieldE (Ml.VarE "trampoline__", "extern"), "eval_extern_rel")
