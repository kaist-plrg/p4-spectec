open Il.Ast
open Error
open Runtime_static.Envs
open Util.Source

(* Collect binding identifiers,
   while enforcing shallow binding *)

let error_deep (exp : exp) =
  error exp.at
    (Format.asprintf "%s not allowed in shallow binding"
       (Il.Print.string_of_exp exp))

let error_deep_msg (exp : exp) (msg : string) =
  error exp.at
    (Format.asprintf "%s not allowed in shallow binding: %s"
       (Il.Print.string_of_exp exp)
       msg)

(* Expressions *)

let rec collect_exp (dctx : Dctx.t) (exp : exp) : Bind.BEnv.t =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> error_deep exp
  | VarE id ->
      if VEnv.mem id dctx.bounds then Bind.BEnv.empty
      else Bind.BEnv.singleton id (exp.note $ exp.at)
  | UnE _ | BinE _ | CmpE _ -> error_deep exp
  | UpCastE (_, ({ it = VarE _; _ } as exp))
  | UpCastE (_, ({ it = CaseE _; _ } as exp)) ->
      collect_exp dctx exp
  | UpCastE _ -> error_deep_msg exp "upcast"
  | DownCastE _ | SubE _ | MatchE _ ->
      error exp.at
        (Format.asprintf
           "downcast, subtype check, and match check expressions should appear \
            only after injection analysis")
  | TupleE exps -> collect_exps dctx exps
  | CaseE notexp ->
      let exps = notexp |> snd in
      if
        List.for_all
          (fun exp ->
            match exp.it with
            | VarE _ -> true
            | IterE _ -> true
            | _ -> error_deep_msg exp "case_inner")
          exps
      then collect_exps dctx exps
      else error_deep_msg exp "case"
  | StrE _ | OptE _ | ListE _ | ConsE _ | CatE _ | MemE _ | LenE _ | DotE _
  | IdxE _ | SliceE _ | UpdE _ | CallE _ ->
      error_deep exp
  | IterE (_, ((_, _ :: _) as iterexp)) ->
      error exp.at
        (Format.asprintf
           "iterated expression should initially have no annotations, but got \
            %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterE (exp, (iter, [])) ->
      let binds = collect_exp dctx exp in
      let binds = Bind.BEnv.map (Bind.Occ.add_iter iter) binds in
      binds

and collect_exps (dctx : Dctx.t) (exps : exp list) : Bind.BEnv.t =
  match exps with
  | [] -> Bind.BEnv.empty
  | exp :: exps ->
      let binds_h = collect_exp dctx exp in
      let binds_t = collect_exps dctx exps in
      Bind.BEnv.union binds_h binds_t

(* Arguments *)

and collect_arg (dctx : Dctx.t) (arg : arg) : Bind.BEnv.t =
  match arg.it with
  | ExpA exp -> collect_exp dctx exp
  | DefA _ ->
      error arg.at
        (Format.asprintf "definition argument not allowed in shallow binding")

and collect_args (dctx : Dctx.t) (args : arg list) : Bind.BEnv.t =
  match args with
  | [] -> Bind.BEnv.empty
  | arg :: args ->
      let binds_h = collect_arg dctx arg in
      let binds_t = collect_args dctx args in
      Bind.BEnv.union binds_h binds_t
