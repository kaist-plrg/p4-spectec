open Lang

(* Typed dispatch: [marshal_typed]/[unmarshal_typed] keyed on [Typ.t].
   A match on the head constructor of [typ.it] dispatches to the per-type
   [marshal_<name>] / [unmarshal_<name>] generated in the same Dispatch module.
   Structural arms handle bare [IterT]/[TupleT] heads inline. *)

(* [marshal_typed]/[unmarshal_typed]: Typ.t-dispatched persist bridge.
   Named-type arms delegate to the generated per-type functions; structural
   arms handle [IterT]/[TupleT] recursively without a named helper. *)
let compile_marshal_dispatch (typs : Sl.typ list) : Ml.funcdef list =
  let keys =
    List.filter_map
      (fun (t : Sl.typ) ->
        match t.it with
        | Il.VarT (id, _) -> Some (id.it, Naming.name t)
        | _ -> None)
      typs
    |> List.sort_uniq compare
  in
  let scrut = Ml.FieldE (Ml.VarE "typ", "it") in
  let var_pat key =
    Ml.LitP (Printf.sprintf "Il.VarT ({ it = %S; _ }, _)" key)
  in
  let marshal_arms_named =
    List.map
      (fun (key, iname) ->
        ( var_pat key,
          Ml.AppE
            ( Ml.LitE ("marshal_" ^ iname),
              [ Ml.AppE (Ml.LitE "Obj.obj", [ Ml.VarE "x" ]) ] ) ))
      keys
  in
  let unmarshal_arms_named =
    List.map
      (fun (key, iname) ->
        ( var_pat key,
          Ml.AppE
            ( Ml.LitE "Obj.repr",
              [ Ml.AppE (Ml.VarE ("unmarshal_" ^ iname), [ Ml.VarE "v" ]) ] )
        ))
      keys
  in
  (* Structural arms: IterT/TupleT — recurse via marshal_typed/unmarshal_typed *)
  let marshal_arm_list =
    ( Ml.LitP "Il.IterT (elem__, Il.List)",
      Ml.LitE
        "(let xs : Obj.t list = Obj.obj x in\n\
        \  Value.Make.list\n\
        \    (Typ.Make.iter elem__ Il.List)\n\
        \    (List.map (marshal_typed elem__) xs))" )
  in
  let marshal_arm_opt =
    ( Ml.LitP "Il.IterT (elem__, Il.Opt)",
      Ml.LitE
        "(let xo : Obj.t option = Obj.obj x in\n\
        \  Value.Make.opt\n\
        \    (Typ.Make.iter elem__ Il.Opt)\n\
        \    (Option.map (marshal_typed elem__) xo))" )
  in
  let marshal_arm_tuple =
    ( Ml.LitP "Il.TupleT elems__",
      Ml.LitE
        "(let xs : Obj.t list = Obj.obj x in\n\
        \  Value.Make.tuple\n\
        \    (Typ.Make.tuple elems__)\n\
        \    (List.map2 marshal_typed elems__ xs))" )
  in
  let unmarshal_arm_list =
    ( Ml.LitP "Il.IterT (elem__, Il.List)",
      Ml.LitE
        "(let vs = Value.Get.list v in\n\
        \  Obj.repr (List.map (unmarshal_typed elem__) vs))" )
  in
  let unmarshal_arm_opt =
    ( Ml.LitP "Il.IterT (elem__, Il.Opt)",
      Ml.LitE
        "(let vo = Value.Get.opt v in\n\
        \  Obj.repr (Option.map (unmarshal_typed elem__) vo))" )
  in
  let unmarshal_arm_tuple =
    ( Ml.LitP "Il.TupleT elems__",
      Ml.LitE
        "(let vs = Value.Get.tuple v in\n\
        \  Obj.repr (List.map2 unmarshal_typed elems__ vs))" )
  in
  let wild name =
    ( Ml.WildP,
      Ml.AppE
        ( Ml.LitE "failwith",
          [
            Ml.BinopE
              ( "^",
                Ml.StrE (name ^ ": unknown type "),
                Ml.AppE (Ml.LitE "Typ.to_string", [ Ml.VarE "typ" ]) );
          ] ) )
  in
  let marshal_arms =
    marshal_arms_named
    @ [ marshal_arm_list; marshal_arm_opt; marshal_arm_tuple; wild "marshal_typed" ]
  in
  let unmarshal_arms =
    unmarshal_arms_named
    @ [
        unmarshal_arm_list;
        unmarshal_arm_opt;
        unmarshal_arm_tuple;
        wild "unmarshal_typed";
      ]
  in
  [
    ( "marshal_typed",
      [],
      [ ("typ", Some (Ml.NameT "Typ.t")); ("x", Some (Ml.NameT "Obj.t")) ],
      Some (Ml.NameT "Value.t"),
      Ml.MatchE (scrut, marshal_arms) );
    ( "unmarshal_typed",
      [],
      [ ("typ", Some (Ml.NameT "Typ.t")); ("v", Some (Ml.NameT "Value.t")) ],
      Some (Ml.NameT "Obj.t"),
      Ml.MatchE (scrut, unmarshal_arms) );
  ]
