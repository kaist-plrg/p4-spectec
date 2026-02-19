open Domain
open Lib
open Lang
open Ol.Ast
module Typ = Runtime.Dynamic_Sl.Typ
open Util.Source

(* Remove redundant let and rule bindings from the code,
   which appears due to the concatenation of multiple rules and clauses *)

module Bind = struct
  (* Expression unit *)

  type expunit = exp * iterexp list

  let string_of_expunit (expunit : expunit) : string =
    let exp, iterexps = expunit in
    Format.asprintf "(%s)%s"
      (Sl.Print.string_of_exp exp)
      (Sl.Print.string_of_iterexps iterexps)

  let string_of_expunits (expunits : expunit list) : string =
    String.concat ", " (List.map string_of_expunit expunits)

  let eq_expunit (expunit_a : expunit) (expunit_b : expunit) : bool =
    let exp_a, iterexps_a = expunit_a in
    let exp_b, iterexps_b = expunit_b in
    Sl.Eq.eq_exp exp_a exp_b && Sl.Eq.eq_iterexps iterexps_a iterexps_b

  let eq_expunits (expunits_a : expunit list) (expunits_b : expunit list) : bool
      =
    List.length expunits_a = List.length expunits_b
    && List.for_all2 eq_expunit expunits_a expunits_b

  (* Binding *)

  type t =
    | LetBind of expunit * expunit
    | RuleBind of id * expunit list * expunit list

  let to_string (bind : t) : string =
    match bind with
    | LetBind (expunit_l, expunit_r) ->
        Format.asprintf "Let %s = %s"
          (string_of_expunit expunit_l)
          (string_of_expunit expunit_r)
    | RuleBind (id, expunits_input, expunits_output) ->
        Format.asprintf "%s |- %s : %s)" (Id.to_string id)
          (string_of_expunits expunits_input)
          (string_of_expunits expunits_output)

  (* Constructors *)

  let init_expunit (exp : exp) (iterexps : iterexp list) : expunit =
    let ids = Il.Free.free_exp exp in
    let iterexps =
      List.map
        (fun (iter, vars) ->
          let vars = List.filter (fun (id, _, _) -> IdSet.mem id ids) vars in
          (iter, vars))
        iterexps
    in
    (exp, iterexps)

  let init_let_bind (exp_l : exp) (exp_r : exp) (iterinstrs : iterinstr list) :
      t =
    let iterexps_bound, iterexps_bind =
      iterinstrs
      |> List.map (fun (iter, vars_bound, vars_bind) ->
             ((iter, vars_bound), (iter, vars_bind)))
      |> List.split
    in
    let expunit_l = init_expunit exp_l iterexps_bind in
    let expunit_r = init_expunit exp_r iterexps_bound in
    LetBind (expunit_l, expunit_r)

  let init_rule_bind (id : id) (notexp : notexp) (inputs : Hints.Input.t)
      (iterinstrs : iterinstr list) : t =
    let exps_input, exps_output =
      let _, exps = notexp in
      Hints.Input.split inputs exps
    in
    let iterexps_bound, iterexps_bind =
      iterinstrs
      |> List.map (fun (iter, vars_bound, vars_bind) ->
             ((iter, vars_bound), (iter, vars_bind)))
      |> List.split
    in
    let expunits_input =
      List.map
        (fun exp_input -> init_expunit exp_input iterexps_bound)
        exps_input
    in
    let expunits_output =
      List.map
        (fun exp_output -> init_expunit exp_output iterexps_bind)
        exps_output
    in
    RuleBind (id, expunits_input, expunits_output)

  (* Collapsing two bindings,
     if two bindings have syntactically equal right-hand sides,
     and the left-hand sides are equal up to renaming,
     then we can collapse them into a single binding *)

  let rec collapse_exp (renamer : Re.Renamer.t) (exp : exp) (exp_target : exp) :
      Re.Renamer.t option =
    match (exp.it, exp_target.it) with
    | VarE id, VarE id_target ->
        let renamer =
          if Sl.Eq.eq_id id id_target then renamer
          else Re.Renamer.add id_target id renamer
        in
        Some renamer
    | TupleE exps, TupleE exps_target -> collapse_exps renamer exps exps_target
    | CaseE (mixop, exps), CaseE (mixop_target, exps_target)
      when Sl.Eq.eq_mixop mixop mixop_target ->
        collapse_exps renamer exps exps_target
    | StrE expfields, StrE expfields_target ->
        let atoms, exps = List.split expfields in
        let atoms_target, exps_target = List.split expfields_target in
        if Sl.Eq.eq_atoms atoms atoms_target then
          collapse_exps renamer exps exps_target
        else None
    | OptE exp_opt, OptE exp_opt_target -> (
        match (exp_opt, exp_opt_target) with
        | Some exp, Some exp_target -> collapse_exp renamer exp exp_target
        | None, None -> Some renamer
        | _ -> None)
    | ListE exps, ListE exps_target -> collapse_exps renamer exps exps_target
    | ConsE (exp_head, exp_tail), ConsE (exp_head_target, exp_tail_target) ->
        let exps = [ exp_head; exp_tail ] in
        let exps_target = [ exp_head_target; exp_tail_target ] in
        collapse_exps renamer exps exps_target
    | IterE (exp, iterexp), IterE (exp_target, iterexp_target) -> (
        match collapse_exp renamer exp exp_target with
        | Some renamer ->
            let iterexp_target_renamed =
              Re.Renamer.rename_iterexp renamer iterexp_target
            in
            if Sl.Eq.eq_iterexp iterexp iterexp_target_renamed then Some renamer
            else None
        | None -> None)
    | _ -> None

  and collapse_exps (renamer : Re.Renamer.t) (exps : exp list)
      (exps_target : exp list) : Re.Renamer.t option =
    match (exps, exps_target) with
    | [], [] -> Some renamer
    | exp_h :: exps_t, exp_target_h :: exps_target_t -> (
        match collapse_exp renamer exp_h exp_target_h with
        | Some renamer -> collapse_exps renamer exps_t exps_target_t
        | None -> None)
    | _ -> None

  let collapse_expunit (renamer : Re.Renamer.t) (expunit : expunit)
      (expunit_target : expunit) : Re.Renamer.t option =
    let exp, iterexps = expunit in
    let exp_target, iterexps_target = expunit_target in
    let renamer_opt = collapse_exp renamer exp exp_target in
    match renamer_opt with
    | Some renamer ->
        let iterexps_target_renamed =
          Re.Renamer.rename_iterexps renamer iterexps_target
        in
        if Sl.Eq.eq_iterexps iterexps iterexps_target_renamed then Some renamer
        else None
    | None -> None

  let rec collapse_expunits (renamer : Re.Renamer.t) (expunits : expunit list)
      (expunits_target : expunit list) : Re.Renamer.t option =
    match (expunits, expunits_target) with
    | [], [] -> Some renamer
    | expunit_h :: expunits_t, expunit_target_h :: expunits_target_t -> (
        match collapse_expunit renamer expunit_h expunit_target_h with
        | Some renamer -> collapse_expunits renamer expunits_t expunits_target_t
        | None -> None)
    | _ -> None

  let collapse_bind (bind : t) (bind_target : t) : Re.Renamer.t option =
    match (bind, bind_target) with
    | ( LetBind (expunit_l, expunit_r),
        LetBind (expunit_target_l, expunit_target_r) )
      when eq_expunit expunit_r expunit_target_r ->
        collapse_expunit Re.Renamer.empty expunit_l expunit_target_l
    | ( RuleBind (id, expunits_input, expunits_output),
        RuleBind (id_target, expunits_target_input, expunits_target_output) )
      when Sl.Eq.eq_id id id_target
           && eq_expunits expunits_input expunits_target_input ->
        collapse_expunits Re.Renamer.empty expunits_output
          expunits_target_output
    | _ -> None
end

let downstream (bind : Bind.t) (block : block) : (block * block) option =
  match block with
  | { it = LetI (exp_l, exp_r, iterinstrs, block); _ } :: block_t -> (
      let bind_target = Bind.init_let_bind exp_l exp_r iterinstrs in
      let renamer_opt = Bind.collapse_bind bind bind_target in
      match renamer_opt with
      | Some renamer ->
          let block = Re.Renamer.rename_block renamer block in
          Some (block, block_t)
      | None -> None)
  | { it = RuleI (id, notexp, inputs, iterinstrs, block); _ } :: block_t -> (
      let bind_target = Bind.init_rule_bind id notexp inputs iterinstrs in
      let renamer_opt = Bind.collapse_bind bind bind_target in
      match renamer_opt with
      | Some renamer ->
          let block = Re.Renamer.rename_block renamer block in
          Some (block, block_t)
      | None -> None)
  | _ -> None

let rec upstream (block : block) : block =
  match block with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, block_then); at; _ } :: block_t ->
      let block_then = upstream block_then in
      let instr_h = IfI (exp_cond, iterexps, block_then) $ at in
      let block_t = upstream block_t in
      instr_h :: block_t
  | { it = HoldI (id, notexp, iterexps, block_hold, block_nothold); at; _ }
    :: block_t ->
      let block_hold = upstream block_hold in
      let block_nothold = upstream block_nothold in
      let instr_h =
        HoldI (id, notexp, iterexps, block_hold, block_nothold) $ at
      in
      let block_t = upstream block_t in
      instr_h :: block_t
  | { it = CaseI (exp, cases, total); at; _ } :: block_t ->
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map upstream blocks in
        List.combine guards blocks
      in
      let instr_h = CaseI (exp, cases, total) $ at in
      let block_t = upstream block_t in
      instr_h :: block_t
  | ({ it = GroupI (id_group, rel_signature, exps_group, block); _ } as instr_h)
    :: block_t ->
      let block = upstream block in
      let instr_h =
        GroupI (id_group, rel_signature, exps_group, block) $ instr_h.at
      in
      let block_t = upstream block_t in
      instr_h :: block_t
  | ({ it = LetI (exp_l, exp_r, iterinstrs, block); _ } as instr_h) :: block_t
    -> (
      let bind = Bind.init_let_bind exp_l exp_r iterinstrs in
      match downstream bind block_t with
      | Some (block_merge, block_t) ->
          let block = Merge.merge_block block block_merge in
          let instr_h = LetI (exp_l, exp_r, iterinstrs, block) $ instr_h.at in
          upstream (instr_h :: block_t)
      | None ->
          let block = upstream block in
          let instr_h = LetI (exp_l, exp_r, iterinstrs, block) $ instr_h.at in
          let block_t = upstream block_t in
          instr_h :: block_t)
  | ({ it = RuleI (id, notexp, inputs, iterinstrs, block); _ } as instr_h)
    :: block_t -> (
      let bind = Bind.init_rule_bind id notexp inputs iterinstrs in
      match downstream bind block_t with
      | Some (block_merge, block_t) ->
          let block = Merge.merge_block block block_merge in
          let instr_h =
            RuleI (id, notexp, inputs, iterinstrs, block) $ instr_h.at
          in
          upstream (instr_h :: block_t)
      | None ->
          let block = upstream block in
          let instr_h =
            RuleI (id, notexp, inputs, iterinstrs, block) $ instr_h.at
          in
          let block_t = upstream block_t in
          instr_h :: block_t)
  | instr_h :: block_t ->
      let block_t = upstream block_t in
      instr_h :: block_t

let apply (block : block) : block = upstream block
