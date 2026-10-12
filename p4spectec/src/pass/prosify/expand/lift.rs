//! Lift nested SL calls and wrap their owning instruction in let bindings
//!
//! `lift_instr` repeatedly removes the leftmost eligible call from one
//! instruction. Expression and path dispatchers preserve the iteration state
//! needed to rebuild the surrounding let instructions.
//!
//! For example, `let z = $f($g(x))` becomes
//! `let y = $g(x) { let z = $f(y) }`: the nested call is bound first,
//! and the outer call keeps its place.
//! A call lifted out of an iteration keeps that dimension, so `$f(x)` under
//! `(...)*` binds a fresh `y*` and is read back as `y` inside the iteration.

use crate::lang::{
    common::ds::{map::IdMap, set::IdSet},
    hints::input,
    traits::{eq::SyntaxEq, free::FreeVars},
};

use crate::lang::il::{self, ast as il_ast};

use crate::lang::sl::ast as sl;

// == Call lifting

/// Tracks call nesting along one expression path.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CallNesting {
    /// Nothing entered yet.
    None,
    /// The root call itself, which stays in place.
    Outer,
    /// Below the root, where a call is lifted.
    Nested,
}

impl CallNesting {
    /// Updates the nesting state at an expression boundary.
    fn enter_exp(self, exp_kind_sl: &sl::ExpKind) -> Self {
        match exp_kind_sl {
            // A root call is outer, any other call nested
            il_ast::ExpKind::Call(_, _, _) => match self {
                Self::None => Self::Outer,
                Self::Outer | Self::Nested => Self::Nested,
            },
            // Iterations are transparent
            il_ast::ExpKind::Iter(_, _) => self,
            // Any operator makes its children nested
            _ => Self::Nested,
        }
    }
}

/// Describes one call removed from an expression and its replacement binding.
struct LiftedCall {
    /// The fresh variable standing where the call was.
    exp_new_sl: sl::Exp,
    /// The call removed from the expression.
    exp_call_sl: sl::Exp,
    /// Variables the call reads, with the iterations crossed so far.
    vars_call_sl: Vec<sl::Var>,
    /// The fresh variable, with the iterations crossed so far.
    var_new_sl: sl::Var,
    /// Iterations crossed while lifting, innermost first.
    iter_exps_enclosing_sl: Vec<sl::ExpIter>,
}

impl LiftedCall {
    /// Moves this call out of one enclosing iteration.
    fn lift_out_of_iter(
        mut self,
        vars_remaining_sl: &mut [sl::Var],
        iter: sl::Iter,
        vars_bound_sl: &mut Vec<sl::Var>,
    ) -> Self {
        // Find iteration variables used inside the lifted call
        let vars_call_matched_sl = self
            .vars_call_sl
            .iter()
            .filter(|var_call_sl| {
                vars_bound_sl
                    .iter()
                    .any(|var_bound_sl| var_bound_sl.syntax_eq(var_call_sl))
            })
            .cloned()
            .collect::<Vec<_>>();
        let vars_remaining_matched_sl = vars_remaining_sl
            .iter()
            .filter(|var_remaining_sl| {
                vars_bound_sl
                    .iter()
                    .any(|var_bound_sl| var_bound_sl.syntax_eq(var_remaining_sl))
            })
            .cloned()
            .collect::<Vec<_>>();

        // Carry remaining variables to the next enclosing iteration
        for var_remaining_sl in vars_remaining_sl {
            if vars_remaining_matched_sl
                .iter()
                .any(|var_matched_sl| var_matched_sl.syntax_eq(var_remaining_sl))
            {
                var_remaining_sl.iters.push(iter);
            }
        }
        // A call using no iteration variable is unaffected by this iteration
        if vars_call_matched_sl.is_empty() {
            return self;
        }

        // Extend matched call variables for this iteration
        for var_call_sl in &mut self.vars_call_sl {
            if vars_call_matched_sl
                .iter()
                .any(|var_matched_sl| var_matched_sl.syntax_eq(var_call_sl))
            {
                var_call_sl.iters.push(iter);
            }
        }

        // Partition bindings between the lifted call and remaining expression
        let vars_iter_sl = vars_bound_sl
            .iter()
            .filter(|var_bound_sl| {
                vars_call_matched_sl
                    .iter()
                    .any(|var_matched_sl| var_matched_sl.syntax_eq(var_bound_sl))
            })
            .cloned()
            .collect::<Vec<_>>();
        let vars_kept_sl = vars_bound_sl
            .iter()
            .filter(|var_bound_sl| {
                !vars_call_matched_sl
                    .iter()
                    .any(|var_matched_sl| var_matched_sl.syntax_eq(var_bound_sl))
                    || vars_remaining_matched_sl
                        .iter()
                        .any(|var_matched_sl| var_matched_sl.syntax_eq(var_bound_sl))
            })
            .cloned()
            .collect::<Vec<_>>();

        // Record the iteration on the fresh variable and replacement binding
        let var_new_inner_sl = self.var_new_sl.clone();
        self.var_new_sl.iters.push(iter);
        self.iter_exps_enclosing_sl
            .push(sl::ExpIter { iter, vars: vars_iter_sl });
        *vars_bound_sl = std::iter::once(var_new_inner_sl)
            .chain(vars_kept_sl)
            .collect();
        self
    }

    /// Wraps an instruction in the let binding for this lifted call.
    fn wrap_instr(self, instr_sl: sl::Instr) -> sl::Instr {
        let Self { exp_new_sl, exp_call_sl, var_new_sl, iter_exps_enclosing_sl, .. } = self;
        // Separate callee dimensions from iterations crossed during lifting
        let num_iters_enclosing = iter_exps_enclosing_sl.len();
        let num_iters_callee = var_new_sl.iters.len() - num_iters_enclosing;
        let mut var_bind_sl = sl::Var {
            id: var_new_sl.id,
            typ: var_new_sl.typ,
            iters: var_new_sl.iters[..num_iters_callee].to_vec(),
        };

        // Rebuild instruction iterations from the inside out
        let mut iter_instrs_sl = Vec::new();
        for sl::ExpIter { iter, vars: vars_bound_sl } in iter_exps_enclosing_sl {
            iter_instrs_sl.push(sl::InstrIter {
                iter,
                vars_bound: vars_bound_sl,
                vars_bind: vec![var_bind_sl.clone()],
            });
            var_bind_sl.iters.push(iter);
        }

        // Bind the extracted call immediately before its owning instruction
        let span = instr_sl.span.clone();
        crate::phrase! {
            node: sl::InstrKind::Let(sl::LetInstr {
                exp_l: exp_new_sl,
                exp_r: exp_call_sl,
                iter_instrs: iter_instrs_sl,
                block: vec![instr_sl],
            }),
            span: span,
        }
    }
}

// == Expressions

// - Expression

/// Lifts the leftmost nested call from an expression.
fn lift_from_exp(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    exp_target_sl: &mut sl::Exp,
) -> Option<LiftedCall> {
    let nesting = nesting.enter_exp(&exp_target_sl.node);
    if nesting == CallNesting::Nested
        && let Some(call_lifted) = try_lift_call(ids_used, exp_target_sl)
    {
        return Some(call_lifted);
    }
    // Continue into children in source order
    lift_from_exp_kind(ids_used, nesting, &mut exp_target_sl.node)
}

/// Dispatches the search over an expression's children in source order.
fn lift_from_exp_kind(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    exp_kind_sl: &mut sl::ExpKind,
) -> Option<LiftedCall> {
    match exp_kind_sl {
        il_ast::ExpKind::Bool(_)
        | il_ast::ExpKind::Num(_)
        | il_ast::ExpKind::Text(_)
        | il_ast::ExpKind::Id(_) => None,
        il_ast::ExpKind::Un(_, _, exp_inner_sl)
        | il_ast::ExpKind::UpCast(_, exp_inner_sl)
        | il_ast::ExpKind::DownCast(_, exp_inner_sl)
        | il_ast::ExpKind::Sub(exp_inner_sl, _, _)
        | il_ast::ExpKind::Match(exp_inner_sl, _)
        | il_ast::ExpKind::Len(exp_inner_sl)
        | il_ast::ExpKind::Dot(exp_inner_sl, _) => lift_from_exp(ids_used, nesting, exp_inner_sl),
        il_ast::ExpKind::Bin(_, _, exp_l_sl, exp_r_sl)
        | il_ast::ExpKind::Cmp(_, _, exp_l_sl, exp_r_sl)
        | il_ast::ExpKind::Cons(exp_l_sl, exp_r_sl)
        | il_ast::ExpKind::Cat(exp_l_sl, exp_r_sl)
        | il_ast::ExpKind::Mem(exp_l_sl, exp_r_sl)
        | il_ast::ExpKind::Idx(exp_l_sl, exp_r_sl) => {
            lift_from_binary_exp(ids_used, nesting, exp_l_sl, exp_r_sl)
        }
        il_ast::ExpKind::Tuple(exps_sl) | il_ast::ExpKind::List(exps_sl) => {
            lift_from_exps(ids_used, nesting, exps_sl)
        }
        il_ast::ExpKind::Case(not_exp_sl) => lift_from_case_exp(ids_used, nesting, not_exp_sl),
        il_ast::ExpKind::Str(fields_sl) => lift_from_struct_exp(ids_used, nesting, fields_sl),
        il_ast::ExpKind::Opt(Some(exp_sl)) => lift_from_exp(ids_used, nesting, exp_sl),
        il_ast::ExpKind::Opt(None) => None,
        il_ast::ExpKind::Slice(exp_base_sl, exp_idx_sl, exp_len_sl) => {
            lift_from_slice_exp(ids_used, nesting, exp_base_sl, exp_idx_sl, exp_len_sl)
        }
        il_ast::ExpKind::Upd(exp_base_sl, path_sl, exp_field_sl) => {
            lift_from_update_exp(ids_used, nesting, exp_base_sl, path_sl, exp_field_sl)
        }
        il_ast::ExpKind::Call(_, _, args_sl) => lift_from_call_exp(ids_used, nesting, args_sl),
        il_ast::ExpKind::Iter(exp_inner_sl, iter_exp_sl) => {
            lift_from_iter_exp(ids_used, nesting, exp_inner_sl, iter_exp_sl)
        }
    }
}

/// Lifts the leftmost nested call from a sequence of expressions.
fn lift_from_exps(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    exps_sl: &mut [sl::Exp],
) -> Option<LiftedCall> {
    // Search expressions in source order
    for exp_sl in exps_sl {
        if let Some(call_lifted) = lift_from_exp(ids_used, nesting, exp_sl) {
            return Some(call_lifted);
        }
    }
    None
}

// - Call extraction

/// Extracts a non-nullary call at the expression root.
fn try_lift_call(ids_used: &mut IdSet, exp_target_sl: &mut sl::Exp) -> Option<LiftedCall> {
    // Reject expressions that do not need a replacement binding
    let il_ast::ExpKind::Call(_, _, args_sl) = &exp_target_sl.node else {
        return None;
    };
    // Nullary calls read like constants and stay
    if args_sl.is_empty() {
        return None;
    }

    // Allocate the replacement variable from the call result type
    let typ_sl = crate::phrase! {
        node: exp_target_sl.note.as_ref().clone(),
        span: exp_target_sl.span.clone(),
    };
    let var_new_sl =
        il::fresh::var_from_typ(&IdMap::new(), ids_used, exp_target_sl.span.clone(), &typ_sl);
    ids_used.insert(var_new_sl.id.clone());
    let exp_new_sl = il::var::as_exp(true, &var_new_sl);
    let exp_call_sl = std::mem::replace(exp_target_sl, exp_new_sl.clone());
    let il_ast::ExpKind::Call(_, _, args_sl) = &exp_call_sl.node else {
        unreachable!();
    };

    // Track variables needed inside the call while crossing iterations
    let vars_call_sl = args_sl.as_slice().free_vars();
    Some(LiftedCall {
        exp_new_sl,
        exp_call_sl,
        vars_call_sl,
        var_new_sl,
        iter_exps_enclosing_sl: Vec::new(),
    })
}

// - Binary expression

/// Searches the left operand, then the right.
fn lift_from_binary_exp(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    exp_l_sl: &mut sl::Exp,
    exp_r_sl: &mut sl::Exp,
) -> Option<LiftedCall> {
    // Search the left operand first
    if let Some(call_lifted) = lift_from_exp(ids_used, nesting, exp_l_sl) {
        return Some(call_lifted);
    }

    // Search the right operand second
    lift_from_exp(ids_used, nesting, exp_r_sl)
}

// - Case expression

/// Searches the notation's arguments, then writes them back.
fn lift_from_case_exp(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    not_exp_sl: &mut sl::NotExp,
) -> Option<LiftedCall> {
    let mut exps_sl = not_exp_sl.args().to_vec();
    let call_lifted = lift_from_exps(ids_used, nesting, &mut exps_sl)?;
    let mut exps_sl = exps_sl.into_iter();
    *not_exp_sl = not_exp_sl.map(|_| exps_sl.next().expect("lifting preserves notation arity"));
    Some(call_lifted)
}

// - Struct expression

/// Searches the field values in source order.
fn lift_from_struct_exp(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    fields_sl: &mut [il_ast::ExpField],
) -> Option<LiftedCall> {
    // Search field values in source order
    for il_ast::ExpField { exp: exp_sl, .. } in fields_sl {
        if let Some(call_lifted) = lift_from_exp(ids_used, nesting, exp_sl) {
            return Some(call_lifted);
        }
    }
    None
}

// - Slice expression

/// Searches base, index, then length.
fn lift_from_slice_exp(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    exp_base_sl: &mut sl::Exp,
    exp_idx_sl: &mut sl::Exp,
    exp_len_sl: &mut sl::Exp,
) -> Option<LiftedCall> {
    // Search the base before slice operands
    if let Some(call_lifted) = lift_from_exp(ids_used, nesting, exp_base_sl) {
        return Some(call_lifted);
    }

    // Search the index before the length
    if let Some(call_lifted) = lift_from_exp(ids_used, nesting, exp_idx_sl) {
        return Some(call_lifted);
    }

    // Search the length last
    lift_from_exp(ids_used, nesting, exp_len_sl)
}

// - Update expression

/// Searches base, path, then replacement field.
fn lift_from_update_exp(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    exp_base_sl: &mut sl::Exp,
    path_sl: &mut sl::Path,
    exp_field_sl: &mut sl::Exp,
) -> Option<LiftedCall> {
    // Search the base before the update path and field
    if let Some(call_lifted) = lift_from_exp(ids_used, nesting, exp_base_sl) {
        return Some(call_lifted);
    }

    // Search the path before the replacement field
    if let Some(call_lifted) = lift_from_path(ids_used, nesting, path_sl) {
        return Some(call_lifted);
    }

    // Search the replacement field last
    lift_from_exp(ids_used, nesting, exp_field_sl)
}

// - Call expression

/// Searches the expression arguments; function arguments hold no calls.
fn lift_from_call_exp(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    args_sl: &mut [sl::Arg],
) -> Option<LiftedCall> {
    // Search expression arguments in source order
    for arg_sl in args_sl {
        let il_ast::ArgKind::Exp(exp_sl) = &mut arg_sl.node else {
            continue;
        };
        if let Some(call_lifted) = lift_from_exp(ids_used, nesting, exp_sl) {
            return Some(call_lifted);
        }
    }
    None
}

// - Iterated expression

/// Lifts a call out of an iteration while preserving its variable dimensions.
fn lift_from_iter_exp(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    exp_inner_sl: &mut sl::Exp,
    iter_exp_sl: &mut sl::ExpIter,
) -> Option<LiftedCall> {
    let call_lifted = lift_from_exp(ids_used, nesting, exp_inner_sl)?;
    let mut vars_remaining_sl = exp_inner_sl.free_vars();
    Some(call_lifted.lift_out_of_iter(
        &mut vars_remaining_sl,
        iter_exp_sl.iter,
        &mut iter_exp_sl.vars,
    ))
}

// == Paths

/// Lifts the leftmost nested call from a path.
fn lift_from_path(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    path_sl: &mut sl::Path,
) -> Option<LiftedCall> {
    match &mut path_sl.node {
        il_ast::PathKind::Root => None,
        // The inner path first, then the index
        il_ast::PathKind::Idx(path_inner_sl, exp_idx_sl) => {
            if let Some(call_lifted) = lift_from_path(ids_used, nesting, path_inner_sl) {
                return Some(call_lifted);
            }
            lift_from_exp(ids_used, nesting, exp_idx_sl)
        }
        il_ast::PathKind::Slice(path_inner_sl, exp_idx_sl, exp_len_sl) => {
            lift_from_slice_path(ids_used, nesting, path_inner_sl, exp_idx_sl, exp_len_sl)
        }
        il_ast::PathKind::Dot(path_inner_sl, _) => lift_from_path(ids_used, nesting, path_inner_sl),
    }
}

/// Searches the inner path, then index, then length.
fn lift_from_slice_path(
    ids_used: &mut IdSet,
    nesting: CallNesting,
    path_inner_sl: &mut sl::Path,
    exp_idx_sl: &mut sl::Exp,
    exp_len_sl: &mut sl::Exp,
) -> Option<LiftedCall> {
    // Search the inner path before slice operands
    if let Some(call_lifted) = lift_from_path(ids_used, nesting, path_inner_sl) {
        return Some(call_lifted);
    }

    // Search the index before the length
    if let Some(call_lifted) = lift_from_exp(ids_used, nesting, exp_idx_sl) {
        return Some(call_lifted);
    }

    // Search the length last
    lift_from_exp(ids_used, nesting, exp_len_sl)
}

// == Instructions

// - Instruction

/// Finds the next call an instruction owns directly, if any.
fn lift_from_instr(ids_used: &mut IdSet, instr_sl: &mut sl::Instr) -> Option<LiftedCall> {
    match &mut instr_sl.node {
        sl::InstrKind::Let(instr_sl) => lift_from_let_instr(ids_used, instr_sl),
        sl::InstrKind::Rule(instr_sl) => lift_from_rule_instr(ids_used, instr_sl),
        sl::InstrKind::Hold(instr_sl) => lift_from_hold_instr(ids_used, instr_sl),
        // Results and returns own their expressions at the root
        sl::InstrKind::Result(instr_sl) => {
            lift_from_exps(ids_used, CallNesting::None, &mut instr_sl.exps)
        }
        sl::InstrKind::Return(instr_sl) => {
            lift_from_exp(ids_used, CallNesting::None, &mut instr_sl.exp)
        }
        // Conditions, cases, groups, and debugs keep their calls
        sl::InstrKind::If(_)
        | sl::InstrKind::Case(_)
        | sl::InstrKind::Group(_)
        | sl::InstrKind::Debug(_) => None,
    }
}

// - Let instruction

/// Lifts the leftmost nested call from a let instruction's right-hand side.
fn lift_from_let_instr(ids_used: &mut IdSet, instr_sl: &mut sl::LetInstr) -> Option<LiftedCall> {
    let mut call_lifted = lift_from_exp(ids_used, CallNesting::None, &mut instr_sl.exp_r)?;
    let mut vars_remaining_sl = instr_sl.exp_r.free_vars();
    // Carry the call out through each instruction iteration
    for iter_instr_sl in &mut instr_sl.iter_instrs {
        call_lifted = call_lifted.lift_out_of_iter(
            &mut vars_remaining_sl,
            iter_instr_sl.iter,
            &mut iter_instr_sl.vars_bound,
        );
    }
    Some(call_lifted)
}

// - Rule instruction

/// Lifts the leftmost eligible call from a rule instruction's inputs.
fn lift_from_rule_instr(ids_used: &mut IdSet, instr_sl: &mut sl::RuleInstr) -> Option<LiftedCall> {
    // Separate relation inputs from result positions
    let exps_sl = instr_sl.not_exp.args().to_vec();
    let (mut exps_input_sl, exps_output_sl) = input::split(&instr_sl.input_hint, exps_sl)
        .expect("elaboration validates relation inputs; lifting preserves notation arity");
    let call_lifted = lift_from_exps(ids_used, CallNesting::Outer, &mut exps_input_sl);

    // Restore notation only when an input changed
    let call_lifted = call_lifted?;
    let mut vars_remaining_sl = exps_input_sl.as_slice().free_vars();
    let mut call_lifted = call_lifted;
    // Carry the call out through each instruction iteration
    for iter_instr_sl in &mut instr_sl.iter_instrs {
        call_lifted = call_lifted.lift_out_of_iter(
            &mut vars_remaining_sl,
            iter_instr_sl.iter,
            &mut iter_instr_sl.vars_bound,
        );
    }

    // Restore the relation notation after changing one input
    let exps_sl = input::combine(&instr_sl.input_hint, exps_input_sl, exps_output_sl)
        .expect("elaboration validates relation inputs; lifting preserves notation arity");
    let mut exps_sl = exps_sl.into_iter();
    instr_sl.not_exp = instr_sl
        .not_exp
        .map(|_| exps_sl.next().expect("lifting preserves notation arity"));
    Some(call_lifted)
}

// - Hold instruction

/// Lifts the leftmost eligible call from a hold instruction's arguments.
fn lift_from_hold_instr(ids_used: &mut IdSet, instr_sl: &mut sl::HoldInstr) -> Option<LiftedCall> {
    // Lift only calls owned by notation arguments
    let mut exps_sl = instr_sl.not_exp.args().to_vec();
    let call_lifted = lift_from_exps(ids_used, CallNesting::Outer, &mut exps_sl);

    // Restore notation only when an argument changed
    let call_lifted = call_lifted?;
    let mut vars_remaining_sl = exps_sl.as_slice().free_vars();
    let mut call_lifted = call_lifted;
    for iter_exp_sl in &mut instr_sl.iter_exps {
        call_lifted = call_lifted.lift_out_of_iter(
            &mut vars_remaining_sl,
            iter_exp_sl.iter,
            &mut iter_exp_sl.vars,
        );
    }
    let mut exps_sl = exps_sl.into_iter();
    instr_sl.not_exp = instr_sl
        .not_exp
        .map(|_| exps_sl.next().expect("lifting preserves notation arity"));
    Some(call_lifted)
}

// == Entry point

/// Lifts all calls directly owned by an instruction.
///
/// The boolean reports whether the returned instruction contains new bindings.
pub(super) fn lift_instr(ids_used: &mut IdSet, mut instr_sl: sl::Instr) -> (sl::Instr, bool) {
    let mut calls_lifted = Vec::new();
    // Collect direct calls before nesting their bindings
    while let Some(call_lifted) = lift_from_instr(ids_used, &mut instr_sl) {
        calls_lifted.push(call_lifted);
    }
    let lifted = !calls_lifted.is_empty();

    // Preserve left-to-right evaluation in the nesting order of let bindings
    for call_lifted in calls_lifted.into_iter().rev() {
        instr_sl = call_lifted.wrap_instr(instr_sl);
    }
    (instr_sl, lifted)
}
