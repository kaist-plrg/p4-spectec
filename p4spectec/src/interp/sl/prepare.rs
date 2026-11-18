//! Lower SL rule calls while resolving slots and interning mixops
//!
//! A rule call whose continuation only returns its outputs becomes a result,
//! and results and returns conclude with values or with tail calls.
//! Tail position follows the interpreter's selection mode and otherwise blocks;
//! table rows form one sequential block even in deterministic mode.
//! Only a tail-position rule call that immediately returns its outputs,
//! or a returned call in tail position, is lowered, and only under `tco`.
//! Instructions are numbered in pre-order;
//! without `tco` the numbering matches the source pre-order.
//! Defined relations, table functions, and defined functions become callables
//! with their own frame layouts.

use std::{mem, rc::Rc};

use crate::lang::{
    data::{callable::Callable, frame::FrameLayout, notation::MixopArena},
    hints::input,
    traits::eq::SyntaxEq,
};

use crate::lang::sl::{ast as source, prepared as ast};

use crate::runtime::{envs::elab::TDEnv, typdef::TypeDef};

use crate::interp::shared::prepare::{
    Prepare,
    il::{self, ExpIterFor},
};

// - Preparation context

/// Records SL slots and interned mixops.
pub struct PrepareContext<'a> {
    /// The specification's notation arena.
    pub arena_mixop: &'a mut MixopArena,
    /// Source type definitions, with the callable's type parameters.
    pub tdenv: &'a TDEnv,
    /// The frame layout of the unit being prepared.
    pub layout: &'a mut FrameLayout,
    /// The runner's instruction-selection mode.
    pub det: bool,
    /// Lower tail-position relation calls into tail calls.
    pub tco: bool,
    /// The next instruction id, shared across definitions.
    pub counter_instr: &'a mut u32,
    pos: Position,
}

impl<'a> PrepareContext<'a> {
    /// Borrows the arena, type definitions, and layout for preparation.
    pub fn new(
        arena_mixop: &'a mut MixopArena,
        tdenv: &'a TDEnv,
        layout: &'a mut FrameLayout,
        counter_instr: &'a mut u32,
        det: bool,
        tco: bool,
    ) -> Self {
        Self { arena_mixop, tdenv, layout, det, tco, counter_instr, pos: Position::Tail }
    }

    fn next_instr_id(&mut self) -> ast::InstrId {
        let id = ast::InstrId(*self.counter_instr);
        *self.counter_instr += 1;
        id
    }

    fn il(&mut self) -> il::PrepareContext<'_> {
        self.il_exp_iter_for(ExpIterFor::Eval)
    }

    fn il_exp_iter_for(&mut self, exp_iter_for: ExpIterFor) -> il::PrepareContext<'_> {
        il::PrepareContext {
            arena_mixop: &mut *self.arena_mixop,
            tdenv: self.tdenv,
            layout: &mut *self.layout,
            exp_iter_for,
        }
    }
}

// - Execution position

#[derive(Clone, Copy, PartialEq, Eq)]
enum Position {
    Tail,
    NonTail,
}

impl PrepareContext<'_> {
    fn at(&mut self, pos: Position) -> PrepareContext<'_> {
        PrepareContext {
            arena_mixop: &mut *self.arena_mixop,
            tdenv: self.tdenv,
            layout: &mut *self.layout,
            det: self.det,
            tco: self.tco,
            counter_instr: &mut *self.counter_instr,
            pos,
        }
    }
}

// = Blocks

impl Prepare<PrepareContext<'_>> for source::Block {
    type Output = ast::Block;

    /// Prepares a block using the evaluator's tail-position rule.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let instrs_len = self.instrs.len();
        let mut instrs = Vec::with_capacity(instrs_len);
        for (idx, instr) in self.instrs.into_iter().enumerate() {
            // Deterministic selection gives every candidate the enclosing position
            let pos = if ctx.det || idx + 1 == instrs_len { ctx.pos } else { Position::NonTail };
            instrs.push(prepare_instr(instr, &mut ctx.at(pos)));
        }
        ast::Block { instrs }
    }
}

// = Instructions

/// Numbers the instruction before its nested instructions.
fn prepare_instr(instr: source::Instr, ctx: &mut PrepareContext<'_>) -> ast::Instr {
    // Grow the stack for deep syntax trees
    stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
        let note = ctx.next_instr_id();
        let node = instr.node.prepare(ctx);
        ast::Instr { node, note, span: instr.span }
    })
}

impl Prepare<PrepareContext<'_>> for source::InstrKind {
    type Output = ast::InstrKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::InstrKind::If(instr) => ast::InstrKind::If(instr.prepare(ctx)),
            source::InstrKind::Hold(instr) => ast::InstrKind::Hold(instr.prepare(ctx)),
            source::InstrKind::Case(instr) => ast::InstrKind::Case(instr.prepare(ctx)),
            source::InstrKind::Group(instr) => ast::InstrKind::Group(instr.prepare(ctx)),
            source::InstrKind::Let(instr) => ast::InstrKind::Let(instr.prepare(ctx)),
            source::InstrKind::Rule(instr) => instr.prepare(ctx),
            source::InstrKind::Result(instr) => ast::InstrKind::Result(instr.prepare(ctx)),
            source::InstrKind::Return(instr) => ast::InstrKind::Return(instr.prepare(ctx)),
            source::InstrKind::Debug(instr) => ast::InstrKind::Debug(instr.prepare(ctx)),
        }
    }
}

// - If instruction

impl Prepare<PrepareContext<'_>> for source::IfInstr {
    type Output = ast::IfInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let exp = self.exp.prepare(&mut ctx.il());
        let iter_exps = self.iter_exps.prepare(&mut ctx.il());
        let block = self.block.prepare(ctx);
        ast::IfInstr { exp, iter_exps, block, dangle: self.dangle }
    }
}

// - Hold instruction

impl Prepare<PrepareContext<'_>> for source::HoldInstr {
    type Output = ast::HoldInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let not_exp = self.not_exp.prepare(&mut ctx.il());
        let iter_exps = self.iter_exps.prepare(&mut ctx.il());
        let hold_case = self.hold_case.prepare(ctx);
        ast::HoldInstr { id: self.id, not_exp, iter_exps, hold_case }
    }
}

// - Case instruction

impl Prepare<PrepareContext<'_>> for source::CaseInstr {
    type Output = ast::CaseInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let exp = self.exp.prepare(&mut ctx.il());
        // Guards test the scrutinee, so their checks start from its type
        let typ_scrut = crate::phrase!(node: exp.note.as_ref().clone(), span: exp.span);
        let mut cases = Vec::with_capacity(self.cases.len());
        for case in self.cases {
            let guard = ast::Guard::prepare(ctx, case.guard, &typ_scrut);
            let block = case.block.prepare(ctx);
            cases.push(ast::Case { guard, block });
        }
        ast::CaseInstr { exp, cases, totality: self.totality, dangle: self.dangle }
    }
}

// - Group instruction

impl Prepare<PrepareContext<'_>> for source::GroupInstr {
    type Output = ast::GroupInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let exps = self.exps.prepare(&mut ctx.il());
        let block = self.block.prepare(ctx);
        ast::GroupInstr { id: self.id, rel_signature: self.rel_signature, exps, block }
    }
}

// - Let instruction

impl Prepare<PrepareContext<'_>> for source::LetInstr {
    type Output = ast::LetInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // The left side is a pattern the value binds
        let exp_l = self
            .exp_l
            .prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        let exp_r = self.exp_r.prepare(&mut ctx.il());
        let iter_instrs = self.iter_instrs.prepare(&mut ctx.il());
        let block = self.block.prepare(ctx);
        ast::LetInstr { exp_l, exp_r, iter_instrs, block }
    }
}

// - Rule instruction

impl Prepare<PrepareContext<'_>> for source::RuleInstr {
    /// A call, or the result that makes it in place of the relation.
    type Output = ast::InstrKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // Compare source expressions before their mixops become arena handles
        let tail = ctx.tco && self.is_tail_call(ctx.pos);
        let (exps_input, exps_output) = input::split(&self.input_hint, self.not_exp.into_args())
            .expect("input hint must fit relation");
        let exps_input = exps_input.prepare(&mut ctx.il());
        // A call whose sole continuation returns its outputs concludes in their place
        if tail {
            let call = ast::TailRel { id: self.id, exps_input };
            return ast::InstrKind::Result(ast::ResultInstr::Tail(call));
        }
        // Otherwise the call binds its outputs as patterns and runs its continuation
        let exps_output = exps_output.prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        let iter_instrs = self.iter_instrs.prepare(&mut ctx.il());
        let block = self.block.prepare(ctx);
        let instr = ast::RuleInstr { id: self.id, exps_input, exps_output, iter_instrs, block };
        ast::InstrKind::Rule(instr)
    }
}

impl source::RuleInstr {
    /// Checks tail position and whether the sole continuation returns the outputs.
    fn is_tail_call(&self, pos: Position) -> bool {
        // Non-tail positions and iterated calls must retain their continuation
        if pos != Position::Tail || !self.iter_instrs.is_empty() {
            return false;
        }
        let [instr_result] = self.block.instrs.as_slice() else { return false };
        let source::InstrKind::Result(instr_result) = &instr_result.node else { return false };
        // Output and result expressions must agree in order, ignoring source spans
        let (_, exps_output) = input::split(&self.input_hint, self.not_exp.args().iter().collect())
            .expect("input hint must fit relation");
        exps_output.len() == instr_result.exps.len()
            && exps_output
                .iter()
                .zip(&instr_result.exps)
                .all(|(exp_l, exp_r)| exp_l.syntax_eq(exp_r))
    }
}

// - Result instruction

impl Prepare<PrepareContext<'_>> for source::ResultInstr {
    type Output = ast::ResultInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::ResultInstr::Values(self.exps.prepare(&mut ctx.il()))
    }
}

// - Return instruction

impl Prepare<PrepareContext<'_>> for source::ReturnInstr {
    type Output = ast::ReturnInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // A call returned in tail position is made in place of the function
        let tail = ctx.tco && ctx.pos == Position::Tail;
        ast::ReturnInstr::prepare(self.exp, &mut ctx.il(), tail)
    }
}

// - Debug instruction

impl Prepare<PrepareContext<'_>> for source::DebugInstr {
    type Output = ast::DebugInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let exp = self.exp.prepare(&mut ctx.il());
        let instr = prepare_instr(*self.instr, ctx);
        ast::DebugInstr { exp, instr: Box::new(instr) }
    }
}

// - Holding conditions

impl Prepare<PrepareContext<'_>> for source::HoldCase {
    type Output = ast::HoldCase;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::HoldCase::Both(block_hold, block_not_hold) => {
                ast::HoldCase::Both(block_hold.prepare(ctx), block_not_hold.prepare(ctx))
            }
            source::HoldCase::Hold(block, dangle) => {
                ast::HoldCase::Hold(block.prepare(ctx), dangle)
            }
            source::HoldCase::NotHold(block, dangle) => {
                ast::HoldCase::NotHold(block.prepare(ctx), dangle)
            }
        }
    }
}

// = Relation definitions

impl Prepare<PrepareContext<'_>> for source::RelDef {
    type Output = ast::RelDef;

    /// Prepares the relation; a defined one runs in a frame of its own.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::RelDef::Extern(rel) => {
                let rel = rel.prepare(ctx);
                *ctx.layout = FrameLayout::default();
                ast::RelDef::Extern(rel)
            }
            source::RelDef::Defined(rel) => {
                // Prepare the relation, then transfer the layout it filled
                let def = rel.prepare(ctx);
                let layout = Rc::new(mem::take(ctx.layout));
                let rel = Callable { def, layout };
                ast::RelDef::Defined(rel)
            }
        }
    }
}

// - External relation

impl Prepare<PrepareContext<'_>> for source::ExternRel {
    type Output = ast::ExternRel;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::ExternRel {
            id: self.id,
            rel_signature: self.rel_signature,
            exps_input: self.exps_input.prepare(&mut ctx.il()),
            hints: self.hints,
        }
    }
}

// - Defined relation

impl Prepare<PrepareContext<'_>> for source::DefinedRel {
    type Output = ast::DefinedRel;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // The inputs are patterns the call's arguments bind
        let exps_input = self
            .exps_input
            .prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        // The otherwise block must still catch a failing main body
        let pos = if self.block_else.is_some() { Position::NonTail } else { Position::Tail };
        let block = self.block.prepare(&mut ctx.at(pos));
        let block_else = (self.block_else).map(|block| block.prepare(&mut ctx.at(Position::Tail)));
        ast::DefinedRel {
            id: self.id,
            rel_signature: self.rel_signature,
            exps_input,
            block,
            block_else,
            hints: self.hints,
        }
    }
}

// = Meta-function definitions

impl Prepare<PrepareContext<'_>> for source::MetaFuncDef {
    type Output = ast::MetaFuncDef;

    /// Prepares the function; a table or defined one runs in a frame of its own.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::MetaFuncDef::Extern(func) => {
                let func = func.prepare(ctx);
                *ctx.layout = FrameLayout::default();
                ast::MetaFuncDef::Extern(func)
            }
            source::MetaFuncDef::Builtin(func) => {
                let func = func.prepare(ctx);
                *ctx.layout = FrameLayout::default();
                ast::MetaFuncDef::Builtin(func)
            }
            source::MetaFuncDef::Table(func) => {
                // Prepare the function, then transfer the layout it filled
                let def = func.prepare(ctx);
                let layout = Rc::new(mem::take(ctx.layout));
                let func = Callable { def, layout };
                ast::MetaFuncDef::Table(func)
            }
            source::MetaFuncDef::Defined(func) => {
                let def = func.prepare(ctx);
                let layout = Rc::new(mem::take(ctx.layout));
                let func = Callable { def, layout };
                ast::MetaFuncDef::Defined(func)
            }
        }
    }
}

// - External function

impl Prepare<PrepareContext<'_>> for source::ExternFunc {
    type Output = ast::ExternFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::ExternFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params.prepare(ctx),
            typ: self.typ,
            hints: self.hints,
        }
    }
}

// - Builtin function

impl Prepare<PrepareContext<'_>> for source::BuiltinFunc {
    type Output = ast::BuiltinFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::BuiltinFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params.prepare(ctx),
            typ: self.typ,
            hints: self.hints,
        }
    }
}

// - Table function

impl Prepare<PrepareContext<'_>> for source::TableFunc {
    type Output = ast::TableFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let params = self.params.prepare(ctx);
        let mut instrs_len: usize = self
            .table_rows
            .iter()
            .map(|row| row.block.instrs.len())
            .sum();
        let mut table_rows = Vec::with_capacity(self.table_rows.len());
        for row in self.table_rows {
            let exps_input = row.exps_input.prepare(&mut ctx.il());
            let exp = row.exp.prepare(&mut ctx.il());
            let mut instrs = Vec::with_capacity(row.block.instrs.len());
            for instr in row.block.instrs {
                // Only the last instruction across all rows inherits tail position
                instrs_len -= 1;
                let pos = if instrs_len == 0 { Position::Tail } else { Position::NonTail };
                instrs.push(prepare_instr(instr, &mut ctx.at(pos)));
            }
            let block = ast::Block { instrs };
            table_rows.push(ast::TableRow { exps_input, exp, block });
        }
        ast::TableFunc { id: self.id, params, typ: self.typ, table_rows, hints: self.hints }
    }
}

// - Defined function

impl Prepare<PrepareContext<'_>> for source::DefinedFunc {
    type Output = ast::DefinedFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // Type parameters are opaque types in the body
        let mut tdenv = ctx.tdenv.clone();
        for tparam in &self.tparams {
            tdenv.insert(tparam.clone(), TypeDef::Parameter);
        }
        let ctx = &mut PrepareContext {
            arena_mixop: &mut *ctx.arena_mixop,
            tdenv: &tdenv,
            layout: &mut *ctx.layout,
            det: ctx.det,
            tco: ctx.tco,
            counter_instr: &mut *ctx.counter_instr,
            pos: ctx.pos,
        };
        let params = self.params.prepare(ctx);
        // The otherwise block must still catch a failing main body
        let pos = if self.block_else.is_some() { Position::NonTail } else { Position::Tail };
        let block = self.block.prepare(&mut ctx.at(pos));
        let block_else = (self.block_else).map(|block| block.prepare(&mut ctx.at(Position::Tail)));
        ast::DefinedFunc {
            id: self.id,
            tparams: self.tparams,
            params,
            typ: self.typ,
            block,
            block_else,
            hints: self.hints,
        }
    }
}

// - Parameters

impl Prepare<PrepareContext<'_>> for source::ParamKind {
    type Output = ast::ParamKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            // The parameter is a pattern the argument binds
            source::ParamKind::Exp(typ, exp) => {
                let exp = exp.prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
                ast::ParamKind::Exp(typ, exp)
            }
            source::ParamKind::Def(id, tparams, params, typ) => {
                // The parameter holds its function value in a slot
                ctx.layout.resolve_func(&id);
                ast::ParamKind::Def(id, tparams, params.prepare(ctx), typ)
            }
        }
    }
}

// - Guards

impl ast::Guard {
    /// Prepares a guard on a scrutinee of type `typ_scrut`.
    fn prepare(
        ctx: &mut PrepareContext<'_>,
        guard: source::Guard,
        typ_scrut: &source::Typ,
    ) -> Self {
        match guard {
            source::Guard::Bool(value) => ast::Guard::Bool(value),
            source::Guard::Cmp(op, typ_op, exp) => {
                ast::Guard::Cmp(op, typ_op, exp.prepare(&mut ctx.il()))
            }
            source::Guard::Sub(typ) => {
                ast::Guard::Sub(ast::Subcheck::prepare(&mut ctx.il(), typ_scrut, &typ))
            }
            source::Guard::Match(pattern) => ast::Guard::Match(pattern.prepare(&mut ctx.il())),
            source::Guard::Mem(exp) => ast::Guard::Mem(exp.prepare(&mut ctx.il())),
        }
    }
}
