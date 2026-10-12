//! Lower SL rule calls while resolving slots and interning mixops
//!
//! Source rules become ordinary or tail calls in the prepared stage.
//! Tail position follows the interpreter's selection mode and otherwise blocks;
//! table rows form one sequential block even in deterministic mode.
//! Only a tail-position call that immediately returns its outputs is lowered.
//! Top-level relations and functions return callables with their own layouts.

use std::{mem, rc::Rc};

use crate::lang::{data::notation::MixopArena, hints::input, traits::eq::SyntaxEq};

use crate::lang::sl::{ast as source, prepared as ast};

use crate::runtime::envs::interp::shared::{callable::Callable, frame::FrameLayout};

use crate::interp::shared::prepare::{Prepare, il};

// - Preparation context

/// Records SL slots and interned mixops.
pub struct PrepareContext<'a> {
    /// The specification's notation arena.
    pub arena_mixop: &'a mut MixopArena,
    /// The callable's frame layout.
    pub layout: &'a mut FrameLayout,
    /// The runner's instruction-selection mode.
    pub det: bool,
    pos: Position,
}

impl<'a> PrepareContext<'a> {
    /// Borrows the notation arena and callable layout for preparation.
    pub fn new(arena_mixop: &'a mut MixopArena, layout: &'a mut FrameLayout, det: bool) -> Self {
        Self { arena_mixop, layout, det, pos: Position::Tail }
    }

    fn il(&mut self) -> il::PrepareContext<'_> {
        il::PrepareContext { arena_mixop: &mut *self.arena_mixop, layout: &mut *self.layout }
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
            layout: &mut *self.layout,
            det: self.det,
            pos,
        }
    }

    /// Prepares a block using the evaluator's tail-position rule.
    fn prepare_block(&mut self, block: source::Block) -> ast::Block {
        let instrs_len = block.len();
        // Deterministic selection gives every candidate the enclosing position
        block
            .into_iter()
            .enumerate()
            .map(|(idx, instr)| {
                let pos =
                    if self.det || idx + 1 == instrs_len { self.pos } else { Position::NonTail };
                instr.prepare(&mut self.at(pos))
            })
            .collect()
    }
}

// = Instructions

impl Prepare<PrepareContext<'_>> for source::InstrKind {
    type Output = ast::InstrKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::InstrKind::If(instr) => ast::InstrKind::If(instr.prepare(ctx)),
            source::InstrKind::Hold(instr) => ast::InstrKind::Hold(instr.prepare(ctx)),
            source::InstrKind::Case(instr) => ast::InstrKind::Case(instr.prepare(ctx)),
            source::InstrKind::Group(instr) => ast::InstrKind::Group(instr.prepare(ctx)),
            source::InstrKind::Let(instr) => ast::InstrKind::Let(instr.prepare(ctx)),
            source::InstrKind::Rule(instr) => ast::InstrKind::Rule(instr.prepare(ctx)),
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
        ast::IfInstr {
            exp: self.exp.prepare(&mut ctx.il()),
            iter_exps: self.iter_exps.prepare(&mut ctx.il()),
            block: ctx.prepare_block(self.block),
            dangle: self.dangle,
        }
    }
}

// - Hold instruction

impl Prepare<PrepareContext<'_>> for source::HoldInstr {
    type Output = ast::HoldInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::HoldInstr {
            id: self.id,
            not_exp: self.not_exp.prepare(&mut ctx.il()),
            iter_exps: self.iter_exps.prepare(&mut ctx.il()),
            hold_case: self.hold_case.prepare(ctx),
        }
    }
}

// - Case instruction

impl Prepare<PrepareContext<'_>> for source::CaseInstr {
    type Output = ast::CaseInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::CaseInstr {
            exp: self.exp.prepare(&mut ctx.il()),
            cases: self.cases.prepare(ctx),
            dangle: self.dangle,
        }
    }
}

// - Group instruction

impl Prepare<PrepareContext<'_>> for source::GroupInstr {
    type Output = ast::GroupInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::GroupInstr {
            id: self.id,
            rel_signature: self.rel_signature,
            exps: self.exps.prepare(&mut ctx.il()),
            block: ctx.prepare_block(self.block),
        }
    }
}

// - Let instruction

impl Prepare<PrepareContext<'_>> for source::LetInstr {
    type Output = ast::LetInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::LetInstr {
            exp_l: self.exp_l.prepare(&mut ctx.il()),
            exp_r: self.exp_r.prepare(&mut ctx.il()),
            iter_instrs: self.iter_instrs.prepare(&mut ctx.il()),
            block: ctx.prepare_block(self.block),
        }
    }
}

// - Rule instruction

impl Prepare<PrepareContext<'_>> for source::RuleInstr {
    type Output = ast::RuleInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // Compare source expressions before their mixops become arena handles
        if self.is_tail_call(ctx.pos) {
            let (exps_input, _) = input::split(&self.input_hint, self.not_exp.into_args())
                .expect("input hint must fit relation");
            return ast::RuleInstr::Tail(ast::RuleTailInstr {
                id: self.id,
                exps_input: exps_input.prepare(&mut ctx.il()),
            });
        }
        // Ordinary calls retain output binding, iterations, and the continuation
        ast::RuleInstr::Call(source::RuleInstr {
            id: self.id,
            not_exp: self.not_exp.prepare(&mut ctx.il()),
            input_hint: self.input_hint,
            iter_instrs: self.iter_instrs.prepare(&mut ctx.il()),
            block: ctx.prepare_block(self.block),
        })
    }
}

impl source::RuleInstr {
    /// Checks tail position and whether the sole continuation returns the outputs.
    fn is_tail_call(&self, pos: Position) -> bool {
        // Non-tail positions and iterated calls must retain their continuation
        if pos != Position::Tail || !self.iter_instrs.is_empty() {
            return false;
        }
        let [instr_result] = self.block.as_slice() else { return false };
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
        ast::ResultInstr {
            rel_signature: self.rel_signature,
            exps: self.exps.prepare(&mut ctx.il()),
        }
    }
}

// - Return instruction

impl Prepare<PrepareContext<'_>> for source::ReturnInstr {
    type Output = ast::ReturnInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::ReturnInstr { exp: self.exp.prepare(&mut ctx.il()) }
    }
}

// - Debug instruction

impl Prepare<PrepareContext<'_>> for source::DebugInstr {
    type Output = ast::DebugInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::DebugInstr { exp: self.exp.prepare(&mut ctx.il()), instr: self.instr.prepare(ctx) }
    }
}

// - Holding conditions

impl Prepare<PrepareContext<'_>> for source::HoldCase {
    type Output = ast::HoldCase;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::HoldCase::Both(block_hold, block_not_hold) => ast::HoldCase::Both(
                ctx.prepare_block(block_hold),
                ctx.prepare_block(block_not_hold),
            ),
            source::HoldCase::Hold(block, dangle) => {
                ast::HoldCase::Hold(ctx.prepare_block(block), dangle)
            }
            source::HoldCase::NotHold(block, dangle) => {
                ast::HoldCase::NotHold(ctx.prepare_block(block), dangle)
            }
        }
    }
}

// - Case analysis

impl Prepare<PrepareContext<'_>> for source::Case {
    type Output = ast::Case;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::Case { guard: self.guard.prepare(ctx), block: ctx.prepare_block(self.block) }
    }
}

// = Relation definitions

impl Prepare<PrepareContext<'_>> for source::RelDef {
    type Output = Callable<ast::RelDef>;

    /// Prepares the relation and transfers its callable layout.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let rel = match self {
            source::RelDef::Extern(rel) => {
                let rel = rel.prepare(ctx);
                ast::RelDef::Extern(rel)
            }
            source::RelDef::Defined(rel) => {
                let rel = rel.prepare(ctx);
                ast::RelDef::Defined(rel)
            }
        };

        // Transfer this callable's layout and leave the next one empty
        let layout = mem::take(ctx.layout);
        let layout = Rc::new(layout);
        Callable { def: rel, layout }
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
        // The otherwise block must still catch a failing main body
        let pos = if self.block_else.is_some() { Position::NonTail } else { Position::Tail };
        ast::DefinedRel {
            id: self.id,
            rel_signature: self.rel_signature,
            exps_input: self.exps_input.prepare(&mut ctx.il()),
            block: ctx.at(pos).prepare_block(self.block),
            block_else: self
                .block_else
                .map(|block| ctx.at(Position::Tail).prepare_block(block)),
            hints: self.hints,
        }
    }
}

// = Meta-function definitions

impl Prepare<PrepareContext<'_>> for source::MetaFuncDef {
    type Output = Callable<ast::MetaFuncDef>;

    /// Prepares the function and transfers its callable layout.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let func = match self {
            source::MetaFuncDef::Extern(func) => {
                let func = func.prepare(ctx);
                ast::MetaFuncDef::Extern(func)
            }
            source::MetaFuncDef::Builtin(func) => {
                let func = func.prepare(ctx);
                ast::MetaFuncDef::Builtin(func)
            }
            source::MetaFuncDef::Table(func) => {
                let func = func.prepare(ctx);
                ast::MetaFuncDef::Table(func)
            }
            source::MetaFuncDef::Defined(func) => {
                let func = func.prepare(ctx);
                ast::MetaFuncDef::Defined(func)
            }
        };

        // Transfer this callable's layout and leave the next one empty
        let layout = mem::take(ctx.layout);
        let layout = Rc::new(layout);
        Callable { def: func, layout }
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
        let mut instrs_len: usize = self.table_rows.iter().map(|row| row.block.len()).sum();
        // Only the last instruction across all rows inherits tail position
        let table_rows = self
            .table_rows
            .into_iter()
            .map(|row| {
                let exps_input = row.exps_input.prepare(&mut ctx.il());
                let exp = row.exp.prepare(&mut ctx.il());
                let block = row
                    .block
                    .into_iter()
                    .map(|instr| {
                        instrs_len -= 1;
                        let pos = if instrs_len == 0 { Position::Tail } else { Position::NonTail };
                        instr.prepare(&mut ctx.at(pos))
                    })
                    .collect();
                ast::TableRow { exps_input, exp, block }
            })
            .collect();
        ast::TableFunc { id: self.id, params, typ: self.typ, table_rows, hints: self.hints }
    }
}

// - Defined function

impl Prepare<PrepareContext<'_>> for source::DefinedFunc {
    type Output = ast::DefinedFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // The otherwise block must still catch a failing main body
        let pos = if self.block_else.is_some() { Position::NonTail } else { Position::Tail };
        ast::DefinedFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params.prepare(ctx),
            typ: self.typ,
            block: ctx.at(pos).prepare_block(self.block),
            block_else: self
                .block_else
                .map(|block| ctx.at(Position::Tail).prepare_block(block)),
            hints: self.hints,
        }
    }
}

// - Parameters

impl Prepare<PrepareContext<'_>> for source::ParamKind {
    type Output = ast::ParamKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::ParamKind::Exp(typ, exp) => {
                ast::ParamKind::Exp(typ, exp.prepare(&mut ctx.il()))
            }
            source::ParamKind::Def(id, tparams, params, typ) => {
                ast::ParamKind::Def(id, tparams, params.prepare(ctx), typ)
            }
        }
    }
}

// - Guards

impl Prepare<PrepareContext<'_>> for source::Guard {
    type Output = ast::Guard;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::Guard::Bool(value) => ast::Guard::Bool(value),
            source::Guard::Cmp(op, typ_op, exp) => {
                ast::Guard::Cmp(op, typ_op, exp.prepare(&mut ctx.il()))
            }
            source::Guard::Sub(typ, subcheck) => {
                ast::Guard::Sub(typ, subcheck.prepare(&mut ctx.il()))
            }
            source::Guard::Match(pattern) => ast::Guard::Match(pattern.prepare(&mut ctx.il())),
            source::Guard::Mem(exp) => ast::Guard::Mem(exp.prepare(&mut ctx.il())),
        }
    }
}
