//! Lower SL rule calls while resolving slots and interning mixops
//!
//! Source rules become ordinary or tail calls in the prepared stage.
//! Tail position follows the interpreter's selection mode and otherwise blocks;
//! table rows form one sequential block even in deterministic mode.
//! Only a tail-position call that immediately returns its outputs is lowered.

use crate::lang::{hints::input, traits::eq::SyntaxEq};

use crate::lang::sl::{ast as source, prepared as ast};

use crate::interp::shared::prepare::{Prepare, PrepareContext};

// = Instruction preparation

#[derive(Clone, Copy, PartialEq, Eq)]
enum Position {
    Tail,
    NonTail,
}

impl PrepareContext<'_> {
    /// Prepares a block using the evaluator's tail-position rule.
    fn prepare_block(&mut self, block: source::Block, pos: Position) -> ast::Block {
        let instrs_len = block.len();
        // Deterministic selection gives every candidate the enclosing position
        block
            .into_iter()
            .enumerate()
            .map(|(idx, instr)| {
                let pos = if self.det || idx + 1 == instrs_len { pos } else { Position::NonTail };
                self.prepare_instr(instr, pos)
            })
            .collect()
    }

    /// Prepares one instruction and propagates its execution position.
    fn prepare_instr(&mut self, instr: source::Instr, pos: Position) -> ast::Instr {
        // Grow the stack just as the shared phrase preparation does
        stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
            let instr_kind = match instr.node {
                source::InstrKind::If(instr) => ast::InstrKind::If(ast::IfInstr {
                    exp: instr.exp.prepare(self),
                    iter_exps: instr.iter_exps.prepare(self),
                    block: self.prepare_block(instr.block, pos),
                    dangle: instr.dangle,
                }),
                source::InstrKind::Hold(instr) => ast::InstrKind::Hold(ast::HoldInstr {
                    id: instr.id,
                    not_exp: instr.not_exp.prepare(self),
                    iter_exps: instr.iter_exps.prepare(self),
                    hold_case: self.prepare_hold_case(instr.hold_case, pos),
                }),
                source::InstrKind::Case(instr) => ast::InstrKind::Case(ast::CaseInstr {
                    exp: instr.exp.prepare(self),
                    cases: instr
                        .cases
                        .into_iter()
                        .map(|case| ast::Case {
                            guard: case.guard.prepare(self),
                            block: self.prepare_block(case.block, pos),
                        })
                        .collect(),
                    dangle: instr.dangle,
                }),
                source::InstrKind::Group(instr) => ast::InstrKind::Group(ast::GroupInstr {
                    id: instr.id,
                    rel_signature: instr.rel_signature,
                    exps: instr.exps.prepare(self),
                    block: self.prepare_block(instr.block, pos),
                }),
                source::InstrKind::Let(instr) => ast::InstrKind::Let(ast::LetInstr {
                    exp_l: instr.exp_l.prepare(self),
                    exp_r: instr.exp_r.prepare(self),
                    iter_instrs: instr.iter_instrs.prepare(self),
                    block: self.prepare_block(instr.block, pos),
                }),
                source::InstrKind::Rule(instr) => {
                    ast::InstrKind::Rule(self.prepare_rule(instr, pos))
                }
                source::InstrKind::Result(instr) => ast::InstrKind::Result(ast::ResultInstr {
                    rel_signature: instr.rel_signature,
                    exps: instr.exps.prepare(self),
                }),
                source::InstrKind::Return(instr) => {
                    ast::InstrKind::Return(ast::ReturnInstr { exp: instr.exp.prepare(self) })
                }
                source::InstrKind::Debug(instr) => ast::InstrKind::Debug(ast::DebugInstr {
                    exp: instr.exp.prepare(self),
                    instr: Box::new(self.prepare_instr(*instr.instr, pos)),
                }),
            };
            crate::phrase!(node: instr_kind, span: instr.span)
        })
    }

    /// Lowers a rule to a tail call only when no continuation remains to run.
    fn prepare_rule(&mut self, instr: source::RuleInstr, pos: Position) -> ast::RuleInstr {
        // Compare source expressions before their mixops become arena handles
        if pos == Position::Tail && returns_outputs(&instr) {
            let (exps_input, _) = input::split(&instr.input_hint, instr.not_exp.into_args())
                .expect("input hint must fit relation");
            return ast::RuleInstr::Tail(ast::RuleTailInstr {
                id: instr.id,
                exps_input: exps_input.prepare(self),
            });
        }
        // Ordinary calls retain output binding, iterations, and the continuation
        ast::RuleInstr::Call(source::RuleInstr {
            id: instr.id,
            not_exp: instr.not_exp.prepare(self),
            input_hint: instr.input_hint,
            iter_instrs: instr.iter_instrs.prepare(self),
            block: self.prepare_block(instr.block, pos),
        })
    }

    fn prepare_hold_case(&mut self, hold_case: source::HoldCase, pos: Position) -> ast::HoldCase {
        match hold_case {
            source::HoldCase::Both(block_hold, block_not_hold) => ast::HoldCase::Both(
                self.prepare_block(block_hold, pos),
                self.prepare_block(block_not_hold, pos),
            ),
            source::HoldCase::Hold(block, dangle) => {
                ast::HoldCase::Hold(self.prepare_block(block, pos), dangle)
            }
            source::HoldCase::NotHold(block, dangle) => {
                ast::HoldCase::NotHold(self.prepare_block(block, pos), dangle)
            }
        }
    }

    /// Prepares concatenated table rows with sequential selection at their root.
    fn prepare_table_func(&mut self, func: source::TableFunc) -> ast::TableFunc {
        let params = func.params.prepare(self);
        let mut instrs_len: usize = func.table_rows.iter().map(|row| row.block.len()).sum();
        // Only the last instruction across all rows inherits tail position
        let table_rows = func
            .table_rows
            .into_iter()
            .map(|row| {
                let exps_input = row.exps_input.prepare(self);
                let exp = row.exp.prepare(self);
                let block = row
                    .block
                    .into_iter()
                    .map(|instr| {
                        instrs_len -= 1;
                        let pos = if instrs_len == 0 { Position::Tail } else { Position::NonTail };
                        self.prepare_instr(instr, pos)
                    })
                    .collect();
                ast::TableRow { exps_input, exp, block }
            })
            .collect();
        ast::TableFunc { id: func.id, params, typ: func.typ, table_rows, hints: func.hints }
    }
}

/// Checks whether a rule's sole continuation returns its outputs unchanged.
fn returns_outputs(instr: &source::RuleInstr) -> bool {
    // Iterated calls and non-result continuations must retain their execution
    if !instr.iter_instrs.is_empty() {
        return false;
    }
    let [instr_result] = instr.block.as_slice() else { return false };
    let source::InstrKind::Result(instr_result) = &instr_result.node else { return false };
    // Output and result expressions must agree in order, ignoring source spans
    let (_, exps_output) = input::split(&instr.input_hint, instr.not_exp.args().iter().collect())
        .expect("input hint must fit relation");
    exps_output.len() == instr_result.exps.len()
        && exps_output
            .iter()
            .zip(&instr_result.exps)
            .all(|(exp_l, exp_r)| exp_l.syntax_eq(exp_r))
}

// = Definition preparation

// - Relation definitions

impl Prepare for source::RelDef {
    type Output = ast::RelDef;

    /// Prepares a relation while preserving its otherwise-block fallback.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            // External relations only resolve their input bindings
            source::RelDef::Extern(rel) => ast::RelDef::Extern(ast::ExternRel {
                id: rel.id,
                rel_signature: rel.rel_signature,
                exps_input: rel.exps_input.prepare(ctx),
                hints: rel.hints,
            }),
            source::RelDef::Defined(rel) => {
                // The otherwise block must still catch a failing main body
                let pos = if rel.block_else.is_some() { Position::NonTail } else { Position::Tail };
                ast::RelDef::Defined(ast::DefinedRel {
                    id: rel.id,
                    rel_signature: rel.rel_signature,
                    exps_input: rel.exps_input.prepare(ctx),
                    block: ctx.prepare_block(rel.block, pos),
                    block_else: rel
                        .block_else
                        .map(|block| ctx.prepare_block(block, Position::Tail)),
                    hints: rel.hints,
                })
            }
        }
    }
}

// - Meta-function definitions

impl Prepare for source::MetaFuncDef {
    type Output = ast::MetaFuncDef;

    /// Prepares a function's bindings and the blocks executed by its body.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            // External functions only resolve their parameters
            source::MetaFuncDef::Extern(func) => ast::MetaFuncDef::Extern(ast::ExternFunc {
                id: func.id,
                tparams: func.tparams,
                params: func.params.prepare(ctx),
                typ: func.typ,
                hints: func.hints,
            }),
            // Builtin functions also have no body to lower
            source::MetaFuncDef::Builtin(func) => ast::MetaFuncDef::Builtin(ast::BuiltinFunc {
                id: func.id,
                tparams: func.tparams,
                params: func.params.prepare(ctx),
                typ: func.typ,
                hints: func.hints,
            }),
            // Table rows share one sequential selection
            source::MetaFuncDef::Table(func) => {
                ast::MetaFuncDef::Table(ctx.prepare_table_func(func))
            }
            source::MetaFuncDef::Defined(func) => {
                // The otherwise block must still catch a failing main body
                let pos =
                    if func.block_else.is_some() { Position::NonTail } else { Position::Tail };
                ast::MetaFuncDef::Defined(ast::DefinedFunc {
                    id: func.id,
                    tparams: func.tparams,
                    params: func.params.prepare(ctx),
                    typ: func.typ,
                    block: ctx.prepare_block(func.block, pos),
                    block_else: func
                        .block_else
                        .map(|block| ctx.prepare_block(block, Position::Tail)),
                    hints: func.hints,
                })
            }
        }
    }
}

// - Parameters

impl Prepare for source::ParamKind {
    type Output = ast::ParamKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::ParamKind::Exp(typ, exp) => ast::ParamKind::Exp(typ, exp.prepare(ctx)),
            source::ParamKind::Def(id, tparams, params, typ) => {
                ast::ParamKind::Def(id, tparams, params.prepare(ctx), typ)
            }
        }
    }
}

// - Guards

impl Prepare for source::Guard {
    type Output = ast::Guard;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::Guard::Bool(value) => ast::Guard::Bool(value),
            source::Guard::Cmp(op, typ_op, exp) => ast::Guard::Cmp(op, typ_op, exp.prepare(ctx)),
            source::Guard::Sub(typ, subcheck) => ast::Guard::Sub(typ, subcheck.prepare(ctx)),
            source::Guard::Match(pattern) => ast::Guard::Match(pattern.prepare(ctx)),
            source::Guard::Mem(exp) => ast::Guard::Mem(exp.prepare(ctx)),
        }
    }
}
