//! SL instruction coverage
//!
//! `Cover` numbers source instructions in the pre-order used by SL preparation,
//! so a prepared instruction's `InstrId` indexes its source node.
//! `InstrHandler` marks the instructions one program enters in shared `Hits`,
//! and `Cover::extend` folds them into per-instruction program counts.

use std::{cell::RefCell, rc::Rc};

use crate::lang::{data::arena::Arena, sl};

use crate::runner::Handler;

use sl::ast::{
    Block, DefKind, Hint, HoldCase, Id, Instr, InstrKind, MetaFuncDef, RelDef, Spec, TableRow,
};

/// The hint excluding a definition's instructions from coverage.
const HINT_IGNORE: &str = "cover_instr_ignore";

// == Coverage map

/// One numbered instruction.
#[derive(Clone, Debug)]
pub struct Node {
    /// The enclosing relation or function.
    pub origin: Id,
    /// Whether the enclosing definition is excluded from coverage.
    pub ignored: bool,
    /// The number of programs that entered the instruction.
    pub hits: u32,
}

/// Coverage of every numbered instruction, indexed by `InstrId`.
#[derive(Clone, Debug)]
pub struct Cover {
    nodes: Vec<Node>,
}

/// Hit and total counts over instructions that are not ignored.
#[derive(Clone, Copy, Debug)]
pub struct Measure {
    pub hit: usize,
    pub total: usize,
}

impl Measure {
    /// The hit ratio as a percentage, zero when nothing is counted.
    pub fn percent(&self) -> f64 {
        if self.total == 0 { 0.0 } else { self.hit as f64 / self.total as f64 * 100.0 }
    }
}

impl Cover {
    /// Numbers the specification's instructions, all unhit.
    pub fn new(spec: &Spec) -> Self {
        let mut nodes = Vec::new();
        for def in spec {
            for_each_callable(def, |id, hints, blocks| {
                let ignored = is_ignored(hints);
                for block in blocks {
                    number_block(&mut nodes, id, ignored, block);
                }
            });
        }
        Self { nodes }
    }

    /// The number of instructions, including ignored ones.
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Whether the specification has no instructions.
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// The node with the given index.
    pub fn node(&self, index: usize) -> &Node {
        &self.nodes[index]
    }

    /// Counts one program's hits and clears them for the next program.
    pub fn extend(&mut self, hits: &Hits) {
        let mut flags = hits.flags.borrow_mut();
        for (node, flag) in self.nodes.iter_mut().zip(flags.iter_mut()) {
            if *flag {
                node.hits += 1;
                *flag = false;
            }
        }
    }

    /// Counts hit instructions outside ignored definitions.
    pub fn measure(&self) -> Measure {
        let counted = self.nodes.iter().filter(|node| !node.ignored);
        let (hit, total) = counted
            .fold((0, 0), |(hit, total), node| (hit + usize::from(node.hits > 0), total + 1));
        Measure { hit, total }
    }
}

/// Whether the definition opts out of coverage.
pub(super) fn is_ignored(hints: &[Hint]) -> bool {
    hints
        .iter()
        .any(|hint| hint.id.node.as_ref() == HINT_IGNORE)
}

/// Calls `f` with each definition that has instructions, in preparation order:
/// a relation's or function's block then its otherwise block, or a table's row blocks.
pub(super) fn for_each_callable<'a>(
    def: &'a sl::ast::Def,
    mut f: impl FnMut(&'a Id, &'a [Hint], Vec<&'a Block>),
) {
    match &def.node {
        DefKind::Rel(RelDef::Defined(rel)) => {
            let blocks = std::iter::once(&rel.block).chain(&rel.block_else);
            f(&rel.id, &rel.hints, blocks.collect());
        }
        DefKind::MetaFunc(MetaFuncDef::Defined(func)) => {
            let blocks = std::iter::once(&func.block).chain(&func.block_else);
            f(&func.id, &func.hints, blocks.collect());
        }
        DefKind::MetaFunc(MetaFuncDef::Table(func)) => {
            let blocks = func.table_rows.iter().map(|row: &TableRow| &row.block);
            f(&func.id, &func.hints, blocks.collect());
        }
        DefKind::Rel(RelDef::Extern(_))
        | DefKind::MetaFunc(MetaFuncDef::Extern(_) | MetaFuncDef::Builtin(_))
        | DefKind::Typ(_)
        | DefKind::Var(_) => {}
    }
}

/// Numbers a block's instructions in order.
fn number_block(nodes: &mut Vec<Node>, origin: &Id, ignored: bool, block: &Block) {
    for instr in &block.instrs {
        number_instr(nodes, origin, ignored, instr);
    }
}

/// Numbers an instruction before its nested instructions, as preparation does.
fn number_instr(nodes: &mut Vec<Node>, origin: &Id, ignored: bool, instr: &Instr) {
    nodes.push(Node { origin: origin.clone(), ignored, hits: 0 });
    for block in nested_blocks(instr) {
        number_block(nodes, origin, ignored, block);
    }
    if let InstrKind::Debug(instr) = &instr.node {
        number_instr(nodes, origin, ignored, &instr.instr);
    }
}

/// The blocks nested in an instruction, in preparation order.
fn nested_blocks(instr: &Instr) -> Vec<&Block> {
    match &instr.node {
        InstrKind::If(instr) => vec![&instr.block],
        InstrKind::Hold(instr) => match &instr.hold_case {
            HoldCase::Both(block_hold, block_not_hold) => vec![block_hold, block_not_hold],
            HoldCase::Hold(block, _) | HoldCase::NotHold(block, _) => vec![block],
        },
        InstrKind::Case(instr) => instr.cases.iter().map(|case| &case.block).collect(),
        InstrKind::Group(instr) => vec![&instr.block],
        InstrKind::Let(instr) => vec![&instr.block],
        InstrKind::Rule(instr) => vec![&instr.block],
        InstrKind::Result(_) | InstrKind::Return(_) | InstrKind::Debug(_) => vec![],
    }
}

// == Hit recording

/// Instructions entered by the current program, shared with `InstrHandler`.
#[derive(Clone)]
pub struct Hits {
    flags: Rc<RefCell<Vec<bool>>>,
}

impl Hits {
    /// No hits over `len` instructions.
    pub fn new(len: usize) -> Self {
        Self { flags: Rc::new(RefCell::new(vec![false; len])) }
    }
}

/// Marks each prepared SL instruction the interpreter enters.
pub struct InstrHandler {
    hits: Hits,
}

impl InstrHandler {
    /// Records into `hits`, which must cover every prepared instruction id.
    pub fn new(hits: Hits) -> Self {
        Self { hits }
    }
}

impl Handler for InstrHandler {
    fn on_instr(&mut self, _arena: &Arena, instr_sl: &sl::prepared::Instr) {
        self.hits.flags.borrow_mut()[instr_sl.note.index()] = true;
    }
}
