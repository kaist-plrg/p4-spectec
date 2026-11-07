//! SL instruction coverage
//!
//! `Cover` numbers the source instructions in the pre-order of SL preparation,
//! so a prepared instruction's `InstrId` indexes its source node.
//! `InstrHandler` marks each instruction an input enters in the shared `Hits`,
//! and `Cover::add_hits` adds them to the per-instruction input counts.

use std::{cell::RefCell, rc::Rc};

use crate::lang::{data::arena::Arena, sl};

use crate::runner::Handler;

use sl::ast::{Block, Def, DefKind, Hint, HoldCase, Instr, InstrKind, MetaFuncDef, RelDef, Spec};

use sl::prepared::InstrId;

// == Coverage map

/// One numbered source instruction.
#[derive(Clone, Debug)]
pub struct Node {
    /// Whether the enclosing definition is excluded from coverage.
    pub ignored: bool,
    /// The number of inputs that entered the instruction.
    pub hits: u32,
}

/// Coverage of every numbered instruction, indexed by `InstrId`.
#[derive(Clone, Debug)]
pub struct Cover {
    nodes: Vec<Node>,
}

impl Cover {
    /// Numbers the specification's instructions, all unhit.
    pub fn new(spec: &Spec) -> Self {
        let mut nodes = Vec::new();
        for def in spec {
            number_def(&mut nodes, def);
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

    /// Counts one input's hits and clears them for the next input.
    pub fn add_hits(&mut self, hits: &Hits) {
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

// == Numbering

/// The hint excluding a definition's instructions from coverage.
const HINT_IGNORE: &str = "cover_instr_ignore";

// - Definitions

/// Numbers a definition's instructions in the order SL preparation visits them.
fn number_def(nodes: &mut Vec<Node>, def: &Def) {
    match &def.node {
        // The body comes before the otherwise block
        DefKind::Rel(RelDef::Defined(rel)) => {
            let ignored = has_ignore_hint(&rel.hints);
            number_block(nodes, ignored, &rel.block);
            if let Some(block_else) = &rel.block_else {
                number_block(nodes, ignored, block_else);
            }
        }
        // The body comes before the otherwise block
        DefKind::MetaFunc(MetaFuncDef::Defined(func)) => {
            let ignored = has_ignore_hint(&func.hints);
            number_block(nodes, ignored, &func.block);
            if let Some(block_else) = &func.block_else {
                number_block(nodes, ignored, block_else);
            }
        }
        // Table rows come in order
        DefKind::MetaFunc(MetaFuncDef::Table(func)) => {
            let ignored = has_ignore_hint(&func.hints);
            for row in &func.table_rows {
                number_block(nodes, ignored, &row.block);
            }
        }
        // Externs, builtins, types, and variables have no instructions
        DefKind::Rel(RelDef::Extern(_))
        | DefKind::MetaFunc(MetaFuncDef::Extern(_) | MetaFuncDef::Builtin(_))
        | DefKind::Typ(_)
        | DefKind::Var(_) => {}
    }
}

/// Checks whether a definition opts out of coverage.
fn has_ignore_hint(hints: &[Hint]) -> bool {
    hints
        .iter()
        .any(|hint| hint.id.node.as_ref() == HINT_IGNORE)
}

// - Instructions

/// Numbers a block's instructions in order.
fn number_block(nodes: &mut Vec<Node>, ignored: bool, block: &Block) {
    for instr in &block.instrs {
        number_instr(nodes, ignored, instr);
    }
}

/// Numbers an instruction before its nested instructions, as preparation does.
fn number_instr(nodes: &mut Vec<Node>, ignored: bool, instr: &Instr) {
    nodes.push(Node { ignored, hits: 0 });
    match &instr.node {
        InstrKind::If(instr) => number_block(nodes, ignored, &instr.block),
        InstrKind::Hold(instr) => match &instr.hold_case {
            HoldCase::Both(block_hold, block_not_hold) => {
                number_block(nodes, ignored, block_hold);
                number_block(nodes, ignored, block_not_hold);
            }
            HoldCase::Hold(block, _) | HoldCase::NotHold(block, _) => {
                number_block(nodes, ignored, block)
            }
        },
        InstrKind::Case(instr) => {
            for case in &instr.cases {
                number_block(nodes, ignored, &case.block);
            }
        }
        InstrKind::Group(instr) => number_block(nodes, ignored, &instr.block),
        InstrKind::Let(instr) => number_block(nodes, ignored, &instr.block),
        InstrKind::Rule(instr) => number_block(nodes, ignored, &instr.block),
        InstrKind::Debug(instr) => number_instr(nodes, ignored, &instr.instr),
        InstrKind::Result(_) | InstrKind::Return(_) => {}
    }
}

// == Hit recording

/// Instructions entered by the current input, shared with `InstrHandler`.
#[derive(Clone)]
pub struct Hits {
    flags: Rc<RefCell<Vec<bool>>>,
}

impl Hits {
    /// No hits over `len` instructions.
    pub fn new(len: usize) -> Self {
        Self { flags: Rc::new(RefCell::new(vec![false; len])) }
    }

    /// Marks the instruction as entered.
    fn mark(&self, id: InstrId) {
        self.flags.borrow_mut()[id.index()] = true;
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
        self.hits.mark(instr_sl.note);
    }
}
