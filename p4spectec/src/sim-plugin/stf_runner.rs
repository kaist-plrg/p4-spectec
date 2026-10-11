//! Transforms and executes STF statements against one runner and per-run state
//!
//! `run_stf_test` parses the P4 program, initializes the pipeline,
//! and executes the STF statements in order.
//! Packets drive the pipeline; `expect` lines are matched against outputs
//! in either order, so both keep a queue; the rest update tables, mirrors,
//! multicast groups, and registers through the architecture.

use std::path::{Path, PathBuf};

use num_bigint::BigInt;

use crate::util::text::escape_text;

use crate::lang::{
    common::source::{Phrase, Span},
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
    traits::print::Print,
};

use crate::runner::{ExternError, Interface, Interpreter, Runner, RunnerContext};

use crate::interface::p4::{error::P4Error, parse};

use crate::stf::{
    self,
    ast::{Action, MatchKind, Name, Statement, TableMatch},
};

use crate::sim_plugin::error::{self, SimError};

use super::{
    arch::Architecture,
    io::{self, Expectation, Rx, Tx},
    state::SimState,
    table,
};

// == Helpers

/// Parses an optionally signed integer with a `0x`, `0o` or `0b` radix prefix.
fn parse_int<Int: strtoint::StrToInt>(text: &str) -> Result<Int, SimError> {
    strtoint::strtoint(&text.to_ascii_lowercase())
        .map_err(|_| error::integer_invalid(format!("invalid integer: {text}")))
}

/// Rewrites STF's `hdr$0` index spelling to the P4 `hdr[0]` form.
fn convert_dollar_to_brackets(name: &str) -> String {
    let mut text = String::new();
    let mut chars = name.chars().peekable();
    while let Some(char) = chars.next() {
        // A `$` followed by digits is an index
        if char == '$' && chars.peek().is_some_and(char::is_ascii_digit) {
            text.push('[');
            while chars.peek().is_some_and(char::is_ascii_digit) {
                text.push(chars.next().expect("peeked digit"));
            }
            text.push(']');
        } else {
            text.push(char);
        }
    }
    text
}

// == Run state

/// Pipeline values and STF queues belong to one independent input program.
pub struct Run {
    /// Pipeline state for this program.
    pub state: SimState,
    /// Outputs not yet claimed by an expectation.
    pub tx_output_queue: Vec<Tx>,
    /// Expectations not yet met by an output.
    pub expect_queue: Vec<Expectation>,
    /// Source PASS payloads, in statement order.
    pub matches: Vec<Tx>,
}

impl Run {
    /// A run with empty queues.
    pub fn new(state: SimState) -> Self {
        Self { state, tx_output_queue: vec![], expect_queue: vec![], matches: vec![] }
    }

    /// Only the first new transmission can consume a pending expectation.
    pub fn on_tx_output(&mut self) -> Result<Option<Tx>, SimError> {
        // No output: nothing to match
        let Some(tx) = self.state.txs.first() else {
            return Ok(None);
        };
        // No pending expectation on that port: queue every output
        let Some(idx) = self
            .expect_queue
            .iter()
            .position(|expect| expect.tx.port == tx.port)
        else {
            self.tx_output_queue.extend_from_slice(&self.state.txs);
            return Ok(None);
        };
        let expect = &self.expect_queue[idx];
        // A pending expectation must match, else the test fails here
        if !io::matches(tx, expect) {
            return Err(error::packet_mismatch(&expect.tx, tx));
        }
        // Consume the expectation; later outputs wait in the queue
        let expect = self.expect_queue.remove(idx);
        self.tx_output_queue.extend_from_slice(&self.state.txs[1..]);
        Ok(Some(expect.tx))
    }

    /// Matches an expectation against a queued output, or queues it.
    pub fn on_tx_expect(&mut self, expect: Expectation) -> Result<Option<Tx>, SimError> {
        // No queued output on that port: wait for one
        let Some(idx) = self
            .tx_output_queue
            .iter()
            .position(|tx| tx.port == expect.tx.port)
        else {
            self.expect_queue.push(expect);
            return Ok(None);
        };
        // The first output on the port must match
        let tx = &self.tx_output_queue[idx];
        if !io::matches(tx, &expect) {
            return Err(error::packet_mismatch(&expect.tx, tx));
        }
        Ok(Some(self.tx_output_queue.remove(idx)))
    }

    /// Fails if any output or expectation is left unmatched.
    pub fn finish(&self) -> Result<(), SimError> {
        if self.tx_output_queue.is_empty() && self.expect_queue.is_empty() {
            Ok(())
        } else {
            Err(error::packet_expectation_incomplete(&self.tx_output_queue, &self.expect_queue))
        }
    }
}

// == Pipeline initialization

/// Parses the program and initializes the pipeline in a fresh runner.
pub fn init_pipe<Interp, Iface, Arch>(
    runner: &mut Runner<Interp, Iface, Arch>,
    includes: &[PathBuf],
    path: &Path,
) -> Result<Run, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    // Each program starts from an empty arena and cleared extern state
    runner.reset();
    let program =
        parse::parse_file(runner.arena_mut(), includes, path).map_err(P4Error::into_report)?;
    let state =
        Arch::init_pipe(&mut runner.context(), program).map_err(ExternError::into_report)?;
    Ok(Run::new(state))
}

// == STF statements

/// Executes one statement; returns the output packet it matched, if any.
pub fn run_stf_stmt<Interp, Iface, Arch>(
    runner: &mut Runner<Interp, Iface, Arch>,
    run: &mut Run,
    stmt: &Phrase<Statement>,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    // Fresh output list; the architecture may rewrite the statement first
    run.state.txs.clear();
    let stmt_kind = Arch::transform_stf_stmt(stmt.node.clone());
    let mut ctx = runner.context();
    let result = match stmt_kind {
        // Packets drive the pipeline
        Statement::Packet { port, packet } => {
            run_stf_packet_stmt(&mut ctx, &stmt.span, run, port, packet)
        }
        // Expectations match outputs
        Statement::Expect { port, packet_expected, exact } => {
            run_stf_expect_stmt(run, port, packet_expected, exact)
        }
        // Table control plane
        Statement::Add { table, priority, matches, action, .. } => {
            run_stf_add_stmt(&mut ctx, &stmt.span, &mut run.state, table, priority, matches, action)
        }
        Statement::SetDefault { table, action } => {
            run_stf_set_default_stmt(&mut ctx, &stmt.span, &mut run.state, table, action)
        }
        // Mirror sessions
        Statement::MirroringAdd { session, port } => {
            run_stf_mirroring_add_stmt(&mut ctx, &stmt.span, &mut run.state, session, port)
        }
        Statement::MirroringAddMc { session, group_id } => {
            run_stf_mirroring_add_mc_stmt(&mut ctx, &stmt.span, &mut run.state, session, group_id)
        }
        // Multicast groups and nodes
        Statement::McGroupCreate { group_id } => {
            run_stf_mc_group_create_stmt(&mut ctx, &stmt.span, &mut run.state, group_id)
        }
        Statement::McNodeCreate { replication_id, ports } => {
            run_stf_mc_node_create_stmt(&mut ctx, &stmt.span, &mut run.state, replication_id, ports)
        }
        Statement::McNodeAssociate { group_id, handle } => {
            run_stf_mc_node_associate_stmt(&mut ctx, &stmt.span, &mut run.state, group_id, handle)
        }
        // Registers
        Statement::RegisterRead { name, index } => {
            run_stf_register_read_stmt(&mut ctx, &stmt.span, &mut run.state, name, index)
        }
        Statement::RegisterWrite { name, index, value } => {
            run_stf_register_write_stmt(&mut ctx, &stmt.span, &mut run.state, name, index, value)
        }
        Statement::RegisterReset { name } => {
            run_stf_register_reset_stmt(&mut ctx, &stmt.span, &mut run.state, name)
        }
        // Statements with no effect here
        Statement::MirroringGet { .. } | Statement::Wait => Ok(None),
        // Anything else is unsupported
        stmt => Err(error::statement_unsupported(Print::to_string(&stmt))),
    };
    // Attach the statement's span to failures that have none
    let tx = result.map_err(|report| {
        if stmt.span == Span::default() {
            report
        } else {
            Box::new((*report).with_span(&stmt.span))
        }
    })?;
    // Record matched outputs for the caller
    if let Some(tx) = &tx {
        run.matches.push(tx.clone());
    }
    Ok(tx)
}

// - Packet I/O

/// Drives one packet through the pipeline and matches the first output.
fn run_stf_packet_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    run: &mut Run,
    port: String,
    packet: String,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    // Payloads compare in uppercase hex
    let rx = Rx { port: parse_int::<usize>(&port)?, packet: packet.to_ascii_uppercase() };
    Arch::drive_pipe(ctx, &mut run.state, &rx)
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    run.on_tx_output()
}

/// Records an expectation, matching a queued output if one is waiting.
fn run_stf_expect_stmt(
    run: &mut Run,
    port: String,
    packet_expected: Option<String>,
    exact: bool,
) -> Result<Option<Tx>, SimError> {
    let expect = Expectation {
        tx: Tx {
            port: parse_int::<usize>(&port)?,
            packet: packet_expected.unwrap_or_default().to_ascii_uppercase(),
        },
        exact,
    };
    run.on_tx_expect(expect)
}

// - Match-action table updates

/// Encodes STF match keys as the specification's `tableKeyInterface` list.
fn encode_table_keys(arena: &mut Arena, matches: &[TableMatch]) -> Result<Value, SimError> {
    let typ_key = typ::make::var(
        crate::phrase!(node: "tableKeyInterface".to_owned(), span: Span::default()),
        vec![],
    );
    let mut values_key = Vec::new();
    for key in matches {
        let value_name = value::make::text(
            arena,
            convert_dollar_to_brackets(key.name.as_str()),
            Span::default(),
        )?;
        // Numbers keep their radix spelling as a tagged text
        let value_key = match &key.kind {
            MatchKind::Number(num) => {
                let (shape, num) = if let Some(num) = num.strip_prefix("0x") {
                    ("_HEX text", num)
                } else if let Some(num) = num.strip_prefix("0b") {
                    ("_BIN text", num)
                } else {
                    ("_DEC text", num.as_str())
                };
                let value_num = value::make::text(arena, num.to_owned(), Span::default())?;
                value::make::case_shaped! {
                    arena: arena,
                    shape: shape,
                    args: vec![value_num],
                    typ: "tableKeyValueInterface",
                    span: Span::default(),
                }?
            }
            // `prefix/mask` becomes a text and a natural
            MatchKind::Slash(prefix, mask) => {
                let value_prefix = value::make::text(arena, prefix.clone(), Span::default())?;
                let mask = BigInt::from(parse_int::<i128>(mask)?);
                let nat = crate::lang::common::prim::num::Natural::try_from(mask)?;
                let value_mask = value::make::nat(arena, nat, Span::default())?;
                value::make::case_shaped! {
                    arena: arena,
                    shape: "text _SLASH nat",
                    args: vec![value_prefix, value_mask],
                    typ: "tableKeyValueInterface",
                    span: Span::default(),
                }?
            }
        };
        values_key.push(value::make::tuple(
            arena,
            typ_key.node.clone().into(),
            vec![value_name, value_key],
            Span::default(),
        )?);
    }
    Ok(value::make::list(arena, typ::make::list(typ_key).node.into(), values_key, Span::default())?)
}

/// Adds a table entry: name, optional priority, keys, and action.
fn run_stf_add_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    state: &mut SimState,
    table: Name,
    priority: Option<i64>,
    matches: Vec<TableMatch>,
    action: Action,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    // Add names use the same escaped spelling as P4 annotation names
    let text_name = escape_text(&table.into_string());
    let value_name =
        value::make::text(ctx.arena_mut(), text_name, Span::default()).map_err(SimError::from)?;
    // Priority is optional
    let value_priority = priority
        .map(|priority| value::make::int(ctx.arena_mut(), priority.into(), Span::default()))
        .transpose()
        .map_err(SimError::from)?;
    let value_priority = value::make::opt(
        ctx.arena_mut(),
        typ::make::opt(typ::make::int()).node.into(),
        value_priority,
        Span::default(),
    )
    .map_err(SimError::from)?;
    let value_keys = encode_table_keys(ctx.arena_mut(), &matches)?;
    let value_action = encode_table_action(ctx.arena_mut(), &action)?;
    state.value_arch = table::add_entry(
        ctx,
        state.value_ctx,
        state.value_arch,
        value_name,
        value_priority,
        value_keys,
        value_action,
    )
    .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Encodes an STF action as the specification's `tableActionInterface`.
fn encode_table_action(arena: &mut Arena, action: &Action) -> Result<Value, SimError> {
    let value_name = value::make::text(arena, action.name.as_str().to_owned(), Span::default())?;
    let typ_arg = typ::make::var(
        crate::phrase!(node: "tableActionArgumentInterface".to_owned(), span: Span::default()),
        vec![],
    );
    let mut values_arg = Vec::new();
    // Each argument is a name and an integer
    for arg in &action.args {
        let value_name = value::make::text(arena, arg.id.clone(), Span::default())?;
        let int = BigInt::from(parse_int::<i128>(&arg.num)?);
        let value_int = value::make::int(arena, int, Span::default())?;
        values_arg.push(value::make::tuple(
            arena,
            typ_arg.node.clone().into(),
            vec![value_name, value_int],
            Span::default(),
        )?);
    }
    // An action is its name and its argument list
    let value_args = value::make::list(
        arena,
        typ::make::list(typ_arg).node.into(),
        values_arg,
        Span::default(),
    )?;
    Ok(value::make::tuple(
        arena,
        typ::make::var(
            crate::phrase!(node: "tableActionInterface".to_owned(), span: Span::default()),
            vec![],
        )
        .node
        .into(),
        vec![value_name, value_args],
        Span::default(),
    )?)
}

/// Sets a table's default action.
fn run_stf_set_default_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    state: &mut SimState,
    table: Name,
    action: Action,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    // Table name and action, then let the table module store it
    let value_name = value::make::text(ctx.arena_mut(), table.into_string(), Span::default())
        .map_err(SimError::from)?;
    let value_action = encode_table_action(ctx.arena_mut(), &action)?;
    state.value_arch =
        table::add_default_action(ctx, state.value_ctx, state.value_arch, value_name, value_action)
            .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

// - Mirror session updates

/// Maps a mirror session to a port.
fn run_stf_mirroring_add_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    state: &mut SimState,
    session: String,
    port: String,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    state.value_arch = Arch::add_mirror_session(
        ctx,
        state.value_arch,
        parse_int::<usize>(&session)?,
        parse_int::<usize>(&port)?,
    )
    .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Maps a mirror session to a multicast group.
fn run_stf_mirroring_add_mc_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    state: &mut SimState,
    session: String,
    id_group: String,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    state.value_arch = Arch::add_mirror_session_mc(
        ctx,
        state.value_arch,
        parse_int::<usize>(&session)?,
        parse_int::<usize>(&id_group)?,
    )
    .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

// - Multicast group updates

/// Creates a multicast group.
fn run_stf_mc_group_create_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    state: &mut SimState,
    id_group: String,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    state.value_arch = Arch::mc_mgrp_create(ctx, state.value_arch, parse_int::<usize>(&id_group)?)
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Creates a multicast node over the listed ports.
fn run_stf_mc_node_create_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    state: &mut SimState,
    id_replication: String,
    ports: Vec<String>,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    let instance = parse_int::<usize>(&id_replication)?;
    let ports = ports
        .iter()
        .map(|port| parse_int::<usize>(port))
        .collect::<Result<Vec<_>, _>>()?;
    state.value_arch = Arch::mc_node_create(ctx, state.value_arch, instance, &ports)
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Adds a multicast node to a group.
fn run_stf_mc_node_associate_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    state: &mut SimState,
    id_group: String,
    handle: String,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    state.value_arch = Arch::mc_node_associate(
        ctx,
        state.value_arch,
        parse_int::<usize>(&id_group)?,
        parse_int::<usize>(&handle)?,
    )
    .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

// - Register updates

/// Reads a register cell.
fn run_stf_register_read_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    state: &mut SimState,
    name: Name,
    idx: String,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    state.value_arch =
        Arch::register_read(ctx, state.value_arch, name.as_str(), parse_int::<usize>(&idx)?)
            .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Writes a register cell.
fn run_stf_register_write_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    state: &mut SimState,
    name: Name,
    idx: String,
    value: String,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    state.value_arch = Arch::register_write(
        ctx,
        state.value_arch,
        name.as_str(),
        parse_int::<usize>(&idx)?,
        BigInt::from(parse_int::<i128>(&value)?),
    )
    .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Clears a register.
fn run_stf_register_reset_stmt<Interp, Iface, Arch>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Arch>,
    span: &Span,
    state: &mut SimState,
    name: Name,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    state.value_arch = Arch::register_reset(ctx, state.value_arch, name.as_str())
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

// == STF tests

/// Runs a whole STF file against a P4 program; fails on the first problem.
pub fn run_stf_test<Interp, Iface, Arch>(
    runner: &mut Runner<Interp, Iface, Arch>,
    includes: &[PathBuf],
    path_p4: &Path,
    path_stf: &Path,
    on_match: &mut dyn FnMut(&Tx),
) -> Result<Run, SimError>
where
    Iface: Interface,
    Arch: Architecture,
    Interp: Interpreter<Iface, Arch>,
{
    let mut run = init_pipe(runner, includes, path_p4)?;
    let stmts = stf::parse::parse_file(path_stf)?;
    for stmt in &stmts {
        if let Some(tx) = run_stf_stmt(runner, &mut run, stmt)? {
            on_match(&tx);
        }
    }
    // Everything expected must have arrived, and nothing unexpected
    run.finish()?;
    Ok(run)
}
