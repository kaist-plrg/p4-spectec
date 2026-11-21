//! Transforms and executes STF statements against one runner and per-run state
//!
//! `run_stf_test` parses the P4 program, initializes the pipeline,
//! and executes the STF statements in order.
//! Packets drive the pipeline; `expect` lines are matched against outputs
//! in either order, so both keep a queue; the rest update tables, mirrors,
//! multicast groups, and registers through the architecture.
//! `expect` holds an `expect` line and matches outputs against it,
//! comparing packets nibble by nibble through `match`.

use std::path::{Path, PathBuf};

use num_bigint::BigInt;

use p4spectec::util::text::escape_text;

use p4spectec::lang::{
    common::source::{Phrase, Span},
    traits::print::Print,
};

use p4spectec::runner::{ExternError, Interface, InterfaceError, Runner};

use crate::p4::{
    ActionCall, EntryStatus, MatchKey, MatchKeyParam, MirroringSessionConfig, P4Value, Radix,
    RuntimeInterface, Sim, TableEntry,
};

use crate::spec::host::{ArchHost, Arena, parse_program, with_host};

use crate::stf::{
    self,
    syntax::ast::{Action, MatchKind, Name, Statement, TableMatch},
};

use crate::error::{self, SimError};

use crate::arch::{
    io::{Rx, Tx},
    state::SimState,
};

use super::transform;

use self::expect::Expectation;

pub(crate) mod expect;
mod r#match;

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
        if !expect::matches(tx, expect) {
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
        if !expect::matches(tx, &expect) {
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

// == Run initialization

/// Parses the program and initializes the pipeline in a fresh runner.
pub fn init_run<Iface, Arch>(
    arena: &mut Arena,
    runner: &mut Runner<Iface, Arch>,
    includes: &[PathBuf],
    path: &Path,
) -> Result<Run, SimError>
where
    Iface: Interface,
    Arch: RuntimeInterface,
{
    // Each program starts from an empty arena and cleared extern state
    runner.reset();
    arena.reset_values();
    let program = parse_program(arena, runner, includes, &[path.to_path_buf()])
        .map_err(InterfaceError::into_report)?;
    let state = with_host(arena, runner, |host| Arch::init_pipe(host, program))
        .map_err(ExternError::into_report)?;
    Ok(Run::new(state))
}

// == STF statements

/// Executes one statement; returns the output packet it matched, if any.
pub fn run_stf_stmt<Iface, Arch>(
    arena: &mut Arena,
    runner: &mut Runner<Iface, Arch>,
    run: &mut Run,
    stmt: &Phrase<Statement>,
) -> Result<Option<Tx>, SimError>
where
    Iface: Interface,
    Arch: RuntimeInterface,
{
    // Fresh output list, then p4c's names rewritten to the specification's
    run.state.txs.clear();
    let stmt_kind = transform::transform_stmt(Arch::NAME, stmt.node.clone());
    let result =
        with_host(arena, runner, |host| run_stf_stmt_kind(host, &stmt.span, run, stmt_kind));
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

/// Executes a transformed statement through a stage-specific context.
fn run_stf_stmt_kind<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    run: &mut Run,
    stmt_kind: Statement,
) -> Result<Option<Tx>, SimError> {
    match stmt_kind {
        // Packets drive the pipeline
        Statement::Packet { port, packet } => run_stf_packet_stmt(host, span, run, port, packet),
        // Expectations match outputs
        Statement::Expect { port, packet_expected, exact } => {
            run_stf_expect_stmt(run, port, packet_expected, exact)
        }
        // Table control plane
        Statement::Add { table, priority, matches, action, .. } => {
            run_stf_add_stmt(host, span, &mut run.state, table, priority, matches, action)
        }
        Statement::SetDefault { table, action } => {
            run_stf_set_default_stmt(host, span, &mut run.state, table, action)
        }
        // Mirror sessions
        Statement::MirroringAdd { session, port } => {
            run_stf_mirroring_add_stmt(host, span, &mut run.state, session, port)
        }
        Statement::MirroringAddMc { session, group_id } => {
            run_stf_mirroring_add_mc_stmt(host, span, &mut run.state, session, group_id)
        }
        // Multicast groups and nodes
        Statement::McGroupCreate { group_id } => {
            run_stf_mc_group_create_stmt(host, span, &mut run.state, group_id)
        }
        Statement::McNodeCreate { replication_id, ports } => {
            run_stf_mc_node_create_stmt(host, span, &mut run.state, replication_id, ports)
        }
        Statement::McNodeAssociate { group_id, handle } => {
            run_stf_mc_node_associate_stmt(host, span, &mut run.state, group_id, handle)
        }
        // Registers
        Statement::RegisterRead { name, index } => {
            run_stf_register_read_stmt(host, span, &mut run.state, name, index)
        }
        Statement::RegisterWrite { name, index, value } => {
            run_stf_register_write_stmt(host, span, &mut run.state, name, index, value)
        }
        Statement::RegisterReset { name } => {
            run_stf_register_reset_stmt(host, span, &mut run.state, name)
        }
        // Statements with no effect here
        Statement::MirroringGet { .. } | Statement::Wait => Ok(None),
        // Anything else is unsupported
        stmt => Err(error::statement_unsupported(Print::to_string(&stmt))),
    }
}

// - Packet I/O

/// Drives one packet through the pipeline and matches the first output.
fn run_stf_packet_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    run: &mut Run,
    port: String,
    packet: String,
) -> Result<Option<Tx>, SimError> {
    // Payloads compare in uppercase hex
    let rx = Rx { port: parse_int::<usize>(&port)?, packet: packet.to_ascii_uppercase() };
    Arch::drive_pipe(&mut Sim::new(host, &mut run.state), &rx)
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

/// An STF match key as a table entry key; a number keeps its radix spelling.
fn match_key(key: &TableMatch) -> Result<MatchKey, SimError> {
    let param = match &key.kind {
        MatchKind::Number(num) => {
            let (radix, digits) = if let Some(digits) = num.strip_prefix("0x") {
                (Radix::Hex, digits)
            } else if let Some(digits) = num.strip_prefix("0b") {
                (Radix::Bin, digits)
            } else {
                (Radix::Dec, num.as_str())
            };
            MatchKeyParam::Number { radix, digits: digits.to_owned() }
        }
        MatchKind::Slash(value, len) => {
            MatchKeyParam::Prefix { value: value.clone(), len: parse_int::<usize>(len)? }
        }
    };
    Ok(MatchKey { name: convert_dollar_to_brackets(key.name.as_str()), param })
}

/// An STF action as an action call with integer arguments.
fn action_call(action: &Action) -> Result<ActionCall, SimError> {
    let args = action
        .args
        .iter()
        .map(|arg| Ok((arg.id.clone(), BigInt::from(parse_int::<i128>(&arg.num)?))))
        .collect::<Result<Vec<_>, SimError>>()?;
    Ok(ActionCall { name: action.name.as_str().to_owned(), args })
}

/// Adds a table entry: name, optional priority, keys, and action.
///
/// When the table rejects the STF key names,
/// the entry is tried once more with the table's own key names in order,
/// skipping `selector` keys, which take no STF value.
fn run_stf_add_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    state: &mut SimState,
    table: Name,
    priority: Option<i64>,
    matches: Vec<TableMatch>,
    action: Action,
) -> Result<Option<Tx>, SimError> {
    let failure =
        |error: ExternError| error::statement_execution_failure(span, error.into_report());
    // Add names use the same escaped spelling as P4 annotation names
    let mut entry = TableEntry {
        table: escape_text(&table.into_string()),
        priority,
        keys: matches.iter().map(match_key).collect::<Result<_, _>>()?,
        action: action_call(&action)?,
    };
    let mut sim = Sim::new(host, state);
    if sim.add_entry(&entry).map_err(failure)? == EntryStatus::Added {
        return Ok(None);
    }
    // Rename the keys after the table's own, in order
    let names: Vec<_> = sim
        .get_keys(&entry.table)
        .map_err(failure)?
        .into_iter()
        .filter(|key| key.match_kind != "selector")
        .map(|key| key.name)
        .collect();
    if names.len() != entry.keys.len() {
        return Err(failure(
            error::table_entry_invalid(format!(
                "table has {} keys, but the entry has {}",
                names.len(),
                entry.keys.len()
            ))
            .into(),
        ));
    }
    for (key, name) in entry.keys.iter_mut().zip(names) {
        key.name = name;
    }
    // A second rejection is final
    match sim.add_entry(&entry).map_err(failure)? {
        EntryStatus::Added => Ok(None),
        EntryStatus::Rejected => {
            Err(failure(error::table_entry_invalid("table entry rejected".to_owned()).into()))
        }
    }
}

/// Sets a table's default action.
fn run_stf_set_default_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    state: &mut SimState,
    table: Name,
    action: Action,
) -> Result<Option<Tx>, SimError> {
    let action = action_call(&action)?;
    Sim::new(host, state)
        .set_default_action(&table.into_string(), &action)
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

// - Mirror session updates

/// Maps a mirror session to a port.
fn run_stf_mirroring_add_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    state: &mut SimState,
    session: String,
    port: String,
) -> Result<Option<Tx>, SimError> {
    let config = MirroringSessionConfig::EgressPort(parse_int::<usize>(&port)?);
    Arch::mirroring_add_session(&mut Sim::new(host, state), parse_int::<usize>(&session)?, config)
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Maps a mirror session to a multicast group.
fn run_stf_mirroring_add_mc_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    state: &mut SimState,
    session: String,
    id_group: String,
) -> Result<Option<Tx>, SimError> {
    let config = MirroringSessionConfig::Mgid(parse_int::<usize>(&id_group)?);
    Arch::mirroring_add_session(&mut Sim::new(host, state), parse_int::<usize>(&session)?, config)
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

// - Multicast group updates

/// Creates a multicast group.
fn run_stf_mc_group_create_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    state: &mut SimState,
    id_group: String,
) -> Result<Option<Tx>, SimError> {
    Arch::mc_mgrp_create(&mut Sim::new(host, state), parse_int::<usize>(&id_group)?)
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Creates a multicast node over the listed ports.
fn run_stf_mc_node_create_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    state: &mut SimState,
    id_replication: String,
    ports: Vec<String>,
) -> Result<Option<Tx>, SimError> {
    let instance = parse_int::<usize>(&id_replication)?;
    let ports = ports
        .iter()
        .map(|port| parse_int::<usize>(port))
        .collect::<Result<Vec<_>, _>>()?;
    Arch::mc_node_create(&mut Sim::new(host, state), instance, &ports)
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Adds a multicast node to a group.
fn run_stf_mc_node_associate_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    state: &mut SimState,
    id_group: String,
    handle: String,
) -> Result<Option<Tx>, SimError> {
    Arch::mc_node_associate(
        &mut Sim::new(host, state),
        parse_int::<usize>(&id_group)?,
        parse_int::<usize>(&handle)?,
    )
    .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

// - Register updates

/// Reads a register cell.
fn run_stf_register_read_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    state: &mut SimState,
    name: Name,
    idx: String,
) -> Result<Option<Tx>, SimError> {
    // The value read is not printed
    Arch::register_read(&mut Sim::new(host, state), name.as_str(), parse_int::<usize>(&idx)?)
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Writes a register cell.
fn run_stf_register_write_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    state: &mut SimState,
    name: Name,
    idx: String,
    value: String,
) -> Result<Option<Tx>, SimError> {
    Arch::register_write(
        &mut Sim::new(host, state),
        name.as_str(),
        parse_int::<usize>(&idx)?,
        P4Value::InfInt(parse_int::<i128>(&value)?.into()),
    )
    .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

/// Clears a register.
fn run_stf_register_reset_stmt<Arch: RuntimeInterface>(
    host: &mut dyn ArchHost<Arch>,
    span: &Span,
    state: &mut SimState,
    name: Name,
) -> Result<Option<Tx>, SimError> {
    Arch::register_reset(&mut Sim::new(host, state), name.as_str())
        .map_err(|error| error::statement_execution_failure(span, error.into_report()))?;
    Ok(None)
}

// == STF tests

/// Runs a whole STF file against a P4 program; fails on the first problem.
pub fn run_stf_test<Iface, Arch>(
    arena: &mut Arena,
    runner: &mut Runner<Iface, Arch>,
    includes: &[PathBuf],
    path_p4: &Path,
    path_stf: &Path,
    on_match: &mut dyn FnMut(&Tx),
) -> Result<Run, SimError>
where
    Iface: Interface,
    Arch: RuntimeInterface,
{
    let mut run = init_run(arena, runner, includes, path_p4)?;
    let stmts = stf::syntax::parse::parse_file(path_stf)?;
    for stmt in &stmts.0 {
        if let Some(tx) = run_stf_stmt(arena, runner, &mut run, stmt)? {
            on_match(&tx);
        }
    }
    // Everything expected must have arrived, and nothing unexpected
    run.finish()?;
    Ok(run)
}
