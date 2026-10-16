//! Syntax model for STF commands
//!
//! Leaf strings retain their source spelling, compound actions and matches
//! preserve input order, and statement variants follow command order. For
//! example, `packet 1 00ff` retains `"1"` as its port and `"00ff"` as its
//! packet data.

use crate::lang::common::source::Phrase;

pub use super::name::Name;

// == Leaf syntax

/// An identifier, such as a table or action name.
pub type Id = String;
/// A numeric literal in its source spelling.
pub type Number = String;
/// A port number.
pub type Port = String;
/// A multicast node handle.
pub type Handle = String;
/// Packet data as hex nibbles.
pub type Packet = String;
/// Expected packet data, where `*` is a wildcard nibble.
pub type ExpectedPacket = String;
/// A clone or mirror session id.
pub type Session = String;
/// A program: statements in source order, each with its span.
pub type Program = Vec<Phrase<Statement>>;

// == Compound syntax

#[derive(Clone, Debug, PartialEq, Eq)]
/// A named action argument.
pub struct Argument {
    pub id: Id,
    pub num: Number,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// An action name with its arguments.
pub struct Action {
    pub name: Name,
    pub args: Vec<Argument>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// How a table key is matched.
pub enum MatchKind {
    /// An exact value.
    Number(Number),
    /// A value and mask.
    Slash(Number, Number),
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// A table key name and how it is matched.
pub struct TableMatch {
    pub name: Name,
    pub kind: MatchKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Which counter cell a check reads.
pub enum CounterTarget {
    /// By name.
    Id(Id),
    /// By array index.
    Index(Number),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// A comparison used in a counter check.
pub enum Condition {
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Whether a counter check reads bytes or packets.
pub enum CounterKind {
    Bytes,
    Packets,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// A counter check: an optional kind, a comparison, and a value.
pub struct CounterCheck {
    pub kind: Option<CounterKind>,
    pub condition: Condition,
    pub num: Number,
}

// == Statements

#[derive(Clone, Debug, PartialEq, Eq)]
/// One STF command: wait, table add/setdefault, packet send/expect,
/// counter and mirror and multicast and register operations.
pub enum Statement {
    /// Wait for pending packets to be processed.
    Wait,
    /// Remove every table entry.
    RemoveAll,
    Expect {
        port: Port,
        packet_expected: Option<ExpectedPacket>,
        exact: bool,
    },
    Packet {
        port: Port,
        packet: Packet,
    },
    /// Assert no packet is transmitted.
    NoPacket,
    Add {
        table: Name,
        priority: Option<i64>,
        matches: Vec<TableMatch>,
        action: Action,
        id: Option<Id>,
    },
    SetDefault {
        table: Name,
        action: Action,
    },
    CheckCounter {
        counter: Id,
        target: CounterTarget,
        check: CounterCheck,
    },
    MirroringAdd {
        session: Session,
        port: Port,
    },
    MirroringAddMc {
        session: Session,
        group_id: Id,
    },
    MirroringGet {
        session: Session,
    },
    McGroupCreate {
        group_id: Id,
    },
    McNodeCreate {
        replication_id: Id,
        ports: Vec<Port>,
    },
    McNodeAssociate {
        group_id: Id,
        handle: Handle,
    },
    RegisterRead {
        name: Name,
        index: Number,
    },
    RegisterWrite {
        name: Name,
        index: Number,
        value: Number,
    },
    RegisterReset {
        name: Name,
    },
}
