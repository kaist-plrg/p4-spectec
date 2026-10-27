//! The levels above a meta base, stored and lent for one call
//!
//! `Upper` stores them in the base runner's extern slot;
//! `Upper::lend` borrows them as `UpperMut` for one execution of the base,
//! with the definitions a reentry runs on.
//! `Top` and `TopMut` are the highest level, stored and lent.

use super::super::interp::{TowerEnv, TowerInterpState};
use super::HostLevel;
use crate::{
    interface::{
        meta::{MetaInterface, shared::Cache},
        p4::P4Interface,
    },
    lang::data::value::flat::Value,
    runner::{Extern, Host, Interface},
};

// == Stored levels

/// The levels above a meta base, which the base's specification interprets.
///
/// Storage is per `HostLevel`, at its `index`:
/// the codec cache of the boundary below each level, C of each level,
/// and the meta builtins of each level below the top.
/// The top is the P4 specification,
/// or the AL or IL program that the highest meta level interprets.
/// Caches and C are shared by every reentry depth;
/// depth 0 uses `hosts`, and each deeper depth its own.
pub(in crate::metacirc) struct Upper<Ext: Extern> {
    /// The codec cache of the boundary below each level, through the top.
    pub caches: Vec<Cache>,
    /// C of each level for the current program, loaded on first use.
    pub ctxs: Vec<Option<Value>>,
    /// The meta builtins of each level between the base and the top, at depth 0.
    pub hosts: Vec<MetaInterface>,
    pub top: Top<Ext>,
    /// Depths 1 and deeper, made on the first reentry of a program.
    pub depth_next: Option<Box<TowerInterpState>>,
}

/// The highest level above a meta base.
pub(in crate::metacirc) enum Top<Ext: Extern> {
    /// A P4 specification, answering the builtins and externs of its program.
    P4(Host<P4Interface, Ext>),
    /// The AL or IL program of the highest meta level, answering only its builtins.
    Program(MetaInterface),
}

impl<Ext: Extern> Upper<Ext> {
    /// Clears every level's state between programs.
    pub(in crate::metacirc) fn clear(&mut self) {
        self.caches.iter_mut().for_each(Cache::clear);
        self.ctxs.fill(None);
        self.hosts.iter_mut().for_each(Interface::clear);
        match &mut self.top {
            Top::P4(host) => host.reset(),
            Top::Program(interface) => interface.clear(),
        }
        self.depth_next = None;
    }

    /// Lends the stored levels at depth 0, with the definitions reentry runs on.
    pub(in crate::metacirc) fn lend<'l>(&'l mut self, env: &'l TowerEnv<'l>) -> UpperMut<'l, Ext> {
        let top = match &mut self.top {
            Top::P4(host) => {
                let (interface, external) = host.parts_mut();
                TopMut::P4 { interface, external }
            }
            Top::Program(interface) => TopMut::Program(interface),
        };
        UpperMut {
            env,
            caches: &mut self.caches,
            ctxs: &mut self.ctxs,
            hosts: &mut self.hosts,
            top,
            depth_next: &mut self.depth_next,
        }
    }
}

// == Lent levels

/// The levels above the base, borrowed for one execution of the base's specification.
pub(in crate::metacirc) struct UpperMut<'l, Ext: Extern> {
    /// Definitions shared by every depth, which a reentry runs on.
    pub env: &'l TowerEnv<'l>,
    /// The codec cache of the boundary below each level, shared by every depth.
    pub caches: &'l mut [Cache],
    /// C of each level for the current program, shared by every depth.
    pub ctxs: &'l mut [Option<Value>],
    /// The meta builtins of each level between the base and the top, at this depth.
    pub hosts: &'l mut [MetaInterface],
    pub top: TopMut<'l, Ext>,
    /// The depths beneath this one, which answer reentry.
    pub depth_next: &'l mut Option<Box<TowerInterpState>>,
}

/// The top of `UpperMut`, borrowed from `Top` or from the context of a P4 call.
pub(in crate::metacirc) enum TopMut<'l, Ext: Extern> {
    /// A P4 specification, answering the builtins and externs of its program.
    P4 { interface: &'l mut P4Interface, external: &'l mut Ext },
    /// The AL or IL program of the highest meta level, answering only its builtins.
    Program(&'l mut MetaInterface),
}

impl<'l, Ext: Extern> UpperMut<'l, Ext> {
    /// Returns the codec cache of the boundary below `level`.
    pub(in crate::metacirc) fn cache(&mut self, level: HostLevel) -> &mut Cache {
        &mut self.caches[level.index()]
    }

    /// Returns C of `level`, if loaded for the current program.
    pub(in crate::metacirc) fn ctx(&self, level: HostLevel) -> Option<Value> {
        self.ctxs[level.index()]
    }

    /// Records C of `level` for the current program.
    pub(in crate::metacirc) fn set_ctx(&mut self, level: HostLevel, value_ctx: Value) {
        self.ctxs[level.index()] = Some(value_ctx);
    }

    /// Returns the level of the top, the highest level with a boundary.
    ///
    /// A program top is one level above the last `LevelSpec`:
    /// it has a boundary but no specification, and it never reenters.
    pub(in crate::metacirc) fn level_top(&self) -> HostLevel {
        HostLevel::new(self.caches.len())
    }
}
