//! A normal form of KORE terms, to compare a run with krun's output
//!
//! It covers how krun writes a configuration, not what K computes:
//! associativity sugar expanded; for Map and Set sorts, nested concatenations
//! flattened, units dropped, and elements sorted by their text (lists keep
//! their order); variables named as the LLVM backend's printer names them,
//! and bound variables then named by binder depth, since the printer names
//! them afresh (`substitution.md`).
//! For example, `\left-assoc{}(_Set_(SetItem(2), SetItem(1)))` and
//! `_Set_(SetItem(1), _Set_(SetItem(2), .Set))` have one normal form.
//!
//! The printer of the LLVM backend (`runtime/util/ConfigurationPrinter.cpp`)
//! takes each binder's variable as a variable of its own and free variables
//! as one per name, and names each when first printed, adding a number from
//! one counter if another variable has that name already: krun prints
//! `(\y.y) y` as `(\y.y) y0`. Primed names (`y'`, from the specification's
//! substitution) are named as the name without primes.

use std::collections::{HashMap, HashSet};

use super::{
    ast::{Pattern, Sort},
    info::Info,
};

// == Associativity sugar

/// `\left-assoc{}(f(a, b, c))` as `f(f(a, b), c)`, and
/// `\right-assoc{}(f(a, b, c))` as `f(a, f(b, c))`.
fn desugar_assoc(pattern: &Pattern) -> Pattern {
    stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || match pattern {
        Pattern::App(assoc, _, args)
            if (assoc == "\\left-assoc" || assoc == "\\right-assoc")
                && matches!(args.as_slice(), [Pattern::App(..)]) =>
        {
            let [Pattern::App(f, sorts, elems)] = args.as_slice() else {
                unreachable!("checked above")
            };
            let mut elems: Vec<Pattern> = elems.iter().map(desugar_assoc).collect();
            if elems.is_empty() {
                return Pattern::App(f.clone(), sorts.clone(), elems);
            }
            let join = |l, r| Pattern::App(f.clone(), sorts.clone(), vec![l, r]);
            if assoc == "\\left-assoc" {
                let first = elems.remove(0);
                elems.into_iter().fold(first, join)
            } else {
                let last = elems.pop().expect("not empty");
                elems
                    .into_iter()
                    .rev()
                    .fold(last, |acc, elem| join(elem, acc))
            }
        }
        Pattern::App(f, sorts, args) => {
            Pattern::App(f.clone(), sorts.clone(), args.iter().map(desugar_assoc).collect())
        }
        pattern => pattern.clone(),
    })
}

// == Variable names

/// A KVar domain value `\dv{S}("x")`: its sort and name.
fn kvar<'p>(info: &Info, pattern: &'p Pattern) -> Option<(&'p Sort, &'p str)> {
    match pattern {
        Pattern::App(dv, sorts, args) if dv == "\\dv" => {
            match (sorts.as_slice(), args.as_slice()) {
                ([sort @ Sort::Sort(name, _)], [Pattern::Str(x)])
                    if info.sort_hook(name) == Some("KVAR.KVar") =>
                {
                    Some((sort, x.as_str()))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// The variable a binder binds, its first argument `\dv{S}("x")`.
fn bound(pattern: &Pattern) -> Option<(&Sort, &str)> {
    match pattern {
        Pattern::App(dv, sorts, args) if dv == "\\dv" => {
            match (sorts.as_slice(), args.as_slice()) {
                ([sort], [Pattern::Str(x)]) => Some((sort, x.as_str())),
                _ => None,
            }
        }
        _ => None,
    }
}

/// The domain value `\dv{S}("x")`.
fn dv(sort: &Sort, x: String) -> Pattern {
    Pattern::App("\\dv".to_owned(), vec![sort.clone()], vec![Pattern::Str(x)])
}

/// Variable names as the LLVM backend's printer gives them.
struct Printer<'a> {
    info: &'a Info,
    used: HashSet<String>,
    free: HashMap<String, String>,
    counter: usize,
}

impl Printer<'_> {
    /// A new name for a variable named `x`, with a number if it is taken.
    fn name(&mut self, x: &str) -> String {
        let base = x.trim_end_matches('\'');
        let mut suffix = String::new();
        while self.used.contains(&format!("{base}{suffix}")) {
            suffix = self.counter.to_string();
            self.counter += 1;
        }
        let y = format!("{base}{suffix}");
        self.used.insert(y.clone());
        y
    }

    /// A pattern with its variables renamed; `env` maps bound names.
    fn walk(&mut self, env: &[(String, String)], pattern: &Pattern) -> Pattern {
        stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || {
            if let Some((sort, x)) = kvar(self.info, pattern) {
                if let Some((_, y)) = env.iter().rev().find(|(x_e, _)| x_e == x) {
                    return dv(sort, y.clone());
                }
                let y = match self.free.get(x) {
                    Some(y) => y.clone(),
                    None => {
                        let y = self.name(x);
                        self.free.insert(x.to_owned(), y.clone());
                        y
                    }
                };
                return dv(sort, y);
            }
            let Pattern::App(f, sorts, args) = pattern else {
                return pattern.clone();
            };
            if self.info.is_binder(f)
                && let Some((first, rest)) = args.split_first()
                && let Some((sort, x)) = bound(first)
            {
                let y = self.name(x);
                let mut env = env.to_vec();
                env.push((x.to_owned(), y.clone()));
                let mut args = vec![dv(sort, y)];
                args.extend(rest.iter().map(|arg| self.walk(&env, arg)));
                return Pattern::App(f.clone(), sorts.clone(), args);
            }
            let args = args.iter().map(|arg| self.walk(env, arg)).collect();
            Pattern::App(f.clone(), sorts.clone(), args)
        })
    }
}

/// Bound variables named by binder depth, `#bound0`, `#bound1`, ...
fn alpha(info: &Info, env: &[(String, String)], depth: usize, pattern: &Pattern) -> Pattern {
    stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || {
        if let Some((sort, x)) = kvar(info, pattern) {
            return match env.iter().rev().find(|(x_e, _)| x_e == x) {
                Some((_, y)) => dv(sort, y.clone()),
                None => pattern.clone(),
            };
        }
        let Pattern::App(f, sorts, args) = pattern else {
            return pattern.clone();
        };
        if info.is_binder(f)
            && let Some((first, rest)) = args.split_first()
            && let Some((sort, x)) = bound(first)
        {
            let y = format!("#bound{depth}");
            let mut env = env.to_vec();
            env.push((x.to_owned(), y.clone()));
            let mut args = vec![dv(sort, y)];
            args.extend(rest.iter().map(|arg| alpha(info, &env, depth + 1, arg)));
            return Pattern::App(f.clone(), sorts.clone(), args);
        }
        let args = args
            .iter()
            .map(|arg| alpha(info, env, depth, arg))
            .collect();
        Pattern::App(f.clone(), sorts.clone(), args)
    })
}

// == Unordered collections

/// The text of a pattern in normal form, and for a Map or Set its
/// concatenation and sorted elements.
fn norm(
    unordered: &HashMap<&str, &str>,
    pattern: &Pattern,
) -> (String, Option<(String, Vec<String>)>) {
    stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || match pattern {
        Pattern::App(f, sorts, args) => {
            if let Some(unit) = unordered.get(f.as_str()) {
                let mut elems = Vec::new();
                for arg in args {
                    match arg {
                        Pattern::App(g, _, g_args) if g == unit && g_args.is_empty() => {}
                        arg => match norm(unordered, arg) {
                            (_, Some((g, es))) if &g == f => elems.extend(es),
                            (text, _) => elems.push(text),
                        },
                    }
                }
                // Elements in the order of their text, as krun prints them
                elems.sort();
                let text = match elems.split_first() {
                    None => format!("{unit}{{}}()"),
                    Some((first, rest)) => rest
                        .iter()
                        .fold(first.clone(), |acc, elem| format!("{f}{{}}({acc}, {elem})")),
                };
                return (text, Some((f.clone(), elems)));
            }
            let sorts = sorts
                .iter()
                .map(Sort::to_string)
                .collect::<Vec<_>>()
                .join(", ");
            let args = args
                .iter()
                .map(|arg| norm(unordered, arg).0)
                .collect::<Vec<_>>()
                .join(", ");
            (format!("{f}{{{sorts}}}({args})"), None)
        }
        pattern => (pattern.to_string(), None),
    })
}

/// The normal form of a pattern, as text.
pub fn normal_text(info: &Info, pattern: &Pattern) -> String {
    // The concatenations of Map and Set sorts, with their units
    let unordered: HashMap<&str, &str> = info
        .collections
        .iter()
        .filter(|(sort, _)| matches!(info.sort_hook(sort), Some("MAP.Map" | "SET.Set")))
        .map(|(_, constructors)| (constructors.concat.as_str(), constructors.unit.as_str()))
        .collect();
    let mut pattern = desugar_assoc(pattern);
    // Names, in a definition with binders
    if info.has_binders() {
        let mut printer = Printer { info, used: HashSet::new(), free: HashMap::new(), counter: 0 };
        pattern = printer.walk(&[], &pattern);
        pattern = alpha(info, &[], 0, &pattern);
    }
    norm(&unordered, &pattern).0
}
