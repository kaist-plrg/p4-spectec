use crate::interp::report::ReportExt;

use p4spec_rust::{
    diagnostic::{Label, Report},
    interp::{
        al::backtrack::{choose_deterministic, choose_sequential},
        shared::{
            backtrack::Backtrack,
            error::{self, EntityKind, Error},
        },
    },
    lang::common::source::{Position, Span},
    runner::InterpreterError,
};

fn report(name: &str, line: usize) -> Error {
    let span =
        Span::new(Position::new("choice.watsup", line, 0), Position::new("choice.watsup", line, 1));
    let diagnostic = error::context::binding_undefined(EntityKind::Value, name.into())
        .with_label(Label::primary(&span, ""));
    Box::new(Report::from(diagnostic))
}

fn mismatch<T>(name: &str, line: usize) -> Backtrack<T> {
    Err(InterpreterError::Mismatch(vec![*report(name, line)]))
}

#[test]
fn sequential_choice_stops_at_first_success_or_fatal() {
    for fatal in [false, true] {
        let mut visited = Vec::new();
        let result = choose_sequential([0, 1, 2], |idx| {
            visited.push(*idx);
            match idx {
                0 => mismatch("first", 1),
                1 if fatal => Err(InterpreterError::Fatal(report("fatal", 2))),
                _ => Ok(42),
            }
        });
        assert_eq!(visited, [0, 1]);
        if fatal {
            let InterpreterError::Fatal(report) = result.unwrap_err() else {
                panic!("expected fatal")
            };
            assert_eq!(report.span().left.line, 2);
        } else {
            assert_eq!(result.unwrap(), 42);
        }
    }
}

#[test]
fn exhausted_choices_select_later_ties_or_merge_deterministic_failures() {
    for det in [false, true] {
        let result: Backtrack<()> = if det {
            choose_deterministic(
                [1, 2],
                |line| mismatch("candidate", *line),
                |_, _| panic!("no success"),
            )
        } else {
            choose_sequential([1, 2], |line| mismatch("candidate", *line))
        };
        let InterpreterError::Mismatch(reports) = result.unwrap_err() else {
            panic!("expected mismatch")
        };
        assert_eq!(
            reports
                .iter()
                .map(|report| report.span().left.line)
                .collect::<Vec<_>>(),
            if det { vec![1, 2] } else { vec![2] }
        );
    }
    let empty: Backtrack<()> = choose_sequential([], |_: &()| panic!("empty choice evaluated"));
    assert!(matches!(empty, Err(InterpreterError::Mismatch(reports)) if reports.is_empty()));
    let empty: Backtrack<()> = choose_deterministic(
        [],
        |_: &()| panic!("empty choice evaluated"),
        |_, _| panic!("overlap"),
    );
    assert!(matches!(empty, Err(InterpreterError::Mismatch(reports)) if reports.is_empty()));
}

#[test]
fn sequential_choice_preserves_the_deepest_failure_set() {
    for (depths, line_expect) in [([1, 3, 2], 2), ([3, 1, 3], 3), ([3, 0, 0], 1)] {
        let result: Backtrack<()> = choose_sequential(0..depths.len(), |idx| {
            let depth = depths[*idx];
            if depth == 0 {
                return Err(InterpreterError::Mismatch(vec![]));
            }
            // Keep both nested causes and a sibling in the selected candidate
            let line = idx + 1;
            let mut error = *report("nested", line);
            for _ in 1..depth {
                error = Report::frame(error.span(), "call", vec![error]);
            }
            Err(InterpreterError::Mismatch(vec![error, *report("sibling", line + 10)]))
        });
        let InterpreterError::Mismatch(reports) = result.unwrap_err() else {
            panic!("expected mismatch")
        };
        assert_eq!(reports.len(), 2, "depths={depths:?}");
        assert_eq!(reports[0].span().left.line, line_expect);
        assert_eq!(reports[1].span().left.line, line_expect + 10);
        let cause = reports[0].find_code("runtime/binding-undefined").unwrap();
        assert_eq!(cause.span().left.line, line_expect);
        assert!(cause.diagnostic().message.contains("nested"));
        assert!(reports[1].diagnostic().message.contains("sibling"));
    }
}

#[test]
fn deterministic_choice_rejects_equal_successes_and_stops() {
    let mut visited = Vec::new();
    let result = choose_deterministic(
        ["miss", "first", "second", "later"],
        |id| {
            visited.push(*id);
            if *id == "miss" { mismatch("miss", 1) } else { Ok(42) }
        },
        |id_a, id_b| report(&format!("{id_a}, {id_b}"), 3),
    );
    assert_eq!(visited, ["miss", "first", "second"]);
    let InterpreterError::Fatal(report) = result.unwrap_err() else { panic!("expected fatal") };
    assert!(report.diagnostic().message.contains("first, second"));
}

#[test]
fn fatal_before_or_after_success_wins_without_retry() {
    for success in [false, true] {
        let mut visited = Vec::new();
        let result: Backtrack<u32> = choose_deterministic(
            [0, 1, 2],
            |idx| {
                visited.push(*idx);
                match idx {
                    0 if success => Ok(7),
                    0 => mismatch("miss", 1),
                    1 => Err(InterpreterError::Fatal(report("fatal", 2))),
                    _ => panic!("evaluated after fatal"),
                }
            },
            |_, _| panic!("one success"),
        );
        assert_eq!(visited, [0, 1]);
        assert!(matches!(result, Err(InterpreterError::Fatal(_))));
    }
}

#[test]
fn unique_success_survives_mismatches_without_constructing_overlap() {
    let mut visited = Vec::new();
    let result = choose_deterministic(
        [0, 1, 2],
        |idx| {
            visited.push(*idx);
            if *idx == 1 { Ok(7) } else { mismatch("miss", 1) }
        },
        |_, _| panic!("unique success constructed overlap"),
    );
    assert_eq!(visited, [0, 1, 2]);
    assert_eq!(result.unwrap(), 7);
}
