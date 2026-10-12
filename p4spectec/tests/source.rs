//! Source location compatibility across representation changes

use p4spectec::lang::common::source::{Position, Span};
use serde_json::json;

#[test]
fn covering_spans_orders_file_names() {
    let span_z: Span = serde_json::from_value(json!({
        "left": { "file": "z.watsup", "line": 1, "column": 2 },
        "right": { "file": "z.watsup", "line": 3, "column": 4 }
    }))
    .unwrap();
    let span_a: Span = serde_json::from_value(json!({
        "left": { "file": "a.watsup", "line": 5, "column": 6 },
        "right": { "file": "a.watsup", "line": 7, "column": 8 }
    }))
    .unwrap();
    let span = Span::over(&[span_z, span_a]);
    assert_eq!(
        serde_json::to_value(&span).unwrap(),
        json!({
            "left": { "file": "a.watsup", "line": 5, "column": 6 },
            "right": { "file": "z.watsup", "line": 3, "column": 4 }
        })
    );
    assert_eq!(span, Span::over(&[span_a, span_z]));
}

#[test]
fn spans_roundtrip_file_names() {
    let json = json!({
        "left": { "file": "source.watsup", "line": 2, "column": 3 },
        "right": { "file": "source.watsup", "line": 2, "column": 7 }
    });
    let span: Span = serde_json::from_value(json.clone()).unwrap();
    let span_other: Span = serde_json::from_value(json.clone()).unwrap();
    assert_eq!(span, span_other);
    assert_eq!(serde_json::to_value(&span).unwrap(), json);
    assert_eq!(span.to_string(), "source.watsup:2.4-2.8");
}

#[test]
fn generated_spans_keep_empty_file_names() {
    let span = Span::default();
    assert_eq!(span.to_string(), "");
    assert_eq!(Span::over(&[]), span);
    assert_eq!(
        serde_json::to_value(span).unwrap(),
        json!({
            "left": { "file": "", "line": 0, "column": 0 },
            "right": { "file": "", "line": 0, "column": 0 }
        })
    );
}

#[test]
fn maximum_column_prints_one_based() {
    let pos: Position = serde_json::from_value(json!({
        "file": "wide.watsup", "line": 1, "column": 4294967295_u64
    }))
    .unwrap();
    assert_eq!(pos.to_string(), "1.4294967296");
}
