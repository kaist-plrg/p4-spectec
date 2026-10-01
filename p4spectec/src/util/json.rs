//! Ordering for serde JSON values

use std::cmp::Ordering;

use serde_json::Number;

#[allow(non_camel_case_types)]
/// A serde JSON value.
pub type json = serde_json::Value;

/// A rank ordering the JSON kinds.
fn rank(json: &json) -> u8 {
    match json {
        json::Null => 0,
        json::Bool(_) => 1,
        json::Number(_) => 2,
        json::String(_) => 3,
        json::Array(_) => 4,
        json::Object(_) => 5,
    }
}

/// Orders two JSON numbers, integers before floats.
fn compare_num(num_l: &Number, num_r: &Number) -> Ordering {
    let int = |num: &Number| {
        num.as_i64()
            .map(i128::from)
            .or_else(|| num.as_u64().map(i128::from))
    };
    match (int(num_l), int(num_r)) {
        (Some(int_l), Some(int_r)) => int_l.cmp(&int_r),
        (Some(_), None) => Ordering::Less,
        (None, Some(_)) => Ordering::Greater,
        (None, None) => num_l
            .as_f64()
            .expect("finite JSON float")
            .partial_cmp(&num_r.as_f64().expect("finite JSON float"))
            .expect("finite JSON floats have a total order"),
    }
}

/// Orders two JSON values structurally, independent of object key order.
pub(crate) fn compare(json_l: &json, json_r: &json) -> Ordering {
    match (json_l, json_r) {
        (json::Null, json::Null) => Ordering::Equal,
        (json::Bool(bool_l), json::Bool(bool_r)) => bool_l.cmp(bool_r),
        (json::Number(num_l), json::Number(num_r)) => compare_num(num_l, num_r),
        (json::String(text_l), json::String(text_r)) => text_l.cmp(text_r),
        (json::Array(jsons_l), json::Array(jsons_r)) => jsons_l
            .iter()
            .zip(jsons_r)
            .map(|(json_l, json_r)| compare(json_l, json_r))
            .find(|order| !order.is_eq())
            .unwrap_or_else(|| jsons_l.len().cmp(&jsons_r.len())),
        (json::Object(fields_l), json::Object(fields_r)) => {
            // Preserve key-order independence when serde's preserve_order is on
            let mut fields_l = fields_l.iter().collect::<Vec<_>>();
            let mut fields_r = fields_r.iter().collect::<Vec<_>>();
            fields_l.sort_unstable_by_key(|(name, _)| *name);
            fields_r.sort_unstable_by_key(|(name, _)| *name);
            fields_l
                .iter()
                .zip(&fields_r)
                .map(|((name_l, json_l), (name_r, json_r))| {
                    name_l.cmp(name_r).then_with(|| compare(json_l, json_r))
                })
                .find(|order| !order.is_eq())
                .unwrap_or_else(|| fields_l.len().cmp(&fields_r.len()))
        }
        _ => rank(json_l).cmp(&rank(json_r)),
    }
}
