let contains_substring substr str =
  try
    let _ = Str.search_forward (Str.regexp substr) str 0 in
    true
  with Not_found -> false
