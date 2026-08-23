open Source

let string_of_error at msg =
  if at = no_region then msg else string_of_region at ^ ": " ^ msg
