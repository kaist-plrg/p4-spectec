type style = Bold | Dim | Red | Yellow | Blue | Magenta | Cyan
type t = { enabled : bool }

let plain = { enabled = false }
let color = { enabled = true }
let auto ~tty = if tty && Sys.getenv_opt "NO_COLOR" = None then color else plain

let code = function
  | Bold -> "\027[1m"
  | Dim -> "\027[2m"
  | Red -> "\027[31m"
  | Yellow -> "\027[33m"
  | Blue -> "\027[34m"
  | Magenta -> "\027[35m"
  | Cyan -> "\027[36m"

let reset = "\027[0m"

let style ansi styles s =
  if (not ansi.enabled) || styles = [] then s
  else String.concat "" (List.map code styles) ^ s ^ reset
