open Il.Ast
module Value = Runtime_dynamic.Value
module Cache = Runtime_dynamic.Cache
module F = Format
open Attempt
open Util.Source

type res =
  | Pass of value list
  | Fail of region * string
  | IllFormed of region * string

let run ?(debug : bool = false) ?(profile : bool = false)
    ?(trace : bool = false) (spec : spec) (relname : string)
    (includes_p4 : string list) (filename_p4 : string) : res =
  Builtin.init ();
  Value.refresh ();
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache;
  Trace.set_mode (if trace then Trace.Full else Trace.Concise);
  try
    let value_program = Interface.Parse.parse_file includes_p4 filename_p4 in
    let ctx = Ctx.empty ~debug ~profile filename_p4 in
    let ctx = Interp.load_spec ctx spec in
    let+ ctx, values =
      Interp.invoke_rel ctx (relname $ no_region) [ value_program ]
    in
    Ctx.profile ctx;
    Pass values
  with
  | Util.Error.ParseError (at, msg) -> IllFormed (at, msg)
  | Util.Error.InterpError (at, msg) -> Fail (at, msg)
