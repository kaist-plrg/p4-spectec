#!/bin/sh
# krun --parser: convert $PGM to a KORE term on stdout.
#
# `krun` has no --input json, so a JSON term always needs converting here.
# Accepting a .watsup as well means the boot step happens inside the parser,
# and a target runs in one command rather than two:
#
#   KDEF=al-kompiled krun -d al-kompiled --parser ./kast-json.sh examples/add.watsup
#
# $KDEF supplies the definition because krun passes this script only the input
# file, leaving no way to hand it a -d of its own.  $SPECTEC_BOOT overrides the
# path to spectec-boot when it is not ./spectec-boot.
#
# Scratch files go under ./.tmp/, not /tmp.  This script also creates that
# directory, because a spec that calls a builtin writes its request there from
# inside K (al/5.5-eval-call-func.k), and K rules cannot mkdir.
set -e

mkdir -p ./.tmp

# Sweep request files left by earlier runs.  K removes its own on the way out
# (`finish()` in al/6-entry.k), but a run that dies on a stuck term never gets
# there, so failed runs would otherwise accumulate one file each.  Anything
# still here belongs to a finished process, since krun is synchronous.
rm -f ./.tmp/spectec-k-??????

# krun insists $PGM be a file, so a whole spec directory (`spec/`) cannot be
# named on its command line.  `make k-typecheck` passes a one-line stub file
# holding the spec's path instead; `@` marks it, and it is resolved back here.
#
# The stub has no reader after this point, so it is removed as soon as it is
# resolved: whoever writes one does not have to come back for it.  `$1` is only
# ever the stub krun was pointed at, never a spec of the user's own.
target="$1"
case $(head -c 1 "$target" 2>/dev/null) in
  @) target=$(sed -e 's/^@//' -e 1q "$target"); rm -f "$1" ;;
esac

# A .watsup, or (via the stub above) a directory of them: boot it here so a
# target runs in one command.  Anything else is assumed to be KAST JSON already.
if [ -d "$target" ] || [ "${target%.watsup}" != "$target" ]; then
  json=$(mktemp ./.tmp/spectec-k-kast-XXXXXX.json)
  "${SPECTEC_BOOT:-./spectec-boot}" kast "$target" -o "$json"
  # Not `exec` + EXIT trap: exec replaces this shell, so the trap would never
  # fire and the booted JSON would pile up one file per run.  Run kast as a
  # child instead, then clean up and pass its status on.  `|| status=$?`
  # keeps `set -e` from exiting before the rm.
  status=0
  kast --definition "$KDEF" --input json --output kore --sort Script "$json" \
    || status=$?
  rm -f "$json"
  exit $status
fi

json="$target"

exec kast --definition "$KDEF" --input json --output kore --sort Script "$json"
