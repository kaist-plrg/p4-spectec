#!/bin/sh
# krun --parser: convert $PGM to a KORE term on stdout.
#
# `krun` has no --input json, so a JSON term always needs converting here.
# Accepting a .watsup as well means the boot step happens inside the parser, and
# a target runs in one command rather than two:
#
#   KDEF=al-kompiled krun -d al-kompiled \
#     --parser ./spec-meta-k/scripts/kast-json.sh examples/add.watsup
#
set -e

KSCRATCH=./spec-meta-k

# krun insists $PGM be a file, so a whole spec directory (`spec/`) cannot be
# named on its command line. `make k-typecheck` passes a one-line stub file
# holding the spec's path instead; `@` marks it, and it is resolved back here.
# The stub has no reader afterwards, so it is removed once resolved -- `$1` is
# only ever the stub krun was pointed at, never a spec of the user's own.
target="$1"
case $(head -c 1 "$target" 2>/dev/null) in
  @) target=$(sed -e 's/^@//' -e 1q "$target"); rm -f "$1" ;;
esac

# A .watsup, or (via the stub above) a directory of them: boot it here so a
# target runs in one command.  Anything else is assumed to be KAST JSON already.
if [ -d "$target" ] || [ "${target%.watsup}" != "$target" ]; then
  json=$(mktemp $KSCRATCH/spectec-k-kast-XXXXXX.json)
  "${SPECTEC_BOOT:-./spectec-boot}" kast "$target" -o "$json"
  status=0
  kast --definition "$KDEF" --input json --output kore --sort Script "$json" \
    || status=$?
  rm -f "$json"
  exit $status
fi

json="$target"

exec kast --definition "$KDEF" --input json --output kore --sort Script "$json"
