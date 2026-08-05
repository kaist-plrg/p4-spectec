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
set -e

case "$1" in
  *.watsup)
    json=$(mktemp)
    trap 'rm -f "$json"' EXIT
    "${SPECTEC_BOOT:-./spectec-boot}" kast "$1" -o "$json"
    ;;
  *)
    json="$1"
    ;;
esac

exec kast --definition "$KDEF" --input json --output kore --sort Script "$json"
