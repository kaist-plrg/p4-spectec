#!/bin/sh
# krun -pP4: convert $P4 to a KORE term of sort P4Opt on stdout.
#
# `krun -cP4=VALUE` writes VALUE to a temporary file and passes that file to
# this command, so the argument here is a file whose contents are a P4 program
# path -- not the program itself.
#
# The program is parsed by the OCaml P4 front end and emitted already wrapped as
# `someP4(<val>)` by `spectec-boot kast-p4`, so both cases arrive at sort P4Opt
# in a single `kast` parse.
set -e

KSCRATCH=./spec-meta-k

p4=$(cat "$1")

# No program: the term is the constant `noP4()`, which needs no parsing beyond
# kast's own.  `--sort P4Opt` because that is the sort of the `<p4prog>` cell.
if [ -z "$p4" ]; then
  nop4=$(mktemp $KSCRATCH/spectec-k-nop4-XXXXXX)
  printf 'noP4()' > "$nop4"
  status=0
  kast --definition "$KDEF" --output kore --sort P4Opt "$nop4" || status=$?
  rm -f "$nop4"
  exit $status
fi

# A program: boot it to a KAST JSON term, already wrapped as `someP4(...)`.
json=$(mktemp $KSCRATCH/spectec-k-p4-XXXXXX.json)

includes=""
if [ -n "$P4INCLUDE" ]; then
  includes="-i $P4INCLUDE"
fi

# shellcheck disable=SC2086
"${SPECTEC_BOOT:-./spectec-boot}" kast-p4 -p "$p4" $includes -o "$json"

status=0
kast --definition "$KDEF" --input json --output kore --sort P4Opt "$json" \
  || status=$?
rm -f "$json"
exit $status
