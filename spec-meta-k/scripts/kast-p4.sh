#!/bin/sh
# krun -pP4: convert $P4 to a KORE term of sort P4Opt on stdout.
#
# `krun -cP4=VALUE` writes VALUE to a temporary file and passes *that file* to
# this command, so the argument here is a file whose contents are a P4 program
# path -- not the program itself.  Two cases:
#
#   empty     -> noP4(),  selecting the `$main()` entry (al/6-entry.k).
#                Every examples/*.watsup run takes this path, so `make k-run`
#                behaves exactly as it did before `<p4prog>` existed.
#   a path    -> someP4(<program>), selecting the `Program_ok` entry.
#
# The program is parsed by the OCaml P4 front end and emitted already wrapped as
# `someP4(<val>)` by `spectec-boot kast-p4`, so both cases arrive at sort P4Opt
# in a single `kast` parse.
#
# $KDEF supplies the definition, as in kast-json.sh -- krun passes a parser only
# the input file, leaving no way to hand it a -d of its own.  $P4INCLUDE gives
# the P4 preprocessor its include path, and $SPECTEC_BOOT overrides the path to
# spectec-boot when it is not ./spectec-boot.
set -e

mkdir -p ./.tmp

p4=$(cat "$1")

# No program: the term is the constant `noP4()`, which needs no parsing beyond
# kast's own.  `--sort P4Opt` because that is the sort of the `<p4prog>` cell.
if [ -z "$p4" ]; then
  printf 'noP4()' > ./.tmp/spectec-k-nop4.$$
  kast --definition "$KDEF" --output kore --sort P4Opt ./.tmp/spectec-k-nop4.$$ \
    > ./.tmp/spectec-k-nop4.$$.kore
  status=$?
  cat ./.tmp/spectec-k-nop4.$$.kore
  rm -f ./.tmp/spectec-k-nop4.$$ ./.tmp/spectec-k-nop4.$$.kore
  exit $status
fi

# A program: boot it to a KAST JSON term, already wrapped as `someP4(...)`.
json=$(mktemp ./.tmp/spectec-k-p4-XXXXXX.json)

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
