#!/bin/sh
# Run a self-contained SpecTec AL script with the K interpreter.
set -e

usage() {
  echo "usage: $0 SPEC" >&2
}

case ${1-} in
  -h|--help)
    usage
    exit 0
    ;;
  --)
    shift
    ;;
esac

if [ "$#" -ne 1 ]; then
  usage
  exit 2
fi

spec=$1
make --no-print-directory boot

KDEF=al-kompiled krun -d al-kompiled \
  --parser ./spec-meta-k/scripts/kast-spec.sh "$spec" \
  -cP4= -pP4=./spec-meta-k/scripts/kast-p4.sh \
  "-cSPEC=\"$spec\"" --output none
