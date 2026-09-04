#!/bin/sh
# Typecheck a P4 program by running it against the K SpecTec specification.
set -e

usage() {
  echo "usage: $0 P4_PROGRAM" >&2
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

p4=$1
spec=spec
specdir=$(mktemp ./spec-meta-k/spectec-k-specdir-XXXXXX)
trap 'rm -f "$specdir"' 0
printf '@%s\n' "$spec" > "$specdir"

make --no-print-directory boot

KDEF=al-kompiled P4INCLUDE=p4c/p4include krun -d al-kompiled \
  --parser ./spec-meta-k/scripts/kast-spec.sh "$specdir" \
  "-cP4=$p4" -pP4=./spec-meta-k/scripts/kast-p4.sh \
  "-cSPEC=\"$spec\"" --output none
