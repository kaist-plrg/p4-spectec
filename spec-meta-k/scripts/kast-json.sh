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
KCACHE=$KSCRATCH/.kore-cache

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
  # The target spec is cached on a content hash.
  #
  # Three inputs go into the key. Missing any of them caches staleness:
  #
  #   the .watsup sources      what is being compiled
  #   $KDEF/definition.kore    the term is written against this symbol table,
  #                            so a re-kompile that moves a sort invalidates it
  #   spectec-boot             it is the emitter (lib/interface/.../kast.ml),
  #                            so editing the emitter must miss the cache
  boot="${SPECTEC_BOOT:-./spectec-boot}"
  key=$(
    { printf '%s\n' "$target"
      find "$target" -name '*.watsup' -type f | LC_ALL=C sort | xargs cat
      cat "$KDEF/definition.kore" "$boot"
    } 2>/dev/null | sha256sum | cut -d' ' -f1
  )
  cache=$KCACHE/$key.kore

  if [ -r "$cache" ]; then
    exec cat "$cache"
  fi

  mkdir -p $KCACHE
  json=$(mktemp $KSCRATCH/spectec-k-kast-XXXXXX.json)
  kore=$(mktemp $KCACHE/kore-XXXXXX.tmp)
  status=0
  { "$boot" kast "$target" -o "$json" \
      && kast --definition "$KDEF" --input json --output kore --sort Script \
           "$json" > "$kore"; } || status=$?
  rm -f "$json"

  if [ $status -ne 0 ]; then
    # Never cache a failed build: leave the miss in place so the next run retries.
    rm -f "$kore"
    exit $status
  fi

  # Rename last, so a concurrent run either sees no cache or a complete one.
  mv -f "$kore" "$cache"
  exec cat "$cache"
fi

json="$target"

exec kast --definition "$KDEF" --input json --output kore --sort Script "$json"
