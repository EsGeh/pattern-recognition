#!/bin/bash

OUTPUT_DIR=prof
LAST_COMMON_ANCESTOR="$(git merge-base fastLearning_heuristics HEAD)"
OUTPUT_DIR="$OUTPUT_DIR/$(git log --oneline $LAST_COMMON_ANCESTOR | head -n1 | sed 's/ /_/g')"

echo "output dir: $OUTPUT_DIR"
mkdir -p -v "$OUTPUT_DIR"

COMPILER_OPTS="--library-profiling --executable-profiling --ghc-options=-fprof-auto"

RUNTIME_OPTS="+RTS -p -h -M2g -RTS"

# -p, -P, -pa:  general time and allocation profiling. Creates *.prof
# -h: basic heap profile. Creates *.hp
### -hc: by cost centre stack
### -hm: by module
### -hd: by closure descr.
### -hy: by type
### -xt: include memory occupied by threads (this should show "stack").

stack test $COMPILER_OPTS --test-arguments="$RUNTIME_OPTS" :neuralNetworks-test

find . -maxdepth 1 \( -name '*.prof' -or -name '*.hp' \) -exec mv {} "$OUTPUT_DIR" \;

#GRAPH_INPUTS="$(find \"$OUTPUT_DIR\" -name '*.hp'""

cd "$OUTPUT_DIR"
find . -name '*.hp' -exec stack exec hp2ps -- -c {} \;
find . -name '*.ps' -exec evince {} \;
cd ..

