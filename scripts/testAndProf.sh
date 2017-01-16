#!/bin/bash

USER_OPTS=""
OUTPUT_SUFFIX=""

if [[ "$1" == "--help" ]]; then
	echo "usage: $0 [USER_OPTS] [OUTPUT_SUFFIX]"
	exit 1
fi

while [[ "$1" != "" && "$1" == [-]* ]] ; do
	# echo $1
	USER_OPTS="$USER_OPTS $1"
	shift
done
if [[ "$1" != "" && "$2" == "" ]]; then
	OUTPUT_SUFFIX="-$1"
elif [[ "$1" != "" ]]; then
	echo "usage: $0 [USER_OPTS] [OUTPUT_SUFFIX]"
	exit 1
fi

OUTPUT_DIR=prof
LAST_COMMON_ANCESTOR="$(git merge-base fastLearning_heuristics HEAD)"
OUTPUT_DIR="$OUTPUT_DIR/$(git log --oneline $LAST_COMMON_ANCESTOR | head -n1 | sed 's/ /_/g')"
OUTPUT_DIR="$OUTPUT_DIR$OUTPUT_SUFFIX"

echo "output dir: $OUTPUT_DIR"
if [[ -e "$OUTPUT_DIR" ]]; then
	read -p "folder \"$OUTPUT_DIR\" exists. overwrite?" -n 1 -r
	echo
	if [[ ! $REPLY =~ ^[Yy]$ ]]; then
		exit 1
	fi
fi
mkdir -p "$OUTPUT_DIR"

COMPILER_OPTS="--library-profiling --executable-profiling --ghc-options=-fprof-auto"

RUNTIME_OPTS="+RTS -p -h -M2g $USER_OPTS -RTS"

# -p, -P, -pa:  general time and allocation profiling. Creates *.prof
# -h: basic heap profile. Creates *.hp
### -hc: by cost centre stack
### -hm: by module
### -hd: by closure descr.
### -hy: by type
### -xt: include memory occupied by threads (this should show "stack").

git diff HEAD > "$OUTPUT_DIR/git-diff"
stack test $COMPILER_OPTS --test-arguments="$RUNTIME_OPTS" :neuralNetworks-test

find . -maxdepth 1 \( -name '*.prof' -or -name '*.hp' \) -exec mv {} "$OUTPUT_DIR" \;

#GRAPH_INPUTS="$(find \"$OUTPUT_DIR\" -name '*.hp'""

cd "$OUTPUT_DIR"
find . -name '*.hp' -exec stack exec hp2ps -- -c {} \;
find . -name '*.ps' -exec evince {} \;
cd ..

