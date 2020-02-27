#!/usr/bin/env bash

set -x
set -o pipefail

# only run tests matching this filter string:
BRANCH="$(git rev-parse --abbrev-ref HEAD)"
[[ "$BRANCH" =~ module ]] && DEFAULT_FILTER="$(echo "$BRANCH" | cut -d'-' -f2)" || DEFAULT_FILTER='version'
[[ -z "$1" ]] && TEST_FILTER="$DEFAULT_FILTER" || TEST_FILTER="$1"

if [[ -z "$TMPDIR" ]]; then
  export TMPDIR=$PWD/.stack-work/tmp
  mkdir -p "$TMPDIR"
fi

### build the binary ###

NIX_ARGS="--pure"
TIMESTAMP=$(date '+%Y-%m-%d_%H:%M')
LOGFILE="ortholang_${TEST_FILTER}_${TIMESTAMP}.log"

nix-run() {
  rm -f ortholang.log
  nix-shell shell.nix $NIX_ARGS --run "$@" 2>&1 | tee -a "$LOGFILE"
  code="$?"
  [[ $code == 0 ]] || cat ortholang.log | tee -a "$LOGFILE"
  return $code
}

nix-run "stack build"

### run tests ###

# other possible tasty settings: https://hackage.haskell.org/package/tasty
export TASTY_QUICKCHECK_TESTS=1000
export TASTY_COLOR="always"
export TASTY_QUICKCHECK_SHOW_REPLAY=True

STACK_CMD="stack exec ortholang --"
TEST_ARGS="--debug '.*' --test '$TEST_FILTER'"

# test using shared cache first because it's faster
nix-run "$STACK_CMD --shared http://shortcut.pmb.berkeley.edu/shared $TEST_ARGS"
code1="$?"

# then locally to verify everything really works
nix-run "$STACK_CMD $TEST_ARGS"
code2="$?"

# exit nonzero if either run failed
[[ $code1 == 0 ]] || exit $code1
exit $code2