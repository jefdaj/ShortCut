#!/usr/bin/env bash

set -x # TODO remove

OUTPATH="$1"; shift
OUTDIR="$(dirname "$OUTPATH")"
GFFPATH="$1"; shift
FAAPATH="$1"; shift

# mkdir -p "$OUTDIR"
# cd "$OUTDIR"
gff3_parser.py -g "$GFFPATH" -f "$FAAPATH" -o "$OUTPATH" 2> "$OUTDIR"/stderr
# cp "$OUTDIR"/* /tmp/

# when it fails we get a one-line file that's just a newline
# any valid fasta file should have at least 2, right? header + sequence
# 1 line probably means the fasta input id hashing was messed up
nlines="$(wc -l "$OUTPATH" | awk '{print $1}')"
[[ $nlines -gt 1 ]] || (echo "ERROR: gff3_parser.py failed" && exit 1) # TODO better error
