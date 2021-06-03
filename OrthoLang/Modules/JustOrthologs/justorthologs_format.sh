#!/usr/bin/env bash

OUTPATH="$1"; shift
GFFPATH="$1"; shift
FAAPATH="$1"; shift

gff3_parser.py -g "$GFFPATH" -f "$FAAPATH" -o "$OUTPATH" &> stderr

# when it fails we get a one-line file that's just a newline
# any valid fasta file should have at least 2, right? header + sequence
# 1 line probably means the fasta input id hashing was messed up
nlines="$(wc -l "$OUTPATH" | awk '{print $1}' || echo 0)"
[[ $nlines -gt 1 ]] || (echo "ERROR: gff3_parser.py failed" && exit 1) # TODO better error
[[ -z stderr ]] && rm stderr
