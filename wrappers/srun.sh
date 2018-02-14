#!/usr/bin/env bash

# This is a wrapper script for running individual commands on other nodes.
# It's meant to be called from ShortCut with all the same arguments that make
# up a regular shell command. It runs the command using srun, then waits for a
# result before exiting. If needed, it can alter the command before running
# it, for example to set the number of threads used. srun inherits options
# from the outer salloc script but they can be overridden here. Certain quick
# commands like `ln` are just run normally.

SRUN="srun --account=co_rosalind --partition=savio2_htc --qos=rosalind_htc2_normal"
SRUN="$SRUN --chdir $(pwd) --nodes=1-1 --ntasks=1"

# unset LIBRARY_PATH
# unset LD_LIBRARY_PATH

# print a message if log level is set (from within ShortCut or by the user)
# TODO send this to a file rather than stdout?
log() { [[ -z $SHORTCUT_LOGLEVEL ]] || echo "[$(date '+%Y-%m-%d %H:%M:%S')] $@"; }

# put any changes needed to optimize specific commands here
case "$(basename "$1")" in
	crb-blast) CMD="$SRUN --cpus-per-task=4 --time=99:00:00 $@ --verbose --split --threads=12"; log "$CMD";;
	parallelblast.py) CMD="$SRUN --cpus-per-task=4 --time=99:00:00 $@"; log "$CMD";;
	blastn|blastp|blastx|tblastn|tblastx|megablast) CMD="$SRUN --cpus-per-task=4 --time=99:00:00 $@"; log "$CMD";;
	cat|ln) CMD="$@";; # TODO does anything echoed here get sent to the output file??
	*) CMD="$@"; log "$CMD";; # run the command as-is in this shell
esac

$CMD