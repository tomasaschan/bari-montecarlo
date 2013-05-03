#!/bin/bash

OUTDIR="outdata"
e0="`head -n 4 input.in | tail -n 1`"
e0unit="eV"
p="`head -n 5 input.in | tail -n 1`"
punit="Torr"
tfin="`head -n 2 input.in | tail -n 1`"


outfile="$OUTDIR/`ls $OUTDIR | tail -n 1`"
newfile="${outfile/outdata\/run_simulation_/outdata-valuable/$e0$e0unit-$p$punit-}"

echo "From: $outfile"
echo "  to: $newfile"
cp "$outfile" "$newfile"
