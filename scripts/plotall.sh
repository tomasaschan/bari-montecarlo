#!/bin/bash

if [[ "$1" -eq "" ]]
then
    run_id="`ls -t out | head -n 1`"
else
    run_id="$1"
fi

OUTDIR="out/$run_id"

declare -a scripts=("eedf-evolution" "ratecoeffs" "ratequote" "populations")

echo "Working with run id: ${run_id}"

for scr in ${scripts[@]};
do
    echo "Plotting ${scr}"
    gnuplot -e "srcdir='${OUTDIR}'" "`pwd`/scripts/plot-${scr}.gp"
done
