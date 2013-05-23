#!/bin/bash


if [[ "$1" -eq "" ]]
then
    run_id="`ls -t out | head -n 1`"
else
    run_id="$1"
fi

OUTDIR="out/$run_id"


cp "input.in" "${OUTDIR}"

echo -n "Running simulation..."

./runner < "input.in"

echo "done!"

mv *.dat $(OUTDIR)
