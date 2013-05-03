#!/bin/bash

lastdata="outdata/`ls outdata | tail -n 1`"

grep "$lastdata" -e "\#" > outdata-valuable/fencedata.out

for i in {2..8..2}
do
    awkcmd="/eedf 0\.${i}0+E-08/ { print \$2, \$3, \$4 }"
    cat $lastdata | awk "$awkcmd" >> outdata-valuable/fencedata.out
done

for i in {10..20..2}
do
    awkcmd="/eedf 0\.${i}0+E-07/ { print \$2, \$3, \$4 }"
    cat $lastdata | awk "$awkcmd" >> outdata-valuable/fencedata.out
done
