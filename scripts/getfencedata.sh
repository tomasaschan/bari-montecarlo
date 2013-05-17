#!/bin/bash

lastdata="outdata/`ls outdata | tail -n 1`"

rm -f "outdata/fences/*"

for i in {1..9..1}
do
    grep "$lastdata" -e "\#" > "outdata/fences/fencedata.$i.out"
    awkcmd="/eedf 0\.${i}0+E-08/ { print $i, \$2, \$3, \$4 }"
    cat $lastdata | awk "$awkcmd" >> "outdata-valuable/fencedata.$i.out"
done

for i in {1..9..1}
do
    grep "$lastdata" -e "\#" > "outdata/fences/fencedata.$i.out"
    awkcmd="/eedf 0\.${i}0+E-07/ { print ($i+9), \$2, \$3, \$4 }"
    cat $lastdata | awk "$awkcmd" >> "outdata/fences/fencedata.rest.out"
done