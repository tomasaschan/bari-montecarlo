#!/usr/bin/gnuplot

reset
set terminal pngcairo enhanced

set output "test.png"
set format x "%.1s"

awkfilter = '<awk ''/^rate/'' '

dfile1 = "outdata/`ls outdata | tail -n 2 | head -n 1`"
dfile2 = "outdata/`ls outdata | tail -n 1`"

print awkfilter.dfile1 
print awkfilter.dfile2

plot awkfilter.dfile1 u 2:3 w lines title '1 keV', awkfilter.dfile2 u 2:3 w lines title '500 eV'
