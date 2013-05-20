#!/usr/bin/gnuplot

reset
set terminal pngcairo enhanced
set output 'ratecoeff-ratios'.ext

p = 50e-3
tfin = 20e-9
timetitle = "Time: ".gprintf("%.1s %c",tfin+0)."s"
pressurettitle = "Pressure: ".gprintf("%.1s %c",p)."Torr"
set title timetitle.",  ".pressurettitle

set xlabel "Time [ns]"
set ylabel "Ratio"
#set yrange [5e-1:1e5]

set logscale y
set format x "%.0s"
#set format y "%.0te%T"
set grid 

set ytics nomirror
set xtics nomirror

set border 3
set key bottom right invert reverse

prefix = '<awk ''/^rate /'' outdata-ratios/'

plot prefix.'1keV.out' u 2:($5/$4) w lines title '1000 eV', \
     prefix.'500ev.out' u 2:($5/$4) w lines title '500 eV', \
     prefix.'200eV.out' u 2:($5/$4) w lines title '200 eV', \
     prefix.'100eV.out' u 2:($5/$4) w lines title '100 eV'