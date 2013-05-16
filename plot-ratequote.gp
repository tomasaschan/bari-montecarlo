#!/usr/bin/gnuplot

reset
set terminal pngcairo enhanced
set output 'ratequote.png'

e0 = "`head -n 4 input.in | tail -n 1`"
p = "`head -n 5 input.in | tail -n 1`"
tfin = "`head -n 2 input.in | tail -n 1`"
energytitle = "Beam energy: ".gprintf("%.1s %c",e0+0)."eV"
timetitle = "Time: ".gprintf("%.1s %c",tfin+0)."s"
pressurettitle = "Pressure: ".gprintf("%.1s %c",p)."Torr"
set title energytitle.",  ".timetitle.",  ".pressurettitle

set xlabel "Time [ns]"
set ylabel "Rate coefficient quotient"
#set yrange [0:.1]

set logscale y
set format x "%.0s"
#set format y "%.0te%T"
set grid 

set ytics nomirror
set xtics nomirror

set key bottom right

plot '<make showlastoutput | grep rate' u 2:($5/$4) w lines lc rgb "blue" title 'N2+(c) / N2+(b)'
