#!/usr/bin/gnuplot

reset
set terminal pngcairo enhanced
set output 'ratecoeffs.png'

datafile = "outdata/`ls outdata | tail -n 1`"
awkcmd = 'awk ''/^rate/'''
cmd = '<'.awkcmd.' '.datafile

e0 = "`head -n 4 input.in | tail -n 1`"
p = "`head -n 5 input.in | tail -n 1`"
tfin = "`head -n 2 input.in | tail -n 1`"
energytitle = "Beam energy: ".gprintf("%.1s %c",e0+0)."eV"
timetitle = "Time: ".gprintf("%.1s %c",tfin+0)."s"
pressurettitle = "Pressure: ".gprintf("%.1s %c",p)."Torr"
set title energytitle.",  ".timetitle.",  ".pressurettitle

set xlabel "Time [ns]"
set ylabel "Rate coefficient"
set yrange [1e-2:1e4]

set logscale y
set format x "%.0s"
set format y
set grid 

set ytics nomirror
set xtics nomirror

set key bottom right

plot cmd \
        u 2:3 w lines lc rgb "blue" title 'Ionization', \
    ''  u 2:4 w lines lc rgb "red" title 'N2+(b) excitation' , \
    ''  u 2:5 w lines lc rgb "green" title 'N2+(c) excitation'
