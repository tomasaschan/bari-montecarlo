#!/usr/bin/gnuplot

reset
set terminal pngcairo enhanced
set output 'ratecoeffs.png'

#set logscale y
#set logscale xy

#set xlabel 'Energy [eV]'
#set ylabel 'Rate coefficient [?]'
#set format y "%.0te%T"
#set xrange [10:`head -n 4 input.in | tail -n 1`*1.1]
#set yrange [1e-3:1e2]

set logscale y
set format x "%.0s %cs"

e0 = "`head -n 4 input.in | tail -n 1`"
p = "`head -n 5 input.in | tail -n 1`"
tfin = "`head -n 2 input.in | tail -n 1`"
datafile = "outdata/`ls outdata | tail -n 1`"
awkcmd = 'awk ''( $1=='.tfin.' ) { print $2,$3; }'''
cmd = '<'.awkcmd.' '.datafile

energytitle = "Beam energy: ".gprintf("%.1s %c",e0+0)."eV"
timetitle = "Time: ".gprintf("%.1s %c",tfin+0)."s"
pressurettitle = "Pressure: ".gprintf("%.1s %c",p)."Torr"
set title energytitle.",  ".timetitle.",  ".pressurettitle

#print cmd


plot datafile \
      u 1:3 w lines lc rgb "red" title 'N2+(b) excitation', \
    ''  u 1:4 w lines lc rgb "green" title 'N2+(c) excitation'

#        u 1:2 w lines lc rgb "blue" title 'Ionization', \
