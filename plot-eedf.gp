#!/usr/bin/gnuplot

reset
set terminal pngcairo enhanced size 40cm,25cm
set output 'eedf.png'

#set logscale y
set logscale y


set xlabel 'Energy [eV]'
set ylabel 'Probability [%]'
set format y "%.0te%T"
set xrange [10:`head -n 4 input.in | tail -n 1`*1.01]
set yrange [1e-3:1e2]

set ytics nomirror
set y2tics
set format y2 "%.1t * 10^{%T}"
set logscale y2
set y2label "Integrand [arbitrary]" rotate by -90
set grid y2


e0 = "`head -n 4 input.in | tail -n 1`"
p = "`head -n 5 input.in | tail -n 1`"
tfin = "`head -n 2 input.in | tail -n 1`"
t = 0.5*tfin
datafile = "outdata/`ls outdata | tail -n 1`"
awkcmd = 'awk ''/^eedf 0\.40+E-08/'''
cmd = '<'.awkcmd.' '.datafile

energytitle = "Beam energy: ".gprintf("%.1s %c",e0+0)."eV"
timetitle = "Time: ".gprintf("%.1s %c",tfin+0)."s"
pressurettitle = "Pressure: ".gprintf("%.1s %c",p)."Torr"
set title energytitle.",  ".timetitle.",  ".pressurettitle

print cmd

plot cmd u 3:4 w lines title 'eedf', \
    '' u 3:8 axes x1y2 w lines title 'Integrand: Ionization' , \
    '' u 3:9 axes x1y2 w lines title 'Integrand: N_2^+(b)' , \
    '' u 3:10 axes x1y2 w lines title 'Integrand: N_2(c)', \
    '' u 3:($5*1e4) axes x1y2 w lines title 'Cross section: Ionization', \
    '' u 3:($6*1e4) axes x1y2 w lines title 'Cross section: N_2^+(b)', \
    '' u 3:($7*1e4) axes x1y2 w lines title 'Cross section: N_2(c)' 
