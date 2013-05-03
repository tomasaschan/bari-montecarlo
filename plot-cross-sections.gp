#!/usr/bin/gnuplot

reset
set terminal pngcairo enhanced size 40cm,25cm
set output 'cs.png'


set xlabel 'Energy [eV]'
set ylabel 'Cross-section [cm^2]'
set xrange [10:`head -n 4 input.in | tail -n 1`*1.01]

set format y "%.1t * 10^{%T}"

e0 = "`head -n 4 input.in | tail -n 1`"
p = "`head -n 5 input.in | tail -n 1`"
tfin = "`head -n 2 input.in | tail -n 1`"
t = tfin
datafile = "outdata/`ls outdata | tail -n 1`"
awkcmd = 'awk ''/^eedf 0\.20+E-07/ {print $3,$5,$6,$7}'''
cmd = '<'.awkcmd.' '.datafile

energytitle = "Beam energy: ".gprintf("%.1s %c",e0+0)."eV"
timetitle = "Time: ".gprintf("%.1s %c",tfin+0)."s"
pressurettitle = "Pressure: ".gprintf("%.1s %c",p)."Torr"
set title energytitle.",  ".timetitle.",  ".pressurettitle
print cmd
plot cmd  \
        u 1:($2*1e4) w lines title 'Cross section: Ionization', \
    ''  u 1:($3*1e5) w lines title 'Cross section: N_2^+(b) [ampl: 10x]', \
    ''  u 1:($4*1e5) w lines title 'Cross section: N_2(c) [ampl: 10x]' 
