load 'plot-common.gp'
set output srcdir.'/eedf.png'

#set logscale y
set logscale y


set xlabel 'Energy [eV]'
set ylabel 'Probability [%]'
set format y "%.0te%T"
set xrange [10:`head -n 4 $infile | tail -n 1`*1.01]
#set yrange [1e-3:1e2]

set ytics nomirror
set y2tics
set format y2 "%.1t * 10^{%T}"
set logscale y2
set y2label "Integrand [arbitrary]" rotate by -90
set grid y2


e0 = "`head -n 4 $infile | tail -n 1`"
p = "`head -n 5 $infile | tail -n 1`"
tfin = "`head -n 2 $infile | tail -n 1`"
t = 0.5*tfin
awkcmd = 'awk ''/^eedf 0\.40+E-08/'''
cmd = '<'.awkcmd.' '.datafile

energytitle = "Beam energy: ".gprintf("%.1s %c",e0+0)."eV"
timetitle = "Time: ".gprintf("%.1s %c",tfin+0)."s"
pressurettitle = "Pressure: ".gprintf("%.1s %c",p)."Torr"
set title energytitle.",  ".timetitle.",  ".pressurettitle

print cmd

plot cmd u 3:4 w lines title 'eedf'
