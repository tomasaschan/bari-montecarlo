#!/bin/bash
gnuplot << EOF

set terminal pngcairo
set output 'energyhistogram.png'
#thetitle = "`head -n 1 $1 | sed 's/^# \(.*\)/\1/'`"
set xlabel 'Energy, eV'
set ylabel 'Probability, %'
set xrange [0:1100]

#set ytics nomirror
#set y2tics nomirror
set logscale y #y2
set yrange [1e-4:100]

me=9.11E-31
e=1.602176E-19
v(eV) = sqrt(2*eV*e/me)
plot '$1' using 1:2 notitle with lines \
      axes x1y1 #, \
#     '$2' using 1:(\$2 > 0 ? \$2 : NaN) w lines \
#      axes x1y2 linecolor rgb "blue" notitle , \
#     '$2' using 1:(\$2 > 0 ? 1/(3e22*v(1e3)*\$2) : NaN) w lines \
#      axes x1y2 linecolor rgb "green" notitle
EOF