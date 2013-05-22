load 'scripts/plot-common.gp'
set output srcdir.'populations'.ext

set key top left

# set logscale y

set xtics format "%.0s" 5e-9
#set ytics format "%.1t e%T"
set ytics nomirror tc lt 1
set y2tics tc lt 2

set xlabel "Time $\\left[\\si{\\nano\\second}\\right]$"
set ylabel "Population $\\left[\\si{\\per\\centi\\meter\\cubed}\\right]$"

plot srcdir.'pops.dat' u 1:($2) w lines title 'N2+(b)' axes x1y2, \
                   ''  u 1:($3) w lines title 'N2(c)' axes x1y1
