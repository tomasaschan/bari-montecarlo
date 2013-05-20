load 'scripts/plot-common.gp'
set output srcdir.'populations'.ext

set key top left

# set logscale y

set xtics format "%.0s" 5e-9
#set ytics format "%.1t e%T"

set xlabel "Time $\\left[\\si{\\nano\\second}\\right]$"
set ylabel "Population $\\left[10^{-2} \\si{\\per\\centi\\meter\\cubed}\\right]$"

plot '<grep '.datafile.' -e pops'   u 2:($3*1e2) w lines title 'N2+(b)', \
                                ''  u 2:($4*1e2) w lines title 'N2(c)'
