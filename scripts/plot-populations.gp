load 'plot-common.gp'
set output srcdir.'populations.png'

set key top left

# set logscale y

set xtics format "%.0s" 1e-9
set ytics format "%.1t e%T"

set xlabel "Time [ns]"
set ylabel "Population"

plot '<grep '.datafile.' -e pops'   u 2:3 w lines title 'N2+(b)', \
                                ''  u 2:4 w lines title 'N2(c)'
