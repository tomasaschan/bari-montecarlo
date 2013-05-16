set terminal pngcairo
set output 'populations.png'

set key top left

# set logscale y

set xtics format "%.0s" 1e-9
set ytics format "%.1s %c"

set xlabel "Time [ns]"

plot '<make showlastoutput | grep pops' u 2:3 w lines title 'N2+(b)', \
                                     '' u 2:4 w lines title 'N2(c)'
