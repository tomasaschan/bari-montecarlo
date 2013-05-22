load 'scripts/plot-common.gp'
set output srcdir.'ratequote.png'


set xlabel "Time [ns]"
set ylabel "Rate coefficient quotient"
#set yrange [0:.1]

#set logscale y
set format x "%.0s"
#set format y "%.0te%T"
set grid 

set ytics nomirror
set xtics nomirror

set key bottom right

plot srcdir.'rate.dat' u 1:($2/$3) w lines lc rgb "blue" title 'N2+(c) / N2+(b)'
