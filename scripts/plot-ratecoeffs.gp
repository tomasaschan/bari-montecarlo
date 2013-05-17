load 'plot-common.gp'

set output srcdir.'ratecoeffs.png'

set xlabel "Time [ns]"
set ylabel "Rate coefficient"
#set yrange [5e-1:1e5]

set logscale y
set format x "%.0s"
set format y "%.0te%T"
set grid 

set ytics nomirror
set xtics nomirror

set key bottom right

plot '<grep '.datafile.' -e rate' \
        u 2:3 w lines lc rgb "blue" title 'Ionization' , \
    ''  u 2:4 w lines lc rgb "red" title 'N2+(b) excitation' , \
    ''  u 2:5 w lines lc rgb "green" title 'N2+(c) excitation'
