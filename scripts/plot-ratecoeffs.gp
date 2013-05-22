load 'scripts/plot-common.gp'


set output srcdir.'ratecoeffs'.ext
set termoption dashed

set xlabel "Time $\\left[\\si{\\nano\\second}\\right]$"
set ylabel "Rate coefficients" # $\\left[\\si{\\centi\\meter\\cubed\\per\\second}\\right]$"
set y2label "Ratio" rotate by -90
#set yrange [5e-1:1e5]

#set logscale y
set format x "%.0s"
set format y "%.0te%T"
set grid 

set ytics nomirror 2e-9
set xtics nomirror

set y2tics nomirror tc rgb "green"

set key outside above 

plot srcdir.'rate.dat' \
        u 1:2 w lines lt 1 lc rgb "red" title 'N2+(b)', \
    ''  u 1:3 w lines lt 1 lc rgb "black" title 'N2+(c)', \
    ''  u 1:($2/$3) axes x1y2 w lines lt 2 lc rgb "green" title 'Ratio' 
    #   u 2:3 w lines lt 1 lc rgb "blue" title 'Ionization' , \
 
