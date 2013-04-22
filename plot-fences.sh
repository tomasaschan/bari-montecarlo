#!/bin/bash

gnuplot << EOF

reset
set terminal pngcairo enhanced

set output 'cs_raw.png'

set logscale z
set view 100, 80

set xlabel 't'
set ylabel 'e'
set zlabel 'P'
set xtics ('0' 0, '1' 100, '2' 200)

AWKCMD='awk ''{if(!NF){print ""}else if(index(\$0,"#")!=1){printf "%s %s %s\n%s %s 0\n\n", \$1,\$2,\$3,\$1,\$2}}'' '

splot '<'.AWKCMD.' $1' u 1:2:3:(column(-2)) w l lc variable notitle


#plot 'data/N2+B_1.spaces.txt' u 1:($2*1e-4) w lines title 'N_2^+(b)', \
#     'data/N2C_1.spaces.txt' u 1:($2*1e-4) w lines title 'N_2(c)', \
#     'data/N2_nist_scaled.dat' u 1:($2*1e-4) w lines title 'N_2 (nist)', \
#     'interptest.out' u 1:2 w lines title 'Interpolated N_2^+(b)', \
#     'interptest.out' u 1:3 w lines title 'Interpolated N_2(c)', \
#     'interptest.out' u 1:4 w lines title 'Interpolated N_2 (nist)'
EOF
