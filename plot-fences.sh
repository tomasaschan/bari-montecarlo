#!/usr/bin/gnuplot --persist

reset
set terminal pngcairo
set output 'test.png'
set logscale z

#set view 0, 260

set ylabel 't'
set xlabel 'e'
set zlabel 'P'
set xtics 200 format "%.0f" rotate by -30
set ytics format "%.1s" rotate by -30
set xyplane .0

set key outside

AWKCMD='awk ''/eedf 0\.20+E-08/'' '
dfile = "outdata-valuable/fencedata.out" #'<'.AWKCMD.' outdata/'."`ls outdata | tail -n 1`"

ttl(t) = sprinf("%d ns", t)

splot for [tm=2:8:2] "<awk '/0\.".tm."0+E-08/' ".dfile u 2:1:3 t sprintf("%.0f", tm) w lines, \
 for [tm=10:20:2] "<awk '/0\.".tm."0+E-07/' ".dfile u 2:1:3 t sprintf("%.0f", tm) w lines
show view

#plot 'data/N2+B_1.spaces.txt' u 1:($2*1e-4) w lines title 'N_2^+(b)', \
#     'data/N2C_1.spaces.txt' u 1:($2*1e-4) w lines title 'N_2(c)', \
#     'data/N2_nist_scaled.dat' u 1:($2*1e-4) w lines title 'N_2 (nist)', \
#     'interptest.out' u 1:2 w lines title 'Interpolated N_2^+(b)', \
#     'interptest.out' u 1:3 w lines title 'Interpolated N_2(c)', \
#     'interptest.out' u 1:4 w lines title 'Interpolated N_2 (nist)'
