#!/usr/bin/gnuplot --persist

reset
set terminal pngcairo
set output 'eedf-evolution.png'
set logscale z

set view 80, 358

set ylabel 't'
set xlabel 'e'
set zlabel 'eedf' rotate by 90
set xtics 100 format "%.0f" rotate by -30
set ytics 5e-9 format "%.0s" rotate by -30
set ztics format "%.0te%T"
set xyplane .0
#set xrange[0:1005]
set key outside
#set yrange[0:12e-8]

dfile = "outdata/`ls outdata | tail -n 1`"
awkcmd(i,e) = "<awk '/eedf 0\\.".i."0+E-0".e."/' "
#"<awk '/eedf 0\.".i."0+E-08/' "


splot for [i=9:1:-1] awkcmd(i,8).dfile u 3:2:4 w lines notitle , \
      for [i=20:10:-1] awkcmd(i,7).dfile u 3:2:4 w lines notitle
