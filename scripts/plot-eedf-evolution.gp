load 'scripts/plot-common.gp'

set output srcdir.'eedf-evolution'.ext
set logscale z

set view 80, 15

set ylabel 't'
set xlabel 'e' offset graph -0.01,-.4,0
set zlabel 'eedf' rotate by 90
set xtics 250 format "%.0f" rotate by -30 offset graph 0.01,-0.2,0
set ytics 5e-9 format "%.0s" rotate by -30
set ztics format "%.0te%T"
set xyplane .0
unset key

ts = system("awk '{ print $1 }' ".srcdir."eedf.dat | uniq | tac")
thistime(t) = '<awk ''/^ '.t.'/'' '.srcdir.'eedf.dat'

splot for [t in ts] thistime(t) u 2:1:3 w lines notitle # , \
#      for [i=20:10:-1] awkcmd(i,7).datafile u 3:2:4 w lines notitle
