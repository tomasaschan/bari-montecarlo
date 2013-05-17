load 'plot-common.gp'

set output 'test.png'

cmd = "awk '/^eedf/ { print $2 }' ".datafile." | uniq"
ts = system(cmd)
thistime(t) = "<awk '/^eedf ".t."/' ".datafile

plot for [t in ts] thistime(t) u 3:4 title t