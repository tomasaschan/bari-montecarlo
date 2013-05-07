#!/usr/bin/gnuplot

set terminal pngcairo
set output "reactions.png"

set xrange[0:1005]

set logscale y

datafile = "outdata/`ls outdata | tail -n 1`"

plot datafile   u 1:2 w lines t 'N_2+', \
        ''      u 1:3 w lines t 'N_2+(b)', \
        ''      u 1:4 w lines t 'N_2(c)'
