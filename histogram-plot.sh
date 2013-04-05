#!/bin/bash

my_title=$(head -n 1 $1 | sed 's/^# \(.*\)/\1/')

echo "set terminal pngcairo

set output 'energyhistogram.png'

set title '$my_title'

set xlabel 'Energy, eV'
set ylabel 'Probability, %'
set xrange [0:1000]

set logscale y


#plot 'histogram.out' using ($1 < 300 ? $1 : NaN):($2 > 0 ? $2 : NaN): xtic(50) notitle with lines
#plot 'histogram.out' using 1:($2 > 0 ? $2 : NaN): xtic(50) notitle with lines
plot 'histogram.out' using 1:2 : xtic(50) notitle with lines" | gnuplot
