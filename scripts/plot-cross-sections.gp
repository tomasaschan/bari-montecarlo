set terminal pngcairo enhanced #size 40cm,25cm
set output 'out/cs.png'


set xlabel 'Energy [eV]'
set ylabel 'Cross-section [cm^2]'
set xrange [0:1000]
#set format y "%.1t * 10^{%T}"
#set ytics .25e-16



plot 'data/N2.dat' every ::2 u 1:2 w lines title 'Cross section: Ionization', \
     '<awk -F'','' ''NR > 1 { print $1, $2 }'' data/N2+B.dat' u 1:(10*$2) w lines title 'Cross section: N_2^+(b) [ampl: 10x]', \
     '<awk -F'','' ''NR > 1 { print $1, $2 }'' data/N2C-1.dat' u 1:($2*10) w lines title 'Cross section: N_2(c) [ampl: 10x]' 
