set terminal pngcairo enhanced font 'CM Roman Greek' #size 40cm,25cm
set output 'cs.png'


set xlabel 'Energy [eV]'
set ylabel 'Cross-section [cm^2]'
set xrange [0:1000]
#set format y "%.1t * 10^{%T}"
#set ytics .25e-16

#'N2.dat' every ::2 u 1:2 w lines title 'Cross section: Ionization', \

plot 'N2A3.dat' every ::2 w lines title 'A^3\Sigma_u^+' lc rgb "black", \
     'N2B3.dat' every ::2 w lines title 'N_2^-(B)', \
     'N2Bprime3.dat' every ::2 w lines title 'N_2(B'')', \
     'N2W3u.dat' every ::2 w lines title 'N_2(W)', \
     '<awk -F'','' ''NR > 1 { print $1, $2 }'' N2+B.dat' w lines title 'Included: N_2^+(b)' lw 3, \
     '<awk -F'','' ''NR > 1 { print $1, $2 }'' N2C-1.dat' w lines title 'Included: N_2(c)' lw 3 lc rgb "red"
