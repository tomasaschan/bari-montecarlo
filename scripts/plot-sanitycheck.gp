set terminal pngcairo
set output "sanitycheck.png"


set xlabel "Time [ns]"
set xtics format "%.0s"
set ytics format "%.0te%T"
plot '<make showlastoutput | grep "e0(t)"' u 2:3 w lines title "Particles at e0"
