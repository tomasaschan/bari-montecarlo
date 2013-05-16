set terminal pngcairo
set output "sanitycheck.png"


plot '<make showlastoutput | grep "e0(t)"' u 2:3 w lines title "EEDF at e0"
