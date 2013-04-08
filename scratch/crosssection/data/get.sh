#!/bin/bash

rm -f nist.dat

for e in {15..200..5}
do
  curl --data T=$e physics.nist.gov/cgi-bin/Ionization/parsesv.pl?N2 >> nist.dat
done

for e in {200..2000..100}
do
  curl --data T=$e physics.nist.gov/cgi-bin/Ionization/parsesv.pl?N2 >> nist.dat
done

cat nist.dat | ./grepperl.sh