#!/bin/bash

rm -f nist*.dat

for e in {15..199..5}
do
  curl --data T=$e physics.nist.gov/cgi-bin/Ionization/parsesv.pl?N2 >> nist.dat
done

for e in {200..1999..100}
do
  curl --data T=$e physics.nist.gov/cgi-bin/Ionization/parsesv.pl?N2 >> nist.dat
done

# Example line we're looking for:
# For N<sub>2</sub>, at 30 eV, <img src="/Images/sigma.gif" alt="sigma"><sub>BEB</sub> = 1.242 &Aring;<sup>2</sup>
# Example result after substitution:
# 30    1.242
grep nist.dat -e For | perl -pe 's|^.*?at ([\d\.]*) eV.*?([\d\.]*) \&.*|\1 \2|g' >> nist_clean.dat
