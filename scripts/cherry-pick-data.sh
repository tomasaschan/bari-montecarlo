#!/bin/bash

awk '/pops/ { print $2, $3 }' "out/$1/simulation.out" > "out/$1/N2b-pop.txt"
awk '/pops/ { print $2, $4 }' "out/$1/simulation.out" > "out/$1/N2c-pop.txt"

